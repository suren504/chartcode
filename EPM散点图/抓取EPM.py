"""用 Python 3.8+ 下载实际 EPM 并保存为 XLSX。"""

from __future__ import annotations

import argparse
import ast
import gzip
import json
import re
import sys
import time
import zlib
from contextlib import closing
from http.client import HTTPException, HTTPSConnection
from pathlib import Path
from tempfile import TemporaryDirectory
from urllib.parse import urljoin, urlsplit, urlunsplit
from xml.etree.ElementTree import Element, SubElement, tostring
from zipfile import ZIP_DEFLATED, ZipFile


URL = "https://dunksandthrees.com/epm/actual?heatmapStyle=fill"

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36"
    ),
    "Accept": (
        "text/html,application/xhtml+xml,application/xml;"
        "q=0.9,image/avif,image/webp,*/*;q=0.8"
    ),
    "Accept-Language": "en-US,en;q=0.9",
    "Cache-Control": "no-cache",
    "Pragma": "no-cache",
    "Referer": "https://dunksandthrees.com/",
    "Upgrade-Insecure-Requests": "1",
}

OUTPUT_PATH = Path(__file__).resolve().parent / "dunksandthrees_epm_actual.xlsx"


def extract_balanced_array(text: str, marker: str = "stats:") -> str:
    """
    从 SvelteKit 注入脚本中定位 stats: [ ... ]，
    用括号配对提取完整数组，避免用简单正则截断。
    """
    marker_pos = text.find(marker)
    if marker_pos == -1:
        raise RuntimeError(f"未找到 {marker!r}")

    start = text.find("[", marker_pos)
    if start == -1:
        raise RuntimeError("找到了 stats:，但后面没有找到 '['")

    depth = 0
    in_string = False
    quote = ""
    escaped = False

    for i in range(start, len(text)):
        ch = text[i]

        if in_string:
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == quote:
                in_string = False
            continue

        if ch in ('"', "'"):
            in_string = True
            quote = ch
        elif ch == "[":
            depth += 1
        elif ch == "]":
            depth -= 1
            if depth == 0:
                return text[start:i + 1]

    raise RuntimeError("啊哦，stats 数组没有找到匹配的结束 ']'")


def parse_js_literal(js_text: str) -> object:
    """
    将当前页面 stats 中的 JavaScript object literal
    转成 Python 可由 ast.literal_eval 解析的形式。

    处理：
    - 裸 key: player_id: -> "player_id":
    - null / undefined / void 0 -> None
    - true / false -> True / False
    - NaN / Infinity -> None
    - .123 / -.123 可直接由 Python literal_eval 解析
    """
    s = js_text

    # 给对象中的裸 key 加引号
    s = re.sub(
        r'([,{]\s*)([A-Za-z_$][A-Za-z0-9_$]*)(\s*:)',
        r'\1"\2"\3',
        s,
    )

    # JS 常量 -> Python 常量
    replacements = [
        (r"\bvoid\s+0\b", "None"),
        (r"\bundefined\b", "None"),
        (r"\bnull\b", "None"),
        (r"\btrue\b", "True"),
        (r"\bfalse\b", "False"),
        (r"\bNaN\b", "None"),
        (r"(?<![\w.])-?Infinity\b", "None"),
    ]

    for pattern, repl in replacements:
        s = re.sub(pattern, repl, s)

    try:
        return ast.literal_eval(s)
    except Exception as exc:
        preview = s[:1000]
        raise RuntimeError(
            "stats 已成功提取，但 JS object literal 转 Python 失败。\n"
            f"错误: {exc}\n"
            f"转换后开头 1000 字符:\n{preview}"
        ) from exc


def decompress_body(body: bytes, encoding: str) -> bytes:
    """按 Content-Encoding 解压响应体。"""
    encoding = encoding.strip().lower()
    if encoding in ("", "identity"):
        return body
    if encoding in ("gzip", "x-gzip"):
        return gzip.decompress(body)
    if encoding == "deflate":
        try:
            return zlib.decompress(body)
        except zlib.error:
            return zlib.decompress(body, -zlib.MAX_WBITS)  # raw deflate
    raise RuntimeError(f"不支持的 Content-Encoding: {encoding}")


def request_html(url: str, encoding: str) -> str:
    headers = {**HEADERS, "Accept-Encoding": encoding}
    for _ in range(5):
        address = urlsplit(url)
        if address.scheme != "https" or address.hostname is None:
            raise RuntimeError(f"不支持的 HTTPS 地址：{url}")
        target = urlunsplit(("", "", address.path or "/", address.query, ""))
        with closing(HTTPSConnection(address.hostname, address.port, timeout=30)) as connection:
            connection.request("GET", target, headers=headers)
            with connection.getresponse() as response:
                location = response.headers.get("Location")
                if response.status in (301, 302, 303, 307, 308) and location:
                    url = urljoin(url, location)
                    continue
                if response.status != 200:
                    raise RuntimeError(f"无法读取实际 EPM 页面，HTTP {response.status}")
                body = decompress_body(response.read(), response.headers.get("Content-Encoding", ""))
                return body.decode(response.headers.get_content_charset() or "utf-8")
    raise RuntimeError("页面重定向次数过多")


def fetch_html(url: str) -> str:
    for attempt, encoding in enumerate(("gzip", "identity", "gzip")):
        try:
            html = request_html(url, encoding)
            break
        except (HTTPException, OSError, EOFError, zlib.error) as exc:
            if attempt == 2:
                raise RuntimeError(f"重试后仍未收到完整的 EPM 页面：{exc}") from exc
            time.sleep(attempt + 1)
    if re.search(r"Just a moment|cf-chl|challenge-platform", html, re.I):
        raise RuntimeError("疑似遇到 Cloudflare challenge，停止解析")
    return html


def parse_actual_stats(html: str) -> list[dict[str, object]]:
    raw_rows = extract_balanced_array(html)
    after_rows = html.index(raw_rows, html.index("stats:")) + len(raw_rows)
    schema = re.match(r"\s*,\s*k\s*:\s*(\{[^{}]*\})", html[after_rows:])
    if schema is None:
        raise RuntimeError("EPM 数据缺少字段索引 k")

    columns = parse_js_literal(schema[1])
    if (
        not isinstance(columns, dict)
        or not {"player_id", "player_name", "tot"} <= columns.keys()
        or any(not isinstance(key, str) or type(index) is not int for key, index in columns.items())
        or set(columns.values()) != set(range(len(columns)))
    ):
        raise RuntimeError("实际 EPM 的字段索引 k 格式不正确")

    rows = parse_js_literal(raw_rows)
    if not isinstance(rows, list) or any(not isinstance(row, list) or len(row) != len(columns) for row in rows):
        raise RuntimeError("实际 EPM 数据行与字段索引 k 不匹配")
    return [{key: row[index] for key, index in columns.items()} for row in rows]


SPREADSHEET_NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
OFFICE_REL_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
PACKAGE_REL_NS = "http://schemas.openxmlformats.org/package/2006/relationships"
CONTENT_TYPES_NS = "http://schemas.openxmlformats.org/package/2006/content-types"
SPREADSHEET_TYPE = "application/vnd.openxmlformats-officedocument.spreadsheetml"


def column_letters(index: int) -> str:
    letters = ""
    while index:
        index, digit = divmod(index - 1, 26)
        letters = chr(ord("A") + digit) + letters
    return letters


def write_cell(row: Element, reference: str, value: object) -> None:
    if value is None:
        return
    cell = SubElement(row, "c", {"r": reference})
    if isinstance(value, bool):
        cell.set("t", "b")
        SubElement(cell, "v").text = "1" if value else "0"
    elif isinstance(value, (int, float)):
        SubElement(cell, "v").text = str(value)
    else:
        if isinstance(value, (dict, list)):
            value = json.dumps(value, ensure_ascii=False, separators=(",", ":"))
        cell.set("t", "inlineStr")
        text = SubElement(SubElement(cell, "is"), "t", {"xml:space": "preserve"})
        text.text = str(value)


def write_workbook(output_path: Path, rows: list[dict[str, object]]) -> None:
    fields = list(rows[0])
    worksheet = Element("worksheet", {"xmlns": SPREADSHEET_NS})
    letters = [column_letters(index) for index in range(1, len(fields) + 1)]
    SubElement(worksheet, "dimension", {"ref": f"A1:{letters[-1]}{len(rows) + 1}"})
    sheet_data = SubElement(worksheet, "sheetData")
    values = [fields] + [[record[field] for field in fields] for record in rows]
    for row_number, record in enumerate(values, 1):
        row = SubElement(sheet_data, "row", {"r": str(row_number)})
        for column, value in zip(letters, record):
            write_cell(row, f"{column}{row_number}", value)

    parts = {
        "[Content_Types].xml": (
            f'<Types xmlns="{CONTENT_TYPES_NS}">'
            '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
            '<Default Extension="xml" ContentType="application/xml"/>'
            f'<Override PartName="/xl/workbook.xml" ContentType="{SPREADSHEET_TYPE}.sheet.main+xml"/>'
            f'<Override PartName="/xl/worksheets/sheet1.xml" ContentType="{SPREADSHEET_TYPE}.worksheet+xml"/>'
            '</Types>'
        ),
        "_rels/.rels": (
            f'<Relationships xmlns="{PACKAGE_REL_NS}">'
            f'<Relationship Id="rId1" Type="{OFFICE_REL_NS}/officeDocument" Target="xl/workbook.xml"/>'
            '</Relationships>'
        ),
        "xl/workbook.xml": (
            f'<workbook xmlns="{SPREADSHEET_NS}" xmlns:r="{OFFICE_REL_NS}">'
            '<sheets><sheet name="实际 EPM" sheetId="1" r:id="rId1"/></sheets></workbook>'
        ),
        "xl/_rels/workbook.xml.rels": (
            f'<Relationships xmlns="{PACKAGE_REL_NS}">'
            f'<Relationship Id="rId1" Type="{OFFICE_REL_NS}/worksheet" Target="worksheets/sheet1.xml"/>'
            '</Relationships>'
        ),
        "xl/worksheets/sheet1.xml": tostring(worksheet, encoding="utf-8", xml_declaration=True),
    }
    with TemporaryDirectory(dir=output_path.parent) as directory:
        temporary = Path(directory) / output_path.name
        with ZipFile(temporary, "w", compression=ZIP_DEFLATED) as workbook:
            for name, content in parts.items():
                workbook.writestr(name, content)
        temporary.replace(output_path)


def main(url: str = URL, output_path: Path = OUTPUT_PATH, *, html_path: Path | None = None) -> None:
    html = fetch_html(url) if html_path is None else html_path.read_text(encoding="utf-8-sig")
    stats = parse_actual_stats(html)
    if not stats:
        raise RuntimeError("stats 解析成功，但数组为空")
    write_workbook(output_path, stats)
    print(f"Saved {len(stats)} rows and {len(stats[0])} fields to {output_path}")


def cli() -> None:
    for stream in (sys.stdout, sys.stderr):
        if hasattr(stream, "reconfigure"):
            stream.reconfigure(errors="backslashreplace")
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=OUTPUT_PATH, help="XLSX 路径，默认保存到脚本所在目录")
    parser.add_argument("--html", type=Path, help="直接解析已保存的页面 HTML")
    args = parser.parse_args()
    try:
        main(output_path=args.output.expanduser(), html_path=args.html)
    except PermissionError as exc:
        parser.exit(1, f"无法访问文件，请检查目录权限或关闭已打开的输出表格：{exc}\n")
    except (OSError, ValueError, RuntimeError) as exc:
        parser.exit(1, f"导出失败：{exc}\n")


if __name__ == "__main__":
    cli()


