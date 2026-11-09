"""统一入口：macOS 优先 Swift，无可用 Swift 时使用跨平台 Pillow。"""

import argparse
from functools import lru_cache
import json
import math
import os
from pathlib import Path
import shutil
import subprocess
import sys

BASE = Path(__file__).resolve().parent
SCALE = 2
INK = (44, 62, 80, 255)
SOFT = (86, 100, 115, 255)
RULE = (44, 62, 80, 36)
ACCENT = (217, 83, 79, 255)


def load_inputs(data_file, config_file=BASE / "绘图配置.json"):
    if not config_file.is_file():
        raise ValueError("请用 --config 指定当次图表配置")
    config = json.loads(config_file.read_text(encoding="utf-8-sig"))
    if not all(isinstance(config.get(key), str) for key in ("title", "subtitle", "playerLabel")):
        raise ValueError("标题、副标题和对象表头必须是文字")
    labels = config.get("columnLabels")
    if not isinstance(labels, list) or len(labels) != 3 or not all(isinstance(v, str) for v in labels):
        raise ValueError("columnLabels 必须包含三个文字表头")
    overlay = config.get("paperWhiteOverlay")
    if type(overlay) not in (int, float) or not math.isfinite(overlay) or not 0 <= overlay <= 1:
        raise ValueError("paperWhiteOverlay 必须在 0 到 1 之间")
    rows = json.loads(data_file.read_text(encoding="utf-8-sig"))
    if not isinstance(rows, list) or not rows:
        raise ValueError("数据必须是非空 JSON 数组")
    for i, row in enumerate(rows, 1):
        if not isinstance(row, dict):
            raise ValueError(f"第 {i} 项必须是对象")
        if not isinstance(row.get("name"), str) or not row["name"].strip():
            raise ValueError(f"第 {i} 项缺少对象名称")
        for key in ("on", "off"):
            value = row.get(key)
            if type(value) not in (int, float) or not math.isfinite(value):
                raise ValueError(f"第 {i} 项 {key} 必须是有限数值")
        difference = row["on"] - row["off"]
        if row.get("on_off") is None:
            row["on_off"] = difference
        if (type(row["on_off"]) not in (int, float) or not math.isfinite(row["on_off"])
                or abs(difference - row["on_off"]) >= 0.02):
            raise ValueError(f"第 {i} 项差值与两项原始数值不一致")
        minutes = row.get("minutes")
        if minutes is not None and (type(minutes) not in (int, float) or not math.isfinite(minutes) or minutes < 0):
            raise ValueError(f"第 {i} 项分钟数无效")
        for key in ("avatar", "logo"):
            path = row.get(key)
            if path is not None and (not isinstance(path, str) or not (data_file.parent / path).is_file()):
                raise ValueError(f"第 {i} 项无法读取素材：{path}")
    return config, sorted(rows, key=lambda row: (-row["on_off"], row["name"]))


def render_python(data_file, output, config, rows):
    # Swift 可用时不会导入 Pillow，也不需要安装它。
    try:
        from PIL import Image, ImageChops, ImageDraw, ImageFont, ImageOps
    except ImportError as exc:
        raise ValueError('Python 引擎需要 Pillow，请先运行：python -m pip install -r "'
                         + str(BASE / "requirements.txt") + '"') from exc

    fonts_dir = Path(os.environ.get("WINDIR", "C:/Windows")) / "Fonts"
    mac_fonts = Path("/System/Library/Fonts")
    pingfang = [(mac_fonts / "PingFang.ttc", 0)]
    assets = Path("/System/Library/AssetsV2/com_apple_MobileAsset_Font7")
    if sys.platform == "darwin" and assets.is_dir():
        pingfang.extend((p, 0) for p in assets.glob("*/AssetData/PingFang.ttc"))

    def choose(candidates, label):
        for path, index in candidates:
            if path.is_file():
                try:
                    ImageFont.truetype(str(path), 20, index=index)
                    return str(path), index
                except OSError:
                    continue
        raise ValueError(f"找不到{label}字体；Windows 请安装微软雅黑，Linux 请安装 DejaVu Sans Mono 和 Noto Sans CJK")

    regular = choose([(fonts_dir / "consola.ttf", 0), (mac_fonts / "Menlo.ttc", 0),
                      (Path("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"), 0)], "英文")
    bold = choose([(fonts_dir / "consolab.ttf", 0), (mac_fonts / "Menlo.ttc", 1),
                   (Path("/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf"), 0)], "英文粗体")
    all_text = "".join([config["title"], config["subtitle"], config["playerLabel"],
                        *config["columnLabels"], *(row["name"] for row in rows)])
    needs_cjk = any(ord(c) > 127 for c in all_text)
    cjk_candidates = [(fonts_dir / "msyh.ttc", 0), *pingfang,
                      (mac_fonts / "Supplemental/Songti.ttc", 0),
                      (Path("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc"), 0)]
    cjk = choose(cjk_candidates, "中文") if needs_cjk else regular
    cjk_bold = choose([(fonts_dir / "msyhbd.ttc", 0), *pingfang,
                       (mac_fonts / "Supplemental/Songti.ttc", 1),
                       (Path("/usr/share/fonts/opentype/noto/NotoSansCJK-Bold.ttc"), 0)], "中文粗体") if needs_cjk else bold

    @lru_cache(maxsize=128)
    def font(size, heavy, chinese):
        path, index = (cjk_bold if heavy else cjk) if chinese else (bold if heavy else regular)
        return ImageFont.truetype(path, max(1, round(size * SCALE)), index=index)

    def runs(value, size, heavy):
        # 显式分配中英文字体，避免 Pillow 缺少系统级字体回退而显示方框。
        result = []
        for char in value:
            chinese = ord(char) > 127
            if result and result[-1][0] == chinese:
                result[-1][1] += char
            else:
                result.append([chinese, char])
        return [(part, font(size, heavy, chinese)) for chinese, part in result]

    def text_width(value, size, heavy=False):
        return sum(f.getlength(part) for part, f in runs(value, size, heavy)) / SCALE

    width, height = 1452, 184 + len(rows) * 54
    canvas = Image.new("RGBA", (width * SCALE, height * SCALE))

    def text(value, x, y, size, color, heavy=False, align="left"):
        if not value:
            return
        parts = runs(value, size, heavy)
        advance = sum(f.getlength(part) for part, f in parts)
        px = x * SCALE - (advance if align == "right" else advance / 2 if align == "center" else 0)
        # 共用基线，同时按首字可见边缘修正标题与副标题的左对齐。
        if align == "left":
            px -= parts[0][1].getbbox(parts[0][0], anchor="ls")[0]
        baseline = (y + size) * SCALE
        draw = ImageDraw.Draw(canvas)
        for part, f in parts:
            draw.text((round(px), round(baseline)), part, font=f, fill=color, anchor="ls")
            px += f.getlength(part)

    def line(x1, y1, x2, y2, color, thickness=1, dashed=False):
        draw = ImageDraw.Draw(canvas)
        weight = max(1, round(thickness * SCALE))
        if dashed:
            pos = y1
            while pos < y2:
                draw.line((round(x1 * SCALE), round(pos * SCALE), round(x2 * SCALE),
                           round(min(pos + 4, y2) * SCALE)), fill=color, width=weight)
                pos += 10
        else:
            points = (round(x1 * SCALE), round(y1 * SCALE), round(x2 * SCALE), round(y2 * SCALE))
            draw.line(points, fill=color, width=weight)
            if thickness > 1:
                radius = thickness * SCALE / 2
                for x, y in ((x1 * SCALE, y1 * SCALE), (x2 * SCALE, y2 * SCALE)):
                    draw.ellipse((round(x-radius), round(y-radius), round(x+radius), round(y+radius)), fill=color)

    def box(x, y, w, h):
        return tuple(round(v * SCALE) for v in (x, y, x+w, y+h))

    @lru_cache(maxsize=16)
    def picture(path, size, circle):
        with Image.open(data_file.parent / path) as source:
            source = ImageOps.exif_transpose(source).convert("RGBA")
        pixels = round(size * SCALE)
        if circle:
            # 与 Swift 一致：填满方框并顶部对齐，保留头像头顶。
            factor = max(pixels / source.width, pixels / source.height)
            source = source.resize((max(pixels, round(source.width * factor)),
                                    max(pixels, round(source.height * factor))), Image.Resampling.LANCZOS)
            left = (source.width - pixels) // 2
            source = source.crop((left, 0, left+pixels, pixels))
            mask = Image.new("L", (pixels, pixels))
            ImageDraw.Draw(mask).ellipse((0, 0, pixels-1, pixels-1), fill=255)
            source.putalpha(ImageChops.multiply(source.getchannel("A"), mask))
            return source
        factor = min(pixels / source.width, pixels / source.height)
        source = source.resize((max(1, round(source.width * factor)),
                                max(1, round(source.height * factor))), Image.Resampling.LANCZOS)
        layer = Image.new("RGBA", (pixels, pixels))
        layer.alpha_composite(source, ((pixels-source.width)//2, (pixels-source.height)//2))
        return layer

    def marker(row, x, y, color, faded):
        ImageDraw.Draw(canvas).ellipse(box(x-22, y-22, 44, 44), fill=(0, 0, 0, 0))
        avatar, logo = row.get("avatar"), row.get("logo")
        if avatar or logo:
            layer = Image.new("RGBA", (68*SCALE, 68*SCALE))
            if avatar and logo:
                layer = picture(logo, 68, False).copy()
                layer.putalpha(layer.getchannel("A").point(lambda a: round(a * 0.75)))
            layer.alpha_composite(picture(avatar or logo, 40, bool(avatar)), (28, 28))
            mask = Image.new("L", layer.size)
            ImageDraw.Draw(mask).ellipse(box(14, 14, 40, 40), fill=255)
            alpha = ImageChops.multiply(layer.getchannel("A"), mask)
            if faded:
                alpha = alpha.point(lambda a: round(a * 0.5))
            layer.putalpha(alpha)
            canvas.alpha_composite(layer, (round((x-34)*SCALE), round((y-34)*SCALE)))
        # Pillow 轮廓位于边界以内，外扩半个线宽以对齐 Swift 居中描边。
        ImageDraw.Draw(canvas).ellipse(box(x-22.2, y-22.2, 44.4, 44.4),
                                       outline=(*SOFT[:3], 128) if faded else color, width=round(2.4*SCALE))

    def number(value, signed=False):
        # Swift 的四舍五入：中点远离零，避免 Python 默认的银行家舍入。
        rounded = math.copysign(math.floor(abs(value)*10 + 0.5) / 10, value)
        rounded = 0.0 if rounded == 0 else rounded
        return ("+" if signed and rounded > 0 else "") + f"{rounded:.1f}"

    values = [row[key] for row in rows for key in ("on", "off")]
    low, high = min(values), max(values)
    padding = max(abs(low)*0.025, 1) if low == high else (high-low)*0.035
    minimum, maximum = low-padding, high+padding

    def x(value):
        return 530 + (value-minimum)/(maximum-minimum)*(width-110-530)

    raw = (maximum-minimum)/6
    magnitude = 10 ** math.floor(math.log10(raw))
    normalized = raw/magnitude
    step = next(v for v in (1, 2, 5, 10) if normalized <= v) * magnitude
    text(config["title"], 40, 17, 32, INK, True)
    text(config["subtitle"], 40, 68, 16, SOFT, True)
    text(config["playerLabel"], 40, 129, 11, SOFT)
    for index, label in enumerate(config["columnLabels"]):
        right = 310 + index*80
        color, center = (ACCENT if index == 2 else SOFT), right-5
        text(label, right-13, 129, 11, color, align="right")
        segments = [(center, 130, center, 142), (center, 142, center-3, 139), (center, 142, center+3, 139)] if index == 2 else [
            (center-2, 135, center-2, 130), (center-2, 130, center-4, 132), (center-2, 130, center, 132),
            (center+2, 137, center+2, 142), (center+2, 142, center, 140), (center+2, 142, center+4, 140)]
        for segment in segments:
            line(*segment, color, 1.3)
    tick = math.ceil(minimum/step)*step
    while tick <= maximum:
        text(f"{0 if abs(tick) < step*1e-9 else tick:g}", x(tick), 129, 11, SOFT, align="center")
        line(x(tick), 160, x(tick), height-24, RULE, dashed=True)
        tick += step
    line(40, 159.5, width-40, 159.5, (*INK[:3], 56))
    for index, row in enumerate(rows):
        y = 187 + index*54
        ratio = 0.5 if len(rows) == 1 else index/(len(rows)-1)
        intensity = abs(ratio-0.5)*2
        root = (0.168*255, 0.532*255, 0.302*255) if ratio < 0.5 else (0.771*255, 0.204*255, 0.169*255)
        color = tuple(round(a*(1-intensity)+b*intensity) for a, b in zip(SOFT[:3], root)) + (255,)
        line(40, y+26.5, width-40, y+26.5, RULE)
        icon = row.get("avatar") or row.get("logo")
        name_x = 82 if icon else 40
        if icon:
            canvas.alpha_composite(picture(icon, 34, bool(row.get("avatar"))), (80, round((y-17)*SCALE)))
            ImageDraw.Draw(canvas).ellipse(box(40, y-17, 34, 34), outline=RULE, width=SCALE)
        size = min(13, 13*(230-name_x)/max(1, text_width(row["name"], 13, True)))
        minutes = row.get("minutes")
        text(row["name"], name_x, y-8 if minutes is None else y-15, size, INK, True)
        if minutes is not None:
            text(f'{math.floor(minutes+0.5)} MIN', name_x, y+4, 10, SOFT)
        text(number(row["on"]), 310, y-8, 13, INK, align="right")
        text(number(row["off"]), 390, y-8, 13, INK, align="right")
        text(number(row["on_off"], True), 470, y-11, 17, color, True, "right")
        line(x(row["off"]), y, x(row["on"]), y, color, 3)
        marker(row, x(row["off"]), y, color, True)
        marker(row, x(row["on"]), y, color, False)

    with Image.open(BASE / "斑驳报纸.png") as source:
        tile = source.convert("RGBA").resize((740*SCALE, 740*SCALE), Image.Resampling.LANCZOS)
    paper = Image.new("RGBA", canvas.size)
    for tile_y in range(0, paper.height, tile.height):
        for tile_x in range(0, paper.width, tile.width):
            paper.paste(tile, (tile_x, tile_y))
    paper = Image.alpha_composite(paper, Image.new("RGBA", paper.size, (255, 255, 255, round(config["paperWhiteOverlay"]*255))))
    paper.alpha_composite(canvas)
    output.mkdir(parents=True, exist_ok=True)
    for name, image in (("表格杠铃图_透明.png", canvas), ("表格杠铃图.png", paper)):
        image.save(output / name)
        print(f"已保存 {output / name}（{image.width}×{image.height}）")
    print(f"字体：{regular[0]}；中文：{cjk[0]}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("data", type=Path, help="数据 JSON 文件")
    parser.add_argument("output", type=Path, nargs="?", help="输出目录，默认数据文件所在目录")
    parser.add_argument("--config", type=Path, default=BASE / "绘图配置.json", help="当次图表配置 JSON")
    parser.add_argument("--engine", choices=("auto", "swift", "python"), default="auto", help="默认 auto；可指定引擎")
    args = parser.parse_args()
    data_file = args.data.resolve()
    output = args.output.resolve() if args.output else data_file.parent
    try:
        config_file = args.config.resolve()
        config, rows = load_inputs(data_file, config_file)
        swift = shutil.which("swift") if sys.platform == "darwin" else None
        if args.engine == "swift" and not swift:
            raise ValueError("Swift 引擎需要 macOS、Swift 和 AppKit；请改用 --engine python")
        if args.engine != "python" and swift:
            print("使用 Swift/AppKit 引擎", flush=True)
            result = subprocess.run([swift, str(BASE / "表格杠铃图.swift"), str(data_file), str(output), str(config_file)])
            if result.returncode == 0:
                return 0
            if args.engine == "swift":
                return result.returncode
            print("Swift 执行失败，自动尝试 Python 引擎。", file=sys.stderr)
        print("使用 Python/Pillow 引擎", flush=True)
        render_python(data_file, output, config, rows)
        return 0
    except (OSError, ValueError, KeyError) as exc:
        print(f"绘图失败：{exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
