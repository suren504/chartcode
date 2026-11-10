"""按输入 JSON 的 type 调用对应模板，成品默认写入成品图。"""

import argparse
from pathlib import Path
import subprocess
import sys

from python._shared import OUTPUT, ROOT, chart_types, read_json, run_swift


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path, help="当次制图 JSON")
    parser.add_argument("output", type=Path, nargs="?", help="输出 PNG")
    parser.add_argument("--engine", choices=("auto", "swift", "python"), default="auto")
    args = parser.parse_args()
    try:
        source = args.input.resolve()
        data = read_json(source)
        name = data.get("type") if isinstance(data, dict) else None
        if name not in chart_types():
            raise ValueError("type 须为：" + "、".join(chart_types()))
        output = args.output.resolve() if args.output else OUTPUT / f"{name}.png"
        if output.suffix.lower() != ".png" or output == source:
            raise ValueError("请指定独立的 PNG 输出路径")
        result = run_swift(name, [source, output], args.engine)
        if result is not None:
            return result
        try:
            import PIL
        except ImportError as exc:
            raise ValueError("请先运行 python -m pip install -r requirements.txt") from exc
        command = [sys.executable, str(ROOT / "python" / f"{name}.py"), str(source), str(output)]
        if name in {"散点图", "表格杠铃图"}:
            command.extend(("--engine", "python"))
        return subprocess.run(command).returncode
    except (OSError, ValueError) as exc:
        parser.exit(1, f"绘图失败：{exc}\n")


if __name__ == "__main__":
    sys.exit(main())
