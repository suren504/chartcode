"""绘制双指标散点图。输入字段见项目 README。"""

import argparse
from itertools import groupby
import math
from pathlib import Path
import re
from statistics import fmean, median
import sys
from tempfile import TemporaryDirectory

from _shared import OUTPUT, heading_style, load_font, read_json, run_swift

SCALE = 2
SIZE = (1600, 1200)
LEFT, TOP, RIGHT, BOTTOM = 116, 143, 1552, 1101
PAPER = (255, 250, 240, 255)
INK = (44, 62, 80, 255)
SOFT = (86, 100, 115, 255)
GRID = (230, 226, 217, 255)
GUIDE = (114, 120, 119, 255)


def read_chart(path):
    chart = read_json(path)
    if not isinstance(chart, dict) or not isinstance(chart.get("title"), str):
        raise ValueError("图表须包含标题")
    if "subtitle" in chart and not isinstance(chart["subtitle"], str):
        raise ValueError("副标题须为文字")
    if not isinstance(chart.get("points"), list) or not chart["points"]:
        raise ValueError("至少需要一个数据点")

    def number(value):
        return type(value) in (int, float) and math.isfinite(value)

    for point in chart["points"]:
        if not isinstance(point, dict) or not all(number(point.get(key)) for key in ("x", "y")):
            raise ValueError("每个数据点须包含有限的 x 和 y 数值")
        if "logo" in point and (not isinstance(point["logo"], str) or not (path.parent / point["logo"]).is_file()):
            raise ValueError(f"无法读取素材：{point['logo']}")
    for key, coord in (("xAxis", "x"), ("yAxis", "y")):
        axis = chart.get(key)
        if not isinstance(axis, dict) or not isinstance(axis.get("label"), str) or not all(
            isinstance(axis.get(k, ""), str) for k in ("low", "high")
        ):
            raise ValueError(f"{key} 的 label、可选 low 和 high 须为文字")
        if "reverse" in axis and type(axis["reverse"]) is not bool:
            raise ValueError(f"{key}.reverse 须为布尔值")
        if "limits" in axis:
            bounds = axis["limits"]
            values = [p[coord] for p in chart["points"]]
            if (not isinstance(bounds, list) or len(bounds) != 2 or not all(map(number, bounds))
                    or bounds[0] >= bounds[1] or bounds[0] > min(values) or bounds[1] < max(values)):
                raise ValueError(f"{key}.limits 须由两个递增的数值组成，并包含所有数据点")
    logo_size = chart.get("logoSize", 80)
    if not number(logo_size) or not 1 <= logo_size <= 200:
        raise ValueError("logoSize 须在 1 到 200 之间")
    quadrants = chart.get("quadrants", {})
    if not isinstance(quadrants, dict) or not all(isinstance(s, str) for s in quadrants.values()):
        raise ValueError("象限标签须为文字")
    colors = chart.get("quadrantColors", {})
    if not isinstance(colors, dict) or not all(
        isinstance(value, str) and re.fullmatch(r"#[0-9a-fA-F]{6}", value) for value in colors.values()
    ):
        raise ValueError("象限颜色须为 #RRGGBB")
    if chart.get("reference", "median") not in ("median", "mean"):
        raise ValueError("reference 须为 median 或 mean")
    if "glow" in chart and type(chart["glow"]) is not bool:
        raise ValueError("glow 须为布尔值")
    return chart


def axis_domain(axis, values, padding_ratio):
    if "limits" in axis:
        return axis["limits"]
    low, high = min(values), max(values)
    pad = (high - low) * padding_ratio if high > low else max(1, abs(low) * 0.05)
    return low - pad, high + pad


def ticks(bounds):
    low, high = bounds
    target = (high - low) / 6
    magnitude = 10 ** math.floor(math.log10(target))
    step = next(factor * magnitude for factor in (1, 2, 2.5, 5, 10) if factor * magnitude >= target)
    start = math.ceil(low / step) * step
    return [start + i * step for i in range(7) if start + i * step <= high + step * 1e-9]


def render_python(chart, source, output):
    try:
        from PIL import Image, ImageDraw, ImageOps
    except ImportError as exc:
        raise ValueError("Python 引擎需要 Pillow，请按 requirements.txt 安装") from exc

    canvas = Image.new("RGBA", tuple(value * SCALE for value in SIZE), PAPER)

    def text(image, value, x, y, size, fill, bold=False, align=0, max_width=None):
        parts = [(group, "".join(chars)) for group, chars in groupby(value, lambda c: ord(c) > 127)]

        def runs(point_size):
            return [(part, load_font(point_size * SCALE, bold=bold, mono=not chinese)) for chinese, part in parts]

        segments = runs(size)
        width = sum(face.getlength(part) for part, face in segments)
        if max_width and width > max_width * SCALE:
            segments = runs(size * max_width * SCALE / width)
            width = sum(face.getlength(part) for part, face in segments)
        position = x * SCALE - align * width
        draw = ImageDraw.Draw(image)
        for part, face in segments:
            draw.text((position, y * SCALE), part, font=face, fill=fill, anchor="ls")
            position += face.getlength(part)

    def axis_map(value, bounds, axis):
        fraction = (value - bounds[0]) / (bounds[1] - bounds[0])
        return 1 - fraction if axis.get("reverse") else fraction

    points, x_axis, y_axis = chart["points"], chart["xAxis"], chart["yAxis"]
    xs, ys = [p["x"] for p in points], [p["y"] for p in points]
    heading = heading_style(__file__)
    padding = heading.get("axisPadding")
    if type(padding) not in (int, float) or not 0 <= padding <= 1:
        raise ValueError("样式.json 的 axisPadding 须在 0 到 1 之间")
    x_domain, y_domain = axis_domain(x_axis, xs, padding), axis_domain(y_axis, ys, padding)

    def x(value):
        return LEFT + (RIGHT - LEFT) * axis_map(value, x_domain, x_axis)

    def y(value):
        return BOTTOM - (BOTTOM - TOP) * axis_map(value, y_domain, y_axis)

    reference_value = fmean if chart.get("reference") == "mean" else median
    mid_x, mid_y = x(reference_value(xs)), y(reference_value(ys))
    centers = [(x(point["x"]), y(point["y"])) for point in points]
    if chart.get("glow"):
        layer = Image.new("RGBA", ((RIGHT - LEFT) * SCALE, (BOTTOM - TOP) * SCALE))
        mask = Image.new("L", (129, 129))
        mask.putdata([
            round(255 * max(0, 1 - math.hypot(i - 64, j - 64) / 64) ** 1.7)
            for j in range(129) for i in range(129)
        ])
        for cx, cy in centers:
            dx = (cx - mid_x) / max(1, RIGHT - mid_x if cx >= mid_x else mid_x - LEFT)
            dy = (mid_y - cy) / max(1, mid_y - TOP if cy <= mid_y else BOTTOM - mid_y)
            score = (dx + dy) / 2
            weight = min(1, max(0, (abs(score) - 0.14) / 0.64))
            if weight == 0:
                continue
            radius = round((42 + 133 * weight) * SCALE)
            tint = (114, 196, 165) if score > 0 else (233, 157, 171)
            glow = Image.new("RGBA", (radius * 2, radius * 2), tint + (255,))
            opacity = mask.resize(glow.size, Image.Resampling.BILINEAR)
            glow.putalpha(opacity.point(lambda alpha: round(alpha * 0.62 * weight)))
            layer.alpha_composite(glow, (round((cx - LEFT) * SCALE) - radius,
                                         round((cy - TOP) * SCALE) - radius))
        canvas.alpha_composite(layer, (LEFT * SCALE, TOP * SCALE))

    draw = ImageDraw.Draw(canvas)

    def line(x1, y1, x2, y2, fill, width=2):
        draw.line(tuple(round(p * SCALE) for p in (x1, y1, x2, y2)), fill=fill, width=width)

    for value in ticks(x_domain):
        location = x(value)
        line(location, TOP, location, BOTTOM, GRID)
        text(canvas, f"{value:g}", location, BOTTOM + 23, 17, SOFT, align=0.5)
    for value in ticks(y_domain):
        location = y(value)
        line(LEFT, location, RIGHT, location, GRID)
        text(canvas, f"{value:g}", LEFT - 10, location + 6, 17, SOFT, align=1)
    for start in range(LEFT, RIGHT, 13):
        line(start, mid_y, min(start + 6, RIGHT), mid_y, GUIDE, width=3)
    for start in range(TOP, BOTTOM, 13):
        line(mid_x, start, mid_x, min(start + 6, BOTTOM), GUIDE, width=3)
    diameter = round(chart.get("logoSize", 80) * SCALE)
    for point, (cx, cy) in zip(points, centers):
        if "logo" in point:
            with Image.open(source.parent / point["logo"]) as image:
                logo = ImageOps.exif_transpose(image).convert("RGBA")
            logo = ImageOps.contain(logo, (diameter, diameter), Image.Resampling.LANCZOS)
            canvas.alpha_composite(logo, (round(cx * SCALE - logo.width / 2),
                                          round(cy * SCALE - logo.height / 2)))
        else:
            draw.ellipse(tuple(round(value * SCALE) for value in (cx - 5, cy - 5, cx + 5, cy + 5)), fill=INK)

    labels = chart.get("quadrants", {})
    colors = chart.get("quadrantColors", {})
    for key, cx, cy, align, tint in (
        ("topLeft", LEFT + 23, TOP + 53, 0, SOFT),
        ("topRight", RIGHT - 23, TOP + 53, 1, (37, 92, 81, 255)),
        ("bottomLeft", LEFT + 23, BOTTOM - 26, 0, (133, 70, 83, 255)),
        ("bottomRight", RIGHT - 23, BOTTOM - 26, 1, SOFT),
    ):
        text(canvas, labels.get(key, ""), cx, cy, 28, colors.get(key, tint), bold=True, align=align)

    def axis_title(axis):
        parts = [axis["label"]]
        if axis.get("low"):
            parts.insert(0, f"<——{axis['low']}")
        if axis.get("high"):
            parts.append(f"{axis['high']}——>")
        return "      ".join(parts)

    text(canvas, chart["title"], 40, 25 + heading["title"], heading["title"], heading["titleColor"],
         bold=heading["titleBold"], max_width=1520)
    text(canvas, chart.get("subtitle", ""), 40, 91 + heading["subtitle"], heading["subtitle"],
         heading["subtitleColor"], bold=heading["subtitleBold"], max_width=1520)
    text(canvas, axis_title(x_axis), (LEFT + RIGHT) / 2, BOTTOM + 64, 20, INK,
         bold=True, align=0.5, max_width=RIGHT - LEFT)
    strip = Image.new("RGBA", ((BOTTOM - TOP) * SCALE, 32 * SCALE))
    text(strip, axis_title(y_axis), (BOTTOM - TOP) / 2, 23, 20, INK,
         bold=True, align=0.5, max_width=BOTTOM - TOP)
    canvas.alpha_composite(strip.transpose(Image.Transpose.ROTATE_90), (33 * SCALE, TOP * SCALE))

    output.parent.mkdir(parents=True, exist_ok=True)
    with TemporaryDirectory(dir=output.parent) as temp:
        staged = Path(temp) / "chart.png"
        canvas.save(staged)
        staged.replace(output)
    print(f"已保存 {output}（3200×2400）")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("data", type=Path)
    parser.add_argument("output", nargs="?", type=Path, default=OUTPUT / "散点图.png")
    parser.add_argument("--engine", choices=("auto", "swift", "python"), default="auto")
    args = parser.parse_args()
    source, output = args.data.resolve(), args.output.resolve()
    try:
        chart = read_chart(source)
        if output.suffix.lower() != ".png" or output == source:
            raise ValueError("请指定独立的 PNG 输出路径")
        result = run_swift("散点图", [source, output], args.engine)
        if result is not None:
            return result
        render_python(chart, source, output)
        return 0
    except (OSError, ValueError) as exc:
        print(f"绘图失败：{exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
