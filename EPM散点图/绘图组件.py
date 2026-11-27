"""EPM 图使用的本地字体和 Pillow 绘图函数。"""

from functools import lru_cache
from itertools import groupby
import math
import os
from pathlib import Path
from statistics import fmean, median
from tempfile import TemporaryDirectory

SCALE = 2
SIZE = (1600, 1200)
LEFT, TOP, RIGHT, BOTTOM = 116, 143, 1552, 1101
PAPER = "#FFFAF0"
INK = (44, 62, 80, 255)
SOFT = (86, 100, 115, 255)
GRID = (230, 226, 217, 255)
GUIDE = (114, 120, 119, 255)


@lru_cache(maxsize=24)
def font_source(bold, mono, language):
    from PIL import ImageFont

    windows = Path(os.environ.get("WINDIR", "C:/Windows")) / "Fonts"
    mac = Path("/System/Library/Fonts")
    linux = Path("/usr/share/fonts/truetype/dejavu")
    noto = Path("/usr/share/fonts/opentype/noto")
    weight = "Bold" if bold else "Regular"
    if mono:
        candidates = [
            (windows / ("consolab.ttf" if bold else "consola.ttf"), 0),
            (mac / "Menlo.ttc", int(bold)),
            (linux / ("DejaVuSansMono-Bold.ttf" if bold else "DejaVuSansMono.ttf"), 0),
        ]
    elif language == "en":
        candidates = [
            (windows / ("segoeuib.ttf" if bold else "segoeui.ttf"), 0),
            (mac / "Helvetica.ttc", 1 if bold else 0),
            (linux / ("DejaVuSans-Bold.ttf" if bold else "DejaVuSans.ttf"), 0),
        ]
    else:
        yahei = "msyhbd.ttc" if bold else "msyh.ttc"
        pingfang_files = [mac / "PingFang.ttc", *Path("/System/Library/AssetsV2").glob(
            "com_apple_MobileAsset_Font*/*/AssetData/PingFang.ttc"
        ), Path("/System/Library/PrivateFrameworks/FontServices.framework/Resources/Reserved/PingFangUI.ttc")]
        candidates = [
            *((path, None) for path in pingfang_files),
            (windows / yahei, 0),
            (Path.home() / "Library/Fonts" / yahei, 0),
            (Path("/Library/Fonts") / yahei, 0),
            (mac / "Hiragino Sans GB.ttc", 2 if bold else 0),
            (mac / ("STHeiti Medium.ttc" if bold else "STHeiti Light.ttc"), 1),
            (noto / f"NotoSansCJK-{weight}.ttc", 2),
        ]
    for path, index in candidates:
        try:
            if index is None:
                face = 0
                while True:
                    font = ImageFont.truetype(str(path), 16, index=face)
                    if font.getname() == ("PingFang SC", "Semibold" if bold else "Regular"):
                        return str(path), face
                    face += 1
            ImageFont.truetype(str(path), 16, index=index)
            return str(path), index
        except OSError:
            continue
    raise ValueError("找不到所需字体，请安装系统中文字体或 Noto Sans CJK")


@lru_cache(maxsize=128)
def load_font(pixels, *, bold=False, mono=False, language="zh-Hans"):
    from PIL import ImageFont

    path, index = font_source(bold, mono, language)
    return ImageFont.truetype(path, max(1, round(pixels)), index=index)


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


def render_python(chart, assets, output, *, heading):
    try:
        from PIL import Image, ImageDraw, ImageOps
    except ImportError as exc:
        raise ValueError("Python 引擎需要 Pillow，请按 requirements.txt 安装") from exc

    paper = chart.get("backgroundColor", PAPER)
    canvas = Image.new("RGBA", tuple(value * SCALE for value in SIZE), paper)

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
    padding = heading.get("axisPadding")
    if type(padding) not in (int, float) or not 0 <= padding <= 1:
        raise ValueError("样式.json 的 axisPadding 须在 0 到 1 之间")
    x_domain, y_domain = axis_domain(x_axis, xs, padding), axis_domain(y_axis, ys, padding)

    def x(value):
        return LEFT + (RIGHT - LEFT) * axis_map(value, x_domain, x_axis)

    def y(value):
        return BOTTOM - (BOTTOM - TOP) * axis_map(value, y_domain, y_axis)

    reference = chart.get("reference", "median")
    if reference == "zero":
        if not (x_domain[0] <= 0 <= x_domain[1] and y_domain[0] <= 0 <= y_domain[1]):
            raise ValueError("使用零值参考线时，两轴范围须包含 0")
        mid_x, mid_y = x(0), y(0)
    else:
        reference_value = fmean if reference == "mean" else median
        mid_x, mid_y = x(reference_value(xs)), y(reference_value(ys))
    # 队标始终以原始数据坐标为中心，允许重叠。
    centers = [(x(point["x"]), y(point["y"])) for point in points]
    diameter = round(chart.get("logoSize", 80) * SCALE)
    dot_radius = chart.get("dotSize", 10) / 2
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
    for point, (cx, cy) in zip(points, centers):
        if "logo" in point:
            with Image.open(assets / point["logo"]) as image:
                logo = ImageOps.exif_transpose(image).convert("RGBA")
            logo = ImageOps.contain(logo, (diameter, diameter), Image.Resampling.LANCZOS)
            canvas.alpha_composite(logo, (round(cx * SCALE - logo.width / 2),
                                          round(cy * SCALE - logo.height / 2)))
        else:
            bounds = (cx - dot_radius, cy - dot_radius, cx + dot_radius, cy + dot_radius)
            draw.ellipse(tuple(round(value * SCALE) for value in bounds), fill=INK)

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
