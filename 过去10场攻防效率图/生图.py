"""Windows: py -m pip install -r requirements.txt，然后运行 py 生成.py [数据.json] [YYYY-MM-DD]。"""
import argparse
import json
import os
from datetime import date
from math import sqrt, isfinite, hypot, floor, copysign
from pathlib import Path
from statistics import fmean
from tempfile import TemporaryDirectory

from PIL import Image, ImageDraw, ImageFont, ImageOps

ROOT = Path(__file__).resolve().parent
SOURCE = ROOT / "过去10场攻防效率数据.json"
TEAM_ASSETS = ROOT / "LOGO"
NBA_MARK = TEAM_ASSETS / "nba-mark.png"
PAPER = ROOT / "粗纹纸张.png"

SIZE = (2000, 2500)
PLOT_TOP = SIZE[1] - SIZE[0]
INK = "#171717"
MUTED = "#625e56"
GUIDE = "#89857c"
LANCZOS = Image.Resampling.LANCZOS
SIZES = {
    "ATL": 106, "BKN": 96, "BOS": 120, "CHA": 114, "CHI": 114, "CLE": 114,
    "DAL": 96, "DEN": 96, "DET": 96, "GSW": 96, "HOU": 150, "IND": 105,
    "LAC": 107, "LAL": 111, "MEM": 102, "MIA": 113, "MIL": 125, "MIN": 95,
    "NOP": 120, "NYK": 122, "OKC": 144, "ORL": 145, "PHI": 121, "PHX": 125,
    "POR": 104, "SAC": 95, "SAS": 123, "TOR": 106, "UTA": 155, "WAS": 94,
}


def font(role, size):
    if os.name == "nt":
        directory = Path(os.environ.get("WINDIR", "C:/Windows")) / "Fonts"
        files = {"title": ["seguibl.ttf", "arialbd.ttf"], "latin": ["segoeui.ttf", "arial.ttf"],
                 "chinese": ["msyh.ttc", "msyh.ttf", "simsun.ttc", "simhei.ttf"]}
        for name in files[role]:
            if (directory / name).is_file():
                return ImageFont.truetype(str(directory / name), size)
        raise ValueError(f"缺少 {role} 字体，请安装 Windows 简体中文补充字体或所需的系统字体")
    directory = Path("/System/Library/Fonts")
    if role == "chinese":
        return ImageFont.truetype(str(directory / "Hiragino Sans GB.ttc"), size)
    return ImageFont.truetype(str(directory / "Avenir Next.ttc"), size, index=8 if role == "title" else 7)


def ink_text(content, face, color=INK):
    left, top, right, bottom = face.getbbox(content)
    stamp = Image.new("RGBA", (right - left, bottom - top))
    ImageDraw.Draw(stamp).text((-left, -top), content, font=face, fill=color)
    return stamp.crop(stamp.getbbox())


def place(canvas, stamp, x, y):
    canvas.paste(stamp, (round(x), round(y)), stamp)


def load_logo(path):
    with Image.open(path) as source:
        mark = source.convert("RGBA")
    return mark.crop(mark.getbbox())


def read_points(path):
    rows = json.loads(path.read_text(encoding="utf-8-sig"))
    points = [(row["team"], row["offRtg"], row["defRtg"]) for row in rows]
    if len(points) != 30 or {team for team, _, _ in points} != SIZES.keys():
        raise ValueError("数据须包含 30 个不重复的 NBA 球队缩写")
    if not all(type(v) in (int, float) and isfinite(v) and v > 0 for _, o, d in points for v in (o, d)):
        raise ValueError("攻防效率须为有限正数")
    return points


def draw_heading(canvas, day):
    heading = ink_text("The Efficiency Landscape", font("title", 128))
    title_top = 52
    if heading.width > 1718:
        heading = heading.resize((1718, round(heading.height * 1718 / heading.width)), LANCZOS)
    place(canvas, heading, 227, title_top)
    latin, chinese = font("latin", 43), font("chinese", 43)
    month = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split()[day.month - 1]
    runs = [
        (f"{month}. {day.day}, {day.year} | ", latin, MUTED),
        ("Last 10 Games Only", latin, "#cf4d48"),
        (" | By @", latin, MUTED),
        ("俗人罢了", chinese, MUTED),
    ]
    subtitle = Image.new("RGBA", (round(sum(face.getlength(s) for s, face, _ in runs)) + 6, 76))
    pen = 3.0
    draw = ImageDraw.Draw(subtitle)
    for content, face, color in runs:
        draw.text((pen, 52), content, font=face, fill=color, anchor="ls")
        pen += face.getlength(content)
    subtitle = subtitle.crop(subtitle.getbbox())
    subtitle_top = 205
    place(canvas, subtitle, (SIZE[0] - subtitle.width) / 2, subtitle_top)

    mark = load_logo(NBA_MARK)
    mark_height = subtitle_top + subtitle.height - title_top
    mark = mark.resize((round(mark.width * mark_height / mark.height), mark_height), LANCZOS)
    place(canvas, mark, 73, title_top)
    assert 227 + heading.width < SIZE[0] - 54
    assert title_top + heading.height < subtitle_top < subtitle_top + subtitle.height < PLOT_TOP


def draw_reference(canvas, x_zero, y_zero):
    draw = ImageDraw.Draw(canvas)
    axis_left, axis_right = 33, 1967
    axis_top, axis_bottom = PLOT_TOP + 20, 2466

    # Equal pixels per rating point make OffRtg = DefRtg a 45-degree line.
    intercept = y_zero - x_zero
    line_start = max(126, round(PLOT_TOP + 30 - intercept))
    line_end = min(1910, round(axis_bottom - intercept))
    assert line_start < line_end

    def guide_y(x):
        return x + intercept

    step, dash = 24 / sqrt(2), 14 / sqrt(2)
    x = line_start
    while x < line_end:
        end = min(x + dash, line_end)
        draw.line((x, guide_y(x), end, guide_y(end)), fill="#a29f97", width=4)
        x += step

    label_face = font("chinese", 32)
    labels = [ink_text(text, label_face, GUIDE) for text in ("联盟强队", "联盟弱队")]
    tracks = Image.new("RGBA", (max(label.width for label in labels) + 6, 82))
    place(tracks, labels[0], 3, 31 - labels[0].height)
    place(tracks, labels[1], 3, 51)
    diagonal_label = tracks.rotate(-45, resample=Image.Resampling.BICUBIC, expand=True)
    label_center = line_start + 115
    place(canvas, diagonal_label, label_center - diagonal_label.width / 2,
          guide_y(label_center) - diagonal_label.height / 2)

    draw.line((axis_left + 16, y_zero, axis_right - 16, y_zero), fill=INK, width=2)
    draw.line((x_zero, axis_top + 16, x_zero, axis_bottom - 16), fill=INK, width=2)
    draw.polygon([(axis_left, y_zero), (axis_left + 17, y_zero - 7), (axis_left + 17, y_zero + 7)], fill=INK)
    draw.polygon([(axis_right, y_zero), (axis_right - 17, y_zero - 7), (axis_right - 17, y_zero + 7)], fill=INK)
    draw.polygon([(x_zero, axis_top), (x_zero - 7, axis_top + 17), (x_zero + 7, axis_top + 17)], fill=INK)
    draw.polygon([(x_zero, axis_bottom), (x_zero - 7, axis_bottom - 17), (x_zero + 7, axis_bottom - 17)], fill=INK)

    axis_face = font("chinese", 55)
    offensive = ink_text("进攻效率", axis_face)
    defensive = ink_text("防守效率", axis_face).transpose(Image.Transpose.ROTATE_90)
    place(canvas, offensive, 1934 - offensive.width, y_zero + 12)
    place(canvas, defensive, x_zero - 11 - defensive.width, axis_top + 25)


def draw_labels(canvas, points, marks, origin):
    colors = {r["team_abbreviation"]: r["team_color"] for r in
              json.loads((ROOT / "teamcolor.json").read_text(encoding="utf-8-sig"))}
    net = [copysign(floor(abs((o - d) * 10) + 0.5), o - d) for _, o, d in points]
    offenses = [o for _, o, _ in points]
    defenses = [d for _, _, d in points]
    ox, oy = origin
    occupied = [*marks.values(), (1700, oy + 8, 1940, oy + 73), (ox - 75, 540, ox - 5, 775)]
    face = font("chinese", 29)
    for (team, o, d), n in zip(points, net):
        labels = []
        for hit, title in [(o == max(offenses), "最佳进攻"), (o == min(offenses), "最差进攻"),
                           (d == min(defenses), "最佳防守"), (d == max(defenses), "最差防守")]:
            if hit:
                labels.append(title)
        if n in (max(net), min(net)):
            if not labels:
                labels.append(f"#联盟第{1 + sum(v > n for v in net)}")
            labels.extend((f"#进攻第{1 + sum(v > o for v in offenses)}",
                           f"#防守第{1 + sum(v < d for v in defenses)}"))
        if not labels:
            continue
        stamps = [ink_text(label, face, colors[team]) for label in labels]
        w = max(stamp.width for stamp in stamps)
        h = (len(stamps) - 1) * 34 + stamps[-1].height
        l, t, r, b = marks[team]
        cx, cy = (l + r) / 2, (t + b) / 2
        best_cost, best = float("inf"), None
        for gap in (9, 26, 48, 76, 110):
            for dx, dy in ((1,0),(-1,0),(0,-1),(0,1),(1,-1),(-1,-1),(1,1),(-1,1)):
                x = cx + dx * ((r - l + w) / 2 + gap) - w / 2
                y = cy + dy * ((b - t + h) / 2 + gap) - h / 2
                if x < 12 or x + w > 1988 or y < PLOT_TOP or y + h > 2488:
                    continue
                overlap = sum(max(0, min(x+w, right+4) - max(x, left-4)) *
                              max(0, min(y+h, bottom+4) - max(y, top-4))
                              for left, top, right, bottom in occupied)
                cost = overlap * 1000 + hypot(x + w/2 - cx, y + h/2 - cy)
                if cost < best_cost:
                    best_cost, best = cost, (x, y)
        if best is None:
            raise ValueError(f"无法放置排名：{team}")
        x, y = best
        occupied.append((x, y, x+w, y+h))
        for index, stamp in enumerate(stamps):
            place(canvas, stamp, x, y + index * 34)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("args", nargs="*", metavar="参数")
    args = parser.parse_args().args
    if len(args) > 2:
        parser.error("用法：py 生成.py [数据.json] [YYYY-MM-DD]")
    source = Path(args[0]) if len(args) == 2 else SOURCE
    day = date.fromisoformat(args[-1] if args else "2026-11-20")
    output = ROOT / f"过去10场攻防效率_{day.isoformat()}_Python.png"
    points = read_points(source)
    off_values = [off for _, off, _ in points]
    def_values = [defense for _, _, defense in points]
    average = fmean(off_values + def_values)
    off_values.append(average)
    def_values.append(average)
    off_mid = (min(off_values) + max(off_values)) / 2
    def_mid = (min(def_values) + max(def_values)) / 2
    span = max(max(off_values) - min(off_values), max(def_values) - min(def_values), 1)
    scale = 1810 / span
    units = floor(scale) if scale >= 1 else scale
    plot_x, plot_y = SIZE[0] / 2, (PLOT_TOP + SIZE[1]) / 2

    def to_canvas(off, defense):
        return plot_x + (off - off_mid) * units, plot_y + (defense - def_mid) * units

    with Image.open(PAPER) as texture:
        canvas = ImageOps.fit(texture.convert("RGB"), SIZE, method=LANCZOS)

    draw_heading(canvas, day)
    marks = {}
    draw_reference(canvas, *to_canvas(average, average))
    for team, off, defense in reversed(points):
        size = SIZES[team]
        mark = ImageOps.contain(load_logo(TEAM_ASSETS / f"{team}.webp"), (size, size), LANCZOS)
        x, y = to_canvas(off, defense)
        left, top = x - mark.width / 2, y - mark.height / 2
        assert 0 <= left < left + mark.width <= SIZE[0], team
        assert PLOT_TOP <= top < top + mark.height <= SIZE[1], team
        place(canvas, mark, left, top)
        marks[team] = (left, top, left + mark.width, top + mark.height)
    draw_labels(canvas, points, marks, to_canvas(average, average))

    with TemporaryDirectory(dir=output.parent) as temp:
        staged = Path(temp) / output.name
        canvas.save(staged, optimize=True)
        staged.replace(output)
    print(f"{len(points)} team logos, {canvas.width} × {canvas.height}, neutral crossing {average:.3f}")
    print(output)


if __name__ == "__main__":
    main()
