"""python 生图.py [数据.json]；需安装 Pillow。路径默认相对于脚本。"""
import io
import json
import math
import os
import sys
from datetime import datetime, timedelta, timezone
from functools import lru_cache
from pathlib import Path
from tempfile import NamedTemporaryFile
from urllib.request import urlopen

from PIL import Image, ImageColor, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parent
SCALE = 300 / 72


def px(value):
    return round(value * SCALE)


def read_data():
    if len(sys.argv) > 1:
        return json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
    matches = []
    for path in ROOT.glob("*.json"):
        if path.name in {"teamcolor.json", "id查询.json"}:
            continue
        data = json.loads(path.read_text(encoding="utf-8"))
        if isinstance(data, dict) and any(gbpm(p) is not None for p in data.get("players", [])):
            matches.append(data)
    if len(matches) != 1:
        raise ValueError("请在脚本旁放一个含 GBPM 的球员 JSON，或运行时指定文件路径")
    return matches[0]


def gbpm(player):
    value = (player.get("stats") or {}).get("gbpm")
    return value if type(value) in (float, int) and math.isfinite(value) else None


@lru_cache(None)
def font(size, mono=False):
    candidates = [("consolab.ttf", 0), ("/System/Library/Fonts/Menlo.ttc", 1)] if mono else [
        ("msyhbd.ttc", 0), ("msyhbd.ttf", 0)]
    if not mono:
        for path in Path("/System/Library/AssetsV2").glob("com_apple_MobileAsset_Font*/*/AssetData/PingFang.ttc"):
            for i in range(64):
                try:
                    face = ImageFont.truetype(str(path), px(size), index=i)
                except OSError:
                    break
                if face.getname() == ("PingFang SC", "Semibold"):
                    candidates.append((str(path), i))
                    break
    for path, index in candidates:
        try:
            return ImageFont.truetype(path, px(size), index=index)
        except OSError:
            continue
    raise RuntimeError("请安装 Consolas 或 Menlo，以及微软雅黑或苹方 SC")


def portrait(player_id):
    folder = ROOT / "头像"
    folder.mkdir(exist_ok=True)
    path = folder / f"{int(player_id)}.png"
    try:
        with Image.open(path) as cached:
            return cached.convert("RGBA")
    except (FileNotFoundError, OSError):
        pass
    url = f"https://cdn.nba.com/headshots/nba/latest/1040x760/{int(player_id)}.png"
    with urlopen(url, timeout=30) as response:
        data = response.read()
    with Image.open(io.BytesIO(data)) as source:
        image = source.convert("RGBA")
    with NamedTemporaryFile(dir=folder, delete=False) as temp:
        temp.write(data)
    Path(temp.name).replace(path)
    return image


def render():
    data = read_data()
    rows = sorted((p for p in data["players"] if gbpm(p) is not None),
                  key=lambda p: (-gbpm(p), p["player_id"]))[:10]
    if not rows:
        raise ValueError("数据中没有有效的 GBPM")
    teams = json.loads((ROOT / "teamcolor.json").read_text(encoding="utf-8"))
    colors = {t["team_abbreviation"]: ImageColor.getrgb(t["team_color"]) for t in teams}
    canvas = Image.new("RGB", (3600, 4800), "#F2F3F4")
    draw = ImageDraw.Draw(canvas)

    def label(text, x, y, size, mono=False, fill="black", stroke=0):
        face = font(size, mono)
        a, b, c, d = draw.textbbox((0, 0), text, font=face)
        draw.text((px(x) - (a+c)/2, px(y) - (b+d)/2), text, font=face,
                  fill=fill, stroke_width=stroke, stroke_fill="black")

    def box(bounds, fill=None, outline=None):
        # Pillow 的线框向内绘制，将中心线与 Swift 保持一致。
        x, y, r, b = bounds
        pad = 2 if outline else 0
        draw.rectangle(tuple(map(px, (x-pad, y-pad, r+pad, b+pad))),
                       fill=fill, outline=outline, width=px(4) if outline else 1)

    def stamp(image, x, y, w, h, bottom=False):
        image = image.convert("RGBA")
        image.thumbnail((px(w), px(h)), Image.Resampling.LANCZOS)
        top = px(y) - (image.height if bottom else image.height // 2)
        canvas.paste(image, (px(x)-image.width//2, top), image)

    label("NBA 球员 GBPM TOP 10", 432, 39, 32)
    subtitle = [("数据来源 PBP Stats｜制图 ", 16, "#555555"), ("俗人罢了", 18, "black")]
    stamp_time = data.get("meta", {}).get("generated_at")
    if stamp_time:
        local = datetime.fromisoformat(stamp_time.replace("Z", "+00:00")).astimezone(timezone(timedelta(hours=8)))
        subtitle.append((local.strftime("｜数据更新至 %Y/%m/%d"), 16, "#555555"))
    widths = [font(size).getlength(s) for s, size, _ in subtitle]
    left = px(432) - sum(widths)/2
    for (s, size, fill), width in zip(subtitle, widths):
        label(s, (left+width/2)/SCALE, 78, size, fill=fill)
        left += width
    for name, x in [("排名",85), ("球队",165), ("靓照",250), ("球员",505.5), ("GBPM",761)]:
        label(name, x, 128, 22)
    portraits = []
    for rank, player in enumerate(rows, 1):
        y = 200 + (rank-1)*98
        team = player["team_abbreviation"]
        rgb = colors[team]
        def tint(amount):
            return tuple(round(v+(255-v)*amount) for v in rgb)
        box((39,y-40,697,y+40), tint(.93))
        box((703,y-40,819,y+40), tint(.45))
        with Image.open(ROOT / "LOGO" / f"{team}.webp") as logo:
            stamp(logo, 165, y, 76, 76)
        box((39,y-40,697,y+40), outline=rgb)
        box((697,y-45,825,y+45), outline=rgb)
        label(f"#{rank}", 85, y, 31, True)
        label(player["player_name"], 505.5, y, 25, True)
        label(f"{gbpm(player):.2f}", 761, y, 39, True, "white", px(39*.04/2))
        portraits.append((player["player_id"], y+38))
    for player_id, bottom in portraits:
        stamp(portrait(player_id), 250, bottom, 128, 94, bottom=True)
    output = ROOT / "NBA_plot.png"
    with NamedTemporaryFile(dir=ROOT, suffix=".png", delete=False) as temp:
        staged = Path(temp.name)
    try:
        canvas.save(staged, dpi=(300, 300))
        os.replace(staged, output)
    finally:
        staged.unlink(missing_ok=True)
    print(f"已保存 {output}（{len(rows)} 位球员）")


if __name__ == "__main__":
    render()
