"""Python 渲染器共用的项目路径、字号、字体与引擎选择。"""

from functools import lru_cache
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "成品图"
ASSETS = ROOT / "素材"


def read_json(path):
    return json.loads(path.read_text(encoding="utf-8-sig"))


@lru_cache(maxsize=1)
def styles():
    return read_json(ROOT / "样式.json")


def chart_types():
    return tuple(name for name in styles() if name != "header")


def heading_style(script):
    return styles()["header"] | styles()[Path(script).stem]


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
        traditional = language == "zh-Hant"
        candidates = [
            (windows / (("msjhbd.ttc" if bold else "msjh.ttc") if traditional
                        else ("msyhbd.ttc" if bold else "msyh.ttc")), 0),
            (mac / ("STHeiti Medium.ttc" if bold else "STHeiti Light.ttc"), 0),
            (mac / "Hiragino Sans GB.ttc", 2 if bold else 0),
            (noto / f"NotoSansCJK-{weight}.ttc", 3 if traditional else 2),
        ]
        if not traditional:
            candidates[1], candidates[2] = candidates[2], candidates[1]
    for path, index in candidates:
        try:
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


def run_swift(name, arguments, engine):
    if engine == "python":
        return None
    swift = shutil.which("swift") if sys.platform == "darwin" else None
    if swift == "/usr/bin/swift" and subprocess.run(
        ["xcode-select", "-p"], capture_output=True
    ).returncode != 0:
        swift = None
    if not swift:
        if engine == "swift":
            raise ValueError("Swift 引擎需要 macOS 和 Swift 开发工具")
        return None
    result = subprocess.run([swift, str(ROOT / "swift" / f"{name}.swift"), *map(str, arguments)])
    if result.returncode == 0 or engine == "swift":
        return result.returncode
    print("Swift 导出失败，改用 Python。", file=sys.stderr)
    return None
