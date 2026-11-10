"""用 Pillow 绘制球队指标蜂群图。"""

import argparse
import math
import re
from pathlib import Path

from PIL import Image, ImageDraw

from _shared import ASSETS, OUTPUT, heading_style, load_font, read_json

LOGOS = ASSETS / 'LOGO'
SCALE = 200 / 72
WIDTH, HEIGHT = 2400, 1800
PANEL_LEFT, PANEL_RIGHT = 28, 836
PANEL_TOP, PANEL_BOTTOM = 106, 606
MARKER_TOP, MARKER_BOTTOM = PANEL_TOP + 32, PANEL_BOTTOM - 32
INK = '#3A3A3A'


def finite(value):
    return type(value) in (int, float) and math.isfinite(value)


def load_input(path):
    data = read_json(path)
    if not isinstance(data, dict) or not isinstance(data.get('title'), str):
        raise ValueError('绘图输入须包含标题 title')
    for key in ('titlePrefix', 'method', 'credit', 'source', 'asOf', 'leftLabel', 'rightLabel'):
        if data.get(key) is not None and not isinstance(data[key], str):
            raise ValueError(f'{key} 必须是文字')
    parts = data.get('subtitle')
    if parts is not None and not isinstance(parts, str):
        if not isinstance(parts, list) or any(
            not isinstance(p, dict) or not isinstance(p.get('label'), str)
            or (p.get('value') is not None and not isinstance(p['value'], str))
            or p.get('style') not in (None, 'mono', 'credit') for p in parts
        ):
            raise ValueError('subtitle 须由 label、可选 value 和 style 组成')
    rows = []
    if 'points' in data:
        points = data['points']
        if not isinstance(points, list) or not 1 <= len(points) <= 30:
            raise ValueError('请提供 1 至 30 个数据点')
        for point in points:
            if not isinstance(point, dict) or not finite(point.get('value')):
                raise ValueError('每个数据点须包含有效数值 value')
            logo = point.get('logo')
            if logo is not None and not isinstance(logo, str):
                raise ValueError('logo 须为素材路径')
            rows.append((path.parent / logo if logo is not None else None, point['value']))
    else:
        teams = data.get('teams')
        if not isinstance(teams, list) or not 1 <= len(teams) <= 30:
            raise ValueError('请提供 points 或 1 至 30 支球队')
        codes = set()
        for team in teams:
            if not isinstance(team, dict) or not isinstance(team.get('team'), str) or not finite(team.get('strength')):
                raise ValueError('每支球队须有球队缩写和有效评分')
            code = team['team'].upper()
            if not re.fullmatch(r'[A-Z]{3}', code) or code in codes:
                raise ValueError(f'球队缩写须为三个英文字母且不能重复：{code}')
            codes.add(code)
            rows.append((LOGOS / f'{code}.webp', team['strength']))
    rows.sort(key=lambda row: row[1])
    axis = data.get('xRange')
    if axis is None:
        low, high = rows[0][1], rows[-1][1]
        padding = (high - low) * 0.12 if high > low else max(1, abs(low) * 0.05)
        minimum, maximum = low - padding, high + padding
    elif isinstance(axis, dict) and finite(axis.get('min')) and finite(axis.get('max')):
        minimum, maximum = axis['min'], axis['max']
    else:
        raise ValueError('xRange 须包含有效的 min 和 max')
    if not math.isfinite(maximum - minimum) or maximum <= minimum or minimum > rows[0][1] or maximum < rows[-1][1]:
        raise ValueError('横轴范围需要覆盖所有评分，且最大值必须大于最小值')
    return data, rows, minimum, maximum


def font(size, mono=False, bold=False):
    return load_font(size * SCALE, mono=mono, bold=bold)


def point(x, y):
    return round(x * SCALE), round(y * SCALE)


def draw_runs(draw, parts, x, y, align='center', heading=False):
    parts = [(value, face, color) for value, face, color in parts if value]
    if not parts:
        return
    if heading and any('\n' in value or '\r' in value for value, _, _ in parts):
        raise ValueError('标题和副标题必须是单行')
    widths = [draw.textlength(value, font=face) for value, face, _ in parts]
    width = sum(widths)
    if heading and width > (PANEL_RIGHT - PANEL_LEFT) * SCALE:
        raise ValueError('标题或副标题超出单行宽度，请缩短文字')
    offset = {'left': 0, 'center': width / 2, 'right': width}[align]
    rise = min(draw.textbbox((0, 0), value, font=face, anchor='ls')[1] for value, face, _ in parts)
    x = x * SCALE - offset
    baseline = y * SCALE - rise
    for (value, face, color), advance in zip(parts, widths):
        draw.text((x, baseline), value, font=face, fill=color, anchor='ls')
        x += advance


def swarm(xs, diameter):
    step = diameter / 12
    half_height = (MARKER_BOTTOM - MARKER_TOP) / 2
    middle = (MARKER_TOP + MARKER_BOTTOM) / 2
    offsets = [0] + [sign * n * step for n in range(1, int(half_height / step) + 1) for sign in (1, -1)]
    centers = []
    for x in xs:
        for offset in offsets:
            y = middle - offset
            if all((x - cx) ** 2 + (y - cy) ** 2 >= diameter ** 2 - 1e-8 for cx, cy in centers):
                centers.append((x, y))
                break
        else:
            return None
    return centers


def render(data, rows, minimum, maximum):
    span = maximum - minimum

    def x_at(value):
        return PANEL_LEFT + (PANEL_RIGHT - PANEL_LEFT) * ((value - minimum) / span)

    xs = [x_at(value) for _, value in rows]
    preferred = 0.68 * 72 if any(path is not None for path, _ in rows) else 13
    diameter = preferred
    centers = swarm(xs, diameter)
    while centers is None and diameter > preferred * 0.3:
        diameter *= 0.85
        centers = swarm(xs, diameter)
    if centers is None:
        raise ValueError('当前评分分布无法放进绘图区')

    canvas = Image.new('RGB', (WIDTH, HEIGHT), '#F4F6F8')
    draw = ImageDraw.Draw(canvas)
    heading = heading_style(__file__)
    title_size, subtitle_size = heading['title'], heading['subtitle']
    title_bold, subtitle_bold = heading['titleBold'], heading['subtitleBold']
    title = [(data.get('titlePrefix'), font(title_size, mono=True, bold=title_bold), heading['titleColor']),
             (data['title'], font(title_size, bold=title_bold), heading['titleColor'])]
    draw_runs(draw, title, 432, 28, heading=True)
    parts = data.get('subtitle')
    separator = '｜' if parts is not None else ' | '
    if isinstance(parts, str):
        parts = [{'label': '', 'value': parts}]
    elif parts is None:
        parts = [
            {'label': '', 'value': data.get('method')},
            {'label': '制图: ', 'value': data.get('credit'), 'style': 'credit'},
            {'label': '数据源: ', 'value': data.get('source'), 'style': 'mono'},
            {'label': '截至 ', 'value': data.get('asOf'), 'style': 'mono'},
        ]
    subtitle = []
    for part in parts:
        if not part.get('value'):
            continue
        style = part.get('style')
        credit = style == 'credit'
        subtitle.append(((separator if subtitle else '') + part['label'],
                         font(subtitle_size, bold=subtitle_bold), heading['subtitleColor']))
        subtitle.append((part['value'], font(subtitle_size * 1.44 if credit else subtitle_size,
                                           mono=style == 'mono', bold=credit or subtitle_bold),
                         '#000000' if credit else heading['subtitleColor']))
    draw_runs(draw, subtitle, 432, 66, heading=True)

    raw_step = span / 8.25
    if raw_step <= 0:
        raise ValueError('横轴范围无法生成刻度')
    magnitude = 10 ** math.floor(math.log10(raw_step))
    if magnitude <= 0:
        raise ValueError('横轴范围无法生成刻度')
    fraction = raw_step / magnitude
    tick_step = next(n for n in (1, 2, 5, 10) if fraction <= n or n == 10) * magnitude
    for index in range(math.ceil(minimum / tick_step), math.floor(maximum / tick_step) + 1):
        tick = index * tick_step
        x = x_at(tick)
        if index == 0:
            for y in range(PANEL_TOP, PANEL_BOTTOM, 7):
                draw.line((point(x, y), point(x, min(y + 4, PANEL_BOTTOM))), fill='#B9B5AE', width=2)
        else:
            draw.line((point(x, PANEL_TOP), point(x, PANEL_BOTTOM)), fill='#E7E4DF', width=2)
        label = ('+' if 'points' not in data and tick > 0 else '') + f'{tick:g}'
        draw_runs(draw, [(label, font(13, mono=True), '#6A6865')], x, PANEL_BOTTOM + 4)

    logo_size = round(diameter * 0.8 * SCALE)
    for (path, _), (x, y) in zip(rows, centers):
        if path is None:
            radius = 5
            draw.ellipse((*point(x - radius, y - radius), *point(x + radius, y + radius)), fill=INK)
            continue
        if not path.is_file():
            raise ValueError(f'找不到 Logo：{path}')
        with Image.open(path) as image:
            logo = image.convert('RGBA')
        ratio = logo_size / max(logo.size)
        logo = logo.resize(tuple(max(1, round(side * ratio)) for side in logo.size), Image.Resampling.LANCZOS)
        canvas.paste(logo, (round(x * SCALE - logo.width / 2), round(y * SCALE - logo.height / 2)), logo)

    edge = 7
    directions = font(15.6, bold=True)
    if data.get('leftLabel'):
        draw_runs(draw, [(f'← {data["leftLabel"]}', directions, INK)], PANEL_LEFT + edge, PANEL_BOTTOM - 27, align='left')
    if data.get('rightLabel'):
        draw_runs(draw, [(f'{data["rightLabel"]} →', directions, INK)], PANEL_RIGHT - edge, PANEL_BOTTOM - 27, align='right')
    return canvas


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input', type=Path, help='Agent 准备的绘图输入 JSON')
    parser.add_argument('output', type=Path, nargs='?', help='输出 PNG 路径')
    args = parser.parse_args()
    try:
        output = args.output or OUTPUT / '蜂群图.png'
        if output.suffix.lower() != '.png' or output.resolve() == args.input.resolve():
            raise ValueError('输出须为独立的 PNG 路径')
        canvas = render(*load_input(args.input))
        output.parent.mkdir(parents=True, exist_ok=True)
        canvas.save(output, format='PNG', dpi=(200, 200))
        print(f'{WIDTH} × {HEIGHT}  {output}')
    except (OSError, ValueError, OverflowError) as exc:
        parser.exit(1, f'绘图失败：{exc}\n')


if __name__ == '__main__':
    main()
