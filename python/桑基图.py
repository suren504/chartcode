"""用 Pillow 绘制总量、两个分组和子类之间的多面板桑基图。"""

import argparse
import math
from pathlib import Path

from PIL import Image, ImageColor, ImageDraw

from _shared import OUTPUT, heading_style, load_font, read_json

SS, DPI = 2, 110
W, H = 8.5, 6.8
BODY_TOP, PANEL_H = 272, 735
BODY_SCALE = PANEL_H / H
PANEL_W = round(W * BODY_SCALE)
X1, X2, X3, NODE_W = 1.45, 3.60, 5.95, 0.20
CHART_TOP = H - 1.42
BG, INK, GRAY = '#F3F4F5', '#2B2B2B', '#7A7A7A'
GROUP_COLORS = ('#8C3A34', '#3C6575')
LEAF_COLORS = (
    ('#9E413A', '#B7746B', '#AC6155', '#78423F', '#D09287'),
    ('#56899A', '#78A3B1', '#315967', '#467685', '#97B5BA'),
)
LANGUAGES = {'zh-Hans', 'zh-Hant', 'en'}


def font(px, **style):
    return load_font(px * SS, **style)


def load_input(path):
    data = read_json(path)
    if not isinstance(data, dict):
        raise ValueError('绘图输入必须是 JSON 对象')
    for key in ('title', 'totalLabel'):
        if not isinstance(data.get(key), str) or not data[key].strip():
            raise ValueError(f'{key} 必须是非空文字')
    for key in ('subtitle', 'credit'):
        if not isinstance(data.get(key, ''), str):
            raise ValueError(f'{key} 必须是文字')
    language = data.get('language', 'zh-Hans')
    if not isinstance(language, str) or language not in LANGUAGES:
        raise ValueError('language 必须是 zh-Hans、zh-Hant 或 en')
    columns = data.get('columns', 4)
    if type(columns) is not int or not 1 <= columns <= 5:
        raise ValueError('columns 必须是 1 到 5 之间的整数')

    groups = data.get('groups')
    if not isinstance(groups, list) or len(groups) != 2:
        raise ValueError('groups 必须包含两个分组')
    keys = []
    for group in groups:
        if not isinstance(group, dict) or not isinstance(group.get('label'), str) or not group['label'].strip():
            raise ValueError('分组缺少 label')
        leaves = group.get('leaves')
        if not isinstance(leaves, list) or not leaves:
            raise ValueError('每个分组必须有至少一个子类')
        for leaf in leaves:
            if not isinstance(leaf, dict) or any(not isinstance(leaf.get(k), str) or not leaf[k].strip()
                                                for k in ('key', 'label')):
                raise ValueError('每个子类必须有非空 key 和 label')
            keys.append(leaf['key'])
    if len(keys) > 6 or len(set(keys)) != len(keys):
        raise ValueError('两个分组最多共用六个子类，子类 key 必须唯一')

    rows = data.get('rows')
    if not isinstance(rows, list) or not rows:
        raise ValueError('rows 必须是非空对象数组')
    for index, row in enumerate(rows, 1):
        if not isinstance(row, dict) or not isinstance(row.get('name'), str) or not row['name'].strip():
            raise ValueError(f'第 {index} 行缺少名称')
        name_lines = row['name'].strip().splitlines()
        if len(name_lines) > 2 or any(not line.strip() for line in name_lines):
            raise ValueError(f'第 {index} 行名称须为一行或两行')
        values = row.get('values')
        if not isinstance(values, dict) or set(values) != set(keys):
            raise ValueError(f'第 {index} 行 values 必须与子类 key 一一对应')
        if any(type(v) not in (int, float) or not math.isfinite(v) or v < 0 for v in values.values()):
            raise ValueError(f'第 {index} 行子类数值必须是有限的非负数')
        if 'color' in row:
            if not isinstance(row['color'], str):
                raise ValueError(f'第 {index} 行颜色必须是文字')
            ImageColor.getrgb(row['color'])
        if 'avatar' in row and (not isinstance(row['avatar'], str) or not (path.parent / row['avatar']).is_file()):
            raise ValueError(f'第 {index} 行无法读取头像：{row["avatar"]}')
    if not any(v > 0 for row in rows for v in row['values'].values()):
        raise ValueError('至少一个子类数值必须大于 0')
    return data


def fitted_font(draw, value, face, width):
    left, _, right, _ = draw.textbbox((0, 0), value, font=face)
    measured = right - left
    if measured > width:
        return face.font_variant(size=max(1, math.floor(face.size * width / measured)))
    return face


def text(draw, value, x, y, face, color=INK, h='center', v='center', width=None):
    if width is not None:
        face = fitted_font(draw, value, face, width)
    left, top, right, bottom = draw.textbbox((0, 0), value, font=face)
    x -= {'left': left, 'center': (left + right) / 2, 'right': right}[h]
    y -= {'top': top, 'center': (top + bottom) / 2, 'bottom': bottom}[v]
    draw.text((x, y), value, font=face, fill=color)


def number(value):
    return str(value) if isinstance(value, int) else f'{value:.12g}'


def render_panel(row, data, groups, gaps, per_unit, input_dir):
    panel = Image.new('RGBA', (PANEL_W * SS, PANEL_H * SS))
    ribbons = Image.new('RGBA', panel.size)
    draw, ribbon_draw = ImageDraw.Draw(panel), ImageDraw.Draw(ribbons)
    scale = BODY_SCALE * SS
    language = data.get('language', 'zh-Hans')
    label_font = font(28 if language == 'en' else 32, language=language)
    values = [row['values'][leaf['key']] for group in groups for leaf in group['leaves']]
    split = len(groups[0]['leaves'])
    mid_values = (sum(values[:split]), sum(values[split:]))
    total = sum(mid_values)
    leaf_colors = [color for group, palette in zip(groups, LEAF_COLORS)
                   for color in palette[:len(group['leaves'])]]

    def point(x, y):
        return x * scale, (H - y) * scale

    def label(value, x, y, face, **kwargs):
        text(draw, value, *point(x, y), face, **kwargs)

    def node(x, span, color):
        if span[0] > span[1]:
            draw.rectangle((*point(x, span[0]), *point(x + NODE_W, span[1])), fill=color)

    def ribbon(xa, xb, start, end, color):
        if start[0] <= start[1]:
            return
        upper, lower = [], []
        for i in range(61):
            t = i / 60
            s = t * t * (3 - 2 * t)
            x = xa + (xb - xa) * t
            upper.append(point(x, start[0] + (end[0] - start[0]) * s))
            lower.append(point(x, start[1] + (end[1] - start[1]) * s))
        ribbon_draw.polygon(upper + lower[::-1], fill=(*ImageColor.getrgb(color), 140))

    cursor = CHART_TOP
    leaf_spans = []
    for value, gap in zip(values, gaps):
        top = cursor - gap
        cursor = top - value * per_unit
        leaf_spans.append((top, cursor))
    yc = (CHART_TOP + cursor) / 2
    total_span = (yc + total * per_unit / 2, yc - total * per_unit / 2)
    split_y = total_span[0] - mid_values[0] * per_unit
    total_split = ((total_span[0], split_y), (split_y, total_span[1]))
    mid_centers = ((leaf_spans[0][0] + leaf_spans[split - 1][1]) / 2,
                   (leaf_spans[split][0] + leaf_spans[-1][1]) / 2)
    mid_spans = [(y + value * per_unit / 2, y - value * per_unit / 2)
                 for y, value in zip(mid_centers, mid_values)]
    for source, target, color in zip(total_split, mid_spans, GROUP_COLORS):
        ribbon(X1 + NODE_W, X2, source, target, color)
    for span, indices in zip(mid_spans, (range(split), range(split, len(values)))):
        cursor = span[0]
        for i in indices:
            next_y = cursor - values[i] * per_unit
            ribbon(X2 + NODE_W, X3, (cursor, next_y), leaf_spans[i], leaf_colors[i])
            cursor = next_y
    node(X1, total_span, row.get('color', '#555555'))
    for span, color in zip(mid_spans, GROUP_COLORS):
        node(X2, span, color)
    for span, color in zip(leaf_spans, leaf_colors):
        node(X3, span, color)

    label(number(total), X1 - 0.16, yc + 0.06, font(62, bold=True, mono=True), h='right', v='bottom', width=1.12 * scale)
    label(data['totalLabel'], X1 - 0.16, yc - 0.06, label_font, color=GRAY, h='right', v='top', width=1.12 * scale)
    xc = (X1 + NODE_W + X2) / 2
    mid_positions = [((source[0] + source[1]) / 2 + y) / 2 for source, y in zip(total_split, mid_centers)]
    if all(mid_values) and mid_positions[0] - mid_positions[1] < 0.75:
        center = sum(mid_positions) / 2
        mid_positions = [center + 0.375, center - 0.375]
    for value, group, y in zip(mid_values, groups, mid_positions):
        if value > 0:
            label(number(value), xc, y + 0.06, font(47, bold=True, mono=True), v='bottom', width=1.75 * scale)
            label(group['label'], xc, y - 0.06, label_font, v='top', width=1.75 * scale)

    xl = X3 + NODE_W + 0.15
    gap = 1.76 if language == 'en' else 1.34
    leaf_labels = [leaf['label'] for group in groups for leaf in group['leaves']]
    for leaf_label, value, span in zip(leaf_labels, values, leaf_spans):
        y = sum(span) / 2
        label(leaf_label, xl, y, label_font, h='left', width=(gap - 0.10) * scale)
        label(number(value), xl + gap, y, font(34, bold=True, mono=True), h='left', width=(W - xl - gap - 0.04) * scale)

    lines = row['name'].strip().splitlines()
    name_font = font(40, mono=not any(ord(c) >= 0x3000 for c in row['name']),
                     language='zh-Hans' if language == 'en' else language)
    side = round(scale) if 'avatar' in row else 0
    name_gap = 0.16 * scale if side else 0
    name_font = fitted_font(draw, max(lines, key=name_font.getlength), name_font, 3.4 * scale)
    name_width = max(draw.textlength(line, font=name_font) for line in lines)
    head_x, head_y = point((X1 + X3 + NODE_W) / 2, CHART_TOP + 0.72)
    head_left = head_x - (side + name_gap + name_width) / 2
    if side:
        with Image.open(input_dir / row['avatar']) as image:
            avatar = image.convert('RGBA').resize((side, side), Image.Resampling.LANCZOS)
        panel.alpha_composite(avatar, (round(head_left), round(head_y - side / 2)))
    for index, line in enumerate(lines):
        offset = (index - (len(lines) - 1) / 2) * 40 * SS
        text(draw, line, head_left + side + name_gap, head_y + offset, name_font, h='left')

    ribbons.alpha_composite(panel)
    return ribbons.resize((PANEL_W, PANEL_H), Image.Resampling.LANCZOS)


def render_heading(canvas, data):
    language = data.get('language', 'zh-Hans')
    heading = heading_style(__file__)
    header = Image.new('RGBA', (canvas.width * SS, BODY_TOP * SS))
    draw = ImageDraw.Draw(header)
    subtitle, credit = data.get('subtitle', ''), data.get('credit', '')
    title_top = 41 if subtitle or credit else 75
    text(draw, data['title'], header.width / 2, title_top * SS,
         font(heading['title'], bold=heading['titleBold'], language=language), color=heading['titleColor'],
         v='top', width=header.width - 80 * SS)
    if subtitle or credit:
        credit_language = 'zh-Hans' if language == 'en' and any(ord(c) >= 0x3000 for c in credit) else language
        parts = (
            (subtitle + (' | ' if subtitle and credit else ''),
             font(heading['subtitle'], bold=heading['subtitleBold'], language=language), heading['subtitleColor']),
            (credit, font(heading['subtitle'], bold=True, language=credit_language), '#000000'),
        )
        measured = sum(draw.textlength(value, font=face) for value, face, _ in parts)
        shrink = min(1, (header.width - 80 * SS) / measured) if measured else 1
        parts = [(value, face.font_variant(size=max(1, round(face.size * shrink))), color)
                 for value, face, color in parts]
        widths = [draw.textlength(value, font=face) for value, face, _ in parts]
        x = (header.width - sum(widths)) / 2
        for (value, face, color), width in zip(parts, widths):
            draw.text((x, 241 * SS), value, font=face, anchor='ls', fill=color)
            x += width
    canvas.alpha_composite(header.resize((canvas.width, BODY_TOP), Image.Resampling.LANCZOS))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input', type=Path, help='Agent 准备的绘图输入 JSON')
    parser.add_argument('output', type=Path, nargs='?', help='输出 PNG 路径')
    args = parser.parse_args()
    try:
        output = args.output or OUTPUT / '桑基图.png'
        if output.suffix.lower() != '.png' or output.resolve() == args.input.resolve():
            raise ValueError('输出须为独立的 PNG 路径')
        data = load_input(args.input)
        rows, groups = data['rows'], data['groups']
        leaf_count = sum(len(group['leaves']) for group in groups)
        gaps = [0] + [0.31] * (leaf_count - 1)
        gaps[len(groups[0]['leaves'])] = 0.35
        maximum = max(sum(row['values'].values()) for row in rows)
        if not math.isfinite(maximum):
            raise ValueError('子类总量超出数值范围')
        per_unit = (CHART_TOP - sum(gaps) - 0.24) / maximum
        columns = min(data.get('columns', 4), len(rows))
        width = round(columns * W * DPI)
        height = BODY_TOP + math.ceil(len(rows) / columns) * PANEL_H
        left = (width - columns * PANEL_W) // 2
        canvas = Image.new('RGBA', (width, height), BG)
        render_heading(canvas, data)
        for index, row in enumerate(rows):
            line, col = divmod(index, columns)
            panel = render_panel(row, data, groups, gaps, per_unit, args.input.parent)
            canvas.alpha_composite(panel, (left + col * PANEL_W, BODY_TOP + line * PANEL_H))
        output.parent.mkdir(parents=True, exist_ok=True)
        canvas.convert('RGB').save(output, format='PNG', dpi=(DPI, DPI), optimize=True)
        print(f'{width} × {height}  {output}')
    except (OSError, ValueError, OverflowError) as exc:
        parser.exit(1, f'绘图失败：{exc}\n')


if __name__ == '__main__':
    main()
