"""自动读取同目录的 EPM XLSX 或 JSON，生成攻防散点图。"""

import argparse
import json
from math import ceil, isfinite
from pathlib import Path
from xml.etree.ElementTree import ParseError
from zipfile import BadZipFile

from openpyxl import load_workbook
from openpyxl.utils.exceptions import InvalidFileException

from 绘图组件 import axis_domain, render_python

HERE = Path(__file__).resolve().parent

PALETTE = ["#C5352B", "#F2F3F4", "#2B884D"]
LEAGUE_SHARE = 0.25
HEADING = {"axisPadding": 0.10, "title": 48, "subtitle": 22, "titleBold": True, "subtitleBold": True,
           "titleColor": "#2C3E50", "subtitleColor": "#566473"}


def read_players(source):
    required = {"season", "seasontype", "scope_id", "team_alias", "player_id", "mp", "off", "def"}
    if source.suffix.lower() == ".xlsx":
        with source.open("rb") as file:
            workbook = load_workbook(file, read_only=True, data_only=True)
            try:
                sheet = next((sheet for sheet in workbook.worksheets if sheet.title == "实际 EPM"),
                             next(iter(workbook.worksheets), None))
                if sheet is None:
                    raise ValueError("工作簿没有数据工作表")
                rows = sheet.values
                columns = next(rows, ())
                records = [dict(zip(columns, row)) for row in rows]
            finally:
                workbook.close()
    elif source.suffix.lower() == ".json":
        records = json.loads(source.read_text(encoding="utf-8-sig"))
        if isinstance(records, dict):
            records = next((records[key] for key in ("players", "data", "stats")
                            if isinstance(records.get(key), list)), None)
        if not isinstance(records, list):
            raise ValueError("JSON 须包含 EPM 球员记录数组")
    else:
        raise ValueError("数据文件须为 XLSX 或 JSON")

    players = []
    for row in records:
        if not isinstance(row, dict) or not required.issubset(row) or not isinstance(row["team_alias"], str):
            continue
        if tuple(str(row[key]) for key in ("season", "seasontype", "scope_id")) != ("2027", "2", "0"):
            continue
        try:
            player_id = int(str(row["player_id"]))
            metrics = {key: float(str(row[key])) for key in ("off", "def", "mp")}
        except ValueError:
            continue
        if all(map(isfinite, metrics.values())):
            players.append({**row, **metrics, "player_id": player_id})
    if not players:
        raise ValueError("缺少 2026-27 常规赛有效的 EPM 球员记录")
    return players


def find_dataset():
    candidates = [path for path in HERE.iterdir() if path.is_file()
                  and not path.name.startswith((".", "~$")) and path.suffix.lower() in {".xlsx", ".json"}]
    candidates.sort(key=lambda path: (-path.stat().st_mtime, path.suffix.lower() != ".xlsx", path.name))
    failures = []
    for path in candidates:
        try:
            return path, read_players(path)
        except (OSError, ValueError, BadZipFile, InvalidFileException, ParseError) as exc:
            failures.append(f"{path.name}（{exc}）")
    details = "；".join(failures) if failures else "未找到 XLSX 或 JSON 文件"
    raise ValueError(f"同目录没有可用的 EPM 数据集：{details}")


def portrait_path(player_id):
    for folder in ("球员组合图", "头像", "素材/球员组合图", "素材/头像"):
        for extension in ("webp", "png"):
            path = Path(folder, f"{player_id}.{extension}")
            if (HERE / path).is_file():
                return str(path)
    raise ValueError(f"缺少球员头像：{player_id}")


def build_chart(players, team):
    if team == "ALL":
        players = sorted(players, key=lambda player: (-player["mp"], player["player_id"]))
        players = players[:ceil(len(players) * LEAGUE_SHARE)]
    else:
        players = [player for player in players if player["team_alias"] == team]
    if not players:
        raise ValueError(f"{team} 没有具备攻防 EPM 的球员，请检查球队缩写或工作簿")

    def axis_limits(metric):
        low, high = axis_domain({}, [player[metric] for player in players], HEADING["axisPadding"])
        zero_padding = (high - low) * 0.05
        return [min(low, -zero_padding), max(high, zero_padding)]

    x_limits, y_limits = axis_limits("off"), axis_limits("def")
    points = []
    for player in sorted(players, key=lambda player: player["def"], reverse=True):
        player_id = player["player_id"]
        point = {"x": player["off"], "y": player["def"], "playerId": player_id,
                 "playerName": player.get("player_name"), "minutes": player["mp"], "logo": portrait_path(player_id)}
        points.append(point)

    return {
        "title": "NBA2026-27常规赛球员EPM攻防散点图",
        "subtitle": "2026/11/27｜制作：俗人罢了｜Ducks&Threes",
        "backgroundColor": PALETTE[1],
        "xAxis": {"label": "进攻偏弱 ← 进攻 EPM → 进攻偏强", "limits": x_limits},
        "yAxis": {"label": "防守偏弱 ← 防守 EPM → 防守偏强", "limits": y_limits},
        "quadrants": {"topRight": "攻防一体", "bottomLeft": "攻防拉完"},
        "quadrantColors": {"bottomLeft": PALETTE[0], "topRight": PALETTE[-1]},
        "reference": "zero",
        "logoSize": 100,
        "team": team,
        "points": points,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", nargs="?", type=Path, help="指定 XLSX 或 JSON，默认自动查找")
    parser.add_argument("--team", default="ALL", type=str.upper, help="球队缩写，默认 ALL")
    args = parser.parse_args()
    team = args.team
    try:
        if args.input is None:
            source, players = find_dataset()
        else:
            source = args.input.resolve()
            players = read_players(source)
        chart = build_chart(players, team)
        name = "EPM攻防图" if team == "ALL" else f"EPM攻防图_{team}"
        print(f"使用数据：{source.name}")
        render_python(chart, HERE, HERE / f"{name}.png", heading=HEADING)
    except (OSError, ValueError, BadZipFile, InvalidFileException, ParseError) as exc:
        parser.exit(1, f"绘图失败：{exc}\n")


if __name__ == "__main__":
    main()
