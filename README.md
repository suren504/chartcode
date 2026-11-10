# 绘图代码

一个供人和 Agent 使用的制图库。用户可以直接给出原始数据并指定图型，也可以只提供数据和想表达的内容，由 Agent 选图。模板维护排版、字体、配色和绘制规则。标题、副标题、指标、标签和数据都在每次制图时提供。

## 怎么选图

| 图型 | 适合表达 |
| --- | --- |
| 散点图 | 每个对象有两个数值，想看关系、分组或离群点 |
| 表格杠铃图 | 每个对象有两项同单位的数值，想同时看原值、方向和差值 |
| 蜂群图 | 每个对象有一个数值，想看分布、聚集和重叠 |
| 桑基图 | 总量可逐层拆成互斥、可加总的类别，想比较构成 |

Agent 优先遵循用户指定的图型。未指定时，先判断指标单位、数据结构和表达目标，再选能准确表达数据的模板。出图前按 [AGENTS.md](AGENTS.md) 确认尚未给出的标题、副标题和适用的轴文案。将输入 JSON 放在临时目录，核对计算、排序、坐标方向和素材后交付图片。成品默认存到 `绘图代码/成品图`。

## 运行

在项目根目录运行。入口读取输入文件的 `type`，macOS 优先使用 Swift，Windows 使用 Python。需要 Python 渲染时先安装 Pillow。

```sh
python -m pip install -r requirements.txt
python 制图.py 数据.json
python 制图.py 数据.json 成品图/我的图.png --engine python
```

macOS 上可以使用 `python3` 运行统一入口。已有 Swift 开发工具时，也可直接运行原生模板。

```sh
swift swift/散点图.swift 数据.json 成品图/我的图.png
```

省略输出路径时，写入 `成品图/<图型>.png`。表格杠铃图同时导出同名的 `_透明.png`。`--engine swift` 和 `--engine python` 可指定引擎。

## 每次制图的输入

四类图都接收一个 JSON 对象。统一入口需要 `type`（上表中的图型名称）。`title` 是当次标题，`subtitle` 是可选副标题。素材支持绝对路径，或相对于输入 JSON 的路径。

| 图型 | 专有字段 |
| --- | --- |
| 散点图 | `xAxis`、`yAxis` 各提供 `label`，可选视觉低端 `low` 和高端 `high` 文案。可用 `reverse` 反转刻度、用 `limits: [min, max]` 固定范围。`points` 每项提供 `x`、`y` 和可选 `logo`，按输入顺序覆盖。`quadrants` 设置四角文案，`quadrantColors` 可用相同的键（`topLeft`、`topRight`、`bottomLeft`、`bottomRight`）指定 `#RRGGBB` 颜色。`logoSize` 调整标记尺寸，`glow` 开启对角色晕。 |
| 表格杠铃图 | `playerLabel` 和三个 `columnLabels` 是表头。`rows` 每项提供 `name`、`on`、`off`，可选 `minutes`、`avatar`、`logo`。默认用 `on - off` 计算并降序排列。 |
| 蜂群图 | `points` 每项提供 `value` 和可选 `logo`，最多 30 个点。可用 `xRange: {min, max}` 固定刻度，`leftLabel`、`rightLabel` 设置方向文案。已有 NBA 输入仍支持 `teams: [{team, strength}]`，按球队缩写复用内置队标。 |
| 桑基图 | `totalLabel` 是总量标签。`groups` 是两个分组，各含 `label` 与 `leaves: [{key, label}]`，共用最多六个子类。每个 `rows` 对象提供 `name` 和 `values`，按子类 key 填入非负数值。可选 `credit`、`columns`、`language`，以及行内的 `avatar`、`color`。 |

散点图的最小输入示例

```json
{
  "type": "散点图",
  "title": "两项指标的关系",
  "xAxis": {"label": "指标一", "low": "较低", "high": "较高"},
  "yAxis": {"label": "指标二", "low": "较低", "high": "较高"},
  "points": [{"x": 2, "y": 4}, {"x": 5, "y": 9}]
}
```

## 样式与扩展

- `样式.json` 是两套引擎共用的标题样式配置。`header` 管理颜色和字重，各图型条目管理标题、副标题字号，也可覆盖通用设置。
- 散点图默认以 X、Y 各自的最小值和最大值定范围，两端按 `样式.json` 中的 `axisPadding` 比例留边。需要固定范围时可显式传入 `limits`。
- 散点图的虚线默认标出两轴中位数。输入 `"reference": "mean"` 可改为均值。
- `swift/` 是 macOS 的 AppKit 渲染器，`python/` 是 Pillow 渲染器。布局和图形绘制规则留在对应模板。不同平台使用各自可用的系统字体。
- `素材/` 存放共用的队标和背景。`R代码/` 保存独立的历史绘图脚本。
- 新增图型时，在 `样式.json` 加一个同名条目，并在两个引擎目录各添加同名渲染器。两版共用输入字段，接收输入 JSON 和输出 PNG 路径，再更新上面的选图表和字段说明。
