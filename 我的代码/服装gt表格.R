library(tidyverse)
library(gt)
library(gtExtras)

# 1. 数据准备
drop3_data <- tribble(
  ~品类, ~单品名称, ~颜色_工艺, ~基础款, ~流行款, ~形象款, ~SKU小计,
  "上装", "绿泥染T恤", "植物染灰/绿", 1, 2, 0, 3,
  "上装", "亚麻混纺工装衬衫", "陶土色/原麻色", 1, 1, 0, 2,
  "上装", "镂空针织短袖", "手工编织感", 0, 1, 1, 2,
  "上装", "极简Logo T恤", "基础色", 1, 0, 0, 1,
  "上装", "扎染长袍 ", "泥染渐变", 0, 0, 1, 1,
  "下装", "雕花扣件工装短裤", "橄榄绿/卡其", 1, 1, 0, 2,
  "下装", "亚麻阔腿长裤", "舒适灰", 1, 1, 0, 2,
  "下装", "手工流苏短裤", "编织细节", 0, 0, 1, 1,
  "配饰", "编织草帽/巴拿马帽", "自然色", 0, 1, 0, 1,
  "配饰", "雕花皮质凉鞋", "擦色皮", 0, 0, 1, 1,
  "配饰", "苔藓纹理地毯/挂饰", "家居线延伸", 0, 0, 1, 1,
  "配饰", "木质串珠手链", "檀木色/黑", 1, 1, 0, 2
)

# 2. 手动构建汇总行 (消除左侧 Stub Column)
totals_d3 <- drop3_data %>%
  summarise(across(c(基础款, 流行款, 形象款, SKU小计), sum)) %>%
  mutate(
    品类 = "小计汇总",   # 标签归入品类列
    单品名称 = "",
    颜色_工艺 = "",
    数量展示 = SKU小计,  # 汇总值为 19
    SKU小计 = NA         # 设为 NA 以保持进度条视觉比例
  )

# 合并数据
final_data_d3 <- bind_rows(
  drop3_data %>% mutate(数量展示 = SKU小计), 
  totals_d3
)

# 3. 渲染专业 PPT 风格表格
table_drop3_fixed <- final_data_d3 %>%
  gt() %>%
  tab_header(
    title = md("**CLOT 2026 春季 Drop 3: “根·骨”单品规划**"), 
    subtitle = "重点：泥染工艺、手工雕花、自然回归"
  ) %>%
  tab_spanner(label = "SKU 拆解", columns = c(基础款, 流行款, 形象款)) %>%
  
  # 设置 Consolas 字体
  tab_style(
    style = cell_text(font = "Consolas", weight = "bold"), 
    locations = cells_body(columns = c(SKU小计, 基础款, 流行款, 形象款, 数量展示))
  ) %>%
  
  # 为汇总行添加大地色背景 (褐色系)
  tab_style(
    style = list(
      cell_fill(color = "#D7CCC8"), # 浅褐色背景
      cell_text(weight = "bold", color = "#3E2723") # 深焦糖色文字
    ),
    locations = cells_body(rows = nrow(final_data_d3))
  ) %>%
  
  # 绘制深褐色进度条
  gt_plt_bar(column = SKU小计, color = "#6D4C41", width = 25) %>%
  
  # 调整列展示逻辑
  cols_move(columns = 数量展示, after = SKU小计) %>%
  cols_label(SKU小计 = "SKU 规模 (可视化)", 数量展示 = "", 颜色_工艺 = "颜色/工艺") %>%
  cols_align(align = "left", columns = 数量展示) %>%
  
  # 基础样式
  tab_options(
    table.border.top.style = "none", 
    row.striping.include_table_body = TRUE, 
    heading.align = "left"
  )

# 4. 导出图片并查看
gtsave(table_drop3_fixed, "drop3_final_fixed.png", vwidth = 1200, vheight = 850)
browseURL("drop3_final_fixed.png")