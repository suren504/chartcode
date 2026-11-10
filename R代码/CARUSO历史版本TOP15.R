library(tidyverse)
library(gt)
library(gtExtras)
library(glue)

# ==============================================================================
# 1. Data Preparation (录入过去十年最强防守球员数据)
# ==============================================================================

df_decade <- tibble(
  player = c("Rudy Gobert", "Rudy Gobert", "Joel Embiid", "Alex Caruso", 
             "Giannis Antetokounmpo", "Alex Caruso", "Paul George", "Kent Bazemore", 
             "Shai Gilgeous-Alexander", "Rudy Gobert", "Jonathan Isaac", 
             "Matisse Thybulle", "Draymond Green", "OG Anunoby"),
  team_abbr = c("UTA", "UTA", "PHI", "CHI", 
                "MIL", "OKC", "OKC", "SAC", 
                "OKC", "UTA", "ORL", 
                "PHI", "GSW", "NYK"),
  season = c("2020-21", "2016-17", "2017-18", "2022-23", 
             "2019-20", "2024-25", "2018-19", "2019-20", 
             "2024-25", "2021-22", "2023-24", 
             "2021-22", "2016-17", "2023-24"),
  rim_ps = c(100, 99.81, 99.43, 41.24, 90.84, 71.88, 75.31, 79.47, 47.35, 99.75, 90.36, 43.21, 88.78, 77.31),
  reb_oe = c(97.26, 79.88, 98.40, 32.66, 97.54, 71.07, 54.17, 57.15, 66.21, 99.32, 85.49, 10.06, 82.39, 43.16),
  stop_pct = c(97.72, 95.52, 80.58, 99.56, 69.02, 99.50, 95.58, 89.13, 89.05, 86.21, 99.21, 99.86, 99.41, 70.44),
  defl_resid = c(80.81, 90.50, 28.70, 99.49, 76.82, 91.73, 50.79, 98.96, 4.67, 57.09, 52.86, 99.10, 44.64, 58.91),
  stat_prior = c(3.15, 2.53, 2.82, 2.82, 2.66, 3.06, 1.26, 0.72, 0.77, 3.21, 1.74, 2.20, 1.96, 0.54),
  def_rapm = c(2.16, 2.47, 2.14, 2.07, 2.22, 1.20, 2.61, 2.81, 2.96, 1.37, 2.54, 2.07, 2.14, 2.76),
  caruso = c(2.55, 2.49, 2.43, 2.43, 2.36, 2.27, 2.27, 2.18, 2.18, 2.13, 2.13, 2.11, 2.09, 2.08)
)

# 中文名字映射
name_map_cn <- c(
  "Rudy Gobert" = "鲁迪·戈贝尔",
  "Joel Embiid" = "乔尔·恩比德",
  "Alex Caruso" = "亚历克斯·卡鲁索",
  "Giannis Antetokounmpo" = "扬尼斯·阿德托昆博",
  "Paul George" = "保罗·乔治",
  "Kent Bazemore" = "肯特·贝兹莫尔",
  "Shai Gilgeous-Alexander" = "亚历山大", 
  "Jonathan Isaac" = "乔纳森·艾萨克",
  "Matisse Thybulle" = "马蒂斯·赛布尔",
  "Draymond Green" = "追梦格林",
  "OG Anunoby" = "OG·阿努诺比"
)

# ==============================================================================
# 2. Data Processing (构建HTML列)
# ==============================================================================

plot_df <- df_decade %>%
  mutate(
    name_cn = name_map_cn[player],
    
    # 1. 球队 Logo (保持原有逻辑，第一列)
    path_logo = paste0("D:/nba/球队logo/球队logo方/", team_abbr, ".png"),
    logo_html = glue("<div style='display:flex; justify-content:center; align-items:center;'><img src='{path_logo}' style='height:60px; width:60px; object-fit:contain;'></div>"),
    
    # 2. 球员头像 (第二列：指定的新文件夹路径)
    # 注意：这里直接使用 player 名字拼接 .png
    path_head = paste0("file:///D:/nba/临时照片/防守球员/", player, ".png"),
    head_html = glue("<div style='display:flex; justify-content:center; align-items:center;'><img src=\"{path_head}\" style='height:70px; width:70px; object-fit:cover; border-radius:50%; background-color:#fffaf0; border: 1px solid #ccc;'></div>"),
    
    # 3. 名字+赛季 组合列 (第三列：定制排版)
    # 逻辑：Div包裹 -> Span(名字, 加粗) -> Span(赛季, 不加粗, 灰色)
    # margin-bottom: 6px 实现"中间空的多一点点"
    info_html = glue(
      "<div style='line-height:1.2; text-align:left;'>
         <span style='font-weight:900; font-size:15px; color:#000; display:block; margin-bottom:6px;'>{name_cn}</span>
         <span style='font-weight:normal; font-size:15px; color:#666; display:block;'>{season} 赛季</span>
       </div>"
    )
  ) %>%
  select(logo_html, head_html, info_html, rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm, caruso)

# ==============================================================================
# 3. Plotting (绘制表格)
# ==============================================================================

# 定义颜色
color_light_bg    <- "#F9F9F9" 
color_soft_red    <- "#F7565A" 
color_soft_orange <- "#FF9800"

final_table <- suppressWarnings(
  plot_df %>%
    gt(id = "caruso_decade_top15") %>%
    # 解析 HTML 列
    fmt_markdown(columns = c(logo_html, head_html, info_html)) %>%
    
    # 数字格式化
    fmt_number(
      columns = c(rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm, caruso),
      decimals = 2
    ) %>%
    
    # --- 颜色设置 (保持热力图风格) ---
    gt_color_rows(
      columns = c(rim_ps, reb_oe, stop_pct, defl_resid),
      palette = c(color_light_bg, color_soft_red),
      domain = NULL 
    ) %>%
    gt_color_rows(
      columns = c(stat_prior, def_rapm),
      palette = c(color_light_bg, color_soft_orange),
      domain = NULL
    ) %>%
    gt_color_rows(
      columns = c(caruso),
      palette = "ggsci::blue_material",
      domain = NULL
    ) %>%
    
    # --- 列名设置 ---
    cols_label(
      logo_html = "球队",
      head_html = "球员",
      info_html = "赛季", 
      rim_ps = "护框能力", reb_oe = "额外篮板", stop_pct = "终结回合",
      defl_resid = "破坏球", stat_prior = "先验",
      def_rapm = "DRAPM", caruso = "CARUSO"
    ) %>%
    
    # --- 对齐与宽度 ---
    # Logo 居中
    cols_align(align = "center", columns = logo_html)
  # 头像 居中 (或者根据喜好可以改 Right)
  %>% cols_align(align = "center", columns = head_html)
  # 信息列(名字赛季) 靠左
  %>% cols_align(align = "left", columns = info_html)
  # 数据列 居右
  %>% cols_align(align = "right", columns = c(rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm, caruso))
  
  %>% cols_width(
    logo_html ~ px(60),
    head_html ~ px(70),
    info_html ~ px(160), # 给名字和赛季足够的宽度
    c(rim_ps, reb_oe, stop_pct, defl_resid) ~ px(75),
    c(stat_prior, def_rapm) ~ px(70),
    caruso ~ px(85)
  ) %>%
    
    # --- 字体设置 ---
    tab_style(
      style = cell_text(font = "Consolas", size = px(16), weight = "bold"),
      locations = cells_body(columns = c(rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm, caruso))
    ) %>%
    
    # --- 标题设置 ---
    tab_header(
      title = "NBA 过去十年单赛季最强防守球员 TOP15 (CARUSO版本)",
      subtitle = md("**先验**是对前面四项能力的总结 | **CARUSO** 为综合球员防守数据")
    ) %>%
    tab_style(
      style = cell_text(align = "center", size = px(28), weight = "900", font = "Microsoft YaHei"),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(align = "center", size = px(16), color = "gray50", font = "Microsoft YaHei"),
      locations = cells_title(groups = "subtitle")
    ) %>%
    
    # --- 538 主题 ---
    gt_theme_538(quiet = TRUE) %>%
    # --- 新增：设置列名背景为米白色 ---
    tab_style(
      style = cell_fill(color = "floralwhite"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    
    # --- 原有的 tab_options ---
    tab_options(
      column_labels.font.weight = "bold",
      data_row.padding = px(9),
      table.background.color = "floralwhite"
    )
)

# 保存
gtsave(final_table, "D:/nba/CARUSO_Decade_Top15.png", expand = 3)
browseURL("D:/nba/CARUSO_Decade_Top15.png")