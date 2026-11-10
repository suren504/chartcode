library(tidyverse)
library(gt)
library(gtExtras)
library(glue)

# ==============================================================================
# 1. Data Preparation (录入 2025-26 赛季数据)
# ==============================================================================

# 替换后的数据框（更新为最新15条数据）
df_2026 <- tibble(
  player = c("Alex Caruso", "Ajay Mitchell", "Neemias Queta", "Cason Wallace", 
             "Jaylin Williams", "Ronald Holland II", "Rudy Gobert", "Chet Holmgren", 
             "Isaiah Hartenstein", "Victor Wembanyama", "Jalen Suggs", 
             "Javonte Green", "Dru Smith", "Vince Williams Jr", "Brook Lopez"),
  team_abbr = c("OKC", "OKC", "BOS", "OKC", "OKC", "DET", "MIN", "OKC", 
                "OKC", "SAS", "ORL", "DET", "MIA", "MEM", "LAC"),
  rim_ps = c(39.01091946, 56.150217, 98.95664548, 52.77861595, 80.36675471, 
             77.32113224, 97.70536464, 99.90334153, 98.5110448, 96.20062282, 
             49.31980665, 60.67745662, 40.00814274, 54.43248246, 99.15010197),
  reb_oe = c(71.25575589, 82.84085877, 98.32424488, 43.2068815, 98.00403386, 
             77.08000494, 92.82586112, 91.81340096, 96.99746488, 98.98000787, 
             2.692506736, 42.82935095, 38.22155887, 63.33098854, 78.1532262),
  stop_pct = c(99.87228055, 91.91973201, 81.72553238, 95.67014688, 66.2585337, 
               97.93623638, 65.43247689, 88.84940713, 77.78144199, 96.23989022, 
               99.67409987, 97.23497872, 98.80573181, 80.41003359, 81.25841404),
  defl_resid = c(93.91543842, 27.16088979, 37.97192244, 78.14231241, 55.15624524, 
                 3.577691563, 57.62374965, 55.5316894, 80.97888951, 95.95204655, 
                 39.4109832, 71.61755798, 58.18457858, 86.07854171, 90.6083919),
  stat_prior = c(2.5175169, 1.0308803, 2.2696888, 0.8591704, 1.5406588, 
                 0.5709255, 0.7968114, 1.1340065, 1.6630278, 0.4473826, 
                 0.81088823, 0.7521389, 1.2482604, 0.9188746, 1.4197607),
  def_rapm = c(1.4, 3.0, 1.2, 3.1, 2.2, 2.9, 2.3, 1.8, 0.7, 2.5, 2.1, 1.9, 0.8, 1.4, 0.1),
  caruso = c(2.192532594, 1.891931579, 1.843401628, 1.841921721, 1.779225858, 
             1.644161867, 1.514626948, 1.425180056, 1.362371715, 1.202167569, 
             1.184572873, 1.163497875, 1.094929036, 1.059671848, 1.042495669)
)

# 1.2 增强版球队中文映射表（新增MIA、LAC的映射）
team_map_cn <- c(
  "OKC" = "俄克拉荷马城雷霆", "BOS" = "波士顿凯尔特人", "DET" = "底特律活塞", 
  "MIN" = "明尼苏达森林狼", "SAS" = "圣安东尼奥马刺", "ORL" = "奥兰多魔术", 
  "MEM" = "孟菲斯灰熊", "DAL" = "达拉斯独行侠", "MIA" = "迈阿密热火", 
  "LAC" = "洛杉矶快船"
)

# 球员中文映射表（新增3名球员的映射）
name_map_cn <- c(
  "Alex Caruso" = "亚历克斯·卡鲁索", "Ajay Mitchell" = "阿贾·米切尔", "Neemias Queta" = "内米亚斯·奎塔",
  "Cason Wallace" = "卡森·华莱士", "Jaylin Williams" = "杰林·威廉姆斯", "Ronald Holland II" = "罗恩·霍兰德",
 "Rudy Gobert" = "鲁迪·戈贝尔", "Chet Holmgren" = "切特·霍姆格伦",
  "Isaiah Hartenstein" = "以赛亚·哈尔滕施泰因", "Victor Wembanyama" = "维克托·文班亚马",
  "Jalen Suggs" = "杰伦·萨格斯", "Javonte Green" = "贾冯特·格林", 
 "Dru Smith" = "德鲁·史密斯", 
  "Vince Williams Jr" = "文斯·威廉姆斯", "Brook Lopez" = "布鲁克·洛佩兹"
)
# ==============================================================================
# 2. Data Processing (Logo移至第一列)
# ==============================================================================

plot_df <- df_2026 %>%
  mutate(
    name_cn = name_map_cn[player],
    team_cn = team_map_cn[team_abbr],
    
    file_name_clean = str_replace_all(player, " ", "_"),
    path_logo = paste0("D:/nba/球队logo/球队logo方/", team_abbr, ".png"),
    path_head = paste0("file:///D:/nba/球员头像/", file_name_clean, ".png"),
    
    logo_html = glue("<div style='display:flex; justify-content:center; align-items:center;'><img src='{path_logo}' style='height:60px; width:60px; object-fit:contain;'></div>"),
    head_html = glue("<div style='display:flex; justify-content:center; align-items:center;'><img src=\"{path_head}\" style='height:65px; width:65px; object-fit:cover; border-radius:50%; background-color:#f8f8f8; border: 1px solid #ddd;'></div>")
  ) %>%
  # --- 关键修改：调整顺序，logo_html 放第一位 ---
  select(logo_html, head_html, name_cn, team_cn, 
         rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm, caruso)

# ==============================================================================
# 3. Plotting & Styling (Logo左置最终版)
# ==============================================================================

# 定义颜色变量
color_light_bg    <- "#F2F2F2" 
color_soft_red    <- "#F7565A" 
color_soft_orange <- "#FF9800"

final_table <- suppressWarnings(
  plot_df %>%
    gt(id = "caruso_2026_left_logo") %>%
    fmt_markdown(columns = c(head_html, logo_html)) %>%
    
    # 字体保持 18px / 12px
    gt_merge_stack(
      col1 = name_cn, col2 = team_cn,
      font_size = c("18px", "12px"), 
      font_weight = c("bold", "normal"),
      palette = c("black", "gray60")
    ) %>%
    
    fmt_number(
      columns = c(rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm, caruso),
      decimals = 2
    ) %>%
    
    # --- 颜色设置 ---
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
    
    # --- 列名映射 ---
    cols_label(
      logo_html = "球队",   # 第一列
      head_html = "球员",   # 第二列
      name_cn = "",         # 第三列(名字)标题留空
      rim_ps = "护框能力", reb_oe = "额外篮板", stop_pct = "终结回合",
      defl_resid = "破坏球", stat_prior = "先验",
      def_rapm = "单赛季DRAPM", caruso = "CARUSO"
    ) %>%
    
    # --- 对齐方式调整 ---
    cols_align(align = "center", columns = logo_html) %>% # 队标居中
    cols_align(align = "right", columns = head_html) %>%  # 头像靠右(贴近名字)
    cols_align(align = "left", columns = name_cn) %>%     # 名字靠左(贴近头像)
    cols_align(
      align = "right", 
      columns = c(rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm, caruso)
    ) %>%
    
    # 数字字体
    tab_style(
      style = cell_text(font = "Consolas", size = px(18), weight = "bold"),
      locations = cells_body(columns = c(rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm, caruso))
    ) %>%
    
    # --- 宽度控制 ---
    cols_width(
      logo_html ~ px(60),   # 球队放最左
      head_html ~ px(70),   # 头像紧随其后
      name_cn ~ px(170),    # 名字
      c(rim_ps, reb_oe, stop_pct, defl_resid, stat_prior, def_rapm) ~ px(82),
      caruso ~ px(90)       
    ) %>%
    
    tab_header(
      title = "NBA 2025-26 赛季 CARUSO 防守影响力联盟前15",
      subtitle = md("CARUSO从护框能力|额外防守篮板|终结回合|破坏球四个维度评价球员防守，先验是对四个维度的总结")) %>%
    
    tab_style(
      style = cell_text(align = "center", size = px(30), weight = "900", font = "Microsoft YaHei"),
      locations = cells_title(groups = "title")
    ) %>%
    # ========== 新增/修改：副标题居中 ==========
  tab_style(
    style = cell_text(align = "center", size = px(18), color = "gray50", font = "Microsoft YaHei"),
    locations = cells_title(groups = "subtitle")
  ) %>%
    # ==========================================
  gt_theme_538(quiet = TRUE) %>%
    tab_options(
      column_labels.font.weight = "bold",
      column_labels.font.size = px(14),
      data_row.padding = px(8),
      table.width = px(950),
      table.background.color = "white"
    )
)

# 保存
gtsave(final_table, "D:/nba/CARUSO_2026_Final_LeftLogo.png", expand = 5)
browseURL("D:/nba/CARUSO_2026_Final_LeftLogo.png")