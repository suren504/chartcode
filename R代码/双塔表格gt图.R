library(tidyverse)
library(gt)
library(gtExtras)
library(glue)

# ==============================================================================
# 1. 完整数据录入 (请在这里替换为你的新数据)
# ==============================================================================
raw_df <- tibble(
  team_en = c("CHI", "CLE", "DET", "MIA", "MIL", "NYK", "HOU", "MIN", "OKC", "UTA"),
  players_combined = c(
    "Jalen Smith&Nikola Vucevic",
    "Jarrett Allen&Evan Mobley",
    "Jalen Duren&Isaiah Stewart",
    "Bam Adebayo&Kel'el Ware",
    "Myles Turner&Jericho Sims",
    "Karl-Anthony Towns&Mitchell Robinson",
    "Alperen Sengun&Steven Adams",
    "Rudy Gobert&Naz Reid",
    "Chet Holmgren&Isaiah Hartenstein",
    "Jusuf Nurkic&Kyle Filipowski"
  ),
  min = c(221, 431, 273, 284, 174, 202, 315, 613, 397, 201),
  off_rtg = c(127.1, 122.9, 119.5, 119.2, 110.6, 117.8, 117.6, 109.3, 126.4, 112.9),
  def_rtg = c(119.9, 113.0, 118.0, 115.7, 119.7, 107.4, 110.9, 102.8, 106.1, 121.2),
  net_rtg = c(7.2, 9.9, 1.5, 3.4, -9.1, 10.4, 6.7, 6.4, 20.3, -8.3)
)
# ==============================================================================
# 2. 映射字典与颜色配置 (补充所有涉及球员的中英对照)
# ==============================================================================
player_dict <- c(
  # 核心球员映射
  "Jalen Smith" = "杰伦·史密斯", "Nikola Vucevic" = "武切维奇",
  "Jarrett Allen" = "贾勒特·阿伦", "Evan Mobley" = "埃文·莫布利",
  "Jalen Duren" = "杰伦·杜伦", "Isaiah Stewart" = "以赛亚·斯图尔特",
  "Bam Adebayo" = "巴姆·阿德巴约", "Kel'el Ware" = "凯尔·维尔",
  "Myles Turner" = "迈尔斯·特纳", "Jericho Sims" = "杰里科·西姆斯",
  "Karl-Anthony Towns" = "卡尔-安东尼·唐斯", "Mitchell Robinson" = "米切尔·罗宾逊",
  "Alperen Sengun" = "阿尔佩伦·申京", "Steven Adams" = "史蒂文·亚当斯",
  "Clint Capela" = "克林特·卡佩拉", "Rudy Gobert" = "鲁迪·戈贝尔",
  "Naz Reid" = "纳兹·里德", "Chet Holmgren" = "切特·霍姆格伦",
  "Isaiah Hartenstein" = "以赛亚·哈尔滕施泰因", "Jusuf Nurkic" = "尤素夫·努尔基奇",
  "Kyle Filipowski" = "凯尔·菲利波夫斯基"
)

nba_team_colors_light <- c(
  "ATL"="#E03A3E", "BOS"="#008348", "BKN"="#666666",
  "CHA"="#00788C", "CHI"="#CD1F43", "CLE"="#860038",
  "DAL"="#0066B5", "DEN"="#FFD76A", "DET"="#C8102E",
  "GSW"="#FFD36F", "HOU"="#CE1141", "IND"="#FEBC30",
  "LAC"="#12173F", "LAL"="#9D7FCA", "MEM"="#5D76A9",
  "MIA"="#961A31", "MIL"="#004C26", "MIN"="#266092",
  "NOP"="#B89A5B", "NYK"="#F58426", "OKC"="#007AC0",
  "ORL"="#0B77BD", "PHI"="#1D428A", "PHX"="#EF9C6C",
  "POR"="#CF0A2C", "SAC"="#592D81", "SAS"="#E1E7EB",
  "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)

# ==============================================================================
# 3. 数据处理
# ==============================================================================
plot_df <- raw_df %>%
  separate(players_combined, into = c("p1_name", "p2_name"), sep = "&", remove = FALSE) %>%
  mutate(
    path_team_logo = paste0("D:/nba/球队logo/球队logo方/", team_en, ".png"),
    
    # 获取对应球队的主题色，如果没有匹配到则默认使用原先的灰色
    team_color = coalesce(nba_team_colors_light[team_en], "#f0f0f0"),
    
    # 路径清洗
    p1_file = p1_name %>% str_remove_all("\\.") %>% str_replace_all("III", "3") %>% str_replace_all(" ", "_"),
    p2_file = p2_name %>% str_remove_all("\\.") %>% str_replace_all("III", "3") %>% str_replace_all(" ", "_"),
    path_head1 = paste0("file:///D:/nba/球员头像/", p1_file, ".png"),
    path_head2 = paste0("file:///D:/nba/球员头像/", p2_file, ".png"),
    
    p1_cn = coalesce(player_dict[p1_name], p1_name),
    p2_cn = coalesce(player_dict[p2_name], p2_name),
    
    duo_heads = glue::glue(
      "<div style='display:flex; justify-content:center; align-items:center; padding-left:25px; position:relative;'>
  <img src=\"{path_head1}\" style='height:70px; width:70px; border-radius:50%; z-index:1; background-color:{team_color}; object-fit:cover; position:relative; box-shadow:0 2px 4px rgba(0,0,0,0.1);'>
  <!-- 核心调整：从-30px改为-20px，重叠更少、间隔更宽 -->
  <img src=\"{path_head2}\" style='height:70px; width:70px; border-radius:50%; z-index:2; background-color:{team_color}; object-fit:cover; position:relative; margin-left:-20px; box-shadow:0 2px 4px rgba(0,0,0,0.2);'>
</div>"
    )
  ) %>%
  arrange(desc(net_rtg)) %>%
  select(path_team_logo, duo_heads, p1_cn, p2_cn, min, off_rtg, def_rtg, net_rtg)

# ==============================================================================
# 4. 绘图设置
# ==============================================================================
gt_theme_china_big <- function(gt_object) {
  gt_object %>%
    opt_table_font(font = list("Microsoft Yahei", "Arial", default_fonts())) %>%
    tab_style(
      style = cell_text(font = "Consolas", weight = "bold", size = px(25)),
      locations = cells_body(columns = where(is.numeric))
    ) %>%
    tab_options(
      column_labels.background.color = "floralwhite",
      table.background.color = "floralwhite",
      table_body.hlines.color = "gray90",
      data_row.padding = px(10), 
      column_labels.font.weight = "bold",
      column_labels.font.size = px(18), 
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black"
    )
}

# ==============================================================================
# 5. 生成最终表格
# ==============================================================================
final_table <- plot_df %>%
  gt() %>%
  fmt_image(columns = path_team_logo, height = 80) %>%
  fmt_markdown(columns = duo_heads) %>%
  
  gt_merge_stack(
    col1 = p1_cn, col2 = p2_cn,
    font_size = c("20px", "20px"),
    font_weight = c("bold", "bold"),
    palette = c("black", "black"), 
    small_cap = FALSE
  ) %>%
  
  cols_label(
    path_team_logo = "球队", duo_heads = "球员靓照", p1_cn = "内线双塔组合",
    min = "出场时间", off_rtg = "进攻效率", def_rtg = "防守效率", net_rtg = "净效率"
  ) %>%
  
  cols_align(align = "center", columns = c(path_team_logo, duo_heads, p1_cn)) %>%
  # 第二部分：右对齐（出场时间、进攻效率、防守效率、净效率）
  cols_align(align = "right", columns = c(min, off_rtg, def_rtg, net_rtg)) %>%
  
  cols_width(
    path_team_logo ~ px(80),
    duo_heads ~ px(160),  
    p1_cn ~ px(240),
    min ~ px(100),
    everything() ~ px(100)
  ) %>%
  
  fmt_number(columns = c(off_rtg, def_rtg, net_rtg), decimals = 1) %>%
  fmt_number(columns = net_rtg, force_sign = TRUE) %>%
  
  # --- 修复1：出场时间（指定domain：100-700，匹配过滤条件+最大值）---
  gt_color_rows(
    columns = min,
    palette = c('#F2F2F2', '#F7565A'), 
    domain = c(100, 700)  # 手动指定范围，消除警告
  ) %>%
  
  # --- 修复2：进攻效率（指定domain：100-130，覆盖所有数据极值）---
  gt_color_rows(
    columns = off_rtg,
    palette = c('#F2F2F2', '#F7565A'), 
    domain = c(100, 130)
  ) %>%
  
  # --- 修复3：防守效率（指定domain：100-130，反向渐变）---
  gt_color_rows(
    columns = def_rtg,
    palette = c('#F7565A', '#F2F2F2'), 
    domain = c(100, 130)
  ) %>%
  
  # --- 修复4：净效率（指定domain：-25-25，原代码已有但补充明确）---
  gt_color_rows(
    columns = net_rtg,
    palette = "ggsci::green_material",
    domain = c(-25, 25)
  ) %>%
  
  gt_theme_china_big() %>%
  tab_header(
    title = md("**🏀 NBA2025-26 赛季：“双塔”组合排行榜**"),
    # --- 新增：利用 HTML 标签单独对“俗人罢了”进行加粗和字体放大 ---
    subtitle = md("“双塔”是BBR网站定义的本赛季有50%以上时间打中锋的球员 | 至少双人组出场时间100分钟 | 数据颜色越深代表越好<br>制作人：<span style='font-size:18px; font-weight:900;'>俗人罢了</span> | 统计截止：2026全明星周末| 数据源：databallr")
  )



# 导出高清图
gtsave(final_table, "D:/nba/双塔中文版_Pro_Big.png", expand = 5)