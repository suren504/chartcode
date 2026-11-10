library(tidyverse)
library(ggchicklet) 
library(ggpath)
library(prismatic)
library(scales)
library(ggtext)

# ————————————————————————————————————————
# 1. 数据准备 (Data Preparation)
# ————————————————————————————————————————
nba_team_colors <- c(
  "ATL"="#E03A3E", "BOS"="#007A33", "BKN"="#000000",
  "CHA"="#00788C", "CHI"="#CE1141", "CLE"="#860038",
  "DAL"="#00538C", "DEN"="#FEC524", "DET"="#1D428A",
  "GSW"="#FFB81C", "HOU"="#CE1141", "IND"="#FDBB30",
  "LAC"="#002147", "LAL"="#552583", "MEM"="#5D76A9",
  "MIA"="#98002E", "MIL"="#00471B", "MIN"="#0C2340",
  "NOP"="#8C4A24", "NYK"="#F58426", "OKC"="#007AC1",
  "ORL"="#0077C0", "PHI"="#006BB6", "PHX"="#E56020",
  "POR"="#E03A3E", "SAC"="#5A2D81", "SAS"="#C4CED4",
  "TOR"="#CE1141", "UTA"="#002B5C", "WAS"="#E31837"
)

nba_team_colors_light <- c(
  "ATL"="#F4A6A6", "BOS"="#66B266", "BKN"="#666666",
  "CHA"="#66B2C1", "CHI"="#F27A8E", "CLE"="#C5667A",
  "DAL"="#66A3D1", "DEN"="#FFD76A", "DET"="#7893C9",
  "GSW"="#FFD36F", "HOU"="#F27A8E", "IND"="#FED584",
  "LAC"="#667D96", "LAL"="#9D7FCA", "MEM"="#9DB4D6",
  "MIA"="#D1667A", "MIL"="#4D8C4D", "MIN"="#5D7AAB",
  "NOP"="#bc8a5f", "NYK"="#F9B57D", "OKC"="#66B8E6",
  "ORL"="#66B2E6", "PHI"="#66A3D1", "PHX"="#EF9C6C",
  "POR"="#F4A6A6", "SAC"="#9D7FCA", "SAS"="#E1E7EB",
  "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)

combined_data <- tribble(
  ~rank, ~team,          ~abbrev, ~violations_forced, ~late_3s_freq, ~late_3s_pct,
  1, "Atlanta",       "ATL",                   31,       0.0806,      "8.1%",
  2, "Oklahoma City", "OKC",                   28,       0.0712,      "7.1%",
  3, "Dallas",        "DAL",                   26,       0.0671,      "6.7%",
  4, "Miami",         "MIA",                   23,       0.0922,      "9.2%",
  5, "Milwaukee",     "MIL",                   23,       0.0756,      "7.6%",
  6, "Cleveland",     "CLE",                   21,       0.0817,      "8.2%",
  7, "Portland",      "POR",                   19,       0.0823,      "8.2%",
  8, "Toronto",       "TOR",                   19,       0.0811,      "8.1%",
  9, "Detroit",       "DET",                   19,       0.0793,      "7.9%",
  10, "Phoenix",       "PHX",                   18,       0.0795,      "8.0%",
  11, "Minnesota",     "MIN",                   18,       0.0783,      "7.8%",
  12, "San Antonio",   "SA",                    18,       0.0690,      "6.9%",
  13, "New Orleans",   "NO",                    18,       0.0673,      "6.7%",
  14, "Chicago",       "CHI",                   17,       0.0731,      "7.3%",
  15, "Boston",        "BOS",                   17,       0.0721,      "7.2%",
  16, "Denver",        "DEN",                   17,       0.0667,      "6.7%",
  17, "Indiana",       "IND",                   16,       0.0743,      "7.4%",
  18, "Memphis",       "MEM",                   15,       0.0702,      "7.0%",
  19, "LA",            "LAC",                   15,       0.0674,      "6.7%",
  20, "Philadelphia",  "PHI",                   15,       0.0581,      "5.8%",
  21, "Brooklyn",      "BKN",                   14,       0.0743,      "7.4%",
  22, "Golden State",  "GS",                    14,       0.0583,      "5.8%",
  23, "Sacramento",    "SAC",                   13,       0.0661,      "6.6%",
  24, "Washington",    "WSH",                   13,       0.0564,      "5.6%",
  25, "Utah",          "UTAH",                  12,       0.0602,      "6.0%",
  26, "Houston",       "HOU",                   11,       0.0643,      "6.4%",
  27, "Los Angeles",   "LAL",                   10,       0.0674,      "6.7%",
  28, "New York",      "NY",                    10,       0.0576,      "5.8%",
  29, "Orlando",       "ORL",                    9,       0.0787,      "7.9%",
  30, "Charlotte",     "CHA",                    9,       0.0703,      "7.0%"
)

# ————————————————————————————————————————
# 2. 数据处理 (Data Wrangling)
# ————————————————————————————————————————
scale_factor <- 3 

df_plot <- combined_data %>%
  mutate(
    color_key = case_when(
      abbrev == "GS"   ~ "GSW", abbrev == "NO"   ~ "NOP", abbrev == "NY"   ~ "NYK",
      abbrev == "SA"   ~ "SAS", abbrev == "UTAH" ~ "UTA", abbrev == "WSH"  ~ "WAS",
      TRUE ~ abbrev
    ),
    primary_color = nba_team_colors[color_key],
    secondary_color = nba_team_colors_light[color_key],
    logo_local_path = paste0("D:/nba/球队logo/球队logo方/", abbrev, ".png"),
    
    val_bottom = -violations_forced, 
    val_top = late_3s_freq * 100 * scale_factor 
  )

# ————————————————————————————————————————
# 3. 绘图 (Plotting) - 修正报错逻辑
# ————————————————————————————————————————
# 确保 p 赋值操作紧跟在管道符之前，没有断行或多余字符
p <- df_plot %>% 
  ggplot(aes(x = fct_reorder(abbrev, violations_forced))) + 
  geom_chicklet(aes(y = val_bottom, fill = primary_color, color = after_scale(clr_darken(fill, 0.3))), 
                alpha = .9, width = 1, radius = grid::unit(3, "pt")) + 
  geom_chicklet(aes(y = val_top, fill = secondary_color, color = after_scale(clr_darken(fill, 0.3))), 
                alpha = .9, width = 1, radius = grid::unit(3, "pt")) + 
  geom_text(aes(y = val_bottom, label = violations_forced), 
            hjust = 1.4, size = 3.5, family = "Consolas", fontface = "bold", color = "black") + 
  geom_text(aes(y = val_top, label = late_3s_pct), 
            hjust = -0.4, size = 3.5, family = "Consolas", fontface = "bold", color = "black") + 
  geom_from_path(aes(y = 0, path = logo_local_path), width = 0.02) + 
  scale_y_continuous(
    breaks = seq(-30, 30, 10),
    labels = c("30", "20", "10", "", "3.3%", "6.7%", "10%"),
    limits = c(-45, 45) 
  ) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_fill_identity() + 
  coord_flip(clip = 'off') + 
  # --- 5. 主题设置 (FloralWhite 风格) ---
  theme_minimal() + 
  theme(
    # 大标题：字号加大，居中
    plot.title = element_markdown(face = "bold", size = 22, hjust = 0.5, margin = margin(b = 10)),
    # 副标题：保持原有风格，居中
    plot.subtitle = element_markdown(size = 10, hjust = 0.5, color = "gray30", margin = margin(b = 20)),
    # 脚注：居中显示
    plot.caption = element_text(size = 9, hjust = 0.5, color = "gray30", margin = margin(t = 15)),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linetype = "dotted"),
    legend.position = "none",
    
    # 背景设置
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    
    # 边距设置
    plot.margin = margin(20, -80, 10, -100)
  ) + 
  labs(
    title = "NBA球队防守压迫对比图：防24秒违例次数vs让对手难以出手频率",
    subtitle = "**深色(左)**: 24s违例(Shot Clock Violations Forced)|**浅色(右)**: 对手出手时间<3s占所有投篮出手比重",
    x = NULL, 
    y = "← 违例次数 (次) | 压迫频率 (%) →",
    caption = "数据源：hoopR | 制作人：俗人罢了 | 统计截止：12.29"
  )

# ————————————————————————————————————————
# 4. 保存与预览
# ————————————————————————————————————————
output_file <- "nba_defense_mirror_final_custom.png"
ggsave(filename = output_file, plot = p, width = 12, height = 9, dpi = 600)
browseURL(output_file)