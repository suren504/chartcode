library(tidyverse)
library(ggtext)
library(ggforce)
library(ggimage)

# ==============================================================================
# 1. 数据
# ==============================================================================

df_2526 <- tribble(
  ~abbr, ~games_2526,
  "ATL",13, "BOS",25, "BKN",2, "CHA",3, "CHI",3, "CLE",24,
  "DAL",23, "DEN",26, "DET",16, "GSW",34, "HOU",28, "IND",9,
  "LAC",21, "LAL",34, "MEM",10, "MIA",5, "MIL",18, "MIN",28,
  "NOP",2, "NYK",34, "OKC",34, "ORL",14, "PHI",14, "PHX",9,
  "POR",8, "SAC",9, "SAS",22, "TOR",2, "UTA",2, "WAS",2
)

df_2627 <- tribble(
  ~abbr, ~games_2627,
  "ATL",10, "BOS",27, "BKN",2, "CHA",6, "CHI",3, "CLE",17,
  "DAL",21, "DEN",28, "DET",19, "GSW",28, "HOU",21, "IND",11,
  "LAC",8, "LAL",34, "MEM",2, "MIA",25, "MIL",2, "MIN",27,
  "NOP",2, "NYK",34, "OKC",30, "ORL",8, "PHI",34, "PHX",9,
  "POR",11, "SAC",2, "SAS",34, "TOR",7, "UTA",6, "WAS",6
)

# ==============================================================================
# 2. 数据处理
# ==============================================================================

df_plot <- df_2526 %>%
  left_join(df_2627, by = "abbr") %>%
  mutate(
    diff = games_2627 - games_2526,
    change_type = case_when(diff > 0 ~ "More", diff < 0 ~ "Less", TRUE ~ "Same"),
    diff_label = case_when(diff > 0 ~ paste0("+", diff), diff < 0 ~ as.character(diff), TRUE ~ "0"),
    value_label = paste0(games_2627, " (", diff_label, ")"),
    label_x = case_when(
      diff > 0 ~ games_2627 + 0.8,
      diff < 0 ~ games_2526 + 0.8,
      TRUE ~ games_2627 + 0.8
    ),
    sort_value = games_2627 + diff / 100,
    abbr = fct_reorder(abbr, sort_value),
    logo_path = paste0("D:/nba/球队logo/队标/", as.character(abbr), ".png")
  )

# ==============================================================================
# 3. 绘图
# ==============================================================================
# 图表中央的离散 Y 轴位置，用于水印
watermark_y <- levels(df_plot$abbr)[15]

p <- ggplot(df_plot) +
  
  annotate(
    "text",
    x = 20,
    y = watermark_y,
    label = "俗人罢了",
    family = "Microsoft YaHei",
    fontface = "bold",
    size = 55,
    angle = 45,
    color = "gray60",
    alpha = 0.055
  ) +
  
  geom_link(
    aes(
      x = games_2526,
      xend = games_2627,
      y = abbr,
      yend = abbr,
      color = change_type,
      linewidth = after_stat(index)
    ),
    n = 500,
    lineend = "round"
  ) +
  
  # 上赛季起点
  geom_point(
    aes(x = games_2526, y = abbr),
    shape = 21, fill = "floralwhite", color = "#B8B8B8",
    size = 2.8, stroke = 1
  ) +
  
  # 新赛季终点
  geom_point(
    aes(x = games_2627, y = abbr, color = change_type),
    shape = 21, fill = "white",
    size = 4.5, stroke = 1.6
  ) +
  
  # 球队 Logo
  geom_image(
    aes(x = -1.3, y = abbr, image = logo_path),
    size = 0.034, asp = 9/16
  ) +
  
  # 新赛季场次 + 同比变化
  geom_text(
    aes(x = label_x, y = abbr, label = value_label, color = change_type),
    hjust = 0,
    family = "Consolas",
    fontface = "bold",
    size = 3.8
  ) +
  
  scale_color_manual(values = c(
    "More" = "#409646",
    "Less" = "#D94451",
    "Same" = "#8A8A8A"
  )) +
  
  scale_linewidth(range = c(0.15, 3.6), guide = "none") +
  
  scale_x_continuous(
    breaks = seq(0, 35, by = 5),
    labels = function(x) paste0(x, "场"),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = paste0(
      "NBA2026-27常规赛球队全美直播场次变化图 ",
      "(<span style='color:#409646'>增加</span> vs ",
      "<span style='color:#D94451'>减少</span>)"
    ),
    subtitle = "对比2025-26赛季 | 按2026-27赛季全美直播场次降序排列 | 括号内为同比变化",
    x = "常规赛全美直播场次",
    y = "",
    caption = "数据范围：ABC / ESPN、NBC / Peacock、Amazon Prime Video  |  制作人：俗人罢了"
  ) +
  
  theme_minimal(base_family = "Consolas") +
  
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    legend.position = "none",
    plot.title.position = "plot",
    
    plot.title = element_markdown(
      family = "Microsoft YaHei",
      size = 20,
      face = "bold",
      color = "#222222",
      lineheight = 1.2,
      margin = margin(b = 8)
    ),
    
    plot.subtitle = element_text(
      family = "Microsoft YaHei",
      size = 10.5,
      color = "#8A8A8A",
      margin = margin(b = 16)
    ),
    
    axis.text.y = element_blank(),
    
    axis.text.x = element_text(
      family = "Microsoft YaHei",
      size = 11,
      face = "bold",
      color = "#555555"
    ),
    
    axis.title.x = element_text(
      family = "Microsoft YaHei",
      size = 12,
      face = "bold",
      color = "#333333",
      margin = margin(t = 12)
    ),
    
    panel.grid.major.y = element_line(
      color = "#E5E1DA",
      linetype = "dashed",
      linewidth = 0.35
    ),
    
    panel.grid.major.x = element_line(
      color = "#E8E4DD",
      linewidth = 0.35
    ),
    
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      family = "Microsoft YaHei",
      size = 9,
      color = "#999999",
      hjust = 0,
      margin = margin(t = 15)
    ),
    
    plot.margin = margin(25, 70, 25, 30)
  ) +
  
  coord_cartesian(
    clip = "off",
    xlim = c(0, 40)
  )

# ==============================================================================
# 4. 保存
# ==============================================================================

ggsave(
  "NBA_2026-27_全美直播场次变化图.png",
  p,
  width = 9,
  height = 16,
  dpi = 300,
  bg = "floralwhite"
)