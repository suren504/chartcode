library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)

# ==========================================
# 1. 布局参数与数据源
# ==========================================
col_x_base <- 0
col_rank <- 0.4
col_logo <- 1.2       # 主队logo
col_away <- 2.3       # 客队logo（原头像列）
col_player <- 4.6     # 日期&场馆
col_value <- 7.8
box_mid <- 6.8
box_end <- 8.8

wnba_team_colors <- c(
  "ATL"="#E31837", "CHI"="#5091CD", "CON"="#F05023", "DAL"="#002B5C", "GSV"="#B38FCF",
  "IND"="#002D62", "LAS"="#552583", "LVA"="#A7A8AA", "MIN"="#266092", "NYL"="#86CEBC",
  "PDX"="#CEE5EB", "PHX"="#3C286E", "SEA"="#2C5235", "TOR"="#441E36", "WAS"="#E03A3E"
)
wnba_team_colors_light <- c(
  "ATL"="#F07C8C", "CHI"="#96BDE1", "CON"="#F6967B", "DAL"="#668BB2", "GSV"="#D1BCE2",
  "IND"="#6681A1", "LAS"="#9D7FCA", "LVA"="#CACBCC", "MIN"="#7DA0BE", "NYL"="#B6E2D7",
  "PDX"="#E2EFF3", "PHX"="#C8102E", "SEA"="#809786", "TOR"="#8591A7", "WAS"="#F4A6A6"
)
wnba_team_colors_extra_light <- c(
  "ATL"="#FEEEEE", "CHI"="#F3F7FB", "CON"="#FEF3EF", "DAL"="#F3F4F6", "GSV"="#FAF7FB",
  "IND"="#EDF0F4", "LAS"="#F5F0FC", "LVA"="#F9F9F9", "MIN"="#F0F4F7", "NYL"="#F6FBFA",
  "PDX"="#FBFDFD", "PHX"="#F1F0F5", "SEA"="#F0F3F1", "TOR"="#F1F2F5", "WAS"="#FEEEEE"
)

data <- tibble(
  排名 = 1:15,
  客队 = c("DAL","IND","IND","IND","CHI","IND","IND","MIN","LVA","LVA","DAL","LAS","MIN","CON","PDX"),
  主队 = c("TOR","DAL","PDX","PDX","PDX","TOR","SEA","TOR","GSV","GSV","GSV","GSV","GSV","GSV","GSV"),
  日期 = c("2026-07-10","2026-08-20","2026-07-31","2026-05-30","2026-05-09",
         "2026-08-18","2026-07-28","2026-07-30","2026-10-04","2026-05-31",
         "2026-06-17","2026-06-15","2026-06-19","2026-05-25","2026-06-02"),
  球馆 = c("Bell Centre","American Airlines Center","Moda Center","Moda Center","Moda Center",
         "Scotiabank Arena","Climate Pledge Arena","Scotiabank Arena","Chase Center","Chase Center",
         "Chase Center","Chase Center","Chase Center","Chase Center","Chase Center"),
  数值 = c(20996, 20297, 19959, 19347, 19335, 19300, 18343, 18145,
         18064, 18064, 18064, 18064, 18064, 18064, 18064)
)

# ==========================================
# 2. 数据处理与绘图
# ==========================================
logo_dir <- "D:/nba/球队logo/wnba/"

df <- data %>%
  mutate(
    col_x     = col_x_base,
    row_y     = 排名 * 1.0,
    ymin      = row_y - 0.4,
    ymax      = row_y + 0.4,
    home_logo = paste0(logo_dir, 主队, ".png"),
    away_logo = paste0(logo_dir, 客队, ".png"),
    color     = wnba_team_colors[主队],
    bar_color = wnba_team_colors_light[主队],
    bg_color  = wnba_team_colors_extra_light[主队],
    数值_label = format(数值, big.mark = ",")
  )

p <- ggplot(df) +
  geom_rect(aes(xmin = col_x, xmax = col_x + box_mid, ymin = ymin, ymax = ymax, fill = bg_color), color = NA) +
  geom_rect(aes(xmin = col_x, xmax = col_x + box_mid, ymin = ymin, ymax = ymax, color = color), fill = NA, linewidth = 1.2) +
  geom_rect(aes(xmin = col_x + box_mid + 0.08, xmax = col_x + box_end - 0.08, ymin = ymin, ymax = ymax, fill = bar_color), color = NA) +
  geom_rect(aes(xmin = col_x + box_mid, xmax = col_x + box_end, ymin = ymin - 0.07, ymax = ymax + 0.07, color = color), fill = NA, linewidth = 1.2) +
  
  geom_text(aes(x = col_x + col_rank, y = row_y, label = paste0("#", 排名)), size = 6, fontface = "bold", family = "Consolas") +
  geom_image(aes(x = col_x + col_logo, y = row_y, image = home_logo), size = 0.065) +
  geom_image(aes(x = col_x + col_away, y = row_y, image = away_logo), size = 0.065) +
  
  # 日期（上）+ 场馆（下）两行
  geom_shadowtext(aes(x = col_x + col_player, y = row_y - 0.17, label = 日期, bg.colour = bg_color),
                  size = 6, fontface = "bold", family = "Consolas", colour = "#555555", bg.r = 0.12) +
  geom_shadowtext(aes(x = col_x + col_player, y = row_y + 0.15, label = 球馆, bg.colour = bg_color),
                  size = 7, fontface = "bold", family = "Consolas", colour = "black", bg.r = 0.12) +
  
  geom_shadowtext(aes(x = col_x + col_value, y = row_y, label = 数值_label),
                  size = 10, fontface = "bold", colour = "white", bg.colour = "black", family = "Consolas") +
  
  annotate("text", x = col_x_base + col_rank,   y = 0.3, label = "排名",      size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_base + col_logo,   y = 0.3, label = "主队",      size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_base + col_away,   y = 0.3, label = "客队",      size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_base + col_player, y = 0.3, label = "日期&场馆", size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_base + col_value,  y = 0.3, label = "观赛人数",  size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  
  coord_cartesian(clip = "off", xlim = c(-0.1, 8.9), ylim = c(15.5, 0.2)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "floralwhite", colour = NA),
    plot.background  = element_rect(fill = "floralwhite", colour = NA),
    plot.margin = margin(10, 5, 10, 5),
    plot.title    = element_markdown(family = "Microsoft YaHei", face = "bold", hjust = 0.5, size = 22, lineheight = 1.3, margin = margin(b = 8)),
    plot.subtitle = element_markdown(family = "Microsoft YaHei", size = 12, face = "bold", color = "#555555", hjust = 0.5, lineheight = 1.6, margin = margin(b = 5))
  ) +
  labs(
    title = "2026赛季WNBA单场现场观众人数TOP15",
    subtitle = "日期口径为当地时间|制图: <span style='font-size:16pt; color:black;'>**俗人罢了**</span>|数据截至总决赛开始前"
  )

ggsave("WNBA_Attendance_TOP15_9x16.png", p, width = 9, height = 16, dpi = 300)