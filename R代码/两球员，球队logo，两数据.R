# Assist Flow Visualization (fixed)
library(tidyverse)
library(ggimage)
library(shadowtext)
library(grid)

# ================== 数据准备 ==================
df <- tibble(
  passer = c("Jamal Murray","Josh Giddey","Keyonte George","James Harden","Cade Cunningham",
             "Nikola Jokić","LaMelo Ball","Austin Reaves","Luka Dončić","LaMelo Ball",
             "Nikola Jokić","Draymond Green","Davion Mitchell","Shai Gilgeous-Alexander","Nikola Jokić"),
  receiver = c("Nikola Jokić","Nikola Vučević","Lauri Markkanen","Ivica Zubac","Jalen Duren",
               "Aaron Gordon","Miles Bridges","Deandre Ayton","Deandre Ayton","Kon Knueppel",
               "Christian Braun","Stephen Curry","Norman Powell","Chet Holmgren","Cameron Johnson"),
  team = c("DEN","CHI","UTA","LAC","DET","DEN","CHA","LAL","LAL","CHA",
           "DEN","GSW","MIA","OKC","DEN"),
  assists = c(37,36,36,34,31,29,28,27,27,26,25,25,25,24,24),
  points_created = c(81,88,83,68,62,69,67,54,54,70,52,63,65,57,55),
  passer_id = c(1627750,1630581,1641718,201935,1630595,203999,1630163,1630559,1629029,1630163,203999,203110,1630558,1628983,203999),
  receiver_id = c(203999,202696,1628374,1627826,1631105,203932,1628970,1629028,1629028,1642851,1631128,201939,1626181,1631096,1629661)
) %>%
  mutate(
    passer_head = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", passer_id, ".png"),
    receiver_head = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", receiver_id, ".png"),
    team_logo = paste0("D:/nba/球队logo/球队logo圆/", team, ".png") # 注意：该路径为本地路径，需确保存在
  ) %>%
  arrange(desc(assists)) %>%
  mutate(y = row_number()) %>%
  mutate(y = max(y) - y + 1)

# 顶部说明栏的 y
top_y <- max(df$y) + 1

# ================== 图表绘制 ==================
p <- ggplot(df, aes(y = y)) +
  
  # 金色热度条
  geom_rect(
    aes(xmin = 4.6, xmax = 5.2, ymin = y - 0.35, ymax = y + 0.35, fill = assists),
    colour = NA
  ) +
  scale_fill_gradient(high = "#ffd700", low = "#ffe4b5") +
  guides(fill = "none") +
  
  # 说明栏
  annotate("text", x = 1,   y = top_y, label = "助攻者",     size = 6, fontface = "bold") +
  annotate("text", x = 3,   y = top_y, label = "受助攻人",   size = 6, fontface = "bold") +
  annotate("text", x = 4,   y = top_y, label = "球队",       size = 6, fontface = "bold") +
  annotate("text", x = 4.9, y = top_y, label = "助攻次数",   size = 6, fontface = "bold") +
  annotate("text", x = 5.8, y = top_y, label = "创造得分",   size = 6, fontface = "bold") +
  
  # 助攻者头像
  geom_image(aes(x = 1, image = passer_head), size = 0.10, by = "width") +
  
  # 箭头 + “助攻”：将size替换为linewidth（核心修改点）
  geom_segment(
    aes(x = 1.45, xend = 2.25, y = y, yend = y),
    arrow = arrow(length = unit(0.18, "cm")),
    linewidth = 1.3,  # 原size = 1.3 替换为linewidth
    colour = "#c2a5cf"
  ) +
  geom_text(
    aes(x = (1.45 + 2.25)/2, y = y + 0.32, label = "助攻"),
    colour = "#c2a5cf",
    size = 5,
    fontface = "bold",
    family = "sans"  # 替换为通用字体sans，避免Microsoft YaHei缺失报错（也可保留原字体）
  ) +
  
  # 受助攻者头像
  geom_image(aes(x = 3, image = receiver_head), size = 0.10, by = "width") +
  
  # 球队 logo
  geom_image(aes(x = 4, image = team_logo), size = 0.08, by = "width") +
  
  # 助攻数 & 创造得分
  geom_shadowtext(
    aes(x = 4.9, label = assists),
    bg.colour = "black", colour = "white",
    size = 6.5, fontface = "bold"
  ) +
  geom_shadowtext(
    aes(x = 5.8, label = points_created),
    bg.colour = "black", colour = "white",
    size = 6.5, fontface = "bold"
  ) +
  
  coord_cartesian(ylim = c(min(df$y)-0.5, top_y + 0.5)) +
  scale_x_continuous(limits = c(0.5, 6.5)) +
  scale_y_continuous(breaks = NULL, expand = c(0.02, 0.02)) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, family = "sans"), # 通用字体
    plot.subtitle = element_text(size = 16, hjust = 0.5, colour = "gray40"),
    plot.caption = element_text(size = 11, colour = "gray60")
  ) +
  labs(
    title = "2025-26 赛季 助攻组合 TOP 15",
    subtitle = "数据统计截止 11.21，数据源：PivotFade",
    caption = "制作：俗人罢了"
  ) +
  theme(
    plot.caption = element_text(
      hjust = 0.5,     # 水平居中
      face = "bold",   # 加粗
      size = 12,       # 字号（可改）
      color = "black"  # 颜色（可改）
    )
  )

# ================== 保存 ==================
# 确保保存路径有效，bg与plot.background一致
ggsave("Assist_Flow_Final.png", p, width = 9, height = 11, dpi = 300, bg = "floralwhite")
# 仅在本地环境中打开，若为服务器环境可注释
browseURL("Assist_Flow_Final.png")