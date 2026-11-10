library(tidyverse)
library(ggimage)

# ===============================
# 1️⃣ 构建数据：赛季、球队、秒数
# ===============================
season_data <- tibble(
  赛季 = c(
    rep("2015-2016", 30), rep("2016-2017", 30), rep("2017-2018", 30),
    rep("2018-2019", 30), rep("2019-2020", 30), rep("2020-2021", 30),
    rep("2021-2022", 30), rep("2022-2023", 30), rep("2023-2024", 30),
    rep("2024-2025", 30), rep("2025-2026", 30)
  ),
  球队 = c(
    # 2015-2016赛季
    "BOS","GSW","PHX","SAC","WAS","HOU","OKC","ATL","NOP","CHI",
    "MIL","ORL","IND","DEN","PHI","BKN","DET","MIN","POR","LAL",
    "LAC","CHA","NYK","DAL","MEM","CLE","MIA","SAS","TOR","UTA",
    # 2016-2017赛季
    "PHX","GSW","HOU","OKC","BKN","ATL","LAL","DEN","NOP","PHI",
    "WAS","NYK","ORL","BOS","CHI","DET","POR","MIL","MIN","IND",
    "CLE","LAC","CHA","TOR","SAS","MIA","SAC","MEM","UTA","DAL",
    # 2017-2018赛季
    "LAL","PHI","GSW","OKC","NOP","PHX","CHI","LAC","ATL","TOR",
    "DEN","DET","HOU","ORL","CHA","NYK","IND","MIL","WAS","BKN",
    "CLE","BOS","POR","SAC","UTA","MIA","MEM","DAL","SAS","MIN",
    # 2018-2019赛季
    "OKC","SAC","ATL","NOP","LAL","GSW","MIL","PHI","WAS","BOS",
    "BKN","UTA","TOR","LAC","DEN","MIN","IND","NYK","DAL","CHA",
    "PHX","POR","ORL","CHI","MIA","SAS","HOU","DET","MEM","CLE",
    # 2019-2020赛季
    "NOP","MIL","ATL","WAS","MEM","HOU","LAC","MIN","BKN","CHI",
    "TOR","LAL","GSW","PHX","SAC","BOS","NYK","POR","DAL","SAS",
    "IND","ORL","OKC","DEN","PHI","UTA","CHA","CLE","MIA","DET",
    # 2020-2021赛季
    "MIL","WAS","IND","MEM","MIN","GSW","NOP","SAC","UTA","CHA",
    "TOR","BKN","OKC","BOS","ATL","HOU","POR","CHI","DEN","PHI",
    "PHX","ORL","SAS","LAC","LAL","MIA","DAL","DET","CLE","NYK",
    # 2021-2022赛季
    "MEM","SAS","MIN","MIL","CHA","IND","SAC","PHX","OKC","GSW",
    "BKN","ATL","ORL","NOP","DET","TOR","POR","LAC","HOU","LAL",
    "BOS","UTA","CHI","MIA","WAS","DEN","NYK","CLE","DAL","PHI",
    # 2022-2023赛季
    "OKC","MEM","SAC","IND","ATL","SAS","GSW","UTA","CHA","NOP",
    "MIL","PHX","LAL","ORL","MIN","TOR","DET","HOU","CHI","BKN",
    "DEN","BOS","LAC","WAS","NYK","POR","MIA","CLE","DAL","PHI",
    # 2023-2024赛季
    "IND","SAC","ATL","WAS","TOR","GSW","OKC","SAS","DAL","HOU",
    "DET","UTA","NOP","POR","BOS","CHI","MIL","BKN","MEM","LAL",
    "PHX","CLE","MIN","DEN","LAC","PHI","CHA","ORL","MIA","NYK",
    # 2024-2025赛季
    "MEM","ATL","CHI","TOR","OKC","CLE","POR","WAS","DEN","GSW",
    "IND","DET","SAS","NOP","HOU","SAC","DAL","UTA","CHA","MIN",
    "BKN","BOS","LAC","MIL","PHX","PHI","ORL","MIA","NYK","LAL",
    # 2025-2026赛季
    "MIA","POR","CLE","ATL","MEM","PHX","UTA","WAS","CHI","IND",
    "ORL","TOR","SAS","NOP","OKC","MIN","DET","DAL","PHI","SAC",
    "BOS","CHA","BKN","GSW","NYK","DEN","HOU","LAL","MIL","LAC"
  ),
  秒数 = c(
    # 2015-2016
    11.8,11.9,12.1,12.2,12.2,12.3,12.4,12.5,12.5,12.6,
    12.8,12.8,12.8,12.9,12.9,13.0,13.0,13.1,13.1,13.2,
    13.3,13.3,13.4,13.5,13.5,13.6,13.7,13.8,14.0,14.2,
    # 2016-2017
    11.8,11.9,12.0,12.1,12.2,12.4,12.4,12.4,12.4,12.5,
    12.5,12.6,12.8,12.8,12.9,13.0,13.1,13.2,13.2,13.2,
    13.3,13.3,13.3,13.4,13.4,13.4,13.7,13.8,14.2,14.2,
    # 2017-2018
    11.1,11.1,11.3,11.3,11.3,11.5,11.6,11.7,11.8,11.9,
    12.0,12.0,12.0,12.1,12.1,12.1,12.1,12.1,12.1,12.2,
    12.3,12.4,12.4,12.4,12.5,12.5,12.5,12.6,12.6,12.7,
    # 2018-2019
    10.2,10.6,10.6,11.0,11.2,11.2,11.2,11.3,11.3,11.4,
    11.4,11.5,11.6,11.6,11.6,11.6,11.8,11.9,11.9,11.9,
    11.9,12.0,12.0,12.1,12.1,12.3,12.3,12.4,12.6,12.6,
    # 2019-2020
    10.8,10.9,11.1,11.2,11.2,11.2,11.3,11.3,11.5,11.6,
    11.6,11.6,11.6,11.6,11.7,11.7,11.9,11.9,11.9,12.0,
    12.0,12.0,12.0,12.1,12.1,12.2,12.2,12.3,12.3,12.4,
    # 2020-2021
    11.0,11.1,11.3,11.3,11.4,11.4,11.5,11.6,11.6,11.7,
    11.7,11.7,11.8,11.9,12.0,12.0,12.1,12.1,12.1,12.1,
    12.1,12.2,12.2,12.3,12.3,12.3,12.3,12.4,12.6,12.8,
    # 2021-2022
    11.1,11.2,11.2,11.4,11.5,11.5,11.5,11.6,11.7,11.7,
    11.7,11.7,11.8,11.8,11.8,11.9,12.0,12.0,12.1,12.1,
    12.1,12.2,12.2,12.3,12.3,12.3,12.4,12.6,12.7,12.8,
    # 2022-2023
    11.1,11.2,11.2,11.2,11.2,11.3,11.4,11.5,11.6,11.6,
    11.6,11.8,11.8,11.8,11.9,11.9,12.0,12.0,12.1,12.1,
    12.1,12.2,12.3,12.3,12.4,12.4,12.5,12.7,12.8,12.9,
    # 2023-2024
    11.1,11.3,11.4,11.4,11.5,11.5,11.5,11.5,11.6,11.7,
    11.7,11.8,11.9,11.9,11.9,11.9,12.0,12.0,12.0,12.1,
    12.1,12.1,12.2,12.2,12.2,12.3,12.3,12.4,12.4,12.5,
    # 2024-2025
    10.6,10.6,10.9,11.1,11.1,11.2,11.4,11.4,11.5,11.5,
    11.5,11.6,11.6,11.7,11.8,11.8,11.8,11.9,12.0,12.0,
    12.2,12.3,12.3,12.3,12.4,12.4,12.4,12.5,12.5,12.5,
    # 2025-2026
    10.3,10.7,10.7,10.9,11.0,11.1,11.1,11.2,11.3,11.3,
    11.3,11.4,11.5,11.5,11.6,11.6,11.7,11.7,11.8,11.9,
    11.9,11.9,12.0,12.0,12.1,12.1,12.1,12.3,12.3,12.9
  )
)

# ===============================
# 2️⃣ 添加 LOGO 图片路径
# ===============================
season_data <- season_data %>%
  mutate(logo_path = paste0("D:/nba/球队logo/球队logo圆/", 球队, ".png"))

# 设置赛季顺序
season_levels <- unique(season_data$赛季)
season_data <- season_data %>%
  mutate(赛季 = factor(赛季, levels = season_levels))

# ===============================
# 绘图
# ===============================
p <- ggplot(season_data, aes(x = 赛季, y = 秒数)) +
  geom_image(
    aes(image = logo_path),
    size = 0.065,
    alpha = 0.9,
    position = position_jitter(width = 0.25, height = 0.15, seed = 42)
  ) +
  scale_y_reverse(
    breaks = seq(floor(min(season_data$秒数)), ceiling(max(season_data$秒数)), 0.5),
    expand = expansion(mult = c(0.2, 0.2))  # 上下各扩展20%，拉长Y轴（可调到0.25或0.3如果还不够）
    # 如果想用固定秒数扩展：expand = expansion(add = c(1.5, 1.5))
  ) +
  labs(
    title = "NBA过去十个赛季 NBA 每支球队平均出手一次投篮所需时间对比图",
    subtitle = "只统计常规赛且2025-2026赛季样本并非整个常规赛样本 | 数据截止2025.12.17 | 制作：俗人罢了 | 数据源：inpredictable.com",
    x = NULL,
    y = "出手一次投篮所需时间（秒）"
  ) +
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    # 背景设置（保持原样）
    plot.background = element_rect(fill = "#F7F4EF", color = NA),
    panel.background = element_rect(fill = "#F7F4EF", color = NA),
    
    # 主标题样式
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, color = "black"),
    
    # 副标题样式（已修正）
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0.5),
    
    # X轴刻度文字
    axis.text.x = element_text(size = 10, angle = 30, hjust = 1),
    
    # Y轴刻度文字（调小一点避免拥挤）
    axis.text.y = element_text(size = 8),
    
    # Y轴标题
    axis.title.y = element_text(size = 14, face = "bold"),
    
    # ========== 核心修改：网格线变淡（不再报alpha警告） ==========
    panel.grid.major = element_line(color = "grey95", linewidth = 0.6),   # 主网格：浅灰、稍粗一点
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.4)    # 次网格：更浅、更细
  )
# ===============================
# 保存图片
# ===============================
ggsave(
  filename = "NBA_2015_2026_shot_time_LOGO_jitter_v2.png",
  plot = p,
  width = 16,
  height = 9,
  dpi = 300,
  bg = "floralwhite"
)

browseURL("NBA_2015_2026_shot_time_LOGO_jitter_v2.png")
