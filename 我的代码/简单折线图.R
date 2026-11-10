# ===============================
# 1️⃣ 加载库
# ===============================
library(tidyverse)
library(hrbrthemes)

# ===============================
# 2️⃣ 构造 Pace 数据
# ===============================
nba_pace <- tibble(
  season = c(
    "1996-97","1997-98","1998-99","1999-00","2000-01",
    "2001-02","2002-03","2003-04","2004-05","2005-06",
    "2006-07","2007-08","2008-09","2009-10","2010-11",
    "2011-12","2012-13","2013-14","2014-15","2015-16",
    "2016-17","2017-18","2018-19","2019-20","2020-21",
    "2021-22","2022-23","2023-24","2024-25","2025-26"
  ),
  pace = c(
    90.1,90.3,88.9,93.1,91.3,
    90.7,91.0,90.1,90.9,90.5,
    91.9,92.4,91.7,92.7,92.1,
    91.3,92.0,93.9,93.9,95.8,
    96.4,97.3,100.0,100.3,99.2,
    98.2,99.2,98.5,98.8,100.0
  )
)

# 把赛季转成有序因子（从旧到新，便于画线）
nba_pace <- nba_pace %>%
  mutate(season = factor(season, levels = season))

# ===============================
# 3️⃣ 作图
# ===============================
p <- ggplot(
  nba_pace,
  aes(x = season, y = pace, group = 1)
) +
  geom_line(color = "grey", linewidth = 1) +
  geom_point(
    shape = 21,
    color = "black",
    fill = "#FF7F0E",  # 橙色
    size = 4
  ) +
  theme_ipsum(base_size = 16) +
  labs(
    title = "NBA 自PBP时代以来Pace（攻防节奏）变化趋势",
    subtitle = "只统计常规赛 | 2025-26赛季进行中（截至2025.12.17）",
    x = "赛季",
    y = "Pace（回合数）",
    caption = "Data: Basketball-Reference.com | 制作：俗人罢了"
  ) +
  theme(
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 25, hjust = 0.5, color = "grey"),
    plot.caption = element_text(size = 30, hjust = 0.5, color = "grey40", margin = margin(t = 10)),
    # 米白色背景
    plot.background = element_rect(fill = "#FAF7F2", color = NA),
    panel.background = element_rect(fill = "#FAF7F2", color = NA)
  )
# 保存并打开图片
ggsave(
  "NBA_pace_trend_big.png",
  plot = p,
  width = 16,   # 稍微加宽一点，容纳更多赛季标签
  height = 9,
  dpi = 300,
  bg = "#FAF7F2"
)

browseURL("NBA_pace_trend_big.png")