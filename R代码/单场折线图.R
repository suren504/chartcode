# ==========================================
# 纽约尼克斯 2025-26赛季 季后赛篮下出手占比走势图
# ==========================================

library(tidyverse)
library(lubridate)
library(ggimage)
library(scales) # 用于Y轴百分比格式化

# 1. 读取原始数据
df_raw <- read.table(text = "Date Opponent AtRimFrequency
2026/4/18 ATL 0.3625
2026/4/20 ATL 0.390804598
2026/4/23 ATL 0.404255319
2026/4/25 ATL 0.373493976
2026/4/28 ATL 0.3625
2026/4/30 ATL 0.376470588
2026/5/4 PHI 0.345238095
2026/5/6 PHI 0.405063291
2026/5/8 PHI 0.342105263
2026/5/10 PHI 0.22826087
2026/5/19 CLE 0.359550562
2026/5/21 CLE 0.313953488
2026/5/23 CLE 0.285714286
2026/5/25 CLE 0.313131313
2026/6/3 SAS 0.305263158
2026/6/5 SAS 0.280898876
2026/6/8 SAS 0.272727273", header = TRUE, stringsAsFactors = FALSE)

# 2. 数据清洗与连续索引构建
df <- df_raw %>%
  arrange(ymd(Date)) %>% # 确保按时间先后排序
  mutate(
    # 美国时间 + 1天 = 北京时间
    Date_CN = ymd(Date) + days(1),
    # 生成 M月D日 标签
    Date_Label = paste0(month(Date_CN), "月", day(Date_CN), "日"),
    # 【核心修正】生成连续的场次索引，确保等距且兼容连续轴
    Game_Index = row_number(),
    # 球队Logo本地路径
    Logo_Path = paste0("D:/nba/球队logo/球队logo圆/", Opponent, ".png")
  )

# 计算水印绝对中心点
mid_x <- (min(df$Game_Index) + max(df$Game_Index)) / 2
mid_y <- mean(df$AtRimFrequency)

# 3. 绘制核心图表
p <- ggplot(df, aes(x = Game_Index, y = AtRimFrequency, group = 1)) +
  

  # 尼克斯专属配色折线
  geom_line(color = "#F58426", linewidth = 3) +
  
  # 数据点替换为对手球队Logo
  geom_image(aes(image = Logo_Path), size = 0.1) +
  
  # 【核心修正】显式声明连续轴，并将数字换成对应的日期标签
  scale_x_continuous(breaks = df$Game_Index, labels = df$Date_Label) +
  
  # Y轴格式化为百分比
  scale_y_continuous(labels = scales::percent) +
  
  # 标题与坐标轴标签
  labs(
    title = "纽约尼克斯2025-26赛季季后赛单场篮下出手占比走势图",
    subtitle = "散点显示为尼克斯当场比赛对手球队LOGO|制作：俗人罢了|数据源:pbp.stats",
    x = "比赛日期",
    y = "篮下出手占比 (At Rim Frequency)"
  ) +
  
  # 4. 社交媒体视觉排版 (Theme)
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    text = element_text(family = "Microsoft YaHei", color = "#333333"),
    
    # 标题排版
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 10)),
    plot.subtitle = element_text(size = 14, color = "#A1A1A1", hjust = 0.5, margin = margin(b = 20)),
    
    # X轴文本旋转，Y轴数字强制 Consolas
    axis.text.x = element_text(family = "Microsoft YaHei", size = 12, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(family = "Consolas", size = 14),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
    
    # 网格线设置
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed")
  )

# 5. 自定义 ggsave 极简导出
ggsave("尼克斯25-26赛季季后赛篮下出手占比走势.png", p, width = 16, height = 9, dpi = 300)