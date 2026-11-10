# 加载必要的包
library(tidyverse)
library(scales)

# 1. 构造包含场次信息的数据框
# 【修改】已移除季后赛相关的胜率和场次数据
df <- tibble(
  season = c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21", 
             "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"),
  reg_played_win_rate = c("64.10%", "66.67%", "55.41%", "66.67%", "51.72%", 
                          "65.15%", "68.66%", "74.29%", "72.88%", "66.20%"),
  reg_missed_win_rate = c("75.00%", "69.23%", "100.00%", "66.67%", "35.29%", 
                          "50.00%", "73.33%", "100.00%", "80.00%", "81.82%"),
  missed_games = c(4, 13, 8, 15, 17, 16, 15, 12, 15, 11)
)

# 2. 数据清洗与格式转换
plot_df <- df %>%
  mutate(
    Season_Index = row_number(),
    
    # 转换百分比为数值
    reg_played = parse_number(reg_played_win_rate) / 100,
    reg_missed = parse_number(reg_missed_win_rate) / 100,
    
    # 构建缺席场次标签
    missed_label = paste0(missed_games, "场")
  )

# 3. 绘制核心图表
p <- ggplot(plot_df, aes(x = Season_Index)) +
  
  # === 常规赛出战线 ===
  # 【修复】补上了 aes(y = reg_played)
  geom_line(aes(y = reg_played, color = "常规赛出战"), linewidth = 2.5) +
  geom_point(aes(y = reg_played, fill = "常规赛出战"), shape = 21, size = 4, color = "floralwhite", stroke = 1.2) +
  
  # === 常规赛缺席线与场次标签 ===
  # 【修复】补上了 aes(y = reg_missed)
  geom_line(aes(y = reg_missed, color = "常规赛缺席"), linewidth = 1.2) +
  geom_point(aes(y = reg_missed, fill = "常规赛缺席"), shape = 21, size = 4, color = "floralwhite", stroke = 1.2) +
  
  geom_label(aes(y = reg_missed, label = missed_label), 
             vjust = -0.6, family = "Microsoft YaHei", size = 4.5, color = "#008348", fontface = "bold",
             fill = "floralwhite", linewidth = NA, label.padding = unit(0.15, "lines")) +
  
  # 显式声明连续轴，并将数字换成对应的日期标签
  scale_x_continuous(breaks = plot_df$Season_Index, labels = plot_df$season) +
  
  # Y轴上限放宽至 1.05 (105%)，防止 100% 处的文字被画布边缘截断
  scale_y_continuous(labels = scales::percent, limits = c(0.3, 1.05), breaks = seq(0.3, 1, by = 0.1)) +
  
  # 显式关闭绘图区域裁剪
  coord_cartesian(clip = "off") +
  
  # 同步配置线条颜色 (color) 和散点填充色 (fill)，确保图例显示正常且统一
  scale_color_manual(
    name = NULL, 
    values = c("常规赛出战" = "#BB9753", "常规赛缺席" = "#008348"),
    breaks = c("常规赛出战", "常规赛缺席")
  ) +
  scale_fill_manual(
    name = NULL, 
    values = c("常规赛出战" = "#BB9753", "常规赛缺席" = "#008348"),
    breaks = c("常规赛出战", "常规赛缺席")
  ) +
  # 防止图例中出现白色边框的小圆点，强制让图例只显示干净的折线线条
  guides(
    color = guide_legend(override.aes = list(linewidth = c(2.5, 1.2))),
    fill = "none"
  ) +
  
  labs(
    title = "杰伦·布朗职业生涯常规赛出战状况与球队胜率走势",
    subtitle = "出战胜率为加粗线条，场次为对应赛季常规赛缺勤场次",
    x = "比赛赛季",
    y = "对应胜率"
  ) +
  
  # 4. 社交媒体视觉排版 (Theme)
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    text = element_text(family = "Microsoft YaHei", color = "#333333"),
    
    # 标题排版
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 10)),
    plot.subtitle = element_text(size = 14, color = "#A1A1A1", hjust = 0.5, margin = margin(b = 20)),
    
    # X轴文本旋转，Y轴数字强制 Consolas
    axis.text.x = element_text(family = "Microsoft YaHei", size = 12, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(family = "Consolas", size = 14),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
    
    # 网格线设置
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
    
    # 图例设置：将图例放到图表顶部，方便阅读
    legend.position = "top",
    legend.text = element_text(family = "Microsoft YaHei", size = 13, face = "bold"),
    legend.key.width = unit(3, "lines")
  )

# 5. 自定义 ggsave 极简导出
ggsave("Jaylen_Brown_Regular_Season_WinRate_Trend.png", p, width = 16, height = 9, dpi = 300)