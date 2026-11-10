library(tidyverse)
library(hrbrthemes)

# 1. 准备数据 (你提供的部分)
combined_fta <- tibble(
  Season = rep(c("2025-26", "2024-25", "2023-24", "2022-23", "2021-22", 
                 "2020-21", "2019-20", "2018-19", "2017-18", "2016-17"), 2),
  League = c(rep("NCAA", 10), rep("WNCAA", 10)),
  FTA = c(21.0, 19.2, 19.2, 18.1, 17.5, 18.3, 18.8, 19.1, 19.6, 20.5,
          17.2, 16.3, 16.5, 16.8, 16.1, 16.8, 16.9, 17.0, 17.1, 17.3)
) %>%
  mutate(Season = factor(Season, levels = rev(unique(Season)), ordered = TRUE))

# 2. 绘图
p <- ggplot(combined_fta, aes(x = Season, y = FTA, group = League, color = League)) +
  # 绘制折线：增加线条粗细
  geom_line(size = 1.2) +
  # 绘制数据点：使用 shape 21 实现填充色与边框分离
  geom_point(aes(fill = League), shape = 21, color = "black", size = 5, stroke = 1) +
  # 添加数值标签
  geom_text(aes(label = sprintf("%.1f", FTA)), # 保留一位小数
            fontface = "bold", 
            vjust = -0.8, # 文字在点上方
            size = 3.5, 
            show.legend = FALSE) + # 标签不显示在图例中
  # 自定义配色：与之前的 NBA 绿色区分开
  scale_color_manual(values = c("NCAA" = "#2c7bb6", "WNCAA" = "#d7191c")) + # 蓝 vs 红
  scale_fill_manual(values = c("NCAA" = "#2c7bb6", "WNCAA" = "#d7191c")) +
  # 应用主题
  theme_ipsum(grid = "Y") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    legend.background = element_rect(fill = "floralwhite", color = NA), # 图例背景
    legend.position = "top", # 图例放顶部
    legend.title = element_blank(), # 去掉图例标题
    legend.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text = element_text(color = "black", face = "bold")
  ) +
  labs(
    title = "NCAA和WNCAA过去10个赛季场均罚球出手变化",
    y = "场均罚球次数 (FTA)"
  ) +
  # 调整 Y 轴范围，给上面的文字标签留出空间
  expand_limits(y = c(min(combined_fta$FTA) - 1, max(combined_fta$FTA) + 1.5))

# 3. 输出与预览
# 保存为高清 PNG (适合视频使用)
ggsave("ncaa_wncaa_fta_trend.png", plot = p, width = 12, height = 7, dpi = 300)

# 自动在默认浏览器/图片查看器中打开预览
browseURL("ncaa_wncaa_fta_trend.png")