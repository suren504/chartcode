library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)

# ----------------- 1. 数据准备 (更新为 NBA 影响力数据) -----------------
df_compare <- tribble(
  ~row_id, ~metric,  ~k_score, ~f_score,
  4,       "EPM",     2.6,     -0.1,
  3,       "DARKO",   0.7,     -0.1,
  2,       "LEBRON",  1.1,      0.0,
  1,       "LAKER",   2.4,      0.8
)

# ----------------- 2. 核心绘图 -----------------
# 针对小数调整间距量 (Gap)
gap <- 0.5  

p <- ggplot(df_compare) +
  
  # 【左侧柱状图：Knueppel】颜色：#1B0962
  geom_segment(aes(x = -gap, xend = -gap - k_score, y = row_id, yend = row_id),
               color = "#1B0962", linewidth = 16, lineend = "butt") +
  
  # 【右侧柱状图：Flagg】颜色：#0064B5
  geom_segment(aes(x = gap, xend = gap + f_score, y = row_id, yend = row_id),
               color = "#0064B5", linewidth = 16, lineend = "butt") +
  
  # 【数值标签：置于条形图内部/边缘】
  # Knueppel 数值
  geom_text(aes(x = -gap - 0.1, y = row_id, label = sprintf("%.1f", k_score)),
            color = "white", family = "Consolas", size = 5, fontface = "bold", hjust = 1) +
  # Flagg 数值 (考虑到有负值，hjust设为0)
  geom_text(aes(x = gap + 0.1, y = row_id, label = sprintf("%.1f", f_score)),
            color = "white", family = "Consolas", size = 5, fontface = "bold", hjust = 0) +
  
  # 【中央指标名】
  geom_text(aes(x = 0, y = row_id, label = metric),
            color = "#333333", family = "Consolas", size = 6, fontface = "bold") +
  
  # 【顶部表头设计】
  annotate("text", x = -1.8, y = 4.7, label = "Knueppel Impact", 
           color = "#1B0962", family = "Consolas", size = 7, fontface = "bold") +
  annotate("text", x = 1.8, y = 4.7, label = "Flagg Impact", 
           color = "#0064B5", family = "Consolas", size = 7, fontface = "bold") +
  annotate("text", x = 0, y = 4.7, label = "Metric", 
           color = "grey50", family = "Consolas", size = 6) +
  
  # 静态表头虚线
  annotate("segment", x = -4, xend = 4, y = 4.5, yend = 4.5, 
           color = "grey80", linewidth = 0.5) +
  
  # 坐标轴限制 (针对影响力数据的小数范围调优)
  scale_x_continuous(limits = c(-4.5, 4.5)) +
  scale_y_continuous(limits = c(0.5, 5.0)) +
  
  # 【主题定制】
  theme_void() +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    plot.margin = margin(10, 20, 10, 20)
  )

# ----------------- 3. 保存 -----------------
ggsave("NBA_Impact_Butterfly_Metric.png", p, width = 14, height = 5, dpi = 300)