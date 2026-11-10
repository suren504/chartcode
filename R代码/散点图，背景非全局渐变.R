# ==============================================================================
# 全运会成年组男篮攻防效率矩阵 - 纯净图表版 (徐大帅定制)
# ==============================================================================
library(tidyverse)
library(ggrepel)
library(showtext)
library(MASS) 

# 1. 环境初始化
font_add("Microsoft YaHei", regular = "msyh.ttc", bold = "msyhbd.ttc")
showtext_auto()
showtext_opts(dpi = 300)

# 2. 核心数据
df <- tribble(
  ~球队, ~百回合得分, ~百回合失分,
  "广东", 128.2, 91.7,
  "辽宁", 124.2, 96.5,
  "四川", 127.0, 102.6,
  "浙江", 111.7, 99.0,
  "上海", 116.1, 90.9,
  "山东", 109.6, 107.8,
  "北京", 118.9, 110.1,
  "天津", 110.2, 107.7,
  "河南", 106.3, 106.5,
  "香港", 113.7, 122.4,
  "江苏", 95.1, 111.5,
  "吉林", 107.9, 119.6,
  "新疆", 106.0, 129.2,
  "湖北", 98.4, 122.1,
  "澳门", 81.7, 132.3,
  "河北", 94.3, 125.7
)

# 3. 计算联盟均值
avg_off <- mean(df$百回合得分)
avg_def <- mean(df$百回合失分)

# ==========================================================
# 🌟 核心重构：像素级网格渲染矩阵
# ==========================================================
pad_x <- (max(df$百回合得分) - min(df$百回合得分)) * 0.15
pad_y <- (max(df$百回合失分) - min(df$百回合失分)) * 0.15

x_seq <- seq(min(df$百回合得分) - pad_x, max(df$百回合得分) + pad_x, length.out = 200)
y_seq <- seq(min(df$百回合失分) - pad_y, max(df$百回合失分) + pad_y, length.out = 200)
grid_data <- expand.grid(百回合得分 = x_seq, 百回合失分 = y_seq)

x_min <- min(df$百回合得分); x_max <- max(df$百回合得分)
y_min <- min(df$百回合失分); y_max <- max(df$百回合失分)

grid_data <- grid_data %>%
  mutate(
    x_norm = pmax(0, pmin(1, (百回合得分 - x_min) / (x_max - x_min))),
    y_norm = pmax(0, pmin(1, (y_max - 百回合失分) / (y_max - y_min))), 
    Score = (x_norm + y_norm) / 2
  )

kde <- kde2d(df$百回合得分, df$百回合失分, n = 200, 
             lims = c(min(x_seq), max(x_seq), min(y_seq), max(y_seq)),
             h = c(15,15)) 

grid_data$density <- as.vector(kde$z)
max_dens <- max(grid_data$density)

grid_data <- grid_data %>%
  mutate(alpha_val = (density / max_dens)^1)

# 4. 绘图开始
p <- ggplot() +
  
  # A. 底层：丝滑像素网格
  geom_tile(data = grid_data, aes(x = 百回合得分, y = 百回合失分, fill = Score, alpha = alpha_val)) +
  scale_fill_gradient(low = "#FFFFFF", high = "#1f77b4", guide = "none") +
  scale_alpha_continuous(range = c(0, 0.45), guide = "none") + 
  
  # B. 均值线
  geom_vline(xintercept = avg_off, linetype = "dashed", color = "gray60", linewidth = 0.6) +
  geom_hline(yintercept = avg_def, linetype = "dashed", color = "gray60", linewidth = 0.6) +
  
  # C. 球队实体散点
  geom_point(data = df, aes(x = 百回合得分, y = 百回合失分, color = ((百回合得分 - x_min)/(x_max - x_min) + (y_max - 百回合失分)/(y_max - y_min))/2), 
             size = 4.5, alpha = 0.95) +
  scale_color_gradient(low = "#99ccee", high = "#1f77b4", guide = "none") + 
  
  # D. 球队标签
  geom_text_repel(
    data = df, aes(x = 百回合得分, y = 百回合失分, label = 球队), 
    color = "#1f77b4", family = "Microsoft YaHei",
    fontface = "bold", size = 4.5, segment.color = NA,
    box.padding = 0.5, point.padding = 0.5, max.overlaps = Inf,
    bg.color = "white", bg.r = 0.12
  ) +
  
  # E. 坐标轴设置
  scale_y_reverse() + 
  labs(
    # 修改这里：去除了 title 和 subtitle
    x = "<--- 进攻较弱 (百回合得分较低)           进攻较强 (百回合得分较高) --->",
    y = "<--- 防守较弱 (百回合失分较高)      防守较强 (百回合失分较低) --->"
  ) +
  
  # F. 清爽主题
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#f2f2f2", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    # 修改这里：彻底隐藏标题占位
    plot.title = element_blank(), 
    plot.subtitle = element_blank(),
    axis.title = element_text(face = "bold", size = 11, color = "gray40"),
    axis.text = element_text(size = 10, color = "#666666", family = "sans"),
    plot.margin = margin(t = 15, r = -20, b = 15, l = 20) # 适当增加边距确保标签不被切掉
  )

# 5. 保存
ggsave("全运会男篮净.png", p, width = 13, height = 6.5, dpi = 300)