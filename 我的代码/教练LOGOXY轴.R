library(tidyverse)
library(ggtext)
library(ggimage)
library(scales)
library(showtext)
library(MASS)

# ==============================================================================
# 1. 原始数据输入：2025-26 NBA 常规赛 教练挑战次数 vs 挑战成功率
# ==============================================================================
df_raw <- tribble(
  ~Team_Full, ~Challenge_Total, ~Success_Rate,
  "OKC", 98, 0.643,
  "MIN", 91, 0.659,
  "PHX", 89, 0.685,
  "POR", 89, 0.640,
  "HOU", 85, 0.647,
  "CLE", 82, 0.695,
  "LAL", 78, 0.718,
  "DET", 74, 0.608,
  "PHI", 73, 0.699,
  "LAC", 72, 0.639,
  "MEM", 70, 0.643,
  "GSW", 69, 0.681,
  "UTA", 64, 0.734,
  "BKN", 63, 0.603,
  "NYK", 59, 0.780,
  "ORL", 59, 0.712,
  "ATL", 58, 0.603,
  "TOR", 56, 0.554,
  "CHA", 55, 0.709,
  "SAS", 55, 0.618,
  "WAS", 53, 0.547,
  "BOS", 51, 0.627,
  "NOP", 49, 0.694,
  "DEN", 49, 0.653,
  "SAC", 48, 0.625,
  "MIA", 45, 0.644,
  "MIL", 44, 0.523,
  "DAL", 40, 0.450,
  "IND", 31, 0.548,
  "CHI", 25, 0.800
)

# 队标路径（你本地已准备好）
df <- df_raw %>%
  mutate(
    logo_path = paste0("D:/nba/球队logo/队标/", Team_Full, ".png")
  )

# 十字分割基准线：中位数
med_x <- median(df$Challenge_Total)
med_y <- median(df$Success_Rate)

# ==============================================================================
# 2. 流体星云渲染矩阵 (KDE)
# ==============================================================================
pad_x <- (max(df$Challenge_Total) - min(df$Challenge_Total)) * 0.15
pad_y <- (max(df$Success_Rate) - min(df$Success_Rate)) * 0.15

x_seq <- seq(
  min(df$Challenge_Total) - pad_x,
  max(df$Challenge_Total) + pad_x,
  length.out = 200
)

y_seq <- seq(
  min(df$Success_Rate) - pad_y,
  max(df$Success_Rate) + pad_y,
  length.out = 200
)

grid_data <- expand.grid(
  Challenge_Total = x_seq,
  Success_Rate = y_seq
)

x_min <- min(df$Challenge_Total)
x_max <- max(df$Challenge_Total)
y_min <- min(df$Success_Rate)
y_max <- max(df$Success_Rate)

grid_data <- grid_data %>%
  mutate(
    x_norm = pmax(0, pmin(1, (Challenge_Total - x_min) / (x_max - x_min))),
    y_norm = pmax(0, pmin(1, (Success_Rate - y_min) / (y_max - y_min))),
    Score = (x_norm + y_norm) / 2
  )

kde <- kde2d(
  df$Challenge_Total,
  df$Success_Rate,
  n = 200,
  lims = c(min(x_seq), max(x_seq), min(y_seq), max(y_seq)),
  h = c((x_max - x_min) / 2, (y_max - y_min) / 2)
)

grid_data$density <- as.vector(kde$z)
max_dens <- max(grid_data$density)

grid_data <- grid_data %>%
  mutate(
    alpha_val = (density / max_dens)^1.1
  )

# ==============================================================================
# 3. 绘图执行
# ==============================================================================
p <- ggplot() +
  annotate(
    "text",
    x = mean(x_seq),
    y = mean(y_seq),
    label = "俗人罢了",
    alpha = 0.06,
    size = 60,
    angle = 30,
    family = "Microsoft YaHei",
    color = "gray40",
    fontface = "bold"
  ) +
  geom_tile(
    data = grid_data,
    aes(x = Challenge_Total, y = Success_Rate, fill = Score, alpha = alpha_val)
  ) +
  scale_fill_gradientn(
    colors = c("#F28FA6", "floralwhite", "#65CDB8"),
    values = c(0, 0.50, 1),
    guide = "none"
  ) +
  scale_alpha_continuous(range = c(0, 0.75), guide = "none") +
  guides(alpha = "none", fill = "none") +
  geom_vline(
    xintercept = med_x,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = med_y,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  ) +
  geom_image(
    data = df,
    aes(x = Challenge_Total, y = Success_Rate, image = logo_path),
    size = 0.075
  ) +
  

  annotate(
    "richtext",
    x = max(x_seq),
    y = max(y_seq),
    label = "<b style='font-size:26pt; color:#1B5E20'>自信且高效</b><br><span style='font-size:16pt; color:#555555'>挑战次数多，成功率也高</span>",
    fill = NA,
    label.color = NA,
    family = "Microsoft YaHei",
    hjust = 1,
    vjust = 1
  ) +
  annotate(
    "richtext",
    x = min(x_seq),
    y = min(y_seq),
    label = "<b style='font-size:26pt; color:#546E7A'>谨慎但低效</b><br><span style='font-size:16pt; color:#555555'>挑战较少，成功率也不行</span>",
    fill = NA,
    label.color = NA,
    family = "Microsoft YaHei",
    hjust = 0,
    vjust = 0
  ) +
  
  labs(
    title = "NBA 2025-26赛季球队挑战次数 vs 挑战成功率",
    subtitle = "数据源：NBA Official Coaches Challenges PDF| 制作人：俗人罢了",
    x = "← 挑战次数少 | 挑战次数 →",
    y = "← 挑战成功率低 | 挑战成功率 →"
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 8)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = pretty_breaks(n = 8)
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    text = element_text(family = "Consolas"),
    plot.title = element_text(
      family = "Microsoft YaHei",
      face = "bold",
      size = 32,
      color = "#222222",
      hjust = 0,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      family = "Microsoft YaHei",
      size = 18,
      color = "#777777",
      hjust = 0,
      margin = margin(b = 15)
    ),
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(family = "Microsoft YaHei", size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    plot.margin = margin(20, 20, 20, 20)
  )

# ==============================================================================
# 4. 保存
# ==============================================================================
ggsave(
  "NBA_教练挑战次数_vs_挑战成功率_流体星云.png",
  p,
  width = 16,
  height = 12,
  dpi = 300
)