library(tidyverse)
library(ggtext)
library(ggimage)
library(scales)
library(showtext)
library(MASS)

# ==============================================================================
# 1. 原始数据输入：2026-27 全美直播场次 vs Vegas 预测胜场
# ==============================================================================
df_raw <- tribble(
  ~Team_Full, ~Nat_TV, ~Vegas_Wins,
  "ATL", 10, 43.5, "BOS", 27, 51.5, "BKN", 2, 24.5, "CHA", 6, 37.5,
  "CHI", 3, 27.5, "CLE", 17, 47.5, "DAL", 21, 34.5, "DEN", 28, 49.5,
  "DET", 19, 50.5, "GSW", 28, 40.5, "HOU", 21, 47.5, "IND", 11, 44.5,
  "LAC", 8, 30.5, "LAL", 34, 46.5, "MEM", 2, 28.5, "MIA", 25, 46.5,
  "MIL", 2, 26.5, "MIN", 27, 48.5, "NOP", 2, 27.5, "NYK", 34, 52.5,
  "OKC", 30, 60.5, "ORL", 8, 43.5, "PHI", 34, 50.5, "PHX", 9, 40.5,
  "POR", 11, 43.5, "SAC", 2, 21.5, "SAS", 34, 59.5, "TOR", 7, 45.5,
  "UTA", 6, 35.5, "WAS", 6, 35.5
)

df <- df_raw %>% mutate(logo_path = paste0("D:/nba/球队logo/队标/", Team_Full, ".png"))

# 十字分割基准线
med_x <- median(df$Nat_TV)
med_y <- median(df$Vegas_Wins)

# ==============================================================================
# 2. 流体星云渲染矩阵 (KDE)
# ==============================================================================
pad_x <- (max(df$Nat_TV) - min(df$Nat_TV)) * 0.15
pad_y <- (max(df$Vegas_Wins) - min(df$Vegas_Wins)) * 0.15

x_seq <- seq(min(df$Nat_TV) - pad_x, max(df$Nat_TV) + pad_x, length.out = 200)
y_seq <- seq(min(df$Vegas_Wins) - pad_y, max(df$Vegas_Wins) + pad_y, length.out = 200)

grid_data <- expand.grid(Nat_TV = x_seq, Vegas_Wins = y_seq)

x_min <- min(df$Nat_TV); x_max <- max(df$Nat_TV)
y_min <- min(df$Vegas_Wins); y_max <- max(df$Vegas_Wins)

grid_data <- grid_data %>%
  mutate(
    x_norm = pmax(0, pmin(1, (Nat_TV - x_min) / (x_max - x_min))),
    y_norm = pmax(0, pmin(1, (Vegas_Wins - y_min) / (y_max - y_min))),
    Score = (x_norm + y_norm) / 2
  )

kde <- kde2d(df$Nat_TV, df$Vegas_Wins, n = 200,
             lims = c(min(x_seq), max(x_seq), min(y_seq), max(y_seq)),
             h = c((x_max - x_min) / 2, (y_max - y_min) / 2))

grid_data$density <- as.vector(kde$z)
max_dens <- max(grid_data$density)

grid_data <- grid_data %>% mutate(alpha_val = (density / max_dens)^1.1)

# ==============================================================================
# 3. 绘图执行
# ==============================================================================
p <- ggplot() +
  annotate("text", x = mean(x_seq), y = mean(y_seq), label = "俗人罢了",
           alpha = 0.06, size = 60, angle = 30, family = "Microsoft YaHei",color = "gray50", fontface = "bold") +
  geom_tile(data = grid_data, aes(x = Nat_TV, y = Vegas_Wins, fill = Score, alpha = alpha_val)) +
  scale_fill_gradientn(colors = c("#F28FA6", "floralwhite", "#65CDB8"), values = c(0, 0.50, 1), guide = "none") +
  scale_alpha_continuous(range = c(0, 0.75), guide = "none") +
  guides(alpha = "none", fill = "none") +
  geom_vline(xintercept = med_x, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_hline(yintercept = med_y, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_image(data = df, aes(x = Nat_TV, y = Vegas_Wins, image = logo_path), size = 0.075) +
  
  # 只保留两个反差标签：左上 + 右下
  annotate("richtext", x = max(x_seq), y = min(y_seq),
           label = "<b style='font-size:26pt; color:#1B5E20'>名气大于实力</b><br><span style='font-size:16pt; color:#555555'>热度高但水平较低</span>",
           fill = NA, label.color = NA, family = "Microsoft YaHei", hjust = 1, vjust = 0) +
  annotate("richtext", x = min(x_seq), y = max(y_seq),
           label = "<b style='font-size:26pt; color:#546E7A'>被低估的强队</b><br><span style='font-size:16pt; color:#555555'>关注度低但有实力</span>",
           fill = NA, label.color = NA, family = "Microsoft YaHei", hjust = 0, vjust = 1) +
  
  labs(
    title = "NBA2026-27赛季球队全美直播场次 vs 预测胜场",
    subtitle = "数据源：BetMGM + NBA Schedule | 制作人：俗人罢了",
    x = "← 全美直播场次少 | 全美直播场次 →",
    y = "← Vegas预测胜场低 | Vegas预测胜场 →"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    text = element_text(family = "Consolas"),
    plot.title = element_text(family = "Microsoft YaHei", face = "bold", size = 32, color = "#222222", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(family = "Microsoft YaHei", size = 22, color = "#777777", hjust = 0, margin = margin(b = 15)),
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
ggsave("NBA_全美直播_vs_Vegas预测胜场_流体星云.png", p, width = 16, height = 12, dpi = 300)