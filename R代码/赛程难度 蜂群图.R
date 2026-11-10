# ==================== 1. 加载包 ====================
library(tidyverse)
library(ggimage)
library(ggtext)
library(ragg)

# ==================== 2. 全局参数 ====================
font_cn  <- "Microsoft YaHei"
font_num <- "Consolas"
logo_dir <- "D:/nba/球队logo/球队logo圆"
bg_color <- "#f7f5f2"

fig_w <- 12; fig_h <- 9          # 4:3 横屏
panel_w_in <- 10.8               # 绘图区估算宽度（英寸）
panel_h_in <- 5.8                # 绘图区估算高度（英寸）
x_lim      <- c(-5, 11.5)
logo_d_in  <- 0.68               # logo 直径（英寸），同时决定避让距离

# ==================== 3. 数据 ====================
strength <- tribble(
  ~team, ~strength,
  "OKC", 10.2, "BOS",  9.4, "HOU",  7.4, "SAS",  7.4, "DET",  6.9, "NYK",  5.4,
  "CLE",  4.9, "DEN",  4.9, "LAL",  4.9, "PHI",  4.9, "TOR",  4.9, "POR",  3.8,
  "ORL",  3.7, "MIA",  3.2, "IND",  2.1, "MIN",  1.9, "ATL",  1.8, "MEM",  1.3,
  "BKN",  1.1, "PHX",  0.3, "LAC",  0.0, "NOP", -0.4, "WAS", -0.5, "CHA", -0.6,
  "GSW", -0.7, "UTA", -1.6, "DAL", -1.8, "CHI", -2.2, "MIL", -2.8, "SAC", -3.4
)

# ==================== 4. 蜂群布局（按物理尺寸避让） ====================
swarm_offsets <- function(x_in, d, step = d / 12, max_k = 80) {
  n    <- length(x_in)
  y    <- rep(NA_real_, n)
  cand <- c(0, as.vector(rbind(seq_len(max_k), -seq_len(max_k))) * step)
  done <- integer(0)
  for (i in order(x_in)) {
    for (cy in cand) {
      ok <- length(done) == 0 ||
        all((x_in[i] - x_in[done])^2 + (cy - y[done])^2 >= d^2)
      if (ok) { y[i] <- cy; break }
    }
    done <- c(done, i)
  }
  y
}

in_per_x <- panel_w_in / diff(x_lim)

df <- strength %>%
  arrange(desc(strength)) %>%
  mutate(
    logo_path = file.path(logo_dir, paste0(team, ".png")),
    y_in      = swarm_offsets(strength * in_per_x, d = logo_d_in),
    y         = y_in * 2 / panel_h_in
  )

missing_logo <- df$team[!file.exists(df$logo_path)]
if (length(missing_logo)) warning("缺少 logo：", paste(missing_logo, collapse = ", "))

# ==================== 5. 主题 ====================
theme_f5 <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = font_cn) +
    theme(
      plot.background    = element_rect(fill = bg_color, color = NA),
      panel.background   = element_rect(fill = bg_color, color = NA),
      panel.grid.major.x = element_line(color = "#e7e4df", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_blank(),
      axis.ticks.y       = element_blank(),
      axis.text.x        = element_text(family = font_num, size = 13, color = "#6a6865"),
      axis.title         = element_blank(),
      plot.title    = element_markdown(hjust = 0.5, size = 25, face = "bold",
                                       color = "#3a3a3a"),
      plot.subtitle = element_markdown(hjust = 0.5, size = 12, color = "#5a5855",
                                       margin = margin(6, 0, 16, 0)),
      plot.margin   = margin(28, 28, 22, 28)
    )
}

# ==================== 6. 绘图 ====================
num <- function(x) paste0("<span style='font-family:", font_num, "'>", x, "</span>")

plot <- ggplot(df, aes(x = strength, y = y)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#b9b5ae", linewidth = 0.4) +
  geom_image(aes(image = logo_path), size = logo_d_in / panel_h_in) +
  annotate("text", x = x_lim[1] + 0.15, y = -0.94, label = "← 实力较弱",
           hjust = 0, family = font_cn, fontface = "bold", size = 5.5, color = "#3a3a3a") +
  annotate("text", x = x_lim[2] - 0.15, y = -0.94, label = "实力较强 →",
           hjust = 1, family = font_cn, fontface = "bold", size = 5.5, color = "#3a3a3a") +
  scale_x_continuous(
    breaks = seq(-4, 10, 2), limits = x_lim, expand = c(0, 0),
    labels = \(x) ifelse(x > 0, paste0("+", x), x)
  ) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  labs(
    title    = paste0(num("NBA2026-27"), "球队实力评级"),
    subtitle = paste0(
      "基于球员 影响力 × 实际出场时间构建|制图: ",
      "<span style='font-size:14pt; color:black;'><b>俗人罢了</b></span>",
      "|数据源:", num("craftednba.com"), "|截至", num("2026/10/26")
    )
  ) +
  theme_f5()

# ==================== 7. 保存（2400×1800，适合微博/INS） ====================
ggsave("NBA_2026-27_team_strength.png", plot,
       width = fig_w, height = fig_h, dpi = 200,
       bg = bg_color, device = ragg::agg_png)