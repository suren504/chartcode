

  library(tidyverse)
  library(ggimage)
  library(ggtext)

# ============================================================
# 1. 数据
# ============================================================
  
  TOP10 <- tribble(
    ~排名, ~球队, ~球员, ~player_id, ~data,
    1, "MIA", "Giannis Antetokounmpo", "203507", 21.1,
    2, "DAL", "Cooper Flagg", "1642843", 20.6,
    3, "SAS", "Victor Wembanyama", "1641705", 18.3,
    4, "NOP", "Zion Williamson", "1629627", 17.9,
    5, "HOU", "Alperen Sengun", "1630578", 17.3,
    6, "TOR", "Scottie Barnes", "1630567", 16.2,
    7, "SAC", "Domantas Sabonis", "1627734", 16.1,
    8, "DEN", "Nikola Jokić", "203999", 15.5,
    9, "BOS", "Jayson Tatum", "1628369", 15.4,
    10, "PHI", "Tyrese Maxey", "1630178", 15.2
  )
  
  BOTTOM10 <- tribble(
    ~排名, ~球队, ~球员, ~player_id, ~data,
    1, "CHA", "Sion James", "1642883", 0.00,
    2, "MIA", "Klay Thompson", "202691", 0.36,
    3, "CHA", "Royce O'Neale", "1626220", 0.51,
    4, "NYK", "Landry Shamet", "1629013", 0.63,
    5, "LAC", "Rui Hachimura", "1629060", 0.82,
    6, "MIA", "Tim Hardaway Jr.", "203501", 1.25,
    7, "NYK", "Miles McBride", "1630540", 1.49,
    8, "WAS", "Bub Carrington", "1642267", 1.56,
    9, "OKC", "Jared McCain", "1642272", 1.84,
    10, "CHI", "Isaac Okoro", "1630171", 1.88
  )
  

# ============================================================
# 2. 字体与颜色
# ============================================================

font_cn <- "Microsoft YaHei"
font_en <- "Consolas"

col_up    <- "#2B884D"   # 最佳：绿色
col_down  <- "#C5352B"   # 最差：红色
bg_col    <- "floralwhite"
col_ink   <- "#111111"
col_first <- "#7A7A7A"   # 名（first name）
col_axis  <- "#4E5661"
col_grid  <- "#E2DAC8"
col_zero  <- "#202020"
col_stripe <- "#FBF3E1"  # 隔行底色

# ============================================================
# 3. 名字拆分（Jr./Sr./II/III 归入姓）
#    Tim Hardaway Jr. -> 第一行 "Tim"，第二行 "Hardaway Jr."
# ============================================================

split_player_name <- function(x) {
  parts  <- str_split(str_squish(x), "\\s+")[[1]]
  n      <- length(parts)
  suffix <- c("Jr.", "Sr.", "II", "III", "IV")
  k      <- if (n >= 3 && parts[n] %in% suffix) 2 else 1   # 姓占几个词
  
  if (n <= k) {
    return(list(first = "", last = paste(parts, collapse = " ")))
  }
  list(
    first = paste(parts[seq_len(n - k)], collapse = " "),
    last  = paste(parts[(n - k + 1):n], collapse = " ")
  )
}

# ============================================================
# 4. 头像路径
# ============================================================

head_dir <- "D:/nba/球员组合图"

find_headshot <- function(player_id) {
  if (is.na(player_id)) return(NA_character_)
  
  id <- as.character(player_id)
  candidates <- file.path(head_dir, paste0(id, c(".png", ".webp", ".jpg", ".jpeg")))
  hit <- candidates[file.exists(candidates)]
  
  if (length(hit) == 0) return(NA_character_)
  normalizePath(hit[1], winslash = "/", mustWork = FALSE)
}

# ============================================================
# 5. 版面（单位：英寸）  —— 想微调，只改这一节
# ============================================================

W <- 14        # 画布宽
H <- 11.4      # 画布高

margin_x <- 0.40   # 画布左右留白
gap_mid  <- 0.50   # 两个面板之间的间距
Pw <- (W - 2 * margin_x - gap_mid) / 2     # 单个面板宽度
px0_L <- margin_x                          # 左面板左边界
px0_R <- margin_x + Pw + gap_mid           # 右面板左边界

row_h    <- 0.82   # 每行高度
bar_h    <- 0.56   # 柱子粗细
y_bottom <- 0.95   # 第 10 名那一行的底边
y_top    <- y_bottom + 10 * row_h          # 第 1 名那一行的顶边

# 每一行内部（相对面板左边界的偏移）
rank_dx   <- 0.17                           # 排名数字中心
avatar_d  <- 0.74                           # 头像直径
avatar_dx <- 0.40 + avatar_d / 2            # 头像圆心
name_dx   <- 0.40 + avatar_d + 0.14         # 名字起点：紧贴头像右边缘（0.14 为间隙）
name_w    <- 1.95                           # 名字列宽度（最长的 Antetokounmpo 约 1.7 英寸）
bar_dx    <- name_dx + name_w + 0.15        # 柱子区起点
bar_len   <- Pw - bar_dx - 0.10            # 柱子区长度

# 两侧各自的刻度范围
left_max  <- 25                             # 左：0 ~ 35
right_min <- -1                             # 右：-1 ~ 5（容纳 -0.89）
right_max <- 2

x_left  <- function(v) px0_L + bar_dx + v / left_max * bar_len
x_right <- function(v) px0_R + bar_dx + (v - right_min) / (right_max - right_min) * bar_len

# ggimage 的 size = 头像边长 / 绘图区较短边（即 snpc），这里较短边就是 H
avatar_size <- avatar_d / min(W, H)

# ============================================================
# 6. 数据整理
# ============================================================

prep <- function(df) {
  nm <- map(df$球员, split_player_name)
  df %>%
    mutate(
      rk          = 排名,
      headshot    = map_chr(player_id, find_headshot),
      y           = y_bottom + row_h * (10 - 排名 + 0.5),
      first_part  = map_chr(nm, "first"),
      last_part   = map_chr(nm, "last"),
      value_label = sprintf("%.2f", data)
    )
}

best <- prep(TOP10) %>%
  mutate(
    x_base = x_left(0),
    x_end  = x_left(data)
  )

worst <- prep(BOTTOM10) %>%
  mutate(
    x_base = x_right(0),
    x_end  = x_right(data),
    # 正值：标签放柱子右端外侧；负值：标签放基线右侧，避免伸出面板
    lab_x  = if_else(data >= 0, x_end + 0.08, x_base + 0.08)
  )

missing_pics <- c(best$球员[is.na(best$headshot)], worst$球员[is.na(worst$headshot)])
if (length(missing_pics) > 0) {
  message("以下球员没找到头像：", paste(missing_pics, collapse = "、"))
}

# 隔行底色
stripe_rows <- tibble(rk = seq(1, 10, by = 2)) %>%
  mutate(
    ymin = y_bottom + row_h * (10 - rk),
    ymax = ymin + row_h
  )

stripes <- bind_rows(
  stripe_rows %>% mutate(xmin = px0_L, xmax = px0_L + Pw),
  stripe_rows %>% mutate(xmin = px0_R, xmax = px0_R + Pw)
)

# 刻度
ticks <- bind_rows(
  tibble(v = seq(0, left_max, by = 5)) %>% mutate(x = x_left(v)),
  tibble(v = seq(right_min, right_max, by = 1)) %>% mutate(x = x_right(v))
)

zero_lines <- tibble(x = c(x_left(0), x_right(0)))

# ============================================================
# 7. 标题文字
# ============================================================

main_title <- "NBA2026-27赛季10月数据最佳与最差球员"
sub_line  <- paste0(
  "数据口径为场均GBPM|制图：<span style='font-size:16pt; color:black;'><b>俗人罢了</b></span>",
  " | 制作时间：2026.11.03"
)

# ============================================================
# 8. 绘图
# ============================================================

p <- ggplot() +
  
  # ---- 隔行底色 ----
geom_rect(
  data = stripes,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  fill = col_stripe
) +
  
  # ---- 参考线 ----
geom_segment(
  data = ticks,
  aes(x = x, xend = x, y = y_bottom, yend = y_top),
  color = col_grid, linewidth = 0.45
) +
  geom_segment(
    data = zero_lines,
    aes(x = x, xend = x, y = y_bottom, yend = y_top),
    color = col_zero, linewidth = 0.9
  ) +
  
  # ---- 柱子 ----
geom_rect(
  data = best,
  aes(xmin = pmin(x_base, x_end), xmax = pmax(x_base, x_end),
      ymin = y - bar_h / 2, ymax = y + bar_h / 2),
  fill = col_up
) +
  geom_rect(
    data = worst,
    aes(xmin = pmin(x_base, x_end), xmax = pmax(x_base, x_end),
        ymin = y - bar_h / 2, ymax = y + bar_h / 2),
    fill = col_down
  ) +
  
  # ---- 头像 ----
ggimage::geom_image(
  data = best %>% filter(!is.na(headshot)),
  aes(x = px0_L + avatar_dx, y = y, image = headshot),
  size = avatar_size, asp = 1
) +
  ggimage::geom_image(
    data = worst %>% filter(!is.na(headshot)),
    aes(x = px0_R + avatar_dx, y = y, image = headshot),
    size = avatar_size, asp = 1
  ) +
  
  # ---- 排名 ----
geom_text(
  data = best,
  aes(x = px0_L + rank_dx, y = y, label = rk),
  family = font_en, fontface = "bold", size = 4.6, color = "#A9A296"
) +
  geom_text(
    data = worst,
    aes(x = px0_R + rank_dx, y = y, label = rk),
    family = font_en, fontface = "bold", size = 4.6, color = "#A9A296"
  ) +
  
  # ---- 名字（左对齐，紧贴头像）----
geom_text(
  data = best,
  aes(x = px0_L + name_dx, y = y + 0.14, label = first_part),
  hjust = 0, family = font_en, size = 4.0, color = col_first
) +
  geom_text(
    data = best,
    aes(x = px0_L + name_dx, y = y - 0.12, label = last_part),
    hjust = 0, family = font_en, fontface = "bold", size = 5.3, color = col_ink
  ) +
  geom_text(
    data = worst,
    aes(x = px0_R + name_dx, y = y + 0.14, label = first_part),
    hjust = 0, family = font_en, size = 4.0, color = col_first
  ) +
  geom_text(
    data = worst,
    aes(x = px0_R + name_dx, y = y - 0.12, label = last_part),
    hjust = 0, family = font_en, fontface = "bold", size = 5.3, color = col_ink
  ) +
  
  # ---- 数值 ----
# 最佳：柱子都很长，数值放在柱子右端内侧（白字）
geom_text(
  data = best,
  aes(x = x_end - 0.10, y = y, label = value_label),
  hjust = 1, family = font_cn, fontface = "bold", size = 5.0, color = "white"
) +
  # 最差：柱子可能很短，数值放在外侧（红字）
  geom_text(
    data = worst,
    aes(x = lab_x, y = y, label = value_label),
    hjust = 0, family = font_cn, fontface = "bold", size = 5.0, color = col_down
  ) +
  
  # ---- 面板标题 + 色条 ----
annotate("text", x = px0_L, y = y_top + 0.40, label = "最佳 TOP 10",
         hjust = 0, family = font_cn, fontface = "bold", size = 6.2, color = col_up) +
  annotate("text", x = px0_R, y = y_top + 0.40, label = "最差 TOP 10",
           hjust = 0, family = font_cn, fontface = "bold", size = 6.2, color = col_down) +
  annotate("segment", x = px0_L, xend = px0_L + Pw, y = y_top + 0.13, yend = y_top + 0.13,
           color = col_up, linewidth = 1.3) +
  annotate("segment", x = px0_R, xend = px0_R + Pw, y = y_top + 0.13, yend = y_top + 0.13,
           color = col_down, linewidth = 1.3) +
  
  # ---- 刻度数字 ----
geom_text(
  data = ticks,
  aes(x = x, y = y_bottom - 0.10, label = v),
  vjust = 1, family = font_cn, size = 4.3, color = col_axis
) +
  
  # ---- 总标题 / 副标题 / 脚注 ----
annotate("text", x = W / 2, y = H - 0.55, label = main_title,
         family = font_cn, fontface = "bold", size = 8.8, color = col_ink) +
  ggtext::geom_richtext(
    data = tibble(x = W / 2, y = H - 1.05, label = sub_line),
    aes(x = x, y = y, label = label),
    family = font_cn, size = 4.6, color = "#555555",
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +

  # ---- 画布：x、y 单位 = 英寸，1:1 ----
coord_fixed(ratio = 1, xlim = c(0, W), ylim = c(0, H), expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    plot.margin      = margin(0, 0, 10, 0)
  )


ggsave(
  "10月最佳与最差球员.png",
  p,
  width = W,
  height = H,
  dpi = 220,
  bg = bg_col
)