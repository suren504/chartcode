library(tidyverse)
library(tibble)
library(ggplot2)
library(ggimage)
library(ggtext)

# ============================================================
# 1. 数据
# ============================================================

TOP10 <- tribble(
  ~排名, ~球队, ~球员, ~player_id, ~data,
  1, "MIA", "Giannis Antetokounmpo", "203507", 31.2,
  2, "DAL", "Cooper Flagg", "1642843", 30.2,
  3, "SAS", "Victor Wembanyama", "1641705", 29.3,
  4, "NOP", "Zion Williamson", "1629627", 24.8,
  5, "OKC", "Ajay Mitchell", "1642349", 23.7,
  6, "LAL", "Walker Kessler", "1631117", 23.7,
  7, "TOR", "Scottie Barnes", "1630567", 23.5,
  8, "CLE", "Jarrett Allen", "1628386", 23.0,
  9, "HOU", "Alperen Sengun", "1630578", 22.6,
  10, "GSW", "Stephen Curry", "201939", 22.3
)

BOTTOM10 <- tribble(
  ~排名, ~球队, ~球员, ~player_id, ~data,
  1, "SAC", "Emanuel Sharp", "1643567", -0.89,
  2, "CHA", "Sion James", "1642883", 0.57,
  3, "MIA", "Klay Thompson", "202691", 0.81,
  4, "NYK", "Landry Shamet", "1629013", 1.27,
  5, "LAC", "Rui Hachimura", "1629060", 1.31,
  6, "MIL", "AJ Green", "1631260", 2.00,
  7, "OKC", "Jaylin Williams", "1631119", 2.42,
  8, "MIA", "Tim Hardaway Jr.", "203501", 3.24,
  9, "POR", "Scoot Henderson", "1630703", 3.56,
  10, "NYK", "Mikal Bridges", "1628969", 3.77
)

# ============================================================
# 2. 字体与颜色
# ============================================================

font_cn <- "Microsoft YaHei"
font_en <- "Consolas"

col_up   <- "#2B884D"   # 最佳：绿色
col_down <- "#C5352B"   # 最差：红色
bg_col   <- "floralwhite"

# ============================================================
# 3. 名字拆分函数
# ============================================================

split_player_name <- function(x) {
  parts <- str_split(str_squish(x), "\\s+")[[1]]
  
  if (length(parts) == 1) {
    return(list(first = "", last = parts[1]))
  }
  
  list(
    first = paste(parts[-length(parts)], collapse = " "),
    last  = parts[length(parts)]
  )
}

# ============================================================
# 4. 头像路径
# ============================================================

head_dir <- "D:/nba/球员组合图"

find_headshot <- function(player_id) {
  if (is.na(player_id)) return(NA_character_)
  
  id <- as.character(player_id)
  
  candidates <- c(
    file.path(head_dir, paste0(id, ".png")),
    file.path(head_dir, paste0(id, ".webp")),
    file.path(head_dir, paste0(id, ".jpg")),
    file.path(head_dir, paste0(id, ".jpeg"))
  )
  
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NA_character_)
  
  normalizePath(hit[1], winslash = "/", mustWork = FALSE)
}

# ============================================================
# 5. 数据整理
# ============================================================

prepare_data <- function(df) {
  name_parts <- map(df$球员, split_player_name)
  
  df %>%
    mutate(
      headshot   = map_chr(player_id, find_headshot),
      y          = 11 - 排名,
      raw_value  = data,
      abs_value  = abs(data),
      value_label = sprintf("%.2f", data),
      first_part = map_chr(name_parts, "first"),
      last_part  = map_chr(name_parts, "last")
    )
}

left_raw  <- prepare_data(TOP10)       # 左：最佳
right_raw <- prepare_data(BOTTOM10)    # 右：最差

# ============================================================
# 6. 左右独立缩放
#    左侧：0~35
#    右侧：0~5
# ============================================================

left_max_tick  <- 35
right_max_tick <- 5

# 画图时左右各占用的“内部柱状区宽度”
panel_half_width <- 30

left <- left_raw %>%
  mutate(
    plot_len = abs_value / left_max_tick * panel_half_width
  )

right <- right_raw %>%
  mutate(
    plot_len = abs_value / right_max_tick * panel_half_width
  )
# 画布半宽（必须定义，给名字留足空间）
x_limit <- 57

# 头像大小（ggimage 的 size 是占画布宽度的比例）
avatar_size <- 0.07
avatar_r    <- avatar_size * x_limit      # 头像半径（以 x 轴单位计）

# 头像中心、名字起点（名字紧贴头像边缘，gap 控制间隙）
avatar_gap <- 0.8
x_left_avatar  <- -(panel_half_width + 4.5)
x_right_avatar <-  (panel_half_width + 4.5)

x_left_name  <- -(panel_half_width + 4.5 + avatar_r + avatar_gap)
x_right_name <-  (panel_half_width + 4.5 + avatar_r + avatar_gap)

# 左右自定义刻度线位置
left_break_values  <- seq(0, left_max_tick, by = 5)
right_break_values <- seq(0, right_max_tick, by = 1)

left_break_pos  <- -left_break_values / left_max_tick * panel_half_width
right_break_pos <-  right_break_values / right_max_tick * panel_half_width

# 除0以外的参考线
grid_positions <- c(left_break_pos[-1], right_break_pos[-1])

# ============================================================
# 8. 标题与副标题
# ============================================================

main_title <- "NBA2026-27赛季10月最佳与最差球员"

sub_title <- paste0(
  "指标：每百回合 GBPM（攻防平均回合标准化）<br>",
  "制图：",
  "<span style='font-size:16pt; color:black;'><b>俗人罢了</b></span>",
  " | 制作时间：2026.11.03"
)

# ============================================================
# 9. 绘图
# ============================================================

p <- ggplot() +
  
  # 参考线
  geom_vline(
    xintercept = grid_positions,
    color = "#DCDCDC",
    linewidth = 0.5
  ) +
  
  # 中央 0 线
  geom_vline(
    xintercept = 0,
    color = "#202020",
    linewidth = 1.0
  ) +
  
  # ----------------------------------------------------------
# 左侧（最佳 TOP10）绿色，镜像向左
# ----------------------------------------------------------
geom_rect(
  data = left,
  aes(
    xmin = -plot_len,
    xmax = 0,
    ymin = y - 0.37,
    ymax = y + 0.37
  ),
  fill = col_up
) +
  
  # ----------------------------------------------------------
# 右侧（最差 TOP10）红色，向右
# ----------------------------------------------------------
geom_rect(
  data = right,
  aes(
    xmin = 0,
    xmax = plot_len,
    ymin = y - 0.37,
    ymax = y + 0.37
  ),
  fill = col_down
) +
  
  ggimage::geom_image(
    data = left %>% filter(!is.na(headshot)),
    aes(x = x_left_avatar, y = y, image = headshot),
    size = avatar_size,
    asp = 1
  ) +
  
  ggimage::geom_image(
    data = right %>% filter(!is.na(headshot)),
    aes(x = x_right_avatar, y = y, image = headshot),
    size = avatar_size,
    asp = 1
  ) +
  # 左侧数值（最佳）
  geom_text(
    data = left,
    aes(
      x = -plot_len - 0.7,
      y = y,
      label = value_label
    ),
    hjust = 1,
    family = font_cn,
    size = 5.2,
    fontface = "bold",
    color = col_up
  ) +
  
  # 右侧数值（最差）
  geom_text(
    data = right,
    aes(
      x = plot_len + 0.7,
      y = y,
      label = value_label
    ),
    hjust = 0,
    family = font_cn,
    size = 5.2,
    fontface = "bold",
    color = col_down
  ) +
  
  # 左侧：名字第一行
  geom_text(
    data = left,
    aes(
      x = x_left_name,
      y = y + 0.16,
      label = first_part
    ),
    hjust = 1,
    vjust = 0.5,
    family = font_en,
    size = 4.3,
    color = "#7A7A7A"
  ) +
  
  # 左侧：名字第二行
  geom_text(
    data = left,
    aes(
      x = x_left_name,
      y = y - 0.15,
      label = last_part
    ),
    hjust = 1,
    vjust = 0.5,
    family = font_en,
    size = 5.8,
    fontface = "bold",
    color = "#111111"
  ) +
  
  # 右侧：名字第一行
  geom_text(
    data = right,
    aes(
      x = x_right_name,
      y = y + 0.16,
      label = first_part
    ),
    hjust = 0,
    vjust = 0.5,
    family = font_en,
    size = 4.3,
    color = "#7A7A7A"
  ) +
  
  # 右侧：名字第二行
  geom_text(
    data = right,
    aes(
      x = x_right_name,
      y = y - 0.15,
      label = last_part
    ),
    hjust = 0,
    vjust = 0.5,
    family = font_en,
    size = 5.8,
    fontface = "bold",
    color = "#111111"
  ) +
  
  # 分区标题
  annotate(
    "text",
    x = -panel_half_width / 2,
    y = 10.72,
    label = "最佳 TOP 10",
    hjust = 0.5,
    family = font_cn,
    size = 5,
    fontface = "bold",
    color = col_up
  ) +
  
  annotate(
    "text",
    x = panel_half_width / 2,
    y = 10.72,
    label = "最差 TOP 10",
    hjust = 0.5,
    family = font_cn,
    size = 5,
    fontface = "bold",
    color = col_down
  ) +
  
  # 左右单独刻度文字
  annotate(
    "text",
    x = left_break_pos,
    y = 0.10,
    label = left_break_values,
    family = font_cn,
    size = 4.8,
    color = "#4E5661",
    vjust = 1
  ) +
  
  annotate(
    "text",
    x = right_break_pos,
    y = 0.10,
    label = right_break_values,
    family = font_cn,
    size = 4.8,
    color = "#4E5661",
    vjust = 1
  ) +
  scale_x_continuous(
    limits = c(-x_limit, x_limit),
    breaks = NULL,
    labels = NULL,
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0.05, 11.1),
    expand = c(0, 0)
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = main_title,
    subtitle = sub_title
  ) +
  
  theme_minimal(base_family = font_en) +
  theme(
    plot.background  = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    
    panel.grid = element_blank(),
    
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(
      family = font_cn,
      size = 24,
      face = "bold",
      color = "#111111",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    
    plot.subtitle = ggtext::element_markdown(
      family = font_cn,
      size = 13,
      color = "#555555",
      hjust = 0.5,
      margin = margin(b = 22)
    ),
    
    plot.margin = margin(25, 50, 30, 50)
  )

# ============================================================
# 10. 输出
# ============================================================

ggsave(
  "10月最佳与最差球员.png",
  p,
  width = 14,
  height = 12,
  dpi = 220,
  bg = bg_col
)