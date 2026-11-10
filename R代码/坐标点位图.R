library(tidyverse)
library(arrow)
library(ggimage)
library(ggtext)
library(ragg)
library(jsonlite)
library(stringi)

# ============ 1. 路径与参数 ============

pbp_path <- "D:/nba/RStudio/HoopR/nba/pbp/parquet/play_by_play_2026.parquet"
head_dir <- "D:/nba/head"
id_json  <- "D:/nba/id查询.json"

output_path <- "D:/nba/nba2026_three_point_blockers_16.png"

top_players <- c(
  "Robert Williams III",
  "Victor Wembanyama",
  "Naz Reid",
  "Julian Champagnie",
  "Anthony Black",
  "Jay Huff",
  "Matisse Thybulle",
  "Al Horford",
  "Caris LeVert",
  "OG Anunoby",
  "Ausar Thompson",
  "Draymond Green",
  "Jerami Grant",
  "Scottie Barnes",
  "Derrick White",
  "Mitchell Robinson"
)

hoop_x <- 25
hoop_y <- 5.25

# 根据 ESPN 原始坐标与文本投篮距离拟合
espn_hoop_y <- 0.8

font_cn <- "Microsoft YaHei"
font_en <- "Consolas"

bg <- "floralwhite"

blocker_size <- 0.17
shooter_size <- 0.05

# 排版
n_col <- 4
n_row <- ceiling(length(top_players) / n_col)

fig_w <- 14
panel_w <- (fig_w - 1.1) / n_col
fig_h <- n_row * (panel_w * 0.96 + 0.45) + 1.7

# 名字与 ID 不匹配时可手动补充
manual_ids <- c()

# ============ 2. 球员姓名 -> ID -> 头像路径 ============

norm <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z ]", "") %>%
    str_squish()
}

ids <- fromJSON(id_json)

id_tbl <- tibble(
  name = names(ids),
  id = as.integer(unlist(ids))
) %>%
  mutate(key = norm(name))

if (length(manual_ids) > 0) {
  id_tbl <- bind_rows(
    tibble(
      name = names(manual_ids),
      id = as.integer(manual_ids),
      key = norm(names(manual_ids))
    ),
    id_tbl
  )
}

id_tbl <- id_tbl %>%
  distinct(key, .keep_all = TRUE)

get_avatar <- function(name) {
  id <- id_tbl$id[match(norm(name), id_tbl$key)]
  
  path <- file.path(
    head_dir,
    paste0(id, ".png")
  )
  
  if_else(
    !is.na(id) & file.exists(path),
    path,
    NA_character_
  )
}

# ============ 3. 提取三分盖帽事件 ============

data <- read_parquet(pbp_path)

three_blocks <- data %>%
  filter(
    !is.na(text),
    str_detect(
      text,
      regex(
        "\\bblocks\\b.*\\bthree[ -]?point\\b",
        ignore_case = TRUE
      )
    )
  ) %>%
  mutate(
    blocker = str_match(
      text,
      regex(
        "^(.+?)\\s+blocks\\b",
        ignore_case = TRUE
      )
    )[, 2],
    
    shooter = str_match(
      text,
      regex(
        "\\bblocks\\s+(.+?)\\s*['\u2019]s\\s",
        ignore_case = TRUE
      )
    )[, 2]
  ) %>%
  filter(!is.na(blocker))

# ============ 4. 统计指定球员的三分盖帽 ============

cnt <- three_blocks %>%
  filter(blocker %in% top_players) %>%
  count(blocker) %>%
  arrange(desc(n), blocker)

cat("\n========== 三分盖帽统计 ==========\n")
print(cnt, n = Inf)

lv <- cnt$blocker

cnt <- cnt %>%
  mutate(
    blocker = factor(blocker, levels = lv)
  )

# ============ 5. ESPN 坐标转换 ============

shots <- three_blocks %>%
  filter(blocker %in% top_players) %>%
  mutate(
    x0 = as.numeric(coordinate_x_raw),
    y0 = as.numeric(coordinate_y_raw)
  ) %>%
  filter(
    is.finite(x0),
    is.finite(y0),
    between(x0, 0, 50),
    between(y0, 0, 47)
  ) %>%
  mutate(
    # ESPN 坐标转换到标准 NBA 半场
    # 原始坐标估计篮筐位置：(25, 0.8)
    # 绘图坐标篮筐位置：(25, 5.25)
    
    x = x0,
    y = y0 + (hoop_y - espn_hoop_y),
    
    avatar = get_avatar(shooter),
    blocker = factor(blocker, levels = lv)
  )

message(
  "有坐标的点：",
  nrow(shots),
  " / ",
  sum(cnt$n)
)

# 检查头像缺失情况
miss <- shots %>%
  filter(is.na(avatar)) %>%
  count(shooter, sort = TRUE)

if (nrow(miss) > 0) {
  message("没找到头像的出手球员（画成灰点）：")
  print(miss, n = Inf)
}

# 盖帽者头像
head_info <- tibble(
  blocker = factor(lv, levels = lv),
  avatar = get_avatar(lv)
)

if (anyNA(head_info$avatar)) {
  warning(
    "盖帽者没找到头像：",
    paste(
      head_info$blocker[is.na(head_info$avatar)],
      collapse = ", "
    )
  )
}

# ============ 6. 构造 NBA 半场 ============

arc <- function(
    cx, cy, r,
    from, to,
    part,
    dash = "solid",
    n = 200
) {
  theta <- seq(
    from,
    to,
    length.out = n
  )
  
  tibble(
    x = cx + r * cos(theta),
    y = cy + r * sin(theta),
    part = part,
    dash = dash
  )
}

# 标准 NBA 三分线
three_radius <- 23.75
corner_x <- 22

th <- acos(corner_x / three_radius)

corner_y <- hoop_y + sqrt(
  three_radius^2 - corner_x^2
)

court <- bind_rows(
  
  # 球场外框
  tibble(
    x = c(0, 50, 50, 0, 0),
    y = c(0, 0, 47, 47, 0),
    part = "outline",
    dash = "solid"
  ),
  
  # 禁区
  tibble(
    x = c(17, 17, 33, 33),
    y = c(0, 19, 19, 0),
    part = "paint",
    dash = "solid"
  ),
  
  # 篮板
  tibble(
    x = c(22, 28),
    y = rep(hoop_y - 1.25, 2),
    part = "board",
    dash = "solid"
  ),
  
  # 篮筐
  arc(
    hoop_x, hoop_y,
    0.75,
    0, 2 * pi,
    "rim"
  ),
  
  # 合理冲撞区
  arc(
    hoop_x, hoop_y,
    4,
    0, pi,
    "restricted"
  ),
  
  # 罚球圆上半部分
  arc(
    25, 19,
    6,
    0, pi,
    "ft_top"
  ),
  
  # 罚球圆下半部分（虚线）
  arc(
    25, 19,
    6,
    pi, 2 * pi,
    "ft_bottom",
    dash = "dashed"
  ),
  
  # 三分线：左底角 + 圆弧 + 右底角
  bind_rows(
    
    tibble(
      x = c(3, 3),
      y = c(0, corner_y)
    ),
    
    arc(
      hoop_x,
      hoop_y,
      three_radius,
      pi - th,
      th,
      "three"
    ) %>%
      select(x, y),
    
    tibble(
      x = c(47, 47),
      y = c(corner_y, 0)
    )
    
  ) %>%
    mutate(
      part = "three",
      dash = "solid"
    )
)

# ============ 7. 绘图 ============

p <- ggplot() +
  
  # 球场线
  geom_path(
    data = court,
    aes(
      x = x,
      y = y,
      group = part,
      linetype = dash
    ),
    color = "grey35",
    linewidth = 0.35,
    alpha = 0.7
  ) +
  
  scale_linetype_identity() +
  
  # 没有头像的投篮：灰点
  geom_point(
    data = filter(shots, is.na(avatar)),
    aes(x, y),
    shape = 21,
    size = 2,
    fill = "grey65",
    color = "grey30",
    stroke = 0.3
  ) +
  
  # 被封盖球员头像
  geom_image(
    data = filter(shots, !is.na(avatar)),
    aes(
      x = x,
      y = y,
      image = avatar
    ),
    size = shooter_size,
    asp = 50 / 48
  ) +
  
  # 盖帽球员头像
  geom_image(
    data = filter(head_info, !is.na(avatar)),
    aes(
      x = 12,
      y = 40.5,
      image = avatar
    ),
    size = blocker_size,
    asp = 50 / 48
  ) +
  
  # 三分盖帽总数
  geom_text(
    data = cnt,
    aes(
      x = 34,
      y = 40.5,
      label = n
    ),
    family = font_en,
    fontface = "bold",
    size = 10,
    color = "grey15"
  ) +
  
  # 4列分面
  facet_wrap(
    ~blocker,
    ncol = n_col
  ) +
  
  # 固定球场比例
  coord_fixed(
    xlim = c(0, 50),
    ylim = c(-1, 47),
    expand = FALSE,
    clip = "off"
  ) +
  
  # 标题
  labs(
    title = paste0(
      "<span style='font-family:",
      font_en,
      "'>NBA2026</span>",
      "常规赛+季后赛三分总盖帽数最多球员"
    ),
    
    subtitle = "每个头像代表一次被盖掉三分的出手球员，中间数字为总盖帽数,部分点位或有缺失",
    
    caption = "Data: hoopR / ESPN"
  ) +
  
  theme_void(
    base_family = font_cn
  ) +
  
  theme(
    
    # 背景
    plot.background = element_rect(
      fill = bg,
      color = NA
    ),
    
    panel.background = element_rect(
      fill = bg,
      color = NA
    ),
    
    # 主标题
    plot.title = element_markdown(
      family = font_cn,
      face = "bold",
      size = 24,
      hjust = 0.5,
      margin = margin(t = 6, b = 4)
    ),
    
    # 副标题
    plot.subtitle = element_text(
      family = font_cn,
      size = 12,
      color = "grey30",
      hjust = 0.5,
      margin = margin(b = 14)
    ),
    
    # 数据来源
    plot.caption = element_text(
      family = font_en,
      size = 8,
      color = "grey45"
    ),
    
    # 球员姓名
    strip.text = element_text(
      family = font_en,
      face = "bold",
      size = 12,
      margin = margin(b = 6)
    ),
    
    # 面板间距
    panel.spacing = unit(
      1.2,
      "lines"
    ),
    
    # 外边距
    plot.margin = margin(
      16, 20, 12, 20
    )
  )

# ============ 8. 保存图片 ============

dir.create(
  dirname(output_path),
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = output_path,
  plot = p,
  width = fig_w,
  height = fig_h,
  dpi = 300,
  bg = bg,
  device = ragg::agg_png
)

cat("\n图片已保存：", output_path, "\n")