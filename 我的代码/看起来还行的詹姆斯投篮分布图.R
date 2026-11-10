library(arrow)
library(dplyr)
library(ggplot2)

data <- read_parquet("D:/nba/shot_detail.parquet")

# ========== 基本参数（沿用原图 1205 像素坐标） ==========
font_family <- "Microsoft YaHei"   # Mac 可改 "PingFang SC"
W  <- 1205
DY <- 200                          # 顶部统计区整体下移的高度（因为多了一排）
H  <- 990 + DY + 40
K  <- (6.5 * 72 / W) / ggplot2::.pt          # 画布单位 -> ggplot 字号，对应输出宽 6.5 英寸
fs <- function(u) u * K

X0 <- 62; X1 <- 1143; Y0 <- 245; Y1 <- 990; Rr <- 22

# ========== 颜色：按命中率渐变（红 -> 浅红 -> 中性灰 -> 浅绿 -> 绿） ==========
pct_fill <- function(p) {
  anchors <- c(0.36, 0.45, 0.50, 0.53, 0.60)
  cols    <- c("#C5352B", "#E39A95", "#E9E4E6", "#9DC9A9", "#2B884D")
  rgbm <- t(col2rgb(cols))
  p <- pmin(pmax(p, min(anchors)), max(anchors))
  r <- approx(anchors, rgbm[, 1], xout = p)$y
  g <- approx(anchors, rgbm[, 2], xout = p)$y
  b <- approx(anchors, rgbm[, 3], xout = p)$y
  rgb(r, g, b, maxColorValue = 255)
}
pct_text <- function(p) sprintf("%d%%", as.integer(floor(100 * p + 0.5)))
# ========== 几何：14 个格子 ==========
pt     <- function(x, y) data.frame(x = x, y = y)
rev_df <- function(d) d[nrow(d):1, ]
cnr    <- function(cx, cy, a0, a1, r = Rr) {
  a <- seq(a0, a1, length.out = 12) * pi / 180
  data.frame(x = cx + r * cos(a), y = cy + r * sin(a))
}
rect_pts <- function(x0, x1, y0, y1) data.frame(x = c(x0, x1, x1, x0), y = c(y0, y0, y1, y1))
arcL <- function() { t <- seq(0, pi / 2, length.out = 40); data.frame(x = 422 - 202 * cos(t), y = 638 + 194 * sin(t)) }
arcR <- function() { t <- seq(pi / 2, 0, length.out = 40); data.frame(x = 783 + 202 * cos(t), y = 638 + 194 * sin(t)) }
Z <- function(zone, ...) bind_rows(...) %>% mutate(zone = zone)

polys <- bind_rows(
  # 三分区
  Z("c3_L", cnr(X0 + Rr, Y0 + Rr, 180, 270), pt(220, Y0), pt(220, 638), pt(X0, 638)),
  Z("w3_L", pt(X0, 638), pt(220, 638), arcL(), pt(422, Y1), cnr(X0 + Rr, Y1 - Rr, 90, 180)),
  Z("w3_C", rect_pts(422, 783, 832, Y1)),
  Z("w3_R", pt(985, 638), pt(X1, 638), cnr(X1 - Rr, Y1 - Rr, 0, 90), pt(783, Y1), arcR()),
  Z("c3_R", pt(985, Y0), cnr(X1 - Rr, Y0 + Rr, 270, 360), pt(X1, 638), pt(985, 638)),
  # 两分区
  Z("mr_L",  rect_pts(220, 422, Y0, 477)),
  Z("mr_LC", pt(220, 477), pt(422, 477), rev_df(arcL())),
  Z("ra_L",  rect_pts(422, 602, Y0, 477)),
  Z("ra_R",  rect_pts(602, 783, Y0, 477)),
  Z("pt_L",  rect_pts(422, 602, 477, 715)),
  Z("pt_R",  rect_pts(602, 783, 477, 715)),
  Z("mr_C",  rect_pts(422, 783, 715, 832)),
  Z("mr_R",  rect_pts(783, 985, Y0, 477)),
  Z("mr_RC", pt(783, 477), pt(985, 477), pt(985, 638), rev_df(arcR()))
) %>% mutate(yp = H - (y + DY))

# 每个格子的文字位置（x, 百分比的 y）
cells <- tibble::tribble(
  ~zone,   ~tx,  ~ty,
  "c3_L",  141,  420,
  "w3_L",  221,  815,
  "w3_C",  602,  913,
  "w3_R",  985,  815,
  "c3_R", 1066,  423,
  "mr_L",  320,  340,
  "mr_LC", 320,  603,
  "ra_L",  512,  340,
  "ra_R",  693,  340,
  "pt_L",  512,  573,
  "pt_R",  693,  577,
  "mr_C",  602,  773,
  "mr_R",  883,  340,
  "mr_RC", 883,  603
)

# ========== 分区规则（按 NBA 官方区域字段） ==========
assign_zone <- function(d) {
  lsign <- sign(median(d$LOC_X[d$SHOT_ZONE_AREA == "Left Side(L)"], na.rm = TRUE))
  if (is.na(lsign) || lsign == 0) lsign <- -1       # 自动判断 LOC_X 正负对应哪一侧
  d %>% mutate(
    side     = case_when(startsWith(SHOT_ZONE_AREA, "Left") ~ "L",
                         startsWith(SHOT_ZONE_AREA, "Right") ~ "R",
                         TRUE ~ "C"),
    rim_side = ifelse(LOC_X * lsign > 0, "L", "R"),
    zone = case_when(
      SHOT_ZONE_BASIC == "Left Corner 3"  ~ "c3_L",
      SHOT_ZONE_BASIC == "Right Corner 3" ~ "c3_R",
      SHOT_ZONE_BASIC == "Above the Break 3" & side == "L" ~ "w3_L",
      SHOT_ZONE_BASIC == "Above the Break 3" & side == "R" ~ "w3_R",
      SHOT_ZONE_BASIC == "Above the Break 3"               ~ "w3_C",
      SHOT_ZONE_BASIC == "Restricted Area"       ~ paste0("ra_", rim_side),
      SHOT_ZONE_BASIC == "In The Paint (Non-RA)" ~ paste0("pt_", rim_side),
      SHOT_ZONE_BASIC == "Mid-Range" & SHOT_ZONE_AREA == "Left Side(L)"         ~ "mr_L",
      SHOT_ZONE_BASIC == "Mid-Range" & SHOT_ZONE_AREA == "Left Side Center(LC)"  ~ "mr_LC",
      SHOT_ZONE_BASIC == "Mid-Range" & SHOT_ZONE_AREA == "Center(C)"             ~ "mr_C",
      SHOT_ZONE_BASIC == "Mid-Range" & SHOT_ZONE_AREA == "Right Side Center(RC)" ~ "mr_RC",
      SHOT_ZONE_BASIC == "Mid-Range" & SHOT_ZONE_AREA == "Right Side(R)"         ~ "mr_R",
      TRUE ~ NA_character_
    )
  )
}

# ========== 主函数 ==========
shot_zone_chart <- function(d = data) {
  d <- assign_zone(d)
  
  # --- 格子数据 ---
  zs <- d %>% filter(!is.na(zone)) %>%
    group_by(zone) %>% summarise(att = n(), made = sum(SHOT_MADE_FLAG), .groups = "drop")
  
  cl <- cells %>%
    left_join(zs, by = "zone") %>%
    mutate(att  = ifelse(is.na(att), 0L, att),
           made = ifelse(is.na(made), 0L, made),
           pct  = ifelse(att > 0, made / att, 0),
           small   = att < 10,                                   # 样本少于 10 次 -> 灰色
           fill    = ifelse(small, "#E6E6E6", pct_fill(pct)),
           txt_col = ifelse(small, "#A0A0A0", "white"),
           pct_lab = ifelse(att == 0, "-", pct_text(pct)),
           cnt_lab = ifelse(att == 0, "", sprintf("(%d-%d)", made, att)),
           yp_pct  = H - (ty + DY),
           yp_cnt  = H - (ty + 48 + DY))
  
  pp <- polys %>% left_join(cl %>% select(zone, fill), by = "zone")
  
  # --- 顶部统计：全部 / 两分球 / 三分球 / 左边 / 中间 / 右边 ---
  subsets <- list(d,
                  filter(d, SHOT_TYPE == "2PT Field Goal"),
                  filter(d, SHOT_TYPE == "3PT Field Goal"),
                  filter(d, side == "L"), filter(d, side == "C"), filter(d, side == "R"))
  hd <- tibble(label = c("全部", "两分球", "三分球", "左边", "中间", "右边"),
               col = c(1, 2, 3, 1, 2, 3), row = c(1, 1, 1, 2, 2, 2),
               made = sapply(subsets, function(x) sum(x$SHOT_MADE_FLAG)),
               att  = sapply(subsets, nrow)) %>%
    mutate(xc = c(243, 602, 963)[col],
           y_big = c(95, 295)[row],
           y_lab = y_big + 83,
           pct_lab = pct_text(made / att),
           num_lab = sprintf("%d/%d", made, att),
           lw = nchar(label) * 38,
           nw = nchar(num_lab) * 0.58 * 38,
           x_start = xc - (lw + 16 + nw) / 2,
           x_num   = x_start + lw + 16,
           yp_big = H - y_big, yp_lab = H - y_lab)
  
  seps <- expand.grid(x = c(424, 783), y0 = c(45, 245)) %>%
    mutate(y1 = y0 + 160, yp0 = H - y0, yp1 = H - y1)
  
  ggplot() +
    geom_polygon(data = pp, aes(x, yp, group = zone, fill = fill), color = "white", linewidth = 0.8) +
    scale_fill_identity() + scale_colour_identity() +
    geom_text(data = cl, aes(tx, yp_pct, label = pct_lab, colour = txt_col),
              family = font_family, fontface = "bold", size = fs(58)) +
    geom_text(data = cl, aes(tx, yp_cnt, label = cnt_lab, colour = txt_col),
              family = font_family, size = fs(39)) +
    geom_segment(data = seps, aes(x = x, xend = x, y = yp0, yend = yp1), colour = "#DDDDDD", linewidth = 0.4) +
    geom_text(data = hd, aes(xc, yp_big, label = pct_lab),
              family = font_family, size = fs(86), colour = "#111111") +
    geom_text(data = hd, aes(x_start, yp_lab, label = label), hjust = 0,
              family = font_family, fontface = "bold", size = fs(38), colour = "#111111") +
    geom_text(data = hd, aes(x_num, yp_lab, label = num_lab), hjust = 0,
              family = font_family, fontface = "bold", size = fs(38), colour = "#A6A6A6") +
    coord_fixed(xlim = c(0, W), ylim = c(0, H), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(0, 0, 0, 0))
}

# ========== 使用 ==========
p <- shot_zone_chart(data)
ggsave("D:/nba/zone_chart.png", p, width = 6.5, height = 6.5 * H / W,
       dpi = 300, bg = "white", device = ragg::agg_png)