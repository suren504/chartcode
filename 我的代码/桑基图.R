library(ggplot2)
library(png)
library(ragg)
library(jsonlite)
library(patchwork)
library(ggtext)

# ============ 1. 路径与开关 ============
csv_path   <- "D:/nba/turnovers_top15_2026-27_regular.csv"
color_path <- "D:/nba/nba_site/src/lib/data/teamcolor.json"
img_dir    <- "D:/nba/球员组合图"
out_path   <- "D:/nba/turnovers_top15_sankey.png"

NCOL     <- 3       # 三列五行，每张桑基图留出更多横向空间
SHOW_PCT <- FALSE   # 活球/死球标签里是否带百分比
OUT_DPI  <- 110
HEAD_H   <- 2.0     # 标题+副标题占的高度(英寸)，上下空隙不对就调它

# 显示名覆盖（不想要变音符号的球员写这里；用 | 分行）
NAME_OVERRIDE <- c("1629029" = "Luka|Doncic")

TITLE_TXT <- "NBA本赛季失误数最多球员及其失误构成"
SUB_TXT <- paste0(
  "数据源：pbpstats|制图：<span style='color:black;'><b>俗人罢了</b></span>",
  " | 数据截至：2026/11/08"
)

BG   <- "#FFFAF0"
INK  <- "#2B2B2B"
GRAY <- "#7A7A7A"
FONT_CN  <- "Microsoft YaHei"
FONT_NUM <- "Consolas"

# ============ 2. 数据 ============
d  <- read.csv(csv_path, fileEncoding = "UTF-8-BOM", check.names = FALSE)
tc <- fromJSON(color_path)

team_color <- function(abbr) {
  x <- tc$team_color[tc$team_abbreviation == abbr]
  if (length(x) != 1) { warning("找不到球队色：", abbr); return("#555555") }
  x
}

# ============ 3. 单张版面参数（单位：英寸，所有人必须一致） ============
W <- 8.5;  H <- 6.8
K  <- 0.09     # 每1次失误的高度
G1 <- 0.31     # 给右侧相邻标签留出行距
G2 <- 0.35
NW <- 0.20
X1 <- 1.45;  X2 <- 3.60;  X3 <- 5.95
CHART_TOP <- H - 1.52

pt2mm <- function(pt) pt / ggplot2::.pt

make_ribbon <- function(id, xa, xb, a_top, a_bot, b_top, b_bot, fill, n = 60) {
  t <- seq(0, 1, length.out = n)
  s <- 3 * t^2 - 2 * t^3
  x <- xa + (xb - xa) * t
  top <- a_top + (b_top - a_top) * s
  bot <- a_bot + (b_bot - a_bot) * s
  data.frame(id = id, x = c(x, rev(x)), y = c(top, rev(bot)), fill = fill,
             stringsAsFactors = FALSE)
}

# ============ 4. 单张图函数 ============
make_panel <- function(r) {
  pid      <- as.character(r[["球员ID"]])
  tot      <- r[["总失误"]];   live <- r[["活球失误"]];  dead <- r[["死球失误"]]
  pass     <- r[["传球失误"]]; drib <- r[["运球失误"]]
  pass_oob <- r[["传球出界"]]; drib_oob <- r[["运球出界"]]; other <- r[["其他失误"]]
  stopifnot(tot == live + dead, live == pass + drib,
            dead == pass_oob + drib_oob + other)
  
  C <- list(
    total = team_color(r[["球队"]]),
    live  = "#8C3A34", live_pass = "#9E413A", live_drib = "#B7746B",
    dead  = "#3C6575", dead_pass = "#56899A", dead_drib = "#78A3B1", dead_other = "#467685"
  )
  
  # --- 节点 ---
  leaf <- data.frame(
    key  = c("live_pass", "live_drib", "dead_pass", "dead_drib", "dead_other"),
    name = c("传球失误", "运球失误", "传球出界", "运球出界", "其他失误"),
    v    = c(pass, drib, pass_oob, drib_oob, other),
    gap  = c(0, G1, G2, G1, G1),
    fill = c(C$live_pass, C$live_drib, C$dead_pass, C$dead_drib, C$dead_other),
    stringsAsFactors = FALSE
  )
  cur <- CHART_TOP
  yc <- cur - (tot * K + sum(leaf$gap)) / 2
  leaf$ytop <- NA_real_; leaf$ybot <- NA_real_
  for (i in seq_len(nrow(leaf))) {
    cur <- cur - leaf$gap[i]
    leaf$ytop[i] <- cur
    leaf$ybot[i] <- cur - leaf$v[i] * K
    cur <- leaf$ybot[i]
  }
  leaf$cy <- (leaf$ytop + leaf$ybot) / 2
  
  mid <- data.frame(
    name = c("活球失误", "死球失误"),
    v    = c(live, dead),
    cy   = c((leaf$ytop[1] + leaf$ybot[2]) / 2, (leaf$ytop[3] + leaf$ybot[5]) / 2),
    fill = c(C$live, C$dead),
    stringsAsFactors = FALSE
  )
  mid$ytop <- mid$cy + mid$v * K / 2
  mid$ybot <- mid$cy - mid$v * K / 2
  
  tot_top <- yc + tot * K / 2
  tot_bot <- yc - tot * K / 2
  
  # --- 流带 ---
  rib <- list(
    make_ribbon("t_live", X1 + NW, X2, tot_top, tot_top - live * K,
                mid$ytop[1], mid$ybot[1], C$live),
    make_ribbon("t_dead", X1 + NW, X2, tot_top - live * K, tot_bot,
                mid$ytop[2], mid$ybot[2], C$dead)
  )
  for (m in 1:2) {
    idx <- if (m == 1) 1:2 else 3:5
    src <- mid$ytop[m]
    for (i in idx) {
      if (leaf$v[i] > 0) {
        rib[[length(rib) + 1]] <- make_ribbon(
          leaf$key[i], X2 + NW, X3, src, src - leaf$v[i] * K,
          leaf$ytop[i], leaf$ybot[i], leaf$fill[i])
      }
      src <- src - leaf$v[i] * K
    }
  }
  rib <- do.call(rbind, rib)
  
  nodes <- rbind(
    data.frame(xmin = X1, xmax = X1 + NW, ymin = tot_bot, ymax = tot_top, fill = C$total),
    data.frame(xmin = X2, xmax = X2 + NW, ymin = mid$ybot, ymax = mid$ytop, fill = mid$fill),
    data.frame(xmin = X3, xmax = X3 + NW, ymin = leaf$ybot, ymax = leaf$ytop, fill = leaf$fill)
  )
  nodes <- nodes[nodes$ymax > nodes$ymin, ]
  
  # --- 流带中央标签：数字为主 ---
  xc  <- (X1 + NW + X2) / 2
  rc  <- c(((tot_top + (tot_top - live * K)) / 2 + mid$cy[1]) / 2,
           (((tot_top - live * K) + tot_bot) / 2 + mid$cy[2]) / 2)
  mid_lab <- if (SHOW_PCT) sprintf("%s  %d%%", mid$name, round(mid$v / tot * 100)) else mid$name
  
  # --- 头部 ---
  nm <- if (pid %in% names(NAME_OVERRIDE)) {
    strsplit(NAME_OVERRIDE[[pid]], "|", fixed = TRUE)[[1]]
  } else {
    parts <- strsplit(r[["球员"]], " ", fixed = TRUE)[[1]]
    c(parts[1], paste(parts[-1], collapse = " "))
  }
  img_file <- file.path(img_dir, paste0(pid, ".png"))
  has_img  <- file.exists(img_file)
  if (!has_img) warning("缺少头像：", img_file)

  img_h <- 1
  header_y <- CHART_TOP + 0.22
  name_grob <- grid::textGrob(
    paste(nm[nzchar(nm)], collapse = "\n"), just = "left",
    gp = grid::gpar(fontfamily = FONT_NUM, fontface = "plain", fontsize = 10,
                    lineheight = 1.5, col = INK)
  )
  img_w <- grid::unit(if (has_img) img_h else 0, "in")
  img_gap <- grid::unit(if (has_img) 0.16 else 0, "in")
  # 用输出设备上的姓名宽度计算头像和姓名的整体居中位置。
  header_left <- grid::unit(0.5, "npc") - (img_w + img_gap + grid::grobWidth(name_grob)) / 2
  header <- grid::grobTree(grid::editGrob(name_grob, x = header_left + img_w + img_gap))
  if (has_img) {
    img <- grDevices::as.raster(png::readPNG(img_file))
    header <- grid::addGrob(header, grid::rasterGrob(img, x = header_left, width = img_w,
                                                   height = img_w, just = "left"))
  }

  xl <- X3 + NW + 0.15
  
  p <- ggplot() +
    geom_polygon(data = rib, aes(x, y, group = id, fill = fill), alpha = 0.55) +
    geom_rect(data = nodes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
    scale_fill_identity() +
    annotation_custom(header, xmin = X1, xmax = X3 + NW,
                      ymin = header_y, ymax = header_y + img_h) +
    # 总失误（左）
    annotate("text", x = X1 - 0.16, y = yc + 0.06, label = tot, hjust = 1, vjust = 0,
             family = FONT_NUM, fontface = "bold", size = pt2mm(15), colour = INK) +
    annotate("text", x = X1 - 0.16, y = yc - 0.06, label = "总失误", hjust = 1, vjust = 1,
             family = FONT_CN, size = pt2mm(7.5), colour = GRAY) +
    # 活球/死球：流带正中，数字大、名称小
    annotate("text", x = xc, y = rc + 0.06, label = mid$v, hjust = 0.5, vjust = 0,
             family = FONT_NUM, fontface = "bold", size = pt2mm(11.5), colour = INK) +
    annotate("text", x = xc, y = rc - 0.06, label = mid_lab, hjust = 0.5, vjust = 1,
             family = FONT_CN, size = pt2mm(7.5), colour = INK) +
    # 五个具体类型（右）
    annotate("text", x = xl, y = leaf$cy, label = leaf$name, hjust = 0,
             family = FONT_CN, size = pt2mm(7.5), colour = INK) +
    annotate("text", x = xl + 1.34, y = leaf$cy, label = leaf$v, hjust = 0,
             family = FONT_NUM, fontface = "bold", size = pt2mm(8.5), colour = INK) +
    coord_fixed(xlim = c(0, W), ylim = c(0, H), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = BG, colour = NA),
          plot.margin = margin(0, 0, 0, 0))
  p
}

# ============ 5. 拼图 ============
plots <- lapply(seq_len(nrow(d)), function(i) make_panel(d[i, ]))
NROW_ <- ceiling(length(plots) / NCOL)

final <- wrap_plots(plots, ncol = NCOL) +
  plot_annotation(
    title    = TITLE_TXT,
    subtitle = SUB_TXT,
    theme = theme(
      plot.background = element_rect(fill = BG, colour = NA),
      plot.margin     = margin(0.45, 0, 0, 0, "in"),
      plot.title      = element_text(family = FONT_CN, face = "bold", size = 30,
                                     colour = INK, hjust = 0.5,
                                     margin = margin(b = 0.12, unit = "in")),
      plot.subtitle   = ggtext::element_markdown(family = FONT_CN, size = 18,
                                                 colour = GRAY, hjust = 0.5,
                                                 margin = margin(b = 0.25, unit = "in"))
    )
  )

ggsave(out_path, final, width = NCOL * W, height = NROW_ * H + HEAD_H,
       dpi = OUT_DPI, device = ragg::agg_png, bg = BG)
