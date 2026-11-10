# ==================== 1. 加载包 ====================
library(tidyverse)
library(ggbump)
library(ggimage)
library(ggtext)
library(ragg)
library(magick)

# ==================== 2. 全局参数 ====================
font_cn   <- "Microsoft YaHei"
font_num  <- "Consolas"
logo_dir  <- "D:/nba/球队logo/球队logo圆"
logo_size <- 0.030        # logo 大小
dot_size  <- 14           # logo 底座圆点大小
fade_line <- 0.15         # 非高亮：连线/底座透明度
fade_logo <- 0.22         # 非高亮：logo 透明度（越小越淡）
out_dir   <- "output"
dir.create(out_dir, showWarnings = FALSE)

# ==================== 3. 数据 ====================
raw <- read.table(text = "
team	Oct-26	Nov-26	Dec-26	Jan-27	Feb-27	Mar-27	Apr-27
CHI	1.7367	0.7649	1.6617	-0.3867	1.1033	-0.2433	-1.6538
MIN	0.4533	-0.8033	-0.083	2.0502	-0.8117	0.9467	-0.7395
BKN	1.6833	-0.8538	2.345	-0.3717	-0.2648	0.3633	0.2319
NOP	-0.4367	0.0221	0.6075	1.2283	-0.155	0.2967	-0.4883
DAL	3.6533	-0.3236	-1.4717	1.019	0.9533	-0.2123	-0.9038
MIL	-0.3767	2.8815	0.9397	-0.856	-0.6067	-0.5867	-1.361
TOR	-1.3967	0.5105	1.2033	-0.406	0.3579	-0.02	0.6319
CHA	-0.88	-0.1758	1.2408	0.6252	0.4742	-0.5733	0.189
NYK	2.3867	-0.3645	-0.9739	-0.406	2.0033	-0.36	0.389
SAC	-0.1867	1.214	-1.2467	0.168	0.4488	0.0096	-0.0752
IND	-1.4217	0.7748	0.5352	-0.8748	-1.1421	1.0846	1.9617
ATL	1.62	0.0072	0.9897	-0.1717	-0.5925	-1.1935	2.3117
WAS	-1.8633	1.7569	1.9867	1.044	-2.6694	-1.3502	-0.1324
ORL	1.4033	-0.161	-1.4285	1.0798	0.0488	-0.3279	0.2783
PHI	0.6783	0.5733	-0.7103	-1.1529	1.017	0.9367	-1.255
MEM	-2.5467	-0.7154	0.5492	1.6212	0.5397	-0.8092	1.0033
CLE	-1.1067	0.2233	0.2306	0.068	-0.3383	0.1264	0.3962
GSW	1.3117	-0.8252	-0.3883	0.0067	-0.3917	0.269	1.1533
POR	-1.8592	0.8471	-0.3921	0.4167	-0.7008	-0.3685	0.0367
PHX	0.8658	-0.15	-1.6925	-0.3567	0.4742	0.519	0.645
MIA	-0.9717	0.811	-2.5217	0.1783	-0.283	0.294	2.3533
LAC	0.9233	-0.6038	-0.1421	0.5815	-0.4148	-0.3467	-0.7395
BOS	-1.0967	-0.0833	-1.8103	-0.6367	1.0033	0.5377	0.495
UTA	-1.2967	-0.0873	0.4715	-0.5408	0.5783	-0.3767	-0.6633
DET	1.6367	-0.4736	0.2867	-0.4667	-0.1633	-0.74	-0.6324
LAL	-1.7717	-1.4502	0.4761	-0.3967	0.7383	0.4502	0.0617
HOU	-1.355	0.0462	0.9533	0.6283	-2.7194	0.4096	-2.2133
SAS	-0.0367	-1.7623	-0.9383	-1.4181	0.5124	1.8833	-0.7324
OKC	-0.3267	-0.54	-0.733	-1.0967	1.5408	-0.68	-1.5633
DEN	-1.0567	-0.4659	-0.5467	-1.3643	0.4806	-0.2154	-0.7538
", header = TRUE, sep = "", check.names = FALSE)

# 球队主色（连线颜色）
team_colors <- c(
  ATL = "#E03A3E", BOS = "#007A33", BKN = "#222222", CHA = "#00788C", CHI = "#CE1141",
  CLE = "#860038", DAL = "#00538C", DEN = "#FEC524", DET = "#C8102E", GSW = "#1D428A",
  HOU = "#CE1141", IND = "#FDBB30", LAC = "#C8102E", LAL = "#552583", MEM = "#5D76A9",
  MIA = "#98002E", MIL = "#00471B", MIN = "#236192", NOP = "#85714D", NYK = "#F58426",
  OKC = "#007AC1", ORL = "#0077C0", PHI = "#006BB6", PHX = "#E56020", POR = "#E03A3E",
  SAC = "#5A2D81", SAS = "#8A8D8F", TOR = "#CE1141", UTA = "#F9A01B", WAS = "#002B5C"
)

divisions <- list(
  "大西洋赛区" = c("BOS", "BKN", "NYK", "PHI", "TOR"),
  "中部赛区"   = c("CHI", "CLE", "DET", "IND", "MIL"),
  "东南赛区"   = c("ATL", "CHA", "MIA", "ORL", "WAS"),
  "西北赛区"   = c("DEN", "MIN", "OKC", "POR", "UTA"),
  "太平洋赛区" = c("GSW", "LAC", "LAL", "PHX", "SAC"),
  "西南赛区"   = c("DAL", "HOU", "MEM", "NOP", "SAS")
)
stopifnot(setequal(unlist(divisions), raw$team))   # 检查 30 队不重不漏

months   <- names(raw)[-1]
x_labels <- c("10月", "11月", "12月", "1月", "2月", "3月", "4月")

# ==================== 4. 整理：每月按数值从大到小排名（#1 = 数值最大 = 当月赛程最难） ====================
df_bump <- raw %>%
  pivot_longer(-team, names_to = "month", values_to = "value") %>%
  mutate(time_idx = match(month, months)) %>%
  group_by(time_idx) %>%
  mutate(Rank = rank(-value, ties.method = "first")) %>%
  ungroup() %>%
  mutate(
    color_hex = team_colors[team],
    logo_path = file.path(logo_dir, paste0(team, ".png"))
  )

missing_logo <- unique(df_bump$team[!file.exists(df_bump$logo_path)])
if (length(missing_logo)) warning("缺少 logo：", paste(missing_logo, collapse = ", "))

n_m <- length(months)

fade_fun <- function(img) {
  img <- magick::image_convert(img, type = "TrueColorAlpha")
  bmp <- magick::image_data(img, channels = "rgba")          # 3 × 宽 × 高 的 raw 数组，第 4 通道是 alpha
  bmp[4, , ] <- as.raw(round(as.integer(bmp[4, , ]) * fade_logo))
  magick::image_read(bmp)
}

fade_dir <- file.path(out_dir, "logo_faded")
dir.create(fade_dir, showWarnings = FALSE)
for (tm in unique(df_bump$team)) {
  src <- file.path(logo_dir, paste0(tm, ".png"))
  img <- magick::image_read(src)
  magick::image_write(fade_fun(img), file.path(fade_dir, paste0(tm, ".png")))
}

# ==================== 5. 单个赛区绘图函数 ====================
plot_division <- function(div_name, div_teams) {
  
  d  <- df_bump %>% mutate(focus = team %in% div_teams)
  bg <- d %>% filter(!focus) %>%
    mutate(logo_path = file.path(fade_dir, paste0(team, ".png")))   # 背景队：淡化 logo
  fg <- d %>% filter(focus)                                         # 本赛区队：正常
  
  ggplot(d, aes(x = time_idx, y = Rank, group = team)) +
    
    # 左侧名次标签与方向提示
    annotate("text", x = 0.55, y = 1:30, label = paste0("#", 1:30),
             family = font_num, fontface = "bold", size = 6, color = "#333333") +
    annotate("text", x = 0.55, y = 31, label = "赛程最易▼",
             family = font_cn, fontface = "bold", size = 5, color = "#2E7D5B") +
    annotate("text", x = 0.55, y = 0, label = "赛程最难▲",
             family = font_cn, fontface = "bold", size = 5, color = "#B23A2E") +
    
    # ---- 背景层：淡化 ----
  geom_bump(data = bg, aes(color = color_hex), linewidth = 2.5, smooth = 8,
            lineend = "round", alpha = fade_line) +
    geom_point(data = bg, aes(color = color_hex), size = dot_size, alpha = fade_line) +
    geom_image(data = bg, aes(image = logo_path), size = logo_size) +
    
    # ---- 高亮层：正常显示（后画，压在上层） ----
  geom_bump(data = fg, aes(color = color_hex), linewidth = 2.5, smooth = 8,
            lineend = "round", alpha = 0.9) +
    geom_point(data = fg, aes(color = color_hex), size = dot_size) +
    geom_image(data = fg, aes(image = logo_path), size = logo_size) +
    
    scale_color_identity() +
    scale_y_reverse(breaks = 1:30, limits = c(31.6, -0.6)) +
    scale_x_continuous(breaks = 1:n_m, labels = x_labels,
                       limits = c(0.1, n_m + 0.3)) +
    
    labs(
      title    = paste0("<span style='font-family:Consolas'>NBA2026-27</span>常规赛",
                        div_name, "球队月度赛程难度变化"),
      subtitle = paste0("排名依据当月赛程难度数值，#1 为当月赛程最难 | 制作：",
                        "<span style='font-size:18pt; color:black;'><b>俗人罢了</b></span>",
                        "|时间口径为北京时间"),
      x = NULL, y = NULL
    ) +
    
    theme_minimal(base_family = font_cn) +
    theme(
      plot.background    = element_rect(fill = "floralwhite", color = NA),
      panel.background   = element_rect(fill = "floralwhite", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "gray85", linetype = "dashed"),
      axis.text.y        = element_blank(),
      axis.text.x        = element_text(size = 22, face = "bold", color = "#333333"),
      plot.title         = element_markdown(size = 32, face = "bold", hjust = 0.5,
                                            margin = margin(t = 10, b = 2)),
      plot.subtitle      = element_markdown(size = 16, color = "#A1A1A1", hjust = 0.5,
                                            margin = margin(b = 4)),
      plot.margin        = margin(10, 0, 10, 0)
    )
}

# ==================== 6. 批量保存 ====================
iwalk(divisions, function(teams, div_name) {
  p <- plot_division(div_name, teams)
  file_name <- paste0("NBA2026-27常规赛", div_name, "球队月度赛程难度变化.png")
  ggsave(file.path(out_dir, file_name), p,
         width = 16, height = 20, dpi = 200,
         bg = "floralwhite", device = ragg::agg_png)
  message("已输出：", file_name)
})