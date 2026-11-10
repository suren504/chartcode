# ==============================================================================
# 0. 加载必要的包
# ==============================================================================
library(ggplot2)
library(dplyr)
library(ggtext)    # 用于富文本标签
library(ggimage)   # 用于加载图片
library(stringr)   
library(scales)    
library(cropcircles) # 核心：把图片变圆！
library(showtext)
showtext_auto()
# ==============================================================================
# 1. 录入数据与颜色字典
# ==============================================================================
df <- tribble(
  ~Team_Standard, ~Total, ~Success, ~Fail, ~Success_Rate,
  "Trail Blazers", 61, 38, 23, 0.623,
  "Timberwolves",  59, 42, 17, 0.712,
  "Rockets",       56, 38, 18, 0.679,
  "Suns",          55, 36, 19, 0.655,
  "Thunder",       55, 34, 21, 0.618,
  "Jazz",          51, 34, 17, 0.667,
  "Warriors",      51, 35, 16, 0.686,
  "Lakers",        48, 31, 17, 0.646,
  "Clippers",      45, 30, 15, 0.667,
  "76ers",         44, 32, 12, 0.727,
  "Cavaliers",     44, 30, 14, 0.682,
  "Grizzlies",     41, 24, 17, 0.585,
  "Nets",          40, 26, 14, 0.650,
  "Pistons",       40, 23, 17, 0.575,
  "Hornets",       39, 28, 11, 0.718,
  "Wizards",       38, 22, 16, 0.579,
  "Hawks",         37, 22, 15, 0.595,
  "Magic",         36, 26, 10, 0.722,
  "Kings",         33, 22, 11, 0.667,
  "Nuggets",       33, 21, 12, 0.636,
  "Pelicans",      33, 22, 11, 0.667,
  "Celtics",       32, 23,  9, 0.719,
  "Heat",          32, 23,  9, 0.719,
  "Knicks",        32, 23,  9, 0.719,
  "Raptors",       32, 19, 13, 0.594,
  "Bucks",         31, 14, 17, 0.452,
  "Mavericks",     29, 14, 15, 0.483,
  "Spurs",         25, 14, 11, 0.560,
  "Pacers",        22, 13,  9, 0.591,
  "Bulls",         16, 14,  2, 0.875
)

team_mapping <- tribble(
  ~Team_Standard,  ~abbr,
  "Trail Blazers", "POR", "Timberwolves", "MIN", "Rockets", "HOU",
  "Suns", "PHX", "Thunder", "OKC", "Jazz", "UTA", "Warriors", "GSW",
  "Lakers", "LAL", "Clippers", "LAC", "76ers", "PHI", "Cavaliers", "CLE",
  "Grizzlies", "MEM", "Nets", "BKN", "Pistons", "DET", "Hornets", "CHA",
  "Wizards", "WAS", "Hawks", "ATL", "Magic", "ORL", "Kings", "SAC",
  "Nuggets", "DEN", "Pelicans", "NOP", "Celtics", "BOS", "Heat", "MIA",
  "Knicks", "NYK", "Raptors", "TOR", "Bucks", "MIL", "Mavericks", "DAL",
  "Spurs", "SAS", "Pacers", "IND", "Bulls", "CHI"
)

nba_team_colors_ <- c(
  "ATL"="#E03A3E", "BOS"="#008348", "BKN"="#666666",
  "CHA"="#00788C", "CHI"="#CD1F43", "CLE"="#860038",
  "DAL"="#0066B5", "DEN"="#FFC600", "DET"="#C8102E",
  "GSW"="#FDB900", "HOU"="white", "IND"="#FED51F",
  "LAC"="#12173F", "LAL"="#360072", "MEM"="#5D76A9",
  "MIA"="#961A31", "MIL"="#004C26", "MIN"="#266092",
  "NOP"="#B89A5B", "NYK"="#F58426", "OKC"="#007AC0",
  "ORL"="#0B77BD", "PHI"="#C4CED4", "PHX"="#FF6214",
  "POR"="#CF0A2C", "SAC"="#592D81", "SAS"="#E1E7EB",
  "TOR"="#FFFFFF", "UTA"="white", "WAS"="#FF1135"
)

# ==============================================================================
# 2. 核心：创建 plot_df 并生成圆图（精简版）
# ==============================================================================
img_dir <- "D:/nba/球队logo/教练"
circle_dir <- "D:/nba/临时照片/教练_圆形带边框"
if (!dir.exists(circle_dir)) dir.create(circle_dir, recursive = TRUE)

plot_df <- df %>%
  left_join(team_mapping, by = "Team_Standard") %>%
  mutate(
    team_color = coalesce(nba_team_colors_[abbr], "#888888"),
    raw_img    = file.path(img_dir, paste0(abbr, ".png")),
    circle_img = file.path(circle_dir, paste0(abbr, "_circle.png"))
  )

for (i in 1:nrow(plot_df)) {
  if (file.exists(plot_df$raw_img[i])) {
    circle_crop(images = plot_df$raw_img[i], to = plot_df$circle_img[i],
                border_size = 8, border_colour = plot_df$team_color[i])
  }
}

# ==============================================================================
# 3. 坐标轴计算与背景渐变计算
# ==============================================================================
x_mid <- mean(plot_df$Total)          
y_mid <- mean(plot_df$Success_Rate)    

xmin <- min(plot_df$Total) - 2
xmax <- max(plot_df$Total) + 2
ymin <- min(plot_df$Success_Rate) - 0.05
ymax <- max(plot_df$Success_Rate) + 0.05

bg_data <- expand.grid(
  x = seq(xmin, xmax, length.out = 200),
  y = seq(ymin, ymax, length.out = 200)
)

# 你选的马卡龙渐变色系
c_bl  <- col2rgb("#FCD2DC") # 樱花粉
c_tr  <- col2rgb("#DCFBDB") # 薄荷绿
c_mid <- col2rgb("floralwhite") # 奶油黄

bg_data <- bg_data %>%
  mutate(
    x_norm = rescale(x, from = c(xmin, xmax)),
    y_norm = rescale(y, from = c(ymin, ymax)),
    diag_score = (x_norm + y_norm) / 2,
    w_bl = ifelse(diag_score < 0.5, 1 - (diag_score * 2), 0),
    w_tr = ifelse(diag_score >= 0.5, (diag_score - 0.5) * 2, 0),
    w_mid = 1 - w_bl - w_tr,
    r = w_bl * c_bl[1] + w_mid * c_mid[1] + w_tr * c_tr[1],
    g = w_bl * c_bl[2] + w_mid * c_mid[2] + w_tr * c_tr[2],
    b = w_bl * c_bl[3] + w_mid * c_mid[3] + w_tr * c_tr[3],
    fill_color = rgb(r, g, b, maxColorValue = 255)
  )

p <- ggplot(plot_df, aes(x = Total, y = Success_Rate)) +
  
  # --- 背景 ---
  geom_tile(data = bg_data, aes(x = x, y = y, fill = fill_color), alpha = 0.6) +
  scale_fill_identity() +
  
  # --- 辅助线 ---
  geom_vline(xintercept = x_mid, linetype = "twodash", color = "gray25", linewidth = 0.5) +
  geom_hline(yintercept = y_mid, linetype = "twodash", color = "gray25", linewidth = 0.5) +
  
  # --- 四象限标签 ---
  # 【左上】
  annotate("richtext", x = xmin+5, y = ymax, hjust = 0, vjust = 1,
           label = "<b style='font-size:25pt; color:#455A64'>轻易不出手的高人</b><br><span style='font-size:16pt; color:#333'>尽管挑战次数少，但成功率极高</span>",
           fill = alpha("white", 0.9), label.color = "#78909C", 
           label.padding = unit(0.6, "lines"), label.r = unit(0.4, "lines")) +
  
  # 【右上】
  annotate("richtext", x = xmax-1, y = ymax, hjust = 1, vjust = 1,
           label = "<b style='font-size:28pt; color:#F57F17'>挑战滴神</b><br><span style='font-size:18pt; color:#333'>频繁挑战且总能成功</span>",
           fill = alpha("white", 0.9), label.color = "#FFD54F", 
           label.padding = unit(0.6, "lines"), label.r = unit(0.4, "lines")) +
  
  # 【左下】
  annotate("richtext", x = xmin+1, y = ymin, hjust = 0, vjust = 0,
           label = "<b style='font-size:28pt; color:#546E7A'>还需努力</b><br><span style='font-size:18pt; color:#333'>不爱挑战，且容易失败</span>",
           fill = alpha("white", 0.9), label.color = "#B0BEC5", 
           label.padding = unit(0.6, "lines"), label.r = unit(0.4, "lines")) +
  
  # 【右下】
  annotate("richtext", x = xmax, y = ymin, hjust = 1, vjust = 0,
           label = "<b style='font-size:25pt; color:#C62828'>浪费暂停</b><br><span style='font-size:16pt; color:#333'>胡乱挑战，效率低下</span>",
           fill = alpha("white", 0.9), label.color = "#E57373", 
           label.padding = unit(0.6, "lines"), label.r = unit(0.4, "lines")) +
  
  # --- 圆形球队教练图 (核心修改：加入 position_jitter) ---
  geom_image(aes(image = circle_img), 
             size = 0.08, by = "width", asp = 1.33,
             # width = 0.8 表示X轴左右最多偏移0.8次；height = 0.008 表示Y轴上下最多偏移0.8%
             # seed 保证每次跑出来的散布状态一致，不会乱跑
             position = position_jitter(width = 1.5, height = 0.012, seed = 2026)) + 
  
  # --- 坐标轴 ---
  scale_x_continuous(breaks = seq(10, 70, 5), expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  
  labs(
    title = "NBA 2025-26赛季教练挑战成功次数对比图",
    subtitle = "X轴: 挑战总次数，越靠右越多 | Y轴: 挑战成功率，越靠上成功率越高",
    x = "挑战总次数 (保守稳健 -------→ 侵略性十足)",
    y = "挑战成功率 (判断失误------- →一挑一个准 )",
    caption = "制图：<span style='color:black; font-size:30pt; font-weight:bold;'>俗人罢了</span> | 统计截至：全明星周末 | 数据源：hoopR"
  )+
  
  # 去掉 base_family = "sans"，让 showtext 自动接管中文字体
  theme_minimal() + 
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 32, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 20, color = "gray30", hjust = 0),
    axis.title = element_text(face = "bold", size = 16), 
    axis.text = element_text(size = 15),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#0000001A"),
    plot.caption = element_markdown(size = 24, color = "gray40", hjust = 0.5),
    plot.background = element_rect(fill = "floralwhite", colour = NA)
  )

# ==============================================================================
# 5. 保存
# ==============================================================================
ggsave("Coach_Challenge_Circles.png", p, width = 16, height = 12, dpi = 300, bg = "floralwhite")