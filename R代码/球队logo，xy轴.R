library(tidyverse)
library(ggimage)
library(ggtext)

# ===============================
# 1. 数据
# ===============================
df <- tribble(
  ~team, ~vs_strong, ~vs_weak,
  "NYK", 17.6, 17.7,
  "DEN", 14.8, 15.9,
  "DET", 6.2, 11.1,
  "SAS", 5.3, 13.4,
  "HOU", 4.8, 19.1,
  "OKC", 1.9, 15.6,
  "ORL", 1.6, 17.7,
  "PHI", -1.5, 9.2,
  "CHI", -1.6, 4.5,
  "CLE", -1.4, 14.6,
  "LAL", -1.8, 10.6,
  "MIA", -2.3, 19.2,
  "PHX", -4.3, 9.0,
  "MIL", -4.5, 3.0,
  "GSW", -4.5, 11.5,
  "LAC", -6.5, -5.3,
  "POR", -8.0, -1.1,
  "DAL", -10.6, -3.3,
  "ATL", -10.7, 14.6,
  "CHA", -11.5, 10.8,
  "UTA", -12.3, 5.4,
  "MEM", -13.7, 14.3,
  "SAC", -14.7, -0.1,
  "IND", -14.8, -9.2,
  "BOS", -15.4, 19.3,
  "MIN", -15.5, 18.4,
  "WAS", -16.5, -11.7,
  "TOR", -16.9, 6.3,
  "NOP", -19.7, -0.2,
  "BKN", -20.9, 4.3
)

# logo 路径
df <- df %>%
  mutate(
    logo = paste0("D:/nba/新建文件夹/", team, ".png"),
    x = vs_strong,
    y = vs_weak
  )

# ===============================
# 2. 坐标轴范围
# ===============================
axis_lim <- max(abs(c(df$x, df$y))) * 1.05
xmin <- -axis_lim; xmax <- axis_lim
ymin <- -axis_lim; ymax <- axis_lim

# 你指定的四角定位
x_left  <- -20
x_right <- 20
y_top   <- 12.5
y_bottom <- -20

# ===============================
# 3. 绘图
# ===============================
p <- ggplot(df, aes(x = x, y = y)) +
  
  # =============== 四象限背景 ===============
annotate("rect", xmin = 0,   xmax = xmax, ymin = 0,   ymax = ymax,
         fill = "#c2a5cf", alpha = .55) +     # 右上：强+弱都打得好
  annotate("rect", xmin = xmin, xmax = 0,   ymin = ymin, ymax = 0,
           fill = "#a6dba0", alpha = .55) +     # 左下：都打不好
  annotate("rect", xmin = xmin, xmax = 0,   ymin = 0,   ymax = ymax,
           fill = "#f7f7f7", alpha = .70) +     # 左上：遇弱强但怕强
  annotate("rect", xmin = 0,   xmax = xmax, ymin = ymin, ymax = 0,
           fill = "#f7f7f7", alpha = .70) +     # 右下：赢强队但输弱队
  
  # =============== 四象限文字 ===============
annotate(
  "richtext",
  x = x_left, y = y_bottom,
  label = "**同时打不过<br>强队和弱队**",
  fill = "#e8f5e9", label.color = NA,
  color = "#1b5e20", size = 7, hjust = 0, vjust = 0
) +
  
  annotate(
    "richtext",
    x = x_right, y = y_top,
    label = "**打强队打弱队<br>都厉害**",
    fill = "#f3e5f5", label.color = NA,
    color = "#6a1b9a", size = 7, hjust = 1, vjust = 1
  ) +
  
  annotate(
    "richtext",
    x = x_left, y = y_top,
    label = "遇强队唯唯诺诺<br>打弱队重拳出击",
    fill = NA, label.color = NA,
    color = "gray30", size = 7, hjust = 0, vjust = 1
  ) +
  
  annotate(
    "richtext",
    x = x_right, y = y_bottom,
    label = "专赢强队<br>却输给弱队",
    fill = NA, label.color = NA,
    color = "gray30", size = 7, hjust = 1, vjust = 0
  )+
  
  # =============== 球队 logo ===============
geom_image(aes(image = logo), size = 0.09, asp = 1.05) +
  
  # =============== 坐标设置 ===============
scale_x_continuous(limits = c(xmin, xmax)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  
  labs(
    title = "NBA本赛季各球队面对联盟强队 | 弱队的净效率",
    subtitle = "联盟强队（TOP10）：雷霆 火箭 掘金 森林狼 活塞 马刺 太阳 纽约 凯尔特人 猛龙<br>
联盟弱队（BOTTOM10）：奇才 篮网 步行者 国王 鹈鹕 灰熊 独行侠 爵士 快船 黄蜂",
    x = "←面对强队输分          面对强队赢分→",
    y = "←面对弱队输分          面对弱队队赢分→",
    caption = "stats via PivotFade｜截止 11.20|制作：俗人罢了"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    plot.subtitle = ggtext::element_markdown(
      size = 16, color = "gray40", hjust = 0.5, lineheight = 1.15
    ),
    plot.caption = element_text(hjust = 0.5, size = 12, face = "bold", color = "gray40"),
    axis.title = element_text(size = 14, face = "bold")
  )

print(p)

ggsave("NBA强弱队净效率.png",
       plot = p,
       width = 14, height = 12, dpi = 320, bg = "white")

