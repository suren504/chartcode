# 加载所有必需包（缺一不可）
library(ggplot2)
library(dplyr)
library(ggtext)  # 支持richtext标注
library(ggimage)  # 支持geom_image显示头像
library(stringr)  # 用于str_remove处理百分比

# 仅保留目标三列的数据
df <- tribble(
  ~球员名字,               ~VersatilityRating, ~MatchupDifficulty,
  "Jrue Holiday",          99.2,               57.9,
  "Jarred Vanderbilt",     97.7,               35.6,
  "Alex Caruso",           97.3,               71.1,
  "Tari Eason",            96.6,               45.7,
  "Dean Wade",             96.2,               56.2,
  "Alperen Sengun",        92.8,               39.1,
  "Moses Moody",           91.6,               44.6,
  "Jaden McDaniels",       90.1,               84.1,
  "Nicolas Batum",         89.7,               38.3,
  "Jordan Goodwin",        88.6,               51.0,
  "Toumani Camara",        87.8,               91.0,
  "Ausar Thompson",        85.2,               71.6,
  "Lonzo Ball",            84.4,               34.2,
  "Josh Okogie",           83.3,               67.5,
  "Brandin Podziemski",    82.5,               30.0,
  "Amen Thompson",         81.4,               76.9,
  "Herbert Jones",         81.0,               95.1,
  "Luka Dončić",           80.2,               17.3,
  "Cason Wallace",         77.2,               69.7,
  "Javonte Green",         76.8,               33.7,
  "Luguentz Dort",         76.0,               77.6,
  "Dyson Daniels",         74.9,               96.7,
  "Mouhamed Gueye",         70.0,               13.8,
  "Shai Gilgeous-Alexander",68.1,              23.8,  
  "Derrick White",         67.3,               36.5,
  "Jonathan Isaac",        67.0,               46.0,
  "Marcus Smart",          62.4,               65.3,
  "Kawhi Leonard",         60.8,               47.9,
  "Chet Holmgren",         60.1,               14.8,
  "Keon Ellis",            55.1,               45.2,
  "John Konchar",          53.0,               42.0,
  "Evan Mobley",           49.0,               35.7,
  "Anthony Black",         35.4,               82.1,
  "Tobias Harris",         33.5,               60.1,
  "Jalen Suggs",           33.1,               81.7,
  "Dru Smith",             32.7,               47.5,
  "Gary Payton II",        32.0,               84.0,
  "Kris Dunn",             31.2,               72.9,
  "Kentavious Caldwell-Pope",30.0,            59.1,
  "Derrick Jones Jr.",     28.5,               85.9,
  "Dwight Powell",         28.0,               32.0,
  "Jalen Johnson",         25.1,               22.4,
  "Jaren Jackson Jr.",     23.6,               32.4,
  "Bam Adebayo",           22.8,               49.8,
  "Giannis Antetokounmpo", 22.4,               43.7,
  "Steven Adams",          21.7,               32.4,
  "OG Anunoby",            21.3,               61.1,
  "Gary Harris",           20.0,               50.0,
  "Draymond Green",        18.6,               57.8,
  "Dereck Lively II",      17.0,               55.0,
  "Oso Ighodaro",          15.2,               31.6,
  "Joel Embiid",           15.0,               33.0,
  "Adem Bona",             15.0,               24.0,
  "Jay Huff",              14.4,               6.3,
  "Jaylin Williams",       14.1,               1.2,
  "Robert Williams III",   14.0,               41.0,
  "Kristaps Porziņģis",    13.3,               12.7,
  "Victor Wembanyama",     12.5,               23.0,
  "Kevon Looney",          12.0,               42.0,
  "Myles Turner",          11.8,               26.0,
  "Isaiah Stewart",        11.4,               23.9,
  "Anthony Davis",         11.0,               51.0,
  "Nikola Jokić",          10.6,               14.3,
  "Jaxson Hayes",          10.3,               14.8,
  "Walker Kessler",        10.0,               61.0,
  "Day'Ron Sharpe",        9.9,                13.3,
  "Luke Kornet",           9.1,                31.8,
  "Jarrett Allen",         8.4,                19.9,
  "Jakob Poeltl",          8.0,                23.2,
  "Paul Reed",             8.0,                32.0,
  "Al Horford",            7.2,                31.8,
  "Brook Lopez",           7.0,                45.0,
  "Donovan Clingan",       6.5,                26.2,
  "Wendell Carter Jr.",    5.3,                35.1,
  "Isaiah Hartenstein",    4.9,                14.2,
  "Rudy Gobert",           4.6,                23.3,
  "Zach Edey",             4.0,                36.0,
  "Neemias Queta",         3.8,                15.1,
  "Daniel Gafford",        3.4,                33.1,
  "Ivica Zubac",           2.7,                21.9,
  "Goga Bitadze",          2.3,                22.1,
  "Clint Capela",          1.0,                46.0,
  "Jusuf Nurkić",          0.4,                16.2
) %>%
  mutate(
    # 构造头像路径（英文名空格转下划线，清理后缀）
    headshot = 球员名字 %>%
      str_replace_all(" ", "_") %>%
      str_remove(" Jr\\.| II$| III$") %>%  # 只清理末尾的Jr./II/III，保留名字里的点号（如Oso Ighodaro无影响）
      paste0("D:/nba/球员头像/", ., ".png"),
    # 找不到本地头像时，用NBA官方占位图（无需本地文件）
    headshot = ifelse(file.exists(headshot), 
                      headshot, 
                      "https://cdn.nba.com/headshots/nba/latest/260x190/placeholder.png"),
    # 关键修复：添加逗号分隔字段！
    x = MatchupDifficulty,      # X轴：对位难度（越大越难）
    y = VersatilityRating       # Y轴：全能评分（越大越万金油）
  )

# ==================== 正式绘图 ====================
p <- ggplot(df, aes(x = x, y = y)) +
  
  # 1. 四象限背景色（右上最牛，左下最菜）
  annotate("rect", xmin = 50, xmax = 100, ymin = 50, ymax = 100, fill = "#a8e6cf", alpha = 0.6) +   # 右上：顶级防守核
  annotate("rect", xmin = 0,  xmax = 50,  ymin = 50, ymax = 100, fill = "#f0f0f0", alpha = 0.7) +   # 左上
  annotate("rect", xmin = 50, xmax = 100, ymin = 0,  ymax = 50,  fill = "#f0f0f0", alpha = 0.7) +   # 右下
  annotate("rect", xmin = 0,  xmax = 50,  ymin = 0,  ymax = 50,  fill = "#d1c4e9", alpha = 0.6) +   # 左下：划水区
  
  # 所有球员头像统一设置：大小0.08 + 透明度75%（alpha=0.75）
  # 无需区分区域，一行代码搞定所有头像
  geom_image(
    aes(image = headshot), 
    size = 0.08,    # 统一大小为0.08
    alpha = 0.75    # 透明度75%（0=完全透明，1=不透明）
  ) +
  
  # 4. 四象限文字（加粗重点区域）
  annotate("richtext", x = 90, y = 100, label = "**防对手最强<br>什么位置都防**", 
           size = 9, color = "#1b5e20", fill = "#e8f5e9", label.colour = NA, family = "sans") +
  annotate("richtext", x = 10, y = 90, label = "**防守难度不大<br>什么位置都防**", 
           size = 7.5, color = "gray40", fill = "#f5f5f5", label.colour = NA) +
  annotate("richtext", x = 90, y = 5, label = "**防的位置单一<br>防守难度很大**", 
           size = 7.5, color = "gray40", fill = "#f5f5f5", label.colour = NA) +
  annotate("richtext", x = 10, y = 35, label = "**防守位置单一<br>防守难度不大**", 
           size = 9, color = "#4a148c", fill = "#ede7f6", label.colour = NA, family = "sans") +
  
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  # 6. 标题副标题（你自己再改）
  labs(
    title = "2025-26赛季 NBA防守悍将们的防守分析对比图",
    subtitle = "X轴=对位难度（越大越难） | Y轴=对位多样性（越大越能防多种类型） | 仅统计CraftedDPM优于联盟80%的球员",
    x = "对位难度 ← 轻松                越难 →",
    y = "对位多样性 ← 专精                    万金油 →",
    caption = "数据来源：craftednba.com | 制作：俗人罢了|制作日期：12.1"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "gray50", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, color = "gray60")
  )

# 出图 + 保存
print(p)
ggsave("NBA防守四象限_纯头像版.png", p, width = 14, height = 11, dpi = 320, bg = "white")
shell.exec("NBA防守四象限_纯头像版.png")