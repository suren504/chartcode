library(tidyverse)
library(ggimage)
library(shadowtext)
library(grid)

# =====================
# 1. 原始数据
# =====================
df <- tribble(
  ~team, ~players, ~net_rating,
  "BOS", "Payton Pritchard, Jordan Walsh, Derrick White, Jaylen Brown, Neemias Queta", 22.4,
  "NYK", "Miles McBride, Jalen Brunson, Josh Hart, Mikal Bridges, Karl-Anthony Towns", 21.2,
  "OKC", "Isaiah Hartenstein, Luguentz Dort, Cason Wallace, Shai Gilgeous-Alexander, Chet Holmgren", 19.6,
  "DEN", "Cameron Johnson, Jamal Murray, Peyton Watson, Nikola Jokić, Spencer Jones", 17.9,
  "DEN", "Cameron Johnson, Aaron Gordon, Jamal Murray, Christian Braun, Nikola Jokić", 13.1,
  "ORL", "Paolo Banchero, Franz Wagner, Jalen Suggs, Wendell Carter Jr., Desmond Bane", 17.0,
  "MEM", "Vince Williams Jr., Jaren Jackson Jr., Jaylen Wells, Zach Edey, Cedric Coward", 15.6,
  "SAS", "Victor Wembanyama, Julian Champagnie, Devin Vassell, Harrison Barnes, Stephon Castle", 12.2,
  "MIN", "Julius Randle, Donte DiVincenzo, Rudy Gobert, Jaden McDaniels, Anthony Edwards", 11.0,
  "DET", "Duncan Robinson, Tobias Harris, Jalen Duren, Ausar Thompson, Cade Cunningham", 10.2,
  "ATL", "Jalen Johnson, Kristaps Porziņģis, Nickeil Alexander-Walker, Dyson Daniels, Zaccharie Risacher", 9.9,
  "HOU", "Jabari Smith Jr, Alperen Sengun, Amen Thompson, Josh Okogie, Kevin Durant", 9.2
)

# =====================
# 2. 拆成5个球员列
# =====================
df <- df %>%
  separate(players,
           into = paste0("p", 1:5),
           sep = ", ",
           remove = FALSE)

# =====================
# 3. 头像路径自动生成（本地路径）
# =====================
make_headshot <- function(name) {
  clean_name <- name %>%
    # 步骤1：去掉所有点（包括Jr.中的.）
    str_replace_all("\\.", "") %>%
    # 步骤2：移除名字中的音标（转换为ASCII字符，自动去掉重音/音标）
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    # 步骤3：处理iconv转换后可能出现的多余撇号（比如Š→S'、ć→c'的情况）
    str_replace_all("'", "") %>%
    # 步骤4：空格替换为下划线
    str_replace_all(" ", "_")
  
  # 拼接头像路径（保留你原有的路径逻辑，这里可根据需要调整）
  path <- paste0("D:/nba/球员头像/", clean_name, ".png")
  
  # 可选：判断文件是否存在，存在返回路径，否则返回NA（你原代码没写，可保留或删除）
  ifelse(file.exists(path), path, NA)
}


df <- df %>%
  mutate(across(starts_with("p"), make_headshot, .names = "img_{col}")) %>%
  mutate(team_logo = paste0("D:/nba/球队logo/球队logo圆/", team, ".png")) %>%
  arrange(desc(net_rating)) %>%
  mutate(y = row_number(),
         y = max(y) - y + 1)

top_y <- max(df$y) + 1

# =====================
# 4. 绘图
# =====================
p <- ggplot(df, aes(y = y)) +
  
  # 金色热度条
  geom_rect(
    aes(xmin = 8.2, xmax = 9.0,
        ymin = y - 0.35, ymax = y + 0.35,
        fill = net_rating),
    colour = NA
  ) +
  # 橙色到浅蓝色的渐变（可直接替换原有代码行）
  scale_fill_gradient(high = "#ff6600", low = "#add8e6") +
  guides(fill = "none") +
  
  # 顶部标题
  annotate("text", x = 1, y = top_y, label = "球队", size = 6, fontface = "bold") +
  annotate("text", x = 5, y = top_y, label = "五人组", size = 6, fontface = "bold") +
  annotate("text", x = 8.6, y = top_y, label = "净胜分", size = 6, fontface = "bold") +
  
  # 球队 logo
  geom_image(aes(x = 1, image = team_logo), size = 0.08, by = "width") +
  
  # 五个球员头像（横向排）
  geom_image(aes(x = 3, image = img_p1), size = 0.11) +
  geom_image(aes(x = 4, image = img_p2), size = 0.11) +
  geom_image(aes(x = 5, image = img_p3), size = 0.11) +
  geom_image(aes(x = 6, image = img_p4), size = 0.11) +
  geom_image(aes(x = 7, image = img_p5), size = 0.11) +
  
  # 净胜分
  geom_shadowtext(
    aes(x = 8.6, label = net_rating),
    bg.colour = "black", colour = "white",
    size = 6.5, fontface = "bold"
  ) +
  
  # 坐标和主题
  scale_x_continuous(limits = c(0.5, 9.5)) +
  scale_y_continuous(breaks = NULL, expand = c(0.02, 0.02)) +
  coord_cartesian(ylim = c(min(df$y)-0.5, top_y + 0.5)) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, colour = "gray40"),
    # 关键修改：添加hjust = 0.5让caption居中，同时保留原有样式
    plot.caption = element_text(
      size = 12, 
      colour = "black", 
      face = "bold",
      hjust = 0.5,  # 水平居中的核心参数（0左对齐，1右对齐，0.5居中）
      vjust = 1     # 可选：垂直对齐，可根据需要调整（0底部，1顶部）
    )
  ) +
  labs(
    title = "NBA2025–26 赛季最强球队五人组",
    subtitle = "统计出场时间至少200个回合的球队5人组，以CTG的Efficiency Differential排名",
    caption = "制作：俗人罢了|数据源：cleaningtheglass|统计时间：2025.12.11"
  )

# 保存
ggsave("NBA_Top5_Lineups.png", p, width = 11, height = 12, dpi = 300, bg = "floralwhite")
browseURL("NBA_Top5_Lineups.png")
