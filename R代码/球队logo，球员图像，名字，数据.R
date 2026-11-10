library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
# ——浅色球队色表（你已经有了）——
nba_team_colors_light <- c(
  "亚特兰大老鹰" = "#F4A6A6", "波士顿凯尔特人" = "#66B266", "布鲁克林篮网" = "#666666",
  "夏洛特黄蜂" = "#7A6FA3", "芝加哥公牛" = "#F27A8E", "克利夫兰骑士" = "#C5667A",
  "达拉斯独行侠" = "#66A3D1", "丹佛掘金" = "#5D7AAB", "底特律活塞" = "#F27A7A",
  "金州勇士" = "#7893C9", "休斯顿火箭" = "#F27A8E", "印第安纳步行者" = "#668BB2",
  "洛杉矶快船" = "#F27A7A", "洛杉矶湖人" = "#9D7FCA", "孟菲斯灰熊" = "#9DB4D6",
  "迈阿密热火" = "#D1667A", "密尔沃基雄鹿" = "#4D8C4D", "明尼苏达森林狼" = "#5D7AAB",
  "新奥尔良鹈鹕" = "#5D7AAB", "纽约尼克斯" = "#66A3D1", "俄克拉荷马城雷霆" = "#66B8E6",
  "奥兰多魔术" = "#66B2E6", "费城76人" = "#66A3D1", "菲尼克斯太阳" = "#7A6FA3",
  "波特兰开拓者" = "#F4A6A6", "萨克拉门托国王" = "#9D7FCA", "圣安东尼奥马刺" = "#E1E7EB",
  "多伦多猛龙" = "#F27A8E", "犹他爵士" = "#668BB2", "华盛顿奇才" = "#668BB2"
)
# ——数据（你原来的）——
df <- tibble(
  name = c("邓罗","东契奇","A·西蒙斯","迪文琴佐","冯泰基奥",
           "库里","谢泼德","内史密斯","CJ·麦科勒姆","格雷森·阿伦",
           "奥尼尔","萨迪克·贝","三球","米切尔","普理查德"),
  team = c("DET","LAL","BOS","MIN","MIA",
           "GSW","HOU","IND","WAS","PHX",
           "PHX","NOP","CHA","CLE","BOS"),
  xfg = c(36.6,38.0,39.1,39.4,39.4,39.6,39.9,39.9,40.2,40.2,
          40.6,40.7,40.7,40.8,40.9),
  player_id = c(1629130,1629029,1629014,1628978,1631323,
                201939,1642263,1630174,203468,1628960,
                1626220,1630180,1630163,1628378,1630202)
) %>%
  mutate(
    headshot = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", player_id, ".png"),
    team_logo = paste0("D:/nba/球队logo/球队logo圆/", team, ".png"),
    
    # 关键：先建一个中文名 → 三字缩写的映射表
    team_cn = case_when(
      team == "DET" ~ "底特律活塞",
      team == "LAL" ~ "洛杉矶湖人",
      team == "BOS" ~ "波士顿凯尔特人",
      team == "MIN" ~ "明尼苏达森林狼",
      team == "MIA" ~ "迈阿密热火",
      team == "GSW" ~ "金州勇士",
      team == "HOU" ~ "休斯顿火箭",
      team == "IND" ~ "印第安纳步行者",
      team == "WAS" ~ "华盛顿奇才",
      team == "PHX" ~ "菲尼克斯太阳",
      team == "NOP" ~ "新奥尔良鹈鹕",
      team == "CHA" ~ "夏洛特黄蜂",
      team == "CLE" ~ "克利夫兰骑士",
      TRUE ~ team
    ),
    
    # 现在就可以安全取浅色了
    bar_color = nba_team_colors_light[team_cn]
  ) %>%
  arrange(xfg) %>%
  mutate(
    name = factor(name, levels = name),
    row = row_number(),
    ymin = row - 0.45,
    ymax = row + 0.45
  )
team_colors <- c(
  "DET"="#C8102E","LAL"="#552583","BOS"="#007A33","MIN"="#0C2340","MIA"="#98002E",
  "GSW"="#006BB6","HOU"="#CE1141","IND"="#002D62","WAS"="#002B5C","PHX"="#1D1160",
  "NOP"="#0C2340","CHA"="#1D1160","CLE"="#6F263D"
)
df2 <- df %>% mutate(color = team_colors[team])
# （你的颜色表和 df2 数据部分完全不动）

p <- ggplot(df2) +
  # 边框 + 浅色条
  geom_rect(aes(xmin = 0.2, xmax = 7.8, ymin = ymin, ymax = ymax, color = color),
            fill = NA, linewidth = 1.5) +
  geom_rect(aes(xmin = 5.3, xmax = 7.2, ymin = row - 0.3, ymax = row + 0.3, fill = bar_color)) +
  scale_fill_identity() +
  
  # logo + 头像
  geom_image(aes(x = 0.62, y = row, image = team_logo), size = 0.062) +
  geom_image(aes(x = 1.56, y = row - 0.15, image = headshot), size = 0.088) +
  
  # 球员名字
  geom_text(aes(x = 3.5, y = row, label = name),
            size = 7.8, fontface = "bold", family = "Microsoft YaHei", color = "#222222") +
  
  # 数字
  geom_shadowtext(aes(x = 6.25, y = row, label = paste0(xfg, "%")),
                  size = 7.5, fontface = "bold", colour = "white", bg.colour = "black") +
  
  scale_color_identity() +
  coord_cartesian(clip = "off", xlim = c(0,8)) +
  scale_y_reverse() +
  
  annotate("text", x = 1.09, y = max(df2$row) -15, label = "球队",      size = 8, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  annotate("text", x = 3.5,  y = max(df2$row) -15, label = "球员",      size = 8, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  annotate("text", x = 6.25, y = max(df2$row) - 15, label = "预期命中率", size = 8, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  # 主题 + 标题 + caption紧贴底部
  theme_void(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    # 上边距留给标题 + 表头，下边距几乎为0，让caption紧贴底
    plot.margin = margin(22, 30, 2, 30),   # 关键！下边距只留2pt
    
    plot.title = element_markdown(
      hjust = 0.5, size = 36, face = "bold", family = "Microsoft YaHei",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_markdown(
      hjust = 0.5, size = 21, color = "gray35",
      margin = margin(b = 16)
    ),
    # caption紧贴底部！几乎0间距
    plot.caption = element_markdown(
      hjust = 0.5, size = 13.5, face = "bold", color = "gray28",
      margin = margin(t = 6, b = 2)   # 上留一点呼吸，下几乎贴死
    )
  ) +
  
  labs(
    title = "<span style='font-size:36pt; font-weight:900;'>2025-26赛季 出手难度最大15人</span>",
    subtitle = "<span style='font-size:23pt; color:#333;'>凯尔特人、太阳各两人霸榜 · 预期命中率最低</span>",
    caption = "**数据：NBA.com Shot Difficulty 制作：俗人罢了**"
  )

# 保存（高度稍微压一点，更紧凑）
ggsave("NBA_出手难度TOP15_终极封神版.png", p,
       width = 9, height = 13.6, dpi = 340, bg = "floralwhite")

browseURL("NBA_出手难度TOP15_终极封神版.png")
message("已封神！表头大粗黑，caption紧贴底，F5看了都沉默！")