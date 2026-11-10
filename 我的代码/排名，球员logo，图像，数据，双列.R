library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
library(ragg)

# =======================================================================
# ⭐⭐⭐ 坐标参数调整 (加宽版)
# =======================================================================

# ----- 两列的 x 位置（已加宽） -----
col_x_left  <- -1.2
col_x_right <- 13 # 维持增大后的间隔

# ----- 底部标题的三个栏位相对位置 (已向右移动以适应更宽的列) -----
col_rank    <- 0.6
col_player  <- 6.0  # 从 4.6 移到 6.0，让姓名居中
col_value   <- 10.0 # 从 7.8 移到 10.0，让数值居中

# 球队颜色表（不变）
nba_team_colors <- c(
  "亚特兰大老鹰"="#E03A3E","波士顿凯尔特人"="#007A33","布鲁克林篮网"="#000000",
  "夏洛特黄蜂"="#1D1160","芝加哥公牛"="#CE1141","克利夫兰骑士"="#860038",
  "达拉斯独行侠"="#00538C","丹佛掘金"="#0E2240","底特律活塞"="#C8102E",
  "金州勇士"="#1D428A","休斯顿火箭"="#CE1141","印第安纳步行者"="#002D62",
  "洛杉矶快船"="#006BB6","洛杉矶湖人"="#552583","孟菲斯灰熊"="#5D76A9",
  "迈阿密热火"="#98002E","密尔沃基雄鹿"="#00471B","明尼苏达森林狼"="#0C2340",
  "新奥尔良鹈鹕"="#002B5C","纽约尼克斯"="#006BB6","俄克拉荷马城雷霆"="#007AC1",
  "奥兰多魔术"="#0077C0","费城76人"="#006BB6","菲尼克斯太阳"="#E56020",
  "波特兰开拓者"="#E03A3E","萨克拉门托国王"="#5A2D81","圣安东尼奥马刺"="#C4CED4",
  "多伦多猛龙"="#CE1141","犹他爵士"="#002B5C","华盛顿奇才"="#E31837"
)
nba_team_colors_light <- c(
  "亚特兰大老鹰"="#F4A6A6","波士顿凯尔特人"="#66B266","布鲁克林篮网"="#666666",
  "夏洛特黄蜂"="#7A6FA3","芝加哥公牛"="#F27A8E","克利夫兰骑士"="#C5667A",
  "达拉斯独行侠"="#66A3D1","丹佛掘金"="#5D7AAB","底特律活塞"="#F27A7A",
  "金州勇士"="#7893C9","休斯顿火箭"="#F27A8E","印第安纳步行者"="#668BB2",
  "洛杉矶快船"="#66A3D1","洛杉矶湖人"="#9D7FCA","孟菲斯灰熊"="#9DB4D6",
  "迈阿密热火"="#D1667A","密尔沃基雄鹿"="#4D8C4D","明尼苏达森林狼"="#5D7AAB",
  "新奥尔良鹈鹕"="#668BB2","纽约尼克斯"="#66A3D1","俄克拉荷马城雷霆"="#66B8E6",
  "奥兰多魔术"="#66B2E6","费城76人"="#66A3D1","菲尼克斯太阳"="#FFB380",
  "波特兰开拓者"="#F4A6A6","萨克拉门托国王"="#9D7FCA","圣安东尼奥马刺"="#E1E7EB",
  "多伦多猛龙"="#F27A8E","犹他爵士"="#668BB2","华盛顿奇才"="#F4A6A6"
)

df <- tribble(
  ~排名, ~姓名_en,           ~球队,  ~CraftedPM,
  "#1",  "Cody Williams",    "UTA",  -4.2,
  "#2",  "Justin Edwards",   "PHI",  -3.7,
  "#3",  "Jordan Hawkins",   "NOP",  -3.7,
  "#4",  "Moussa Cisse",     "DAL",  -3.58,
  "#5",  "Spencer Jones",    "DEN",  -3.5,
  "#6",  "Jett Howard",      "ORL",  -3.5,
  "#7",  "Isaiah Collier",   "UTA",  -3.3,
  "#8",  "Maxime Raynaud",   "SAC",  -3.18,
  "#9",  "Bilal Coulibaly",  "WAS",  -3.1,
  "#10", "Amir Coffey",      "MIL",  -3.1,
  "#11", "Rob Dillingham",   "MIN",  -3.1,
  "#12", "Yves Missi",       "NOP",  -3.0,
  "#13", "Terrence Shannon Jr", "MIN",  -3.0,
  "#14", "Bronny James",     "LAL",  -3.0,
  "#15", "KJ Simpson",       "CHA",  -3.0,
  "#16", "Caleb Love",       "POR",  -2.94,
  "#17", "Keyonte George",   "UTA",  -2.9,
  "#18", "Kyle Kuzma",       "MIL",  -2.8,
  "#19", "Tyrese Proctor",   "CLE",  -2.76,
  "#20", "Kobe Sanders",     "LAC",  -2.71
) %>%
  mutate(
    姓名 = case_when(
      姓名_en == "Cody Williams"       ~ "科迪·威廉姆斯",
      姓名_en == "Justin Edwards"      ~ "贾斯汀·爱德华兹",
      姓名_en == "Jordan Hawkins"      ~ "霍金斯",
      姓名_en == "Moussa Cisse"        ~ "穆萨·西塞",
      姓名_en == "Spencer Jones"       ~ "斯宾塞·琼斯",
      姓名_en == "Jett Howard"         ~ "杰特·霍华德",
      姓名_en == "Isaiah Collier"      ~ "科利尔",
      姓名_en == "Maxime Raynaud"      ~ "马克西姆·雷诺",
      姓名_en == "Bilal Coulibaly"      ~ "库利巴利",
      姓名_en == "Amir Coffey"         ~ "科菲",
      姓名_en == "Rob Dillingham"      ~ "迪林厄姆",
      姓名_en == "Yves Missi"          ~ "米西",
      姓名_en == "Terrence Shannon Jr"  ~ "特伦斯·香农",
      姓名_en == "Bronny James"        ~ "布朗尼·詹姆斯",
      姓名_en == "KJ Simpson"          ~ "KJ·辛普森",
      姓名_en == "Caleb Love"          ~ "凯莱布·乐夫",
      姓名_en == "Keyonte George"      ~ "基昂特·乔治",
      姓名_en == "Kyle Kuzma"          ~ "库兹马",
      姓名_en == "Tyrese Proctor"      ~ "普罗克特",
      姓名_en == "Kobe Sanders"        ~ "科比·桑德斯",
      TRUE ~ 姓名_en
    ),
    
    team_cn = case_when(
      球队 == "UTA" ~ "犹他爵士",  球队 == "PHI" ~ "费城76人",
      球队 == "NOP" ~ "新奥尔良鹈鹕",球队 == "DAL" ~ "达拉斯独行侠",
      球队 == "DEN" ~ "丹佛掘金",  球队 == "ORL" ~ "奥兰多魔术",
      球队 == "SAC" ~ "萨克拉门托国王",球队 == "WAS" ~ "华盛顿奇才",
      球队 == "MIL" ~ "密尔沃基雄鹿",球队 == "MIN" ~ "明尼苏达森林狼",
      球队 == "LAL" ~ "洛杉矶湖人",球队 == "CHA" ~ "夏洛特黄蜂",
      球队 == "POR" ~ "波特兰开拓者",球队 == "CLE" ~ "克利夫兰骑士",
      球队 == "LAC" ~ "洛杉矶快船", TRUE ~ "无球队"
    ),
    
    rank_num = as.integer(str_remove(排名, "#")),
    panel = ifelse(rank_num <= 10, "Left", "Right"),
    relative_row = ifelse(panel == "Left", rank_num, rank_num - 10),
    row_y = relative_row * 1.25,
    ymin = row_y - 0.45,
    ymax = row_y + 0.45,
    col_x = ifelse(panel == "Left", col_x_left, col_x_right), 
    
    headshot = 姓名_en %>% 
      str_replace_all(" ", "_") %>% 
      paste0("D:/nba/球员头像/", ., ".png") %>% 
      ifelse(file.exists(.), ., NA),
    
    team_logo = paste0("D:/nba/球队logo/球队logo方/", 球队, ".png") %>% 
      ifelse(file.exists(.), ., NA),
    
    color = nba_team_colors[team_cn],
    bar_color = nba_team_colors_light[team_cn]
  )

p <- ggplot(df) +
  
  # 外框 (宽度已增加，从 8.8 变为 11.0)
  geom_rect(aes(
    xmin = col_x + 0.2, xmax = col_x + 11.0, # ⭐ 增加 xmax
    ymin = ymin, ymax = ymax, color = color
  ),
  fill = NA, linewidth = 1.6) +
  
  # CraftedPM 背景色 (宽度已增加，整体右移)
  geom_rect(aes(
    xmin = col_x + 9.0, xmax = col_x + 11.0, # ⭐ 整体右移
    ymin = row_y - 0.53, ymax = row_y + 0.53,
    fill = bar_color
  ),
  color = NA) +
  
  geom_rect(aes(
    xmin = col_x + 9.0, xmax = col_x + 11.0, # ⭐ 整体右移
    ymin = row_y - 0.53, ymax = row_y + 0.53,
    color = color
  ),
  fill = NA, linewidth = 1.4) +
  
  # 排名 (位置不变)
  geom_text(aes(x = col_x + 0.7, y = row_y, label = 排名),
            size = 8, fontface = "bold", family = "Microsoft YaHei") +
  
  # logo (位置不变)
  geom_image(aes(x = col_x + 1.8, y = row_y, image = team_logo),
             size = 0.06) +
  
  # 头像 (位置不变)
  geom_image(aes(x = col_x + 3.2, y = row_y - 0.08, image = headshot),
             size = 0.105) +
  
  # 姓名 (已右移到 6.0，留出更多空间)
  geom_text(aes(x = col_x + col_player, y = row_y, label = 姓名), # 使用 col_player=6.0
            size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  
  # CraftedPM 数字 (已右移到 10.0，与背景条对齐)
  geom_shadowtext(aes(x = col_x + col_value, y = row_y, label = CraftedPM), # 使用 col_value=10.0
                  size = 8.8, fontface = "bold",
                  colour = "white", bg.colour = "black") +
  
  scale_fill_identity() +
  scale_color_identity() +
  
  # ======================================================
# ⭐⭐⭐ 底部标题 —— 自动对齐 (已使用新的 col_player 和 col_value)
# ======================================================
annotate("text", x = col_x_left + col_rank,    y = -0.4, label = "排名",
         size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_player, y = -0.4, label = "球员", # 6.0
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_value,  y = -0.4, label = "CraftedPM", # 10.0
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  
  annotate("text", x = col_x_right + col_rank,    y = -0.4, label = "排名",
           size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_player, y = -0.4, label = "球员", # 6.0
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_value,  y = -0.4, label = "CraftedPM", # 10.0
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  
  # ======================================================
# ⭐⭐⭐ 清除浅蓝块 + 网格 + 坐标 
# ======================================================
coord_cartesian(clip = "off", xlim = c(-2, 24.5)) + # 略微调整 xlim 以容纳更宽的列
  scale_y_reverse() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "floralwhite", colour = NA),
    plot.background  = element_rect(fill = "floralwhite", colour = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(8, 35, 8, 35),
    
    plot.title = element_markdown(
      family = "Microsoft YaHei", 
      face = "bold", 
      hjust = 0.5,
      lineheight = 1.25,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_markdown(
      family = "Microsoft YaHei",
      size = 13, face = "bold",
      hjust = 0.5,
      lineheight = 1.6
    ),
    plot.caption = element_text(
      family = "Microsoft YaHei",
      size = 14.5, face = "bold",
      hjust = 0.5
    )
  ) +
  
  labs(
    title = "<span style='font-size:32pt;'>NBA本赛季最烂20人榜单</span>",
    subtitle = "统计数据为CraftedPM，由33.3% DARKO|16.6% LEBRON|16.6% DRIP|16.6% BPM|16.6% LA3RAPM加权<br>本赛季未更新的数据如LEBRON则以上赛季为准",
    caption  = "制作：俗人罢了 | 数据源 craftednba.com | 统计时间：11.26"
  )


ggsave("NBA_CraftedPM_BOTTOM20_FINAL.png", p,
       width = 18, height = 12.5, dpi = 340, 
       bg = "floralwhite", device = agg_png)

shell.exec("NBA_CraftedPM_BOTTOM20_FINAL.png")