library(tidyverse)
library(ggimage)
library(ggtext)
library(glue)
library(shadowtext)

# =====================================================================
# 1️⃣ 球队缩写映射
# =====================================================================
team_abbr <- c(
  "掘金"="DEN","独行侠"="DAL","雷霆"="OKC","雄鹿"="MIL",
  "湖人"="LAL","马刺"="SAS","森林狼"="MIN","勇士"="GSW",
  "太阳"="PHX","尼克斯"="NYK","骑士"="CLE","活塞"="DET",
  "快船"="LAC","凯尔特人"="BOS","热火"="MIA","火箭"="HOU",
  "76人"="PHI","魔术"="ORL","猛龙"="TOR","灰熊"="MEM",
  "开拓者"="POR","国王"="SAC","奇才"="WAS","黄蜂"="CHA",
  "鹈鹕"="NOP","老鹰"="ATL","步行者"="IND",
  "篮网"="BKN","公牛"="CHI","爵士"="UTA"
)

# =====================================================================
# 2️⃣ 颜色映射（保持不变）
# =====================================================================
primary_color <- c(
  "老鹰"="#E03A3E","凯尔特人"="#008348","篮网"="#000000",
  "黄蜂"="#1D1160","公牛"="#CE1141","骑士"="#8B0000",
  "独行侠"="#00538C","掘金"="#FFB81C","活塞"="#C8102E",
  "勇士"="#FFC72C","火箭"="#CE1141","步行者"="#002D62",
  "快船"="#ED174C","湖人"="#552583","灰熊"="#23375B",
  "热火"="#98002E","雄鹿"="#00471B","森林狼"="#005083",
  "鹈鹕"="#C8A97E","尼克斯"="#FF671F","雷霆"="#002D62",
  "魔术"="#0077C0","76人"="#ED174C","太阳"="#E56020",
  "开拓者"="#E03A3E","国王"="#5A2D81","马刺"="#000000",
  "猛龙"="#CE1141","爵士"="#002B5C","奇才"="#E31837"
)

nba_team_colors_light <- c(
  "老鹰"="#F8B4B6","凯尔特人"="#70D6A3","篮网"="#666666",
  "黄蜂"="#A094D1","公牛"="#F8B4B6","骑士"="#F5D0D0",
  "独行侠"="#8EC0FF","掘金"="#FFE4B3","活塞"="#F8B4B6",
  "勇士"="#FFE4B3","火箭"="#F8B4B6","步行者"="#8EC0FF",
  "快船"="#FFC1D5","湖人"="#C0A5E8","灰熊"="#8A9BC2",
  "热火"="#E08A97","雄鹿"="#6FB28A","森林狼"="#8EC0FF",
  "鹈鹕"="#F2E8D5","尼克斯"="#FFE0CC","雷霆"="#D1E0F0",
  "魔术"="#B3E0FF","76人"="#FFC1D5","太阳"="#FFD1A6",
  "开拓者"="#F8B4B6","国王"="#C0A5E8","马刺"="#666666",
  "猛龙"="#F8B4B6","爵士"="#8EC0FF","奇才"="#F8B4B6"
)

# =====================================================================
# 3️⃣ 数据（所有球队名去掉地名，仅留核心队名）
# =====================================================================
top40 <- tribble(
  ~player, ~team, ~player_id, ~rc, ~ringer, ~espn, ~cbs, ~cp, ~avg_rank,
  "约基奇","掘金",203999,1,1,1,1,1,1.0,
  "亚历山大","雷霆",1628983,2,2,2,2,2,2.0,
  "字母哥","雄鹿",203507,3,3,4,4,3,3.4,
  "东契奇","湖人",1629029,4,4,3,3,4,3.6,
  "文班亚马","马刺",1641705,5,5,5,5,9,5.8,
  "爱德华兹","森林狼",1630162,7,6,6,7,6,6.4,
  "库里","勇士",201939,9,7,7,6,5,6.8,
  "杜兰特","火箭",201142,10,11,9,9,8,9.4,
  "布伦森","尼克斯",1628973,11,9,10,8,11,9.8,
  "米切尔","骑士",1628378,6,8,15,11,10,10.0,
  "詹姆斯","湖人",2544,12,18,8,10,7,11.0,
  "坎宁安","活塞",1630595,16,10,12,13,12,12.6,
  "浓眉","独行侠",203076,8,19,14,12,13,13.2,
  "杰威","雷霆",1631114,17,17,11,14,17,15.2,
  "布克","太阳",1626164,21,12,16,16,14,15.8,
  "伦纳德","快船",202695,18,20,20,15,20,18.6,
  "莫布里","骑士",1630596,19,16,13,23,23,18.8,
  "杰伦布朗","凯尔特人",1627759,34,13,19,17,15,19.6,
  "唐斯","尼克斯",1626157,13,21,27,19,18,19.6,
  "哈登","快船",201935,14,22,23,27,19,21.0,
  "巴特勒","勇士",202710,20,24,18,22,24,21.6,
  "班凯罗","魔术",1631094,31,28,17,18,16,22.0,
  "西亚卡姆","步行者",1627783,22,25,22,28,22,23.8,
  "申京","火箭",1630578,25,14,25,29,26,23.8,
  "马克西","76人",1630178,28,15,28,21,36,25.6,
  "恩比德","76人",203954,15,99,47,26,21,27.25,
  "霍姆格伦","雷霆",1631096,37,27,24,24,34,29.2,
  "阿德巴约","热火",1628389,41,30,21,30,25,29.4,
  "贾伦杰克逊.","灰熊",1628991,24,41,31,20,33,29.8,
  "小瓦格纳","魔术",1630532,23,37,32,33,27,30.4,
  "莫兰特","灰熊",1629630,27,99,33,32,30,30.5,
  "特雷杨","老鹰",1629027,30,39,29,25,32,31.0,
  "怀特","凯尔特人",1628401,26,45,26,34,38,33.8,
  "萨博尼斯","国王",1627734,29,38,34,42,31,34.8,
  "福克斯","马刺",1628368,44,36,35,31,29,35.0,
  "阿门汤普森","火箭",1641708,36,34,37,35,44,37.2,
  "贾马尔穆雷","掘金",1627750,40,29,46,39,35,37.8,
  "斯科蒂巴恩斯","猛龙",1630567,49,31,30,46,37,38.6,
  "锡安","鹈鹕",1629627,32,40,57,38,28,39.0,
  "加兰","骑士",1629636,39,44,38,36,41,39.6
)

# =====================================================================
# 4️⃣ 数据处理（添加缺失的步骤，计算 col, row, rn 等）
# =====================================================================
top40 <- top40 %>%
  mutate(
    # 1. 填充NA值 (你的数据中没有NA，但这里是逻辑填充)
    rc = replace_na(rc, 99),
    ringer = replace_na(ringer, 99),
    espn = replace_na(espn, 99),
    cbs = replace_na(cbs, 99),
    cp = replace_na(cp, 99),
    
    # 2. 定义颜色变量
    primary_color = primary_color[team], # 外框深色 (注意：这里使用你重命名的 primary_color)
    light_color   = nba_team_colors_light[team], # 内框浅色
    
    # 3. 定义字符串变量
    # 排名字符串（RC | Ringer | ESPN | CBS | CP）
    rk_string = glue("RC:{ifelse(rc==99,'--',rc)} | R:{ifelse(ringer==99,'--',ringer)} | E:{ifelse(espn==99,'--',espn)} | C:{ifelse(cbs==99,'--',cbs)} | CP:{ifelse(cp==99,'--',cp)}"),
    
    # 球员姓名文字
    player_text = glue("<span style='font-size:11pt; color:{primary_color};'>**{player}**</span>"),
    
    # LOGO 路径
    logo = glue("D:/nba/球队logo/{team_abbr[team]}.png"),
    
    # 头像路径
    headshot = glue("https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png")
  ) %>%
  # --------------------------------------------------------
# 第二个 mutate 块：用于排版和位置计算 (确保 col, row, rn 被创建)
# --------------------------------------------------------
arrange(avg_rank) %>%
  mutate(
    row = rep(1:8, each=5),
    col = rep(1:5, 8),
    rn = row_number()
  )

# =====================================================================
# 5️⃣ 绘图重构：改为横向长方形卡片 (修正 col/row 引用错误)
# =====================================================================
theme_f5 <- function(){
  theme_minimal(base_family="Microsoft YaHei", base_size=9) %+replace%
    theme(
      plot.background = element_rect(fill="floralwhite", colour="floralwhite"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(20,20,20,20)
    )
}
# 1. 修正主题：减小 plot.margin (从 20,20,20,20 减少到 5, 5, 5, 5)
theme_f5 <- function(){
  theme_minimal(base_family="Microsoft YaHei", base_size=9) %+replace%
    theme(
      plot.background = element_rect(fill="floralwhite", colour="floralwhite"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      # 减小所有方向的边距，让卡片有更多空间
      plot.margin = margin(5, 5, 5, 5) 
    )
}

# --- 调整参数以创建横向长方形 ---
spacing_x <- 1.45     # 水平间距
spacing_y <- 1.15     # 垂直间距

card_width <- 1.2    # 外框宽度
card_height <- 0.75  # 外框高度

inner_width <- 1.05  # 内框宽度
inner_height <- 0.6  # 内框高度

# --- 调整 Logo 和 Headshot 的 Y 轴偏移量 ---
logo_offset <- 0.20
headshot_offset <- -0.15

# 绘图代码（修正了 geom_shadowtext 中的 col/row 引用方式）
p <- ggplot(top40, aes(x = col * spacing_x, y = -row * spacing_y)) +
  
  # 1. 深色框（外层）
  geom_tile(aes(fill = primary_color),
            width = card_width, height = card_height,
            color = NA, alpha = 0.9) +
  
  # 2. 浅色框（内层）
  geom_tile(aes(fill = light_color),
            width = inner_width, height = inner_height,
            color = NA, alpha = 0.6) +
  
  # 3. 排名数字 (Top Left) - 修正后的引用
  geom_shadowtext(aes(label = rn,
                      x = col * spacing_x - inner_width/2,  
                      y = -row * spacing_y + inner_height/2),
                  size = 4,
                  vjust = 0.5, hjust = 0.5,
                  bg.colour = "black",
                  color = 'white',
                  bg.r = .15,
                  family = "Microsoft YaHei",
                  fontface = 'bold') +
  
  # 4. LOGO (Top Center)
  geom_image(aes(image = logo, y = -row * spacing_y + logo_offset), 
             size = 0.08, asp = 1) + 
  
  # 5. 球员头像 (Center)
  geom_image(aes(image = headshot, y = -row * spacing_y + headshot_offset), 
             size = 0.1, asp = 1) + 
  
  # 6. 姓名 (Bottom Center)
  geom_richtext(
    aes(label = player_text),
    fill = NA, label.color = NA,
    hjust = 0.5, 
    vjust = 3.8,
    size = 3.5 
  ) +
  
  # 7. 详细排名文字 (Bottom)
  geom_richtext(
    aes(label = rk_string),
    fill = NA, label.color = NA,
    hjust = 0.5, 
    vjust = 5.5,
    size = 2.5 
  ) +
  
  scale_fill_identity() +
  coord_equal() +
  theme_f5() + 
  labs(
    title = "NBA Top 40 Players Ranking (2025-2026)",
    subtitle = "RC | The Ringer | ESPN | CBS | Clutch Points",
    caption = "数据来源: RC / The Ringer / ESPN / CBS / Clutch Points"
  )

# 打印图表
print(p)

# 输出图片
ggsave("NBA_Top40_2025.png", p, width = 14, height = 18, dpi = 600, device="png")
browseURL("NBA_Top40_2025.png")