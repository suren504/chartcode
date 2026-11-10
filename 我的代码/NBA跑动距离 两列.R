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
  "亚特兰大老鹰"="#E13A3E",       # 老鹰LOGO主红
  "波士顿凯尔特人"="#007A33",     # 凯尔特人LOGO主绿
  "布鲁克林篮网"="#000000",       # 篮网LOGO主黑
  "夏洛特黄蜂"="#1D1160",        # 黄蜂LOGO主紫蓝
  "芝加哥公牛"="#CE1141",         # 公牛LOGO主红
  "克利夫兰骑士"="#860038",       # 骑士LOGO主酒红
  "达拉斯独行侠"="#00538C",       # 独行侠LOGO主蓝
  "丹佛掘金"="#FFC72C",           # 掘金LOGO主金（原深色替换为实际LOGO主色）
  "底特律活塞"="#C8102E",         # 活塞LOGO主红
  "金州勇士"="#FFC72C",           # 勇士LOGO主金（原蓝色替换为实际LOGO主色）
  "休斯顿火箭"="#CE1141",         # 火箭LOGO主红
  "印第安纳步行者"="#FFC72C",     # 步行者LOGO主金（原蓝色替换为实际LOGO主色）
  "洛杉矶快船"="#006BB6",         # 快船LOGO主蓝
  "洛杉矶湖人"="#552583",         # 湖人LOGO主紫
  "孟菲斯灰熊"="#5D76A9",         # 灰熊LOGO主灰蓝
  "迈阿密热火"="#98002E",         # 热火LOGO主红
  "密尔沃基雄鹿"="#00471B",       # 雄鹿LOGO主绿
  "明尼苏达森林狼"="#0C2340",     # 森林狼LOGO主深蓝
  "新奥尔良鹈鹕"="#002B5C",       # 鹈鹕LOGO主蓝
  "纽约尼克斯"="#FF6B00",         # 尼克斯LOGO主橙（原蓝色替换为实际LOGO主色）
  "俄克拉荷马城雷霆"="#007AC1",   # 雷霆LOGO主蓝
  "奥兰多魔术"="#0077C0",         # 魔术LOGO主蓝
  "费城76人"="#006BB6",           # 76人LOGO主蓝
  "菲尼克斯太阳"="#E56020",       # 太阳LOGO主橙
  "波特兰开拓者"="#E03A3E",       # 开拓者LOGO主红
  "萨克拉门托国王"="#5A2D81",     # 国王LOGO主紫
  "圣安东尼奥马刺"="#000000",     # 马刺LOGO主黑（原浅灰替换为实际LOGO主色）
  "多伦多猛龙"="#CE1141",         # 猛龙LOGO主红
  "犹他爵士"="#002B5C",           # 爵士LOGO主蓝
  "华盛顿奇才"="#E31837"          # 奇才LOGO主红
)

# 对应淡色系（基于LOGO主色调浅，保持视觉协调）
nba_team_colors_light <- c(
  "亚特兰大老鹰"="#F8BABA",       # 老鹰淡红
  "波士顿凯尔特人"="#66B266",     # 凯尔特人淡绿
  "布鲁克林篮网"="#666666",       # 篮网淡黑
  "夏洛特黄蜂"="#7A6FA3",        # 黄蜂淡紫蓝
  "芝加哥公牛"="#F27A8E",         # 公牛淡红
  "克利夫兰骑士"="#C5667A",       # 骑士淡酒红
  "达拉斯独行侠"="#66A3D1",       # 独行侠淡蓝
  "丹佛掘金"="#FFE088",           # 掘金淡金
  "底特律活塞"="#F27A7A",         # 活塞淡红
  "金州勇士"="#FFE088",           # 勇士淡金
  "休斯顿火箭"="#F27A8E",         # 火箭淡红
  "印第安纳步行者"="#FFE088",     # 步行者淡金
  "洛杉矶快船"="#66A3D1",         # 快船淡蓝
  "洛杉矶湖人"="#9D7FCA",         # 湖人淡紫
  "孟菲斯灰熊"="#9DB4D6",         # 灰熊淡灰蓝
  "迈阿密热火"="#D1667A",         # 热火淡红
  "密尔沃基雄鹿"="#4D8C4D",       # 雄鹿淡绿
  "明尼苏达森林狼"="#5D7AAB",     # 森林狼淡深蓝
  "新奥尔良鹈鹕"="#668BB2",       # 鹈鹕淡蓝
  "纽约尼克斯"="#FFB380",         # 尼克斯淡橙
  "俄克拉荷马城雷霆"="#66B8E6",   # 雷霆淡蓝
  "奥兰多魔术"="#66B2E6",         # 魔术淡蓝
  "费城76人"="#66A3D1",           # 76人淡蓝
  "菲尼克斯太阳"="#FFC080",       # 太阳淡橙
  "波特兰开拓者"="#F8BABA",       # 开拓者淡红
  "萨克拉门托国王"="#9D7FCA",     # 国王淡紫
  "圣安东尼奥马刺"="#666666",     # 马刺淡黑
  "多伦多猛龙"="#F27A8E",         # 猛龙淡红
  "犹他爵士"="#668BB2",           # 爵士淡蓝
  "华盛顿奇才"="#F8BABA"          # 奇才淡红
)
# -----------------------------
# 数据准备 - 进攻/防守距离
# -----------------------------
# 进攻距离（km）
offense_distance <- tibble(
  排名 = paste0("#", 1:10),
  球员 = c(
    "马克西（Tyrese Maxey）",
    "康·克尼佩尔（Kon Knueppel）",
    "米卡尔·布里奇斯（Mikal Bridges）",
    "弗拉格（Cooper Flagg）",
    "亚历山大·沃克（Nickeil Alexander-Walker）",
    "小瓦格纳（Franz Wagner）",
    "戴森·丹尼尔斯（Dyson Daniels）",
    "特雷·墨菲（Trey Murphy）",
    "普里查德（Payton Pritchard）",
    "斯科蒂·巴恩斯（Scottie Barnes）"
  ),
  球队 = c(
    "费城76人（PHI）","夏洛特黄蜂（CHA）","纽约尼克斯（NYK）","达拉斯独行侠（DAL）",
    "亚特兰大老鹰（ATL）","奥兰多魔术（ORL）","亚特兰大老鹰（ATL）",
    "新奥尔良鹈鹕（NOP）","波士顿凯尔特人（BOS）","多伦多猛龙（TOR）"
  ),
  距离 = round(c(36.40,33.70,32.90,31.90,31.80,31.70,31.60,31.30,31.10,30.90)*1.60934,2),
  panel = "Left",
  relative_row = 1:10,
  row_y = (1:10)*1.25
)

# 防守距离（km）
defense_distance <- tibble(
  排名 = paste0("#", 1:10),
  球员 = c(
    "卡马拉（Toumani Camara）",
    "戴森·丹尼尔斯（Dyson Daniels）",
    "斯科蒂·巴恩斯（Scottie Barnes）",
    "奎克利（Immanuel Quickley）",
    "小瓦格纳（Franz Wagner）",
    "泰瑞斯·马克西（Tyrese Maxey）",
    "莫布利（Evan Mobley）",
    "阿夫迪亚（Deni Avdija）",
    "罗林斯（Ryan Rollins）",
    "贝恩（Desmond Bane）"
  ),
  球队 = c(
    "波特兰开拓者（POR）","亚特兰大老鹰（ATL）","多伦多猛龙（TOR）","多伦多猛龙（TOR）",
    "奥兰多魔术（ORL）","费城76人（PHI）","克利夫兰骑士（CLE）","波特兰开拓者（POR）",
    "密尔沃基雄鹿（MIL）","奥兰多魔术（ORL）"
  ),
  距离 = round(c(30.40,29.50,28.80,28.00,27.70,27.70,27.30,27.00,26.80,26.70)*1.60934,2),
  panel = "Right",
  relative_row = 1:10,
  row_y = (1:10)*1.25
)

# -----------------------------
# 合并数据并处理
# -----------------------------
df <- bind_rows(offense_distance, defense_distance) %>%
  mutate(
    # 提取球队中文名称和英文缩写
    team_cn = str_extract(球队, "^[^（]+"),
    team_en = str_extract(球队, "（([A-Z]+)）") %>% str_remove_all("（|）"),
    
    # 提取球员纯中文姓名
    姓名 = str_extract(球员, "^[^（]+"),
    # 提取球员英文姓名（用于头像路径）
    姓名_en = str_extract(球员, "（([^）]+)）") %>% str_remove_all("（|）"),
    
    # 坐标和样式相关
    col_x = ifelse(panel == "Left", col_x_left, col_x_right),
    ymin = row_y - 0.45,
    ymax = row_y + 0.45,
    
    # 头像和logo路径
    headshot = 姓名_en %>% 
      str_replace_all(" ", "_") %>% 
      str_replace_all("III", "3") %>%  # 处理三世这类后缀
      paste0("D:/nba/球员头像/", ., ".png") %>% 
      ifelse(file.exists(.), ., NA),
    
    team_logo = paste0("D:/nba/球队logo/球队logo方/", team_en, ".png") %>% 
      ifelse(file.exists(.), ., NA),
    
    # 球队颜色
    color = nba_team_colors[team_cn],
    bar_color = nba_team_colors_light[team_cn]
  )

# =======================================================================
# 绘图
# =======================================================================
p <- ggplot(df) +
  
  # 外框 (宽度已增加，从 8.8 变为 11.0)
  geom_rect(aes(
    xmin = col_x + 0.2, xmax = col_x + 9,
    ymin = ymin, ymax = ymax, color = color
  ),
  fill = NA, linewidth = 1.6) +
  
  # 距离背景色 (宽度已增加，整体右移)
  geom_rect(aes(
    xmin = col_x + 9.1, xmax = col_x + 10.9,
    ymin = row_y - 0.45, ymax = row_y + 0.45,
    fill = bar_color
  ),
  color = NA) +
  
  geom_rect(aes(
    xmin = col_x + 9.0, xmax = col_x + 11.0,
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
  geom_text(aes(x = col_x + col_player, y = row_y, label = 姓名),
            size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  
  # 距离数字 (已右移到 10.0，与背景条对齐)
  geom_shadowtext(aes(x = col_x + col_value, y = row_y, label = 距离),
                  size = 8.8, fontface = "bold",
                  colour = "white", bg.colour = "black") +
  
  scale_fill_identity() +
  scale_color_identity() +
  
  # ======================================================
# ⭐⭐⭐ 底部标题 —— 自动对齐
# ======================================================
# 左列标题（进攻距离）
annotate("text", x = col_x_left + col_rank,    y = -0.4, label = "排名",
         size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_player, y = -0.4, label = "球员",
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_value,  y = -0.4, label = "进攻距离(km)",
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  
  # 右列标题（防守距离）
  annotate("text", x = col_x_right + col_rank,    y = -0.4, label = "排名",
           size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_player, y = -0.4, label = "球员",
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_value,  y = -0.4, label = "防守距离(km)",
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  
  # ======================================================
# ⭐⭐⭐ 清除浅蓝块 + 网格 + 坐标 
# ======================================================
coord_cartesian(clip = "off", xlim = c(-2, 24.5)) +
  scale_y_reverse() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#F1F0EC", colour = NA),
    plot.background  = element_rect(fill = "#F1f0ec", colour = NA),
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
    title = "<span style='font-size:32pt;'>NBA本赛季球员攻防两端跑动总距TOP10 谁是大牛马？</span>",  # 修正闭合标签
    subtitle = "统计单位：千米(km) | 数据已转换为公里（原始英里×1.60934）<br>进攻距离为球员进攻端累计跑动距离，防守距离为防守端累计跑动距离",
    caption  = "制作：俗人罢了 | 统计时间：2025.12.9 | 数据源：NBA官网"  # 补全引号、修正分隔符
  )

# 保存图片
ggsave("NBA_Player_Distance_TOP10_FINAL.png", p,  # 补全缺失的 p（绘图对象）
       width = 18, height = 12.5, dpi = 340, 
       bg = "#F0EEE9", device = agg_png)

# 打开图片
shell.exec("NBA_Player_Distance_TOP10_FINAL.png")