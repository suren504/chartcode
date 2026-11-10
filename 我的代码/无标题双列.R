library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
library(ragg)

# -----------------------------
# 基础参数设置（保持不变）
# -----------------------------
col_x_left <- -0.2 
col_x_right <- 12 
col_rank <- 0.6
col_player <- 6.5
col_value <- 10.0

# 1. 球队主色
nba_team_colors <- c(
  "ATL"="#E03A3E", "BOS"="#007A33", "BKN"="#000000",
  "CHA"="#00788C", "CHI"="#CE1141", "CLE"="#860038",
  "DAL"="#00538C", "DEN"="#FEC524", "DET"="#1D428A",
  "GSW"="#FFB81C", "HOU"="#CE1141", "IND"="#FDBB30",
  "LAC"="#002147", "LAL"="#552583", "MEM"="#5D76A9",
  "MIA"="#98002E", "MIL"="#00471B", "MIN"="#0C2340",
  "NOP"="#8C4A24", "NYK"="#F58426", "OKC"="#007AC1",
  "ORL"="#0077C0", "PHI"="#006BB6", "PHX"="#E56020",
  "POR"="#E03A3E", "SAC"="#5A2D81", "SAS"="#C4CED4",
  "TOR"="#CE1141", "UTA"="#002B5C", "WAS"="#E31837"
)
# 2. 浅色系
nba_team_colors_light <- c(
  "ATL"="#F4A6A6", "BOS"="#66B266", "BKN"="#666666",
  "CHA"="#66B2C1", "CHI"="#F27A8E", "CLE"="#C5667A",
  "DAL"="#66A3D1", "DEN"="#FFD76A", "DET"="#7893C9",
  "GSW"="#FFD36F", "HOU"="#F27A8E", "IND"="#FED584",
  "LAC"="#667D96", "LAL"="#9D7FCA", "MEM"="#9DB4D6",
  "MIA"="#D1667A", "MIL"="#4D8C4D", "MIN"="#5D7AAB",
  "NOP"="#bc8a5f", "NYK"="#F9B57D", "OKC"="#66B8E6",
  "ORL"="#66B2E6", "PHI"="#66A3D1", "PHX"="#EF9C6C",
  "POR"="#F4A6A6", "SAC"="#9D7FCA", "SAS"="#E1E7EB",
  "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)
# 3. 极浅背景色
nba_team_colors_extra_light <- c(
  "ATL"="#FEEEEE", "BOS"="#EEF8EE", "BKN"="#F2F2F2",
  "CHA"="#F0F8F9", "CHI"="#FEEEEE", "CLE"="#F8EEF0",
  "DAL"="#EEF5FC", "DEN"="#FFFBF0", "DET"="#EEF2F9",
  "GSW"="#FFF9E6", "HOU"="#FEEEEE", "IND"="#FFFBF3",
  "LAC"="#E6E9ED", "LAL"="#F5F0FC", "MEM"="#F0F5FC",
  "MIA"="#F9F0F2", "MIL"="#F3F6F4", "MIN"="#F3F4F6",
  "NOP"="#F9F2ED", "NYK"="#FEF6EF", "OKC"="#F0F8FC",
  "ORL"="#F0F8FC", "PHI"="#EEF5FC", "PHX"="#FDF3EE",
  "POR"="#FEEEEE", "SAC"="#F5F0FC", "SAS"="#F9FAFB",
  "TOR"="#FEEEEE", "UTA"="#F3F4F6", "WAS"="#FEEEEE"
)

# 左：移动射手型球员榜单 (Movement Shooter - CraftedOPM TOP10)
left_data_gravity <- tibble(
  排名 = paste0("#", 1:10),
  中文姓名 = c("斯蒂芬·库里", "小迈克尔·波特", "马尔卡宁", "诺曼·鲍威尔", "以赛亚·乔",
           "萨姆·梅里尔", "唐特·迪温琴佐", "卡梅伦·约翰逊", "萨姆·豪瑟", "康·克内普尔"),
  英文姓名 = c("Stephen Curry", "Michael Porter Jr.", "Lauri Markkanen", "Norman Powell", "Isaiah Joe",
           "Sam Merrill", "Donte DiVincenzo", "Cameron Johnson", "Sam Hauser", "Kon Knueppel"),
  球队 = c("金州勇士（GSW）", "布鲁克林篮网（BKN）", "犹他爵士（UTA）", "迈阿密热火（MIA）", "俄克拉荷马城雷霆（OKC）",
         "克利夫兰骑士（CLE）", "明尼苏达森林狼（MIN）", "丹佛掘金（DEN）", "波士顿凯尔特人（BOS）", "夏洛特黄蜂（CHA）"),
  Value_Raw = c(5, 2.6, 2.4, 1.6, 1.5, 1.1, 0.9, 0.8, 0.8, 0.7),
  panel = "Left",
  relative_row = 1:10,
  row_y = (1:10) * 1.3
)

# 右：定点射手型球员榜单 (Spot Up Shooter - CraftedOPM TOP10)
right_data_gravity <- tibble(
  排名 = paste0("#", 1:10),
  中文姓名 = c("米卡尔·布里奇斯", "阿隆·威金斯", "哈里森·巴恩斯", "约什·哈特", "OG·阿奴诺比",
           "塔里·伊森", "格莱姆斯", "亚历克斯·卡鲁索", "凯文·赫尔特", "肯里奇·威廉姆斯"),
  英文姓名 = c("Mikal Bridges", "Aaron Wiggins", "Harrison Barnes", "Josh Hart", "OG Anunoby",
           "Tari Eason", "Quentin Grimes", "Alex Caruso", "Kevin Huerter", "Kenrich Williams"),
  球队 = c("纽约尼克斯（NYK）", "俄克拉荷马城雷霆（OKC）", "圣安东尼奥马刺（SAS）", "纽约尼克斯（NYK）", "纽约尼克斯（NYK）",
         "休斯顿火箭（HOU）", "费城76人（PHI）", "俄克拉荷马城雷霆（OKC）", "芝加哥公牛（CHI）", "俄克拉荷马城雷霆（OKC）"),
  Value_Raw = c(1.3, 0.8, 0.6, 0.6, 0.6, 0.5, 0.1, 0, 0, -0.1),
  panel = "Right",
  relative_row = 1:10,
  row_y = (1:10) * 1.3
)

# -----------------------------
# 合并数据并处理（保持原有逻辑，适配新球员名）
# -----------------------------
df <- bind_rows(left_data_gravity, right_data_gravity) %>%
  mutate(
    team_cn = str_extract(球队, "^[^（]+"),
    team_en = str_extract(球队, "（([A-Z]+)）") %>% str_remove_all("（|）"),
    姓名 = 中文姓名,
    姓名_en = 英文姓名,
    col_x = ifelse(panel == "Left", col_x_left, col_x_right),
    ymin = row_y - 0.45,
    ymax = row_y + 0.45,
    
    # 头像处理逻辑（适配主控/副控球员名，保留特殊字符替换）
    headshot = 姓名_en %>%
      str_replace_all("č", "c") %>%      
      str_replace_all("ć", "c") %>% 
      str_replace_all("ū", "u") %>%   
      str_replace_all("š", "s") %>%
      str_replace_all("(?i)Jr\\b\\.", "Jr") %>%      
      str_replace_all(" ", "_") %>%
      paste0("D:/nba/球员头像/", ., ".png") %>%
      ifelse(file.exists(.), ., NA),
    
    team_logo = paste0("D:/nba/球队logo/球队logo方/", team_en, ".png") %>%
      ifelse(file.exists(.), ., NA),
    
    color = nba_team_colors[team_en],
    bar_color = nba_team_colors_light[team_en],
    bg_color = nba_team_colors_extra_light[team_en],
    
    # CraftedOPM 保留1位小数，与原数据一致
    数值_label = sprintf("%.1f", Value_Raw)
  )

# =======================================================================
# 绘图部分（仅修改【列上方分类标题】，其余样式/图层完全保留你的修改）
# =======================================================================
p <- ggplot(df) +
  
  # --- 背景与条形图图层（保持不变）---
  geom_rect(aes(xmin = col_x + 0.1, xmax = col_x + 9.1,
                ymin = row_y - 0.45, ymax = row_y + 0.45, 
                fill = bg_color), color = NA) +
  geom_rect(aes(xmin = col_x + 0.2, xmax = col_x + 9,
                ymin = ymin, ymax = ymax, color = color),
            fill = NA, linewidth = 1.6) +
  geom_rect(aes(xmin = col_x + 9.1, xmax = col_x + 10.9,
                ymin = row_y - 0.45, ymax = row_y + 0.45,
                fill = bar_color), color = NA) +
  geom_rect(aes(xmin = col_x + 9.0, xmax = col_x + 11.0,
                ymin = row_y - 0.53, ymax = row_y + 0.53,
                color = color), fill = NA, linewidth = 1.4) +
  
  # --- 文本与图片图层（保持不变，新增na.rm=TRUE消除缺失警告）---
  geom_text(aes(x = col_x + 0.7, y = row_y, label = 排名),
            size = 8, fontface = "bold", family = "Consolas") +
  geom_image(aes(x = col_x + 1.8, y = row_y, image = team_logo),
             size = 0.06, na.rm = TRUE) +
  geom_image(aes(x = col_x + 3.5, y = row_y-0.1 , image = headshot),
             size = 0.085, na.rm = TRUE) +
  geom_text(aes(x = col_x + col_player, y = row_y, label = 姓名),
            size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  geom_shadowtext(aes(x = col_x + col_value, y = row_y, label = 数值_label),
                  size = 8.8, fontface = "bold",
                  colour = "white", bg.colour = "black", family = "Consolas") +
  
  scale_fill_identity() +
  scale_color_identity() +
  
  # ==========================================================
# 核心修改：列上方分类标题替换为【主控/副控】，位置/样式不变
# ==========================================================
# 左侧分类大标题：主控型球员
annotate("text", x = col_x_left + 5.5, y = -0.8, 
         label = "跑动型射手 (Movement Shooter)",
         size = 10, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  # 右侧分类大标题：副控型球员
  annotate("text", x = col_x_right + 5.5, y = -0.8, 
           label = "定点型射手 (Spot Up Shooter)",
           size = 10, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  # --- 原有的小表头（排名/球员/数值）保持不变 ---
  annotate("text", x = col_x_left + col_rank, y = +0.4, label = "排名",
           size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_player, y = +0.4, label = "球员",
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_value, y = +0.4, label = "CraftedOPM",
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  
  annotate("text", x = col_x_right + col_rank, y = +0.4, label = "排名",
           size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_player, y = +0.4, label = "球员",
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_value, y = +0.4, label = "CraftedOPM",
           size = 8, fontface = "bold", family = "Microsoft YaHei") +
  
  # --- 坐标系与主题调整（保持你的所有修改）---
  coord_cartesian(clip = "off", xlim = c(-2, 24.5), ylim = c(13.5, -2)) + 
  scale_y_reverse() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "floralwhite", colour = NA),
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.margin = margin(t = 30, r = -80, b = 8, l = -80),
    plot.caption = element_markdown(
      family = "Microsoft YaHei",
      size = 14.5, 
      face = "bold",
      hjust = 0.5,
      margin = margin(t = 10, b = 5)
    )
  ) +
  
  # --- Labs保持极简（无标题/副标题/注释）---
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

# 保存图片（文件名适配主控+副控主题，避免覆盖）
ggsave("射手.png", p,
       width = 18, height = 14, dpi = 340,
       bg = "floralwhite", device = agg_png)