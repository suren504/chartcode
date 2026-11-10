library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)


col_x_left <- 0 
col_x_right <- 9.5 

# 左列四元素：排名 | 进攻队伍+球员 | 球员名（双行） | 防守队伍+球员
col_rank <- 0.4
col_offense_logo <- 1.0
col_offense_headshot <- 1.7
col_names <- 3.45     # 中间球员名双行
col_defense_logo <- 5.0
col_defense_headshot <- 5.7
col_value <- 7.6

box_mid <- 6.6       
box_end <- 8.5      
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


clean_name_for_join <- function(name) {
  name %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("[^a-z0-9]", "")
}

# ==========================================
# 1. 对位克星数据 (高效终结者 vs 低效终结者)
# ==========================================
high_eff <- tibble(
  排名 = 1:10,
  进攻球员 = c("Rui Hachimura", "OG Anunoby", "Devin Booker", "Victor Wembanyama", 
           "Karl-Anthony Towns", "Onyeka Okongwu", "Donovan Mitchell", "Evan Mobley", 
           "Evan Mobley", "Duncan Robinson"),
  进攻球队 = c("LAL", "NYK", "PHX", "SAS", "NYK", "ATL", "CLE", "CLE", "CLE", "DET"),
  防守球员 = c("Chet Holmgren", "Jalen Johnson", "Luguentz Dort", "Isaiah Hartenstein", 
           "Onyeka Okongwu", "Karl-Anthony Towns", "Ja'Kobe Walter", "Jakob Poeltl", 
           "Tobias Harris", "Desmond Bane"),
  防守球队 = c("OKC", "ATL", "OKC", "OKC", "ATL", "NYK", "TOR", "TOR", "DET", "ORL"),
  对位回合 = c(129, 184, 130, 172, 156, 148, 125, 129, 151, 148),
  对位效率 = c(1.71, 1.41, 1.32, 1.32, 1.28, 1.25, 1.23, 1.21, 1.18, 1.16)
)

low_eff <- tibble(
  排名 = 1:10,
  进攻球员 = c("James Harden", "Derrick White", "Julius Randle", "Tyrese Maxey", 
           "James Harden", "Jalen Suggs", "Payton Pritchard", "Donovan Mitchell", 
           "Nickeil Alexander-Walker", "Rudy Gobert"),
  进攻球队 = c("CLE", "BOS", "MIN", "PHI", "CLE", "ORL", "BOS", "CLE", "ATL", "MIN"),
  防守球员 = c("Mikal Bridges", "VJ Edgecombe", "Stephon Castle", "Mikal Bridges", 
           "Cade Cunningham", "Ausar Thompson", "VJ Edgecombe", "Ausar Thompson", 
           "Mikal Bridges", "Victor Wembanyama"),
  防守球队 = c("NYK", "PHI", "SAS", "NYK", "DET", "DET", "PHI", "DET", "NYK", "SAS"),
  对位回合 = c(146, 131, 134, 140, 130, 128, 137, 155, 158, 145),
  对位效率 = c(0.300, 0.481, 0.493, 0.560, 0.586, 0.591, 0.636, 0.646, 0.667, 0.674)
)

# ==========================================
# 2. 强健的 ID 库处理
# ==========================================
df_id <- readRDS("D:/nba/id查询.rds")

df_id_clean <- df_id %>%
  mutate(join_name = clean_name_for_join(.[[1]])) %>% 
  # 🔥 关键修复：加上 dplyr:: 强制调用正确包的函数
  dplyr::select(join_name, player_id = 2)
# ==========================================
# 3. 数据整合与 ID 匹配
# ==========================================
col_x_left <- 0 
col_x_right <- 9.5 

df <- bind_rows(
  high_eff %>% mutate(side = "Left"),
  low_eff %>% mutate(side = "Right")
) %>%
  mutate(
    col_x = ifelse(side == "Left", col_x_left, col_x_right),
    row_y = 排名 * 1.5,        
    ymin = row_y - 0.6,      
    ymax = row_y + 0.6
  ) %>%
  # 进攻球员 ID 匹配
  mutate(
    join_offense = clean_name_for_join(进攻球员)
  ) %>%
  left_join(
    df_id_clean %>% rename(join_offense = join_name, offense_id = player_id),
    by = "join_offense"
  ) %>%
  # 防守球员 ID 匹配
  mutate(
    join_defense = clean_name_for_join(防守球员)
  ) %>%
  left_join(
    df_id_clean %>% rename(join_defense = join_name, defense_id = player_id),
    by = "join_defense"
  ) %>%
  mutate(
    offense_id = replace_na(as.character(offense_id), "0"),
    defense_id = replace_na(as.character(defense_id), "0"),
    offense_headshot = paste0("D:/nba/头像/", offense_id, ".png"),
    defense_headshot = paste0("D:/nba/头像/", defense_id, ".png"),
    offense_logo = paste0("D:/nba/球队logo/球队logo方/", 进攻球队, ".png"),
    defense_logo = paste0("D:/nba/球队logo/球队logo方/", 防守球队, ".png"),
    color = ifelse(side == "Left", 
                   nba_team_colors[进攻球队], 
                   nba_team_colors[防守球队]),
    bar_color = ifelse(side == "Left",
                       nba_team_colors_light[进攻球队],
                       nba_team_colors_light[防守球队]),
    bg_color = ifelse(side == "Left",
                      nba_team_colors_extra_light[进攻球队],
                      nba_team_colors_extra_light[防守球队]),
    # 格式化数值：效率(回合)
    数值_label = sprintf("%.2f(%d)", 对位效率, 对位回合),
    # 球员名双行显示（中间加&）
    player_names = paste0(进攻球员, " &\n", 防守球员)
  )
# ==========================================
# 4. 绘图引擎：四列对位对比
# ==========================================
p <- ggplot(df) +
  
  # 背景框（主卡片）
  geom_rect(aes(xmin = col_x + 0.1, xmax = col_x + box_mid,
                ymin = ymin, ymax = ymax, fill = bg_color), color = NA) +
  
  # 边框（主卡片）
  geom_rect(aes(xmin = col_x + 0.1, xmax = col_x + box_mid - 0.1,
                ymin = ymin, ymax = ymax, color = color), fill = NA, linewidth = 1.6) +
  
  # 数值背景框
  geom_rect(aes(xmin = col_x + box_mid, xmax = col_x + box_end,
                ymin = ymin, ymax = ymax, fill = bar_color), color = NA) +
  
  # 数值框边框
  geom_rect(aes(xmin = col_x + box_mid - 0.1, xmax = col_x + box_end + 0.1,
                ymin = ymin - 0.08, ymax = ymax + 0.08, color = color), fill = NA, linewidth = 1.4) +
  
  # 排名
  geom_text(aes(x = col_x + col_rank, y = row_y, label = 排名),
            size = 6.5, fontface = "bold", family = "Consolas") +
  
  # 进攻球队LOGO
  geom_image(aes(x = col_x + col_offense_logo, y = row_y, image = offense_logo),
             size = 0.08) +
  
  # 进攻球员头像（覆盖LOGO）
  geom_image(aes(x = col_x + col_offense_headshot, y = row_y - 0.05, image = offense_headshot),
             size = 0.11) +
  
  # 防守球队LOGO
  geom_image(aes(x = col_x + col_defense_logo, y = row_y, image = defense_logo),
             size = 0.08) +
  
  # 防守球员头像（覆盖LOGO）
  geom_image(aes(x = col_x + col_defense_headshot, y = row_y - 0.05, image = defense_headshot),
             size = 0.11) +
  
  # 对位效率数值
  geom_shadowtext(aes(x = col_x + col_value, y = row_y, label = 数值_label),
                  size = 6.5, fontface = "bold",
                  colour = "white", bg.colour = "black", family = "Consolas") +
  
  # 球员名双行
  geom_shadowtext(aes(x = col_x + col_names, y = row_y, 
                      label = player_names, 
                      bg.colour = bg_color), 
                  size = 6, 
                  fontface = "bold", 
                  family = "Consolas",
                  colour = "black",
                  bg.r = 0.12,
                  lineheight = 0.9) +  
  
  scale_fill_identity() +
  scale_color_identity() +
  
  
  annotate("text", x = col_x_left + col_offense_logo, y = +0.5, label = "高效终结者",
           size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_names, y = +0.5, label = "对位球员",
           size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_defense_logo, y = +0.5, label = "对位玩具",
           size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_value, y = +0.5, label = "对位效率（回合数）", 
           size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
 
  annotate("text", x = col_x_right + col_offense_logo, y = +0.5, label = "低效终结者",
           size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_names, y = +0.5, label = "对位球员",
           size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_defense_logo, y = +0.5, label = "对位大锁",
           size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_value, y = +0.5, label = "对位效率（回合数）", 
           size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  
  coord_cartesian(clip = "off", xlim = c(0.1, 17.9)) +
  scale_y_reverse() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "floralwhite", colour = NA),
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.margin = margin(15, 0, 0, 0),
    
    plot.title = element_markdown(
      family = "Microsoft YaHei",
      face = "bold",
      hjust = 0.5, size = 28,
      lineheight = 1.25,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      family = "Microsoft YaHei",
      size = 14, face = "bold", color = "#555555",
      hjust = 0.5,
      lineheight = 1.6,
      margin = margin(b = 5)
    )
  ) +
  
  labs(
    title = "NBA2025-26季后赛20组对位克星", 
    subtitle = "左列：高效终结者 vs 对位玩具（防守端被打爆） | 右列：低效终结者 vs 对位大锁（进攻端被掐死）<br>对位效率=得分/(投篮出手+0.44×罚球出手+失误)|制图: <span style='font-size:18pt; color:black;'>**俗人罢了**</span> | 数据源: NBA官网"
  )

ggsave("NBA_matchup_villains_16x12.png", p, width = 16, height = 12, dpi = 300)