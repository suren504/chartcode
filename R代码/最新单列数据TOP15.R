library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
library(stringi) # 确保加载了 stringi

clean_name_for_join <- function(name) {
  name %>%
    stringr::str_to_lower() %>%
    # 明确告诉 R 去 stringi 包里找这个函数，永不报错
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("[^a-z0-9]", "")
}

# ==========================================
# 1. 布局参数与数据源
# ==========================================
col_x_base <- 0 
col_rank <- 0.4       
col_logo <- 1.2       
col_headshot <- 2.2   
col_player <- 4.5     
col_value <- 7.8     
box_mid <- 6.8       
box_end <- 8.8       

nba_team_colors <- c(
  "ATL"="#E03A3E", "BOS"="#007A33", "BKN"="#000000", "CHA"="#00788C", "CHI"="#CE1141", "CLE"="#860038",
  "DAL"="#00538C", "DEN"="#FEC524", "DET"="#1D428A", "GSW"="#FFB81C", "HOU"="#CE1141", "IND"="#FDBB30",
  "LAC"="#002147", "LAL"="#552583", "MEM"="#5D76A9", "MIA"="#98002E", "MIL"="#00471B", "MIN"="#0C2340",
  "NOP"="#8C4A24", "NYK"="#F58426", "OKC"="#007AC1", "ORL"="#0077C0", "PHI"="#006BB6", "PHX"="#E56020",
  "POR"="#E03A3E", "SAC"="#5A2D81", "SAS"="#C4CED4", "TOR"="#CE1141", "UTA"="#002B5C", "WAS"="#E31837"
)
nba_team_colors_light <- c(
  "ATL"="#F4A6A6", "BOS"="#66B266", "BKN"="#666666", "CHA"="#66B2C1", "CHI"="#F27A8E", "CLE"="#C5667A",
  "DAL"="#66A3D1", "DEN"="#FFD76A", "DET"="#7893C9", "GSW"="#FFD36F", "HOU"="#F27A8E", "IND"="#FED584",
  "LAC"="#667D96", "LAL"="#9D7FCA", "MEM"="#9DB4D6", "MIA"="#D1667A", "MIL"="#4D8C4D", "MIN"="#5D7AAB",
  "NOP"="#bc8a5f", "NYK"="#F9B57D", "OKC"="#66B8E6", "ORL"="#66B2E6", "PHI"="#66A3D1", "PHX"="#EF9C6C",
  "POR"="#F4A6A6", "SAC"="#9D7FCA", "SAS"="#E1E7EB", "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)
nba_team_colors_extra_light <- c(
  "ATL"="#FEEEEE", "BOS"="#EEF8EE", "BKN"="#F2F2F2", "CHA"="#F0F8F9", "CHI"="#FEEEEE", "CLE"="#F8EEF0",
  "DAL"="#EEF5FC", "DEN"="#FFFBF0", "DET"="#EEF2F9", "GSW"="#FFF9E6", "HOU"="#FEEEEE", "IND"="#FFFBF3",
  "LAC"="#E6E9ED", "LAL"="#F5F0FC", "MEM"="#F0F5FC", "MIA"="#F9F0F2", "MIL"="#F3F6F4", "MIN"="#F3F4F6",
  "NOP"="#F9F2ED", "NYK"="#FEF6EF", "OKC"="#F0F8FC", "ORL"="#F0F8FC", "PHI"="#EEF5FC", "PHX"="#FDF3EE",
  "POR"="#FEEEEE", "SAC"="#F5F0FC", "SAS"="#F9FAFB", "TOR"="#FEEEEE", "UTA"="#F3F4F6", "WAS"="#FEEEEE"
)

data <- tibble(
  排名 = 1:15,
  姓名 = c(
    "Nikola Topić", "Tidjane Salaün", "Cam Christie", "Rocco Zikarsky", "Rob Dillingham",
    "AJ Johnson", "Cody Williams", "Johnny Furphy", "Tyler Smith", "Bronny James",
    "Bub Carrington", "Cam Whitmore", "Noah Clowney", "Rayan Rupert", "Nick Smith Jr."
  ),
  球队 = c(
    "OKC", "CHA", "LAC", "MIN", "CHI",
    "MEM", "UTA", "IND", "DAL", "LAL",
    "WAS", "WAS", "BKN", "PHI", "LAL"
  ),
  数值 = c(
    20.8, 20.8, 20.9, 21.4, 21.4,
    21.5, 21.5, 21.5, 21.6, 21.7,
    21.9, 21.9, 21.9, 22.0, 22.1)
)
# ==========================================
# 2. 数据处理与绘图
# ==========================================
# 读取 RDS，并直接处理出干净的映射库
df_id <- readRDS("D:/nba/id查询.rds")
df_id_clean <- df_id %>%
  mutate(join_name = clean_name_for_join(.[[1]])) %>% 
  # 强制保留 join_name 和 第二列（并重命名为 player_id）
  select(join_name, player_id = 2) 


df <- data %>%
  mutate(
    abbr      = 球队, 
    join_name = clean_name_for_join(姓名),
    col_x     = col_x_base,
    row_y     = 排名 * 1.0,        
    ymin      = row_y - 0.4,      
    ymax      = row_y + 0.4
  ) %>%
  # 【修改点】：直接 left_join 处理好的 df_id_clean
  left_join(df_id_clean, by = "join_name") %>% 
  mutate(
    player_id = replace_na(as.character(player_id), "0"),
    headshot  = paste0("D:/nba/头像/", player_id, ".png"),
    team_logo = paste0("D:/nba/球队logo/球队logo方/", abbr, ".png"),
    color     = nba_team_colors[abbr],
    bar_color = nba_team_colors_light[abbr],
    bg_color  = nba_team_colors_extra_light[abbr],
    数值_label = sprintf("%.1f", 数值)
  )
p <- ggplot(df) +
  geom_rect(aes(xmin = col_x , xmax = col_x + box_mid, ymin = ymin, ymax = ymax, fill = bg_color), color = NA) +
  geom_rect(aes(xmin = col_x , xmax = col_x + box_mid , ymin = ymin, ymax = ymax, color = color), fill = NA, linewidth = 1.2) +
  geom_rect(aes(xmin = col_x + box_mid + 0.08, xmax = col_x + box_end - 0.08, ymin = ymin, ymax = ymax, fill = bar_color), color = NA) +
  geom_rect(aes(xmin = col_x + box_mid, xmax = col_x + box_end, ymin = ymin - 0.07, ymax = ymax + 0.07, color = color), fill = NA, linewidth = 1.2) +
  
  geom_text(aes(x = col_x + col_rank, y = row_y, label = paste0("#", 排名)), size = 6, fontface = "bold", family = "Consolas") +
  geom_image(aes(x = col_x + col_logo, y = row_y, image = team_logo), size = 0.065) +
  geom_image(aes(x = col_x + col_headshot, y = row_y - 0.09, image = headshot), size = 0.09) +
  
  geom_shadowtext(aes(x = col_x + col_player, y = row_y, label = 姓名, bg.colour = bg_color), size = 9, fontface = "bold", family = "Consolas", colour = "black", bg.r = 0.12) +  
  geom_shadowtext(aes(x = col_x + col_value, y = row_y, label = 数值_label), size = 10, fontface = "bold", colour = "white", bg.colour = "black", family = "Consolas") +
  
  annotate("text", x = col_x_base + col_rank, y = 0.3, label = "排名", size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_base + col_player, y = 0.3, label = "球员", size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_base + col_value, y = 0.3, label = "预测退役年龄", size = 5.5, fontface = "bold", family = "Microsoft YaHei") +
  
  coord_cartesian(clip = "off", xlim = c(-0.1, 8.9), ylim = c(15.5, 0.2)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "floralwhite", colour = NA),
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.margin = margin(10, 5, 10, 5), 
    plot.title = element_markdown(family = "Microsoft YaHei", face = "bold", hjust = 0.5, size = 22, lineheight = 1.3, margin = margin(b = 8)),
    plot.subtitle = element_markdown(family = "Microsoft YaHei", size = 12, face = "bold", color = "#555555", hjust = 0.5, lineheight = 1.6, margin = margin(b = 5))
  ) +
  labs(
    title = "DARKO预测NBA现役退役年龄最小的球员", 
    subtitle = "数据源:darko.app|制图: <span style='font-size:16pt; color:black;'>**俗人罢了**</span>|制作时间：2026.7.17"
  )

ggsave("NBA_Drives_TOP15_9x16_Tight.png", p, width = 9, height = 16, dpi = 300)