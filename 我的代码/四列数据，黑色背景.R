library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
library(stringi)

# -----------------------------
# 核心清洗函数与 ID 映射
# -----------------------------
clean_name_for_join <- function(name) {
  name %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>% 
    tolower() %>%                                       
    str_remove_all("[^a-z]")                            
}

# 读取本地 RDS 字典
df_id <- readRDS("D:/nba/id查询.rds") %>%
  mutate(join_name = clean_name_for_join(.[[1]])) %>% 
  dplyr::select(join_name, player_id = 2)

# -----------------------------
# 基础参数设置
# -----------------------------
col_x_left <- -0.2 
col_x_right <- 12 
col_rank <- 0.6
col_team <- 1.8
col_player <- 6
col_value <- 10.0
bg_dark <- "floralwhite" 

# 球队主色映射
nba_team_colors <- c(
  "ATL"="#E03A3E", "BOS"="#007A33", "BKN"="#000000", "CHA"="#00788C", "CHI"="#CE1141", 
  "CLE"="#860038", "DAL"="#00538C", "DEN"="#0D2240", "DET"="#1D428A", "GSW"="#FFB81C", 
  "HOU"="#CE1141", "IND"="#FDBB30", "LAC"="#002147", "LAL"="#552583", "MEM"="#5D76A9", 
  "MIA"="#98002E", "MIL"="#00471B", "MIN"="#0C2340", "NOP"="#8C4A24", "NYK"="#F58426", 
  "OKC"="#007AC1", "ORL"="#0077C0", "PHI"="#006BB6", "PHX"="#E56020", "POR"="#E03A3E", 
  "SAC"="#5A2D81", "SAS"="#C4CED4", "TOR"="#CE1141", "UTA"="#002B5C", "WAS"="#E31837"
)

nba_team_colors_light <- c(
  "ATL"="#F4A6A6", "BOS"="#66B266", "BKN"="#666666", "CHA"="#66B2C1", "CHI"="#F27A8E", 
  "CLE"="#C5667A", "DAL"="#66A3D1", "DEN"="#4A5870", "DET"="#7893C9", "GSW"="#FFD36F", 
  "HOU"="#F27A8E", "IND"="#FED584", "LAC"="#667D96", "LAL"="#9D7FCA", "MEM"="#9DB4D6", 
  "MIA"="#D1667A", "MIL"="#4D8C4D", "MIN"="#5D7AAB", "NOP"="#bc8a5f", "NYK"="#F9B57D", 
  "OKC"="#66B8E6", "ORL"="#66B2E6", "PHI"="#66A3D1", "PHX"="#EF9C6C", "POR"="#F4A6A6", 
  "SAC"="#9D7FCA", "SAS"="#E1E7EB", "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)

# 1. 加入以前的极淡配色 (增加了 PHX 映射防止原色卡中的 PHX 与 PHX 不一致)
team_colors_extra_light <- c(
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

# -----------------------------
# 数据录入（全新极简英文格式）
# -----------------------------
quarter1 <- tibble(
  球队 = c("NYK", "SAS", "ORL", "TOR", "BOS"),
  球员 = c("Jalen Brunson", "Victor Wembanyama", "Paolo Banchero", "Scottie Barnes", "Jayson Tatum"),
  Value_Raw = c(7.9, 7.0, 6.9, 6.9, 6.8),
  panel = "Top-Left",      # 第一节：左上
  relative_row = 1:5
)

quarter2 <- tibble(
  球队 = c("ATL", "POR", "PHX", "OKC", "ORL"),
  球员 = c("CJ McCollum", "Deni Avdija", "Jalen Green", "Shai Gilgeous-Alexander", "Paolo Banchero"),
  Value_Raw = c(8.2, 8.0, 7.5, 7.5, 7.3),
  panel = "Bottom-Left",   # 第二节：左下
  relative_row = 1:5
)

quarter3 <- tibble(
  球队 = c("BOS", "DEN", "OKC", "PHX", "CLE"),
  球员 = c("Jaylen Brown", "Nikola Jokić", "Shai Gilgeous-Alexander", "Devin Booker", "Donovan Mitchell"),
  Value_Raw = c(11.5, 10.2, 8.9, 7.8, 7.7),
  panel = "Top-Right",     # 第三节：右上
  relative_row = 1:5
)

quarter4 <- tibble(
  球队 = c("NYK", "PHX", "DET", "TOR", "LAL"),
  球员 = c("Jalen Brunson", "Dillon Brooks", "Cade Cunningham", "RJ Barrett", "LeBron James"),
  Value_Raw = c(9.9, 8.8, 7.8, 7.6, 7.4),
  panel = "Bottom-Right",  # 第四节：右下
  relative_row = 1:5
)


# -----------------------------
# 数据合并与坐标计算 (剥离繁琐的正则，逻辑更轻量)
# -----------------------------
y_offset_bottom <- 7

df <- bind_rows(quarter1, quarter2, quarter3, quarter4) %>%
  mutate(
    # 因为数据已经是纯净版，直接赋值即可
    team_en = 球队,
    姓名 = 球员,
    姓名_en = 球员,
    
    排名 = paste0("#", relative_row),
    col_x = ifelse(str_detect(panel, "Left"), col_x_left, col_x_right),
    base_y = relative_row * 1.3,
    row_y = ifelse(str_detect(panel, "Bottom"), base_y + y_offset_bottom, base_y),
    ymin = row_y - 0.49, ymax = row_y + 0.49,
    
    # 完美的清洗与 RDS 匹配逻辑
    join_name = clean_name_for_join(姓名_en)
  ) %>%
  left_join(df_id, by = "join_name") %>%
  mutate(
    player_id = replace_na(as.character(player_id), "0"),
    headshot = paste0("D:/nba/头像/", player_id, ".png"),
    headshot = ifelse(file.exists(headshot), headshot, NA),
    
    team_logo = paste0("D:/nba/球队logo/球队logo方/", team_en, ".png"),
    color = nba_team_colors[team_en],
    bar_color = nba_team_colors_light[team_en],
    # 应用淡色字典
    bg_color = team_colors_extra_light[team_en], 
    数值_label = sprintf("%.1f", Value_Raw)
  )

# =======================================================================
# 绘图部分 (纯净深色模式)
# =======================================================================
p <- ggplot(df) +
  geom_rect(aes(xmin = col_x + 0.2, xmax = col_x + 9.1, ymin = row_y - 0.49, ymax = row_y + 0.49, fill = bg_color), color = NA) +
  geom_rect(aes(xmin = col_x + 0.2, xmax = col_x + 9, ymin = ymin, ymax = ymax, color = color), fill = NA, linewidth = 1.6) +
  geom_rect(aes(xmin = col_x + 9.1, xmax = col_x + 10.9, ymin = row_y - 0.49, ymax = row_y + 0.49, fill = bar_color), color = NA) +
  geom_rect(aes(xmin = col_x + 9.0, xmax = col_x + 11.0, ymin = row_y - 0.55, ymax = row_y + 0.55, color = color), fill = NA, linewidth = 1.4) +
  geom_text(aes(x = col_x + 0.7, y = row_y, label = 排名), size = 8, fontface = "bold", family = "Consolas", color = "black") +
  geom_image(aes(x = col_x + 1.8, y = row_y, image = team_logo), size = 0.07) +
  geom_image(aes(x = col_x + 3, y = row_y - 0.1 , image = headshot), size = 0.105) +
  geom_shadowtext(aes(x = col_x + col_player, y = row_y, 
                      label = 姓名, 
                      bg.colour = bg_color), 
                  size = 8, 
                  fontface = "bold", 
                  family = "Consolas",
                  colour = "black",     
                  bg.r = 0.12) +  
  geom_shadowtext(aes(x = col_x + col_value, y = row_y, label = 数值_label),
                  size = 8, fontface = "bold",
                  colour = "white", bg.colour = "black", family = "Consolas") +
  
  scale_fill_identity() + scale_color_identity() +
  
  # 表头
  annotate("text", x = col_x_left + col_rank, y = +0.4, label = "排名", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_team, y = +0.4, label = "球队", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_player, y = +0.4, label = "球员", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_value, y = +0.4, label = "第一节场均得分", size = 7, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  annotate("text", x = col_x_left + col_rank, y = y_offset_bottom + 0.4, label = "排名", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_team, y = y_offset_bottom + 0.4, label = "球队", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_player, y = y_offset_bottom + 0.4, label = "球员", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_value, y = y_offset_bottom + 0.4, label = "第二节场均得分", size = 7, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  annotate("text", x = col_x_right + col_rank, y = +0.4, label = "排名", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_team, y = + 0.4, label = "球队", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_right + col_player, y = +0.4, label = "球员", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_right + col_value, y = +0.4, label = "第三节场均得分", size = 7, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  annotate("text", x = col_x_right + col_rank, y = y_offset_bottom + 0.4, label = "排名", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_team, y = y_offset_bottom + 0.4, label = "球队", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_right + col_player, y = y_offset_bottom + 0.4, label = "球员", size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_right + col_value, y = y_offset_bottom + 0.4, label = "第四节场均得分", size = 7, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  coord_cartesian(clip = "off", xlim = c(-2, 24.5)) +
  scale_y_reverse() + theme_void() +
  theme(
    panel.background = element_rect(fill = bg_dark, colour = NA),
    plot.background = element_rect(fill = bg_dark, colour = NA),
    plot.margin = margin(20, -100, -20, -100),
    plot.title = element_markdown(
      family = "Microsoft YaHei",
      face = "bold",
      hjust = 0.5, size = 22,
      lineheight = 1.25,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      family = "Microsoft YaHei",
      size = 15, face = "bold", color = "#555555",
      hjust = 0.5,
      lineheight = 1.6,
      margin = margin(b = 5)
    )
  ) +
  labs(title = "<span style='font-size:30pt;'>NBA2025-26赛季季后赛单节场均得分王TOP5</span>",
       subtitle = "数据源：NBA官网 | 制图：<span style='color:black;'>**俗人罢了**</span> ")

ggsave("NBA_Quarter_Scoring_Final_Success.png", p, width = 16, height = 12, dpi = 300)