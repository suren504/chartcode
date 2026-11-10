library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
library(stringi) # 必须加

clean_name_for_join <- function(name) {
  name %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("[^a-z0-9]", "")
}

col_x_base <- 0.5 

col_rank <- 0.65      
col_logo <- 1.8       
col_headshot <- 3.0   
col_player <- 5.8     
col_value <- 9.5    

box_mid <- 8   
box_end <- 11.0  

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


df_id <- readRDS("D:/nba/id查询.rds")

df_id_clean <- df_id %>%
  mutate(join_name = clean_name_for_join(.[[1]])) %>% 
  select(join_name, player_id = 2) 


# ==========================================
# 数据
# ==========================================
data <- tibble(
  排名 = 1:10,
  
  姓名 = c(
    "LeBron James",
    "Russell Westbrook",
    "James Harden",
    "Al Horford",
    "Kevin Durant",
    "Jeff Green",
    "Mike Conley",
    "DeMar DeRozan",
    "Kyle Lowry",
    "Nicolas Batum"
  ),
  
  球队 = c(
    "LAL",
    "SAC",
    "CLE",
    "GSW",
    "HOU",
    "HOU",
    "MIN",
    "SAC",
    "PHI",
    "LAC"
  ),
  
  数值 = c(
    1923,
    1438,
    1412,
    1382,
    1373,
    1355,
    1348,
    1333,
    1328,
    1284
  ),
  
  最后一战 = c(
    "2026/5/11",
    "2026/3/19",
    "2026/5/25",
    "2026/4/17",
    "2026/4/21",
    "2026/4/26",
    "2026/5/15",
    "2026/4/5",
    "2026/4/30",
    "2026/4/12"
  )
)


# ==========================================
# 数据清洗与坐标映射
# ==========================================
df <- data %>%
  mutate(
    abbr      = 球队, 
    join_name = clean_name_for_join(姓名),
    col_x     = col_x_base,
    
    # 完全保留原来的纵向排版
    row_y     = 排名 * 1.8,    
    ymin      = row_y - 0.75,   
    ymax      = row_y + 0.75
  ) %>%
  left_join(df_id_clean, by = "join_name") %>%
  mutate(
    player_id = replace_na(as.character(player_id), "0"),
    
    headshot  = paste0(
      "D:/nba/头像/",
      player_id,
      ".png"
    ),
    
    team_logo = paste0(
      "D:/nba/球队logo/球队logo方/",
      abbr,
      ".png"
    ),
    
    color     = nba_team_colors[abbr],
    bar_color = nba_team_colors_light[abbr],
    bg_color  = nba_team_colors_extra_light[abbr],
    
    数值_label = as.character(数值)
  )


# ==========================================
# 绘图
# ==========================================
p <- ggplot(df) +
  
  # 第1层：底层背景与边框
  geom_rect(
    aes(
      xmin = col_x,
      xmax = col_x + box_mid,
      ymin = ymin,
      ymax = ymax,
      fill = bg_color
    ),
    color = NA
  ) +
  
  geom_rect(
    aes(
      xmin = col_x,
      xmax = col_x + box_mid,
      ymin = ymin,
      ymax = ymax,
      color = color
    ),
    fill = NA,
    linewidth = 1.6
  ) +
  
  
  # 第2层：数值条背景与边框
  geom_rect(
    aes(
      xmin = col_x + box_mid + 0.1,
      xmax = col_x + box_end - 0.1,
      ymin = ymin,
      ymax = ymax,
      fill = bar_color
    ),
    color = NA
  ) +
  
  geom_rect(
    aes(
      xmin = col_x + box_mid,
      xmax = col_x + box_end,
      ymin = ymin - 0.1,
      ymax = ymax + 0.1,
      color = color
    ),
    fill = NA,
    linewidth = 1.6
  ) +
  
  
  # 排名
  geom_text(
    aes(
      x = col_x + col_rank,
      y = row_y,
      label = paste0("#", 排名)
    ),
    size = 12,
    fontface = "bold",
    family = "Consolas"
  ) +
  
  
  # 球队 Logo
  geom_image(
    aes(
      x = col_x + col_logo,
      y = row_y,
      image = team_logo
    ),
    size = 0.085
  ) +
  
  
  # 球员头像
  geom_image(
    aes(
      x = col_x + col_headshot,
      y = row_y - 0.11,
      image = headshot
    ),
    size = 0.12
  ) +
  
  
  # ==========================================
# 球员姓名
# 稍微向上移动，为下面日期留空间
# ==========================================
geom_shadowtext(
  aes(
    x = col_x + col_player,
    y = row_y - 0.18,
    label = 姓名,
    bg.colour = bg_color
  ),
  size = 10,
  fontface = "bold",
  family = "Consolas",
  colour = "black",
  bg.r = 0.12
) +
  
  
  # ==========================================
# 最后一战日期
# 小字号 + 灰色
# ==========================================
geom_text(
  aes(
    x = col_x + col_player,
    y = row_y + 0.40,
    label = 最后一战
  ),
  size = 7.5,
  family = "Consolas",
  colour = "gray60"
) +
  
  
  # 累计出战场次
  geom_shadowtext(
    aes(
      x = col_x + col_value,
      y = row_y,
      label = 数值_label
    ),
    size = 12,
    fontface = "bold",
    colour = "white",
    bg.colour = "black",
    family = "Consolas"
  ) +
  
  
  # ==========================================
# 表头
# ==========================================
annotate(
  "text",
  x = col_x_base + col_rank,
  y = 0.4,
  label = "排名",
  size = 8,
  fontface = "bold",
  family = "Microsoft YaHei"
) +
  
  annotate(
    "text",
    x = col_x_base + col_player,
    y = 0.4,
    label = "球员（最后一战时间）",
    size = 7.5,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  annotate(
    "text",
    x = col_x_base + col_logo,
    y = 0.4,
    label = "球队",
    size = 8,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  annotate(
    "text",
    x = col_x_base + col_value,
    y = 0.4,
    label = "累计出战场次",
    size = 7.5,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  
  # ==========================================
# 坐标
# ==========================================
coord_cartesian(
  clip = "off", 
  xlim = c(0.5, 11.5), 
  ylim = c(19.5, -0.5) 
) +
  
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_identity() +
  scale_color_identity() +
  
  
  # ==========================================
# 主题
# ==========================================
theme_void() +
  
  theme(
    panel.background = element_rect(
      fill = "floralwhite",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "floralwhite",
      colour = NA
    ),
    
    plot.margin = margin(15, 0, 0, 0), 
    
    plot.title = element_markdown(
      family = "Microsoft YaHei",
      face = "bold",
      hjust = 0.5,
      size = 30, 
      lineheight = 1.3,
      margin = margin(b = 5)
    ),
    
    plot.subtitle = element_markdown(
      family = "Microsoft YaHei",
      size = 16,
      face = "bold",
      color = "#555555",
      hjust = 0.5,
      lineheight = 1.6,
      margin = margin(b = 0)
    )
  ) +
  
  
  # ==========================================
# 标题
# ==========================================
labs(
  title = "NBA2025-26赛季在役球员职业生涯累计出战场次TOP10", 
  
  subtitle = paste0(
    "数据源:darko.app|制图: ",
    "<span style='font-size:18pt; color:black;'>",
    "**俗人罢了**",
    "</span>",
    "|制图时间：2026/8/17"
  )
)


# ==========================================
# 导出
# ==========================================
ggsave(
  "NBA_2025-26_在役球员累计出战场次TOP10.png",
  p,
  width = 12,
  height = 16,
  dpi = 300
)