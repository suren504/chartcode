library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)


col_x_left <- 0 
col_x_right <- 9.5 

col_rank <- 0.6    
col_logo <- 1.4    
col_headshot <- 2.5  
col_player <- 4.6    
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
# 1. 更新数据：左右各 TOP11
# ==========================================

data1 <- tibble(
  排名 = 1:11,
  姓名 = c(
    "Chet Holmgren",
    "Luguentz Dort",
    "Rudy Gobert",
    "Walker Kessler",
    "Clint Capela",
    "Jaren Jackson Jr.",
    "Shai Gilgeous-Alexander",
    "Kevin Durant",
    "Mark Williams",
    "Nikola Jokic",
    "Myles Turner"
  ),
  球队 = c(
    "OKC", "OKC", "MIN", "LAL", "HOU",
    "UTA", "OKC", "HOU", "PHX", "DEN", "MIL"
  ),
  数值 = c(
    4, 3, 3, 3, 2,
    2, 2, 2, 2, 2, 2
  )
)

data2 <- tibble(
  排名 = 1:11,
  姓名 = c(
    "Rudy Gobert",
    "Alperen Sengun",
    "Jaren Jackson Jr.",
    "Nikola Jokic",
    "Chet Holmgren",
    "Jerami Grant",
    "Ivica Zubac",
    "John Collins",
    "Nikola Vucevic",
    "Cade Cunningham",
    "Deni Avdija"
  ),
  球队 = c(
    "MIN", "HOU", "UTA", "DEN", "OKC",
    "MEM", "IND", "DET", "ORL", "DET", "POR"
  ),
  数值 = c(
    15, 13, 13, 13, 11,
    10, 9, 9, 9, 8, 8
  )
)


# ==========================================
# 2. ID 映射
# ==========================================

df_id <- readRDS("D:/nba/id查询.rds")

name_col <- names(df_id)[1]
id_col   <- names(df_id)[2]

df_id_clean <- df_id %>%
  transmute(
    join_name = clean_name_for_join(.data[[name_col]]),
    player_id = as.character(.data[[id_col]])
  ) %>%
  distinct(join_name, .keep_all = TRUE)


# ==========================================
# 3. 布局微调：TOP10 → TOP11
# ==========================================

col_x_left  <- 0
col_x_right <- 9.5

# 稍微压缩行距，使11行保持原图密度
row_step <- 1.35
row_half <- 0.52


# ==========================================
# 4. 合并数据与素材匹配
# ==========================================

df <- bind_rows(
  data1 %>% mutate(side = "Left"),
  data2 %>% mutate(side = "Right")
) %>%
  mutate(
    abbr      = 球队,
    join_name = clean_name_for_join(姓名),
    
    col_x = if_else(
      side == "Left",
      col_x_left,
      col_x_right
    ),
    
    row_y = 排名 * row_step,
    ymin  = row_y - row_half,
    ymax  = row_y + row_half
  ) %>%
  left_join(
    df_id_clean,
    by = "join_name"
  ) %>%
  mutate(
    # 球员头像候选路径
    headshot_candidate = if_else(
      !is.na(player_id),
      paste0("D:/nba/头像/", trimws(player_id), ".png"),
      NA_character_
    ),
    
    # 图片不存在时设为NA，避免prepare_image警告
    headshot = if_else(
      !is.na(headshot_candidate) &
        file.exists(headshot_candidate),
      headshot_candidate,
      NA_character_
    ),
    
    # 球队Logo路径
    team_logo_candidate = paste0(
      "D:/nba/球队logo/球队logo方/",
      abbr,
      ".png"
    ),
    
    team_logo = if_else(
      file.exists(team_logo_candidate),
      team_logo_candidate,
      NA_character_
    ),
    
    color = unname(
      nba_team_colors[abbr]
    ),
    
    bar_color = unname(
      nba_team_colors_light[abbr]
    ),
    
    bg_color = unname(
      nba_team_colors_extra_light[abbr]
    ),
    
    # 次数显示为整数
    数值_label = as.character(round(数值))
  )


# ==========================================
# 5. 绘图
# ==========================================

p <- ggplot(df) +
  
  geom_rect(
    aes(
      xmin = col_x + 0.1,
      xmax = col_x + box_mid,
      ymin = ymin,
      ymax = ymax,
      fill = bg_color
    ),
    color = NA
  ) +
  
  geom_rect(
    aes(
      xmin = col_x + 0.1,
      xmax = col_x + box_mid - 0.1,
      ymin = ymin,
      ymax = ymax,
      color = color
    ),
    fill = NA,
    linewidth = 1.6
  ) +
  
  geom_rect(
    aes(
      xmin = col_x + box_mid,
      xmax = col_x + box_end,
      ymin = ymin,
      ymax = ymax,
      fill = bar_color
    ),
    color = NA
  ) +
  
  geom_rect(
    aes(
      xmin = col_x + box_mid - 0.1,
      xmax = col_x + box_end + 0.1,
      ymin = ymin - 0.08,
      ymax = ymax + 0.08,
      color = color
    ),
    fill = NA,
    linewidth = 1.4
  ) +
  
  geom_text(
    aes(
      x = col_x + col_rank,
      y = row_y,
      label = 排名
    ),
    size = 6.2,
    fontface = "bold",
    family = "Consolas"
  ) +
  
  # 只绘制实际存在的球队Logo
  geom_image(
    data = df %>% filter(!is.na(team_logo)),
    aes(
      x = col_x + col_logo,
      y = row_y,
      image = team_logo
    ),
    size = 0.075
  ) +
  
  # 只绘制实际存在的球员头像
  geom_image(
    data = df %>% filter(!is.na(headshot)),
    aes(
      x = col_x + col_headshot,
      y = row_y - 0.05,
      image = headshot
    ),
    size = 0.105
  ) +
  
  geom_shadowtext(
    aes(
      x = col_x + col_player,
      y = row_y,
      label = 姓名,
      bg.colour = bg_color
    ),
    size = 7.5,
    fontface = "bold",
    family = "Consolas",
    colour = "black",
    bg.r = 0.12
  ) +
  
  geom_shadowtext(
    aes(
      x = col_x + col_value,
      y = row_y,
      label = 数值_label
    ),
    size = 9,
    fontface = "bold",
    colour = "white",
    bg.colour = "black",
    family = "Consolas"
  ) +
  
  scale_fill_identity() +
  scale_color_identity() +
  
  # 左侧表头
  annotate(
    "text",
    x = col_x_left + col_rank,
    y = 0.35,
    label = "排名",
    size = 6.2,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  annotate(
    "text",
    x = col_x_left + col_player,
    y = 0.35,
    label = "球员",
    size = 6.2,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  annotate(
    "text",
    x = col_x_left + col_value,
    y = 0.35,
    label = "封盖文班亚马次数",
    size = 5.8,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  # 右侧表头
  annotate(
    "text",
    x = col_x_right + col_rank,
    y = 0.35,
    label = "排名",
    size = 6.2,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  annotate(
    "text",
    x = col_x_right + col_player,
    y = 0.35,
    label = "球员",
    size = 6.2,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  annotate(
    "text",
    x = col_x_right + col_value,
    y = 0.35,
    label = "被文班封盖次数",
    size = 5.8,
    fontface = "bold",
    family = "Microsoft YaHei"
  ) +
  
  coord_cartesian(
    clip = "off",
    xlim = c(0.1, 17.9)
  ) +
  
  scale_y_reverse(
    expand = expansion(
      mult = c(0.015, 0.025)
    )
  ) +
  
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
    
    plot.margin = margin(
      15, 0, 5, 0
    ),
    
    plot.title = element_markdown(
      family = "Microsoft YaHei",
      face = "bold",
      hjust = 0.5,
      size = 28,
      lineheight = 1.25,
      margin = margin(b = 8)
    ),
    
    plot.subtitle = element_markdown(
      family = "Microsoft YaHei",
      size = 14,
      face = "bold",
      color = "#555555",
      hjust = 0.5,
      lineheight = 1.6,
      margin = margin(b = 15)
    )
  ) +
  
  labs(
    title = "NBA历史封盖与被文班亚马封盖次数最多的球员TOP11",
    
    subtitle = paste0(
      "左列：封盖文班亚马次数最多的球员｜右列：被文班亚马封盖次数最多的球员<br>",
      "｜数据源：hoopR",
      "｜制图：<span style='font-size:18pt; color:black;'>",
      "**俗人罢了**</span>|",
      "数据范围：2023-24至2025-26赛季"
    )
  )


# ==========================================
# 6. 输出
# ==========================================

ggsave(
  filename = "NBA_Wembanyama_Block_TOP11_16x12.5.png",
  plot = p,
  width = 16,
  height = 12.5,
  dpi = 300
)