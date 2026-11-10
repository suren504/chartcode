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

data1 <- tibble(
  排名 = 1:10,
  姓名 = c("Mouhamed Gueye", "Scottie Barnes", "Naz Reid", "Giannis Antetokounmpo", "Gui Santos",
         "Josh Hart", "Nicolas Batum", "Sidy Cissoko", "Tobias Harris", "Dominick Barlow"),
  球队 = c("ATL", "TOR", "MIN", "MIL", "GSW",
         "NYK", "LAC", "POR", "DET", "PHI"),
  数值 = c(2.4, 2.2, 1.5, 1.3, 1.1,
         1.1, 1.0, 1.0, 1.0, 0.8)
)

# 第二张图为data2
data2 <- tibble(
  排名 = 1:10,
  姓名 = c("Derrick White", "Javonte Green", "Dean Wade", "OG Anunoby", "Shai Gilgeous-Alexander",
         "Brandin Podziemski", "Amen Thompson", "Jaden McDaniels", "Jarred Vanderbilt", "Kawhi Leonard"),
  球队 = c("BOS", "DET", "CLE", "NYK", "OKC",
         "GSW", "HOU", "MIN", "LAL", "LAC"),
  数值 = c(2.6, 2.0, 1.9, 1.9, 1.8,
         1.6, 1.5, 1.5, 1.5, 1.5)
)

# ==========================================
# 2. 强健的 ID 库处理 (解决找不到列名/对象的报错)
# ==========================================
# 读取 RDS，并直接处理出干净的映射库
df_id <- readRDS("D:/nba/id查询.rds")
df_id_clean <- df_id %>%
  # 即使 R 抽风找不到列名，用列索引 [[1]] 定位姓名列
  mutate(join_name = clean_name_for_join(.[[1]])) %>% 
  # 强制保留 join_name 和 第二列（并重命名为 player_id）
  select(join_name, player_id = 2) 

# ==========================================
# 3. 极致合并与素材匹配 (左右双列处理逻辑)
# ==========================================
# 定义左右两列的横向起始坐标 (根据你之前的 16x12 尺寸设定)
col_x_left <- 0 
col_x_right <- 9.5 

df <- bind_rows(
  data1 %>% mutate(side = "Left"),
  data2 %>% mutate(side = "Right")
) %>%
  mutate(
    abbr      = 球队, 
    join_name = clean_name_for_join(姓名),
    col_x     = ifelse(side == "Left", col_x_left, col_x_right),
    
    # 恢复双列图的正常行距和厚度
    row_y     = 排名 * 1.5,        
    ymin      = row_y - 0.6,      
    ymax      = row_y + 0.6
  ) %>%
  # 🌟 关联刚才做好的强健版 ID 库
  left_join(df_id_clean, by = "join_name") %>%
  mutate(
    player_id = replace_na(as.character(player_id), "0"),
    headshot  = paste0("D:/nba/www/", player_id, ".png"),
    team_logo = paste0("D:/nba/球队logo/球队logo方/", abbr, ".png"),
    color     = nba_team_colors[abbr],
    bar_color = nba_team_colors_light[abbr],
    bg_color  = nba_team_colors_extra_light[abbr],
    # 速度是小数，统一保留两位
    数值_label = sprintf("%.2f", 数值)
  )
# -----------------------------
# 3. 绘图引擎
# -----------------------------
p <- ggplot(df) +
  
  geom_rect(aes(xmin = col_x + 0.1, xmax = col_x + box_mid,
                ymin = ymin, ymax = ymax, fill = bg_color), color = NA) +
  
  geom_rect(aes(xmin = col_x + 0.1, xmax = col_x + box_mid - 0.1,
                ymin = ymin, ymax = ymax, color = color), fill = NA, linewidth = 1.6) +
  
  geom_rect(aes(xmin = col_x + box_mid, xmax = col_x + box_end,
                ymin = ymin, ymax = ymax, fill = bar_color), color = NA) +
  
  geom_rect(aes(xmin = col_x + box_mid - 0.1, xmax = col_x + box_end + 0.1,
                ymin = ymin - 0.08, ymax = ymax + 0.08, color = color), fill = NA, linewidth = 1.4) +
  
  geom_text(aes(x = col_x + col_rank, y = row_y, label = 排名),
            size = 6.5, fontface = "bold", family = "Consolas") +
  geom_image(aes(x = col_x + col_logo, y = row_y, image = team_logo),
             size = 0.08) +
  
  geom_image(aes(x = col_x + col_headshot, y = row_y - 0.05, image = headshot),
             size = 0.11) +
  
  # 🌟 修改版：球员姓名（带球队浅色描边，防止与头像混淆）
  geom_shadowtext(aes(x = col_x + col_player, y = row_y, 
                      label = 姓名, 
                      bg.colour = bg_color), 
                  size = 6.5, 
                  fontface = "bold", 
                  family = "Consolas",
                  colour = "black",     
                  bg.r = 0.12) +  
  
  # 数值
  geom_shadowtext(aes(x = col_x + col_value, y = row_y, label = 数值_label),
                  size = 7.5, fontface = "bold",
                  colour = "white", bg.colour = "black", family = "Consolas") +
  
  scale_fill_identity() +
  scale_color_identity() +
  
  # --- 左侧表头 ---
  annotate("text", x = col_x_left + col_rank, y = +0.5, label = "排名",
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_player, y = +0.5, label = "球员",
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_value, y = +0.5, label = "Mobile Forwards", 
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  
  # --- 右侧表头 ---
  annotate("text", x = col_x_right + col_rank, y = +0.5, label = "排名",
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_player, y = +0.5, label = "球员",
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_value, y = +0.5, label = "Versatile Stopper", 
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  
  coord_cartesian(clip = "off", xlim = c(0.1, 17.9)) +
  scale_y_reverse() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "floralwhite", colour = NA),
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.margin = margin(15, 0, 0, 0),
    
    # 标题样式
    plot.title = element_markdown(
      family = "Microsoft YaHei",
      face = "bold",
      hjust = 0.5, size = 28,
      lineheight = 1.25,
      margin = margin(b = 8)
    ),
    # 副标题样式
    plot.subtitle = element_markdown(
      family = "Microsoft YaHei",
      size = 14, face = "bold", color = "#555555",
      hjust = 0.5,
      lineheight = 1.6,
      margin = margin(b = 15)
    )
  ) +
  
  labs(
    title = "NBA本赛季常规赛移动速度最快VS最慢球员", 
    subtitle = "左:移动速度最快球员| 右：移动速度最慢球员<br>制图: <span style='font-size:18pt; color:black;'>**俗人罢了**</span> | 数据源: NBA官网·Avg Speed|至少出战42场场均20分钟"
  )
ggsave("NBA_Gravity_Best_TOP10_16x12.png", p, width = 16, height = 12, dpi = 300)