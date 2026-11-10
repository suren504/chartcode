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

# 固定球队 ID、缩写、主色和副色，不依赖外部配色文件。
team_colors <- tibble::tribble(
  ~team_id, ~team_abbreviation, ~team_color, ~team_alternate_color,
  "1610612737", "ATL", "#c8102e", "#fdb927",
  "1610612738", "BOS", "#008348", "#ffffff",
  "1610612751", "BKN", "#000000", "#ffffff",
  "1610612766", "CHA", "#008ca8", "#1d1060",
  "1610612741", "CHI", "#ce1141", "#000000",
  "1610612739", "CLE", "#860038", "#bc945c",
  "1610612742", "DAL", "#0064b1", "#bbc4ca",
  "1610612743", "DEN", "#0e2240", "#fec524",
  "1610612765", "DET", "#1d428a", "#c8102e",
  "1610612744", "GSW", "#fdb927", "#1d428a",
  "1610612745", "HOU", "#ce1141", "#000000",
  "1610612754", "IND", "#0c2340", "#ffd520",
  "1610612746", "LAC", "#12173f", "#c8102e",
  "1610612747", "LAL", "#552583", "#fdb927",
  "1610612763", "MEM", "#5d76a9", "#12173f",
  "1610612748", "MIA", "#98002e", "#000000",
  "1610612749", "MIL", "#00471b", "#eee1c6",
  "1610612750", "MIN", "#266092", "#79bc43",
  "1610612740", "NOP", "#0a2240", "#b4975a",
  "1610612752", "NYK", "#1d428a", "#f58426",
  "1610612760", "OKC", "#007ac1", "#ef3b24",
  "1610612753", "ORL", "#0150b5", "#9ca0a3",
  "1610612755", "PHI", "#1d428a", "#e01234",
  "1610612756", "PHX", "#29127a", "#e56020",
  "1610612757", "POR", "#e03a3e", "#000000",
  "1610612758", "SAC", "#5a2d81", "#6a7a82",
  "1610612759", "SAS", "#000000", "#c4ced4",
  "1610612761", "TOR", "#d91244", "#000000",
  "1610612762", "UTA", "#4e008e", "#79a3dc",
  "1610612764", "WAS", "#e31837", "#002b5c"
)
nba_team_colors <- setNames(team_colors$team_color, team_colors$team_abbreviation)
nba_team_colors_light <- setNames(team_colors$team_alternate_color, team_colors$team_abbreviation)

# 两列 TOP10 已从 DARKO 数据抄录，可直接修改姓名、球队、球员 ID 和数值。
# 数值保留原始美元金额；绘图时四舍五入为整数万。
# sal_market_fixed
data1 <- tibble::tribble(
  ~排名, ~姓名, ~球队, ~player_id, ~数值,
  1, "Nikola Jokic", "DEN", "203999", 101400179.90968999,
  2, "Victor Wembanyama", "SAS", "1641705", 82798106.35851949,
  3, "Shai Gilgeous-Alexander", "OKC", "1628983", 82503515.56730419,
  4, "Kawhi Leonard", "LAC", "202695", 73164862.84264980,
  5, "Cade Cunningham", "DET", "1630595", 69672565.44215730,
  6, "Donovan Mitchell", "CLE", "1628378", 66199219.45613970,
  7, "Anthony Edwards", "MIN", "1630162", 65737235.69834880,
  8, "Jalen Brunson", "NYK", "1628973", 59096744.44249090,
  9, "Jamal Murray", "DEN", "1627750", 57829842.40297820,
  10, "Jayson Tatum", "BOS", "1628369", 57040006.91910420
)

# 合同价值差 = parquet 的 sal_market_fixed − CSV 的新赛季实际薪水。
# CSV 实际薪水单位为百万美元，乘以 1000000 后相减；以下金额均为美元。
# 按球员姓名（含别名）匹配的 433 人重排，缺少任一来源数据者不参与。
# 已抄录预测和实际薪水，方便直接修改；运行时不读取 parquet 或 CSV。
data2 <- tibble::tribble(
  ~排名, ~姓名, ~球队, ~player_id, ~预测薪水, ~实际薪水,
  1, "Victor Wembanyama", "SAS", "1641705", 82798106.35851949, 16900000,
  2, "Nikola Jokic", "DEN", "203999", 101400179.90968999, 59000000,
  3, "Shai Gilgeous-Alexander", "OKC", "1628983", 82503515.56730419, 40800000,
  4, "LeBron James", "PHI", "2544", 37121992.43967960, 3900000,
  5, "Stephon Castle", "SAS", "1642264", 39711139.87172000, 10000000,
  6, "Amen Thompson", "HOU", "1641708", 40767397.72644590, 12300000,
  7, "Moussa Diabate", "CHA", "1631217", 28433565.76828000, 2500000,
  8, "Marcus Smart", "HOU", "203935", 29914841.38527290, 6100000,
  9, "Ajay Mitchell", "OKC", "1642349", 26615618.72758990, 2900000,
  10, "Payton Pritchard", "BOS", "1630202", 31408903.05387930, 7800000
) %>%
  mutate(数值 = 预测薪水 - 实际薪水)

df <- bind_rows(
  data1 %>% mutate(side = "Left"),
  data2 %>% mutate(side = "Right")
) %>%
  mutate(
    abbr      = 球队, 
    col_x     = ifelse(side == "Left", col_x_left, col_x_right),
    
    # 恢复双列图的正常行距和厚度
    row_y     = 排名 * 1.5,        
    ymin      = row_y - 0.6,      
    ymax      = row_y + 0.6
  ) %>%
  mutate(
    # 头像路径：不存在则NA
    headshot = ifelse(
      !is.na(player_id) &
        file.exists(
          paste0("D:/nba/头像/", player_id, ".png")
        ),
      paste0("D:/nba/头像/", player_id, ".png"),
      NA_character_
    ),
    
    
    # 球队logo：NA球队不生成路径
    team_logo = ifelse(
      !is.na(abbr) &
        file.exists(
          paste0("D:/nba/球队logo/球队logo方/", abbr, ".png")
        ),
      paste0("D:/nba/球队logo/球队logo方/", abbr, ".png"),
      NA_character_
    ),
    color     = nba_team_colors[abbr],
    bar_color = nba_team_colors_light[abbr],
    bg_color  = "floralwhite",
    
    
    # 金额按万取整并添加“万”，单位为美元。
    数值_label = ifelse(side == "Right", sprintf("%+.0f万", 数值 / 10000), sprintf("%.0f万", 数值 / 10000))
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
  geom_image(
    data = df %>% filter(球队 != "NA"),
    aes(
      x = col_x + col_logo,
      y = row_y,
      image = team_logo
    ),
    size = 0.08
  )+


geom_image(
  data = df %>% filter(球队 == "NA"),
  aes(
    x = col_x + col_logo,
    y = row_y,
    image = team_logo
  ),
  size = 0.035
)+
  
  geom_image(aes(x = col_x + col_headshot, y = row_y - 0.05, image = headshot),
             size = 0.11) +
  
  # 球员姓名沿用模板背景描边，防止与头像混淆
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
                  colour = "white", bg.colour = "black", family = "Microsoft YaHei") +
  
  scale_fill_identity() +
  scale_color_identity() +
  
  # --- 左侧表头 ---
  annotate("text", x = col_x_left + col_rank, y = +0.5, label = "排名",
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_player, y = +0.5, label = "球员",
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_left + col_value, y = +0.5, label = "新赛季预估薪水", 
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  
  # --- 右侧表头 ---
  annotate("text", x = col_x_right + col_rank, y = +0.5, label = "排名",
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_player, y = +0.5, label = "球员",
           size = 6.5, fontface = "bold", family = "Microsoft YaHei") +
  annotate("text", x = col_x_right + col_value, y = +0.5, label = "新赛季合同价值差", 
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
    title = "DARKO预测NBA新赛季超值合同",
    subtitle = "左：新赛季预估薪水 TOP10 | 右：新赛季合同价值差 TOP10 | 单位：美元<br>制图: <span style='font-size:18pt; color:black;'>**俗人罢了**</span> | 数据源:darko.app"
  )
ggsave("DARKO新赛季超值合同_TOP10_16x12.png", p, width = 16, height = 12, dpi = 300)