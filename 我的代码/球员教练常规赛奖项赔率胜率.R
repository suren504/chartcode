library(tidyverse)
library(ggimage)
library(showtext)
library(scales)
library(ragg)
library(cropcircles)
library(magick)
library(ggrepel) 

# ==============================================================================
# 0. 字体与颜色设置
# ==============================================================================
showtext_auto()
font_add("YaHei", "msyh.ttc")

# NBA多巴胺配色
nba_colors <- c(
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

# ==============================================================================
# 1. 数据准备
# ==============================================================================
library(tidyverse)

plot_data <- tibble(
  奖项类型 = c(
    rep("MVP", 9), rep("DPOY", 9), rep("最佳教练", 9), 
    rep("最快进步球员", 9), rep("最佳新秀", 5), rep("最佳第六人", 9)
  ),
  球队 = c(
    "OKC", "DEN", "DET", "DAL", "BOS", "NYK", "MIN", "GSW", "PHI",
    # DPOY 球队顺序同步修改
    "OKC", "MIN", "TOR", "DET", "HOU", "MIA", "SAS", "BOS", "CLE",
    "DET", "PHX", "BOS", "SAS", "DEN", "CHA", "TOR", "MIA", "OKC",
    "POR", "ATL", "UTA", "ATL", "DET", "MIL", "ORL", "PHX", "PHX",
    "DAL", "CHA", "PHI", "MEM", "NOP",
    "MIN", "SAS", "MIA", "OKC", "MIN", "CHI", "HOU", "DEN", "PHX"
  ),
  球员_教练 = c(
    "Shai Gilgeous-Alexander", "Nikola Jokic", "Cade Cunningham", "Luka Doncic", "Jaylen Brown", "Jalen Brunson", "Anthony Edwards", "Stephen Curry", "Tyrese Maxey",
    "Chet Holmgren", "Rudy Gobert", "Scottie Barnes", "Ausar Thompson", "Amen Thompson", "Bam Adebayo", "Victor Wembanyama", "Derrick White", "Evan Mobley",
    "J.B. Bickerstaff", "Jordan Ott", "Joe Mazzulla", "Mitch Johnson", "David Adelman", "Charles Lee", "Darko Rajakovic", "Erik Spoelstra", "Mark Daigneault",
    "Deni Avdija", "Jalen Johnson", "Keyonte George", "Nickeil Alexander-Walker", "Jalen Duren", "Ryan Rollins", "Anthony Black", "Dillon Brooks", "Collin Gillespie",
    "Cooper Flagg", "Kon Knueppel", "VJ Edgecombe", "Cedric Coward", "Derik Queen",
    "Naz Reid", "Keldon Johnson", "Jaime Jaquez Jr", "Ajay Mitchell", "Ayo Dosunmu", "Anfernee Simons", "Reed Sheppard", "Tim Hardaway Jr", "Grayson Allen"
  ),
  胜率_percent = c(
    67.74, 23.53, 4.76, 4.76, 2.44, 0.40, 0.40, 0.33, 0.20,
    33.33, 5.88, 2.78, 1.64, 1.23, 0.79, 0.66, 0.66, 0.40,
    57.45, 20.00, 14.29, 9.09, 4.76, 4.76, 2.44, 0.99, 0.40,
    52.38, 29.41, 10.00, 8.33, 4.76, 1.96, 1.64, 1.64, 1.64,
    90.91, 14.29, 1.23, 0.20, 0.20,
    34.48, 20.00, 15.38, 7.69, 6.25, 6.25, 5.88, 3.85, 3.23
  )
) %>%
  filter(胜率_percent >= 1.23)
team_abbr_to_cn <- c(
  "OKC" = "雷霆", "DEN" = "掘金", "DET" = "活塞", "DAL" = "独行侠",
  "BOS" = "凯尔特人", "NYK" = "尼克斯", "MIN" = "森林狼", "GSW" = "勇士",
  "PHI" = "76人", "SAS" = "马刺", "TOR" = "猛龙", "HOU" = "火箭",
  "MIA" = "热火", "CLE" = "骑士", "PHX" = "太阳", "CHA" = "黄蜂",
  "POR" = "开拓者", "ATL" = "老鹰", "UTA" = "爵士", "MIL" = "雄鹿",
  "ORL" = "魔术", "MEM" = "灰熊", "NOP" = "鹈鹕", "CHI" = "公牛",
  "LAL" = "湖人", "LAC" = "快船", "SAC" = "国王", "BKN" = "篮网",
  "IND" = "步行者", "WAS" = "奇才"
)

award_levels_raw <- c("MVP", "DPOY", "最佳教练", "最快进步球员", "最佳新秀", "最佳第六人")
award_labels_final <- c("MVP", "DPOY", "年度最佳教练", "最快进步球员", "最佳新秀", "最佳第六人")

# ==============================================================================
# 2. 数据处理
# ==============================================================================
processed_df <- plot_data %>%
  mutate(
    奖项类型 = factor(奖项类型, levels = award_levels_raw, labels = award_labels_final),
    is_coach = str_detect(as.character(奖项类型), "教练"),
    
    raw_head_path = case_when(
      is_coach ~ {
        team_cn <- team_abbr_to_cn[球队]
        paste0("D:/nba/临时照片/教练/", team_cn, ".png")
      },
      TRUE ~ {
        player_clean <- 球员_教练 %>%
          str_replace_all("č", "c") %>% str_replace_all("ć", "c") %>%
          str_replace_all("ū", "u") %>% str_replace_all("š", "s") %>%
          str_replace_all("(?i)Jr\\b\\.", "Jr") %>% str_replace_all(" ", "_")
        paste0("D:/nba/球员头像/", player_clean, ".png")
      }
    ),
    team_color_hex = nba_colors[as.character(球队)]
  ) %>%
  mutate(
    head_circle = circle_crop(
      images = raw_head_path,
      border_size = 12, 
      border_colour = team_color_hex 
    )
  ) %>%
  group_by(奖项类型) %>%
  mutate(
    x_base = as.numeric(奖项类型),
    x_offset = runif(n(), -0.25, 0.25),
    x_final = x_base + x_offset
  ) %>%
  ungroup()

# ==============================================================================
# 3. 绘图 (最终修正版)
# ==============================================================================
p <- ggplot(processed_df, aes(x = x_final, y = 胜率_percent)) +
  
  # --- 1. 头像层 ---
  geom_image(
    aes(image = head_circle),
    size = 0.06, 
    asp = 1.6   
  ) +
  
  # --- 2. 标签层 (核心修改) ---
  # 逻辑：文字用球队色，描边用白色 (Text=TeamColor, Outline=White)
  # 这样既清晰，又能体现球队归属
  geom_text_repel(
    data = filter(processed_df, 胜率_percent < 10), 
    aes(
      label = 球员_教练, 
      color = 球队 # 【关键】文字颜色自动匹配球队
    ),
    family = "YaHei",
    fontface = "bold",
    size = 14,                # 字号加大
    
    # 【关键】白色描边，解决看不清的问题
    bg.color = "black",       
    bg.r = 0.15,              # 描边粗细
    
    min.segment.length = 0,   
    segment.color = "white", 
    segment.size = 0.35,       
    box.padding = 0.5,        
    point.padding = 0.8,      
    force = 3,                
    max.overlaps = 30,
    show.legend = FALSE       # 隐藏图例
  ) +
  
  # --- 3. 颜色映射 ---
  # 确保 geom_text_repel 能读懂球队颜色
  scale_color_manual(values = nba_colors) +
  
  # --- 4. 坐标轴 ---
  scale_y_sqrt(
    breaks = c(1, 5, 10, 25, 50, 75, 90),
    labels = percent_format(scale = 1),
    limits = c(0, 95)
  ) +
  
  scale_x_continuous(
    breaks = 1:6, 
    labels = award_labels_final,
    expand = c(0.12, 0.12)
  ) +
  
  # --- 5. 主题设置 ---
  theme_minimal(base_family = "YaHei", base_size = 50) +  
  
  theme(
    plot.background = element_rect(fill = 'floralwhite', color = NA),
    panel.grid.minor = element_blank(),
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    panel.grid.major.x = element_line(color = "gray70", linetype = "longdash", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "gray80", linewidth = 0.4),
    
    axis.text.x = element_text(color = "gray20", face = "bold", margin = margin(t = 6), size = 60),
    axis.text.y = element_text(color = "gray50", face = "bold", size = 60),
    
    plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 8), size = 80),
    plot.subtitle = element_text(color = "gray35", hjust = 0, margin = margin(b = 15), size = 56),
    plot.caption = element_text(color = "gray10", hjust = 0.5, margin = margin(t = 20), size = 40),
    
    plot.margin = margin(10, -5, 10, 10)
  ) +
  
  labs(
    title = "NBA 2026赛季常规赛各大奖项 球员&教练赔率排名",
    subtitle = "球员&教练越靠上代表获得该奖项的胜率越高，胜率根据球员现有赔率计算，只显示胜率>1%的球员",
    x = NULL,
    y = "球员与教练获奖概率 ",
    caption = "数据源：rotowire 选取多个博彩平台该球员&教练的最佳赔率 | 数据截至：2026.2.9 | 制作：俗人罢了"
  )

# ==============================================================================
# 4. 保存
# ==============================================================================
ggsave(
  "NBA_Awards_Final.png", 
  plot = p, 
  width = 10, 
  height = 6.25, 
  dpi = 600,
  device = ragg::agg_png 
)