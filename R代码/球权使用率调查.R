library(tidyverse)
library(ggimage)
library(shadowtext)
library(scales)
library(stringi)

# ==============================================================================
# 1. 颜色与映射配置 (保持不变)
# ==============================================================================
nba_team_colors_light <- c(
  "ATL"="#F4A6A6", "BOS"="#66B266", "BKN"="#666666", "CHA"="#66B2C1", "CHI"="#F27A8E", 
  "CLE"="#C5667A", "DAL"="#66A3D1", "DEN"="#FFD76A", "DET"="#7893C9", "GSW"="#FFD36F", 
  "HOU"="#F27A8E", "IND"="#FED584", "LAC"="#667D96", "LAL"="#9D7FCA", "MEM"="#9DB4D6", 
  "MIA"="#D1667A", "MIL"="#4D8C4D", "MIN"="#5D7AAB", "NOP"="#bc8a5f", "NYK"="#F9B57D", 
  "OKC"="#66B8E6", "ORL"="#66B2E6", "PHI"="#66A3D1", "PHX"="#EF9C6C", "POR"="#F4A6A6", 
  "SAC"="#9D7FCA", "SAS"="#E1E7EB", "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)

team_mapping <- tribble(
  ~team_cn, ~file_name, ~abbr,
  "掘金", "Nuggets", "DEN",   "雷霆", "Thunder", "OKC",
  "雄鹿", "Bucks", "MIL",     "马刺", "Spurs", "SAS",
  "快船", "Clippers", "LAC",  "勇士", "Warriors", "GSW",
  "湖人", "Lakers", "LAL",    "骑士", "Cavaliers", "CLE",
  "76人", "76ers", "PHI",     "老鹰", "Hawks", "ATL", 
  "活塞", "Pistons", "DET",   "凯尔特人", "Celtics", "BOS",
  "火箭", "Rockets", "HOU",   "灰熊", "Grizzlies", "MEM",
  "爵士", "Jazz", "UTA",      "魔术", "Magic", "ORL",
  "篮网", "Nets", "BKN",      "黄蜂", "Hornets", "CHA",
  "森林狼", "Timberwolves", "MIN", "猛龙", "Raptors", "TOR",
  "开拓者", "Trail Blazers", "POR", "尼克斯", "Knicks", "NYK",
  "国王", "Kings", "SAC",     "独行侠", "Mavericks", "DAL",
  "太阳", "Suns", "PHX",      "步行者", "Pacers", "IND",
  "热火", "Heat", "MIA",      "鹈鹕", "Pelicans", "NOP",
  "公牛", "Bulls", "CHI",     "奇才", "Wizards", "WAS"
)

# ==============================================================================
# 2. 读取数据 (保持不变)
# ==============================================================================
raw_data <- read_csv("D:/nba/球权占比.csv", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

processed_raw <- raw_data %>%
  select(1, 2, 3) %>%
  set_names(c("player", "team", "load")) %>%
  mutate(load = as.numeric(load)) %>%
  filter(!is.na(load))

# ==============================================================================
# 3. 数据关联与处理 (保持你确认过的“绝对正确”逻辑)
# ==============================================================================
final_df <- processed_raw %>%
  left_join(team_mapping, by = c("team" = "team_cn")) %>%
  filter(!is.na(abbr)) %>%
  group_by(team) %>%
  mutate(
    max_load = max(load, na.rm = TRUE),
    is_leader = (load == max_load),
    
    # --- 头像处理逻辑 ---
    headshot_path = player %>%
      str_replace_all("č", "c") %>%      
      str_replace_all("ć", "c") %>% 
      str_replace_all("ū", "u") %>%   
      str_replace_all("š", "s") %>%
      str_replace_all("(?i)Jr\\b\\.", "Jr") %>%      
      str_replace_all(" ", "_") %>%
      paste0("D:/nba/球员头像/", ., ".png") %>%
      { ifelse(file.exists(.), ., NA) },
    
    # --- LOGO 处理逻辑 ---
    local_logo_path = file_name %>%
      paste0("D:/nba/球队logo/队标/", ., ".png") %>%
      { ifelse(file.exists(.), ., NA) }
  ) %>%
  ungroup() %>%
  mutate(team_ordered = fct_reorder(team, -max_load))

# ==============================================================================
# 4. 绘图 (终极修正版：Logo 回归)
# ==============================================================================
ggplot(final_df, aes(x = team_ordered, y = load)) +
  
  geom_image(
    data = final_df %>% 
      distinct(team_ordered, local_logo_path) %>% 
      filter(!is.na(local_logo_path)),
    aes(x = team_ordered, y = 6, image = local_logo_path),
    size = 0.04, 
    asp = 1.8
  ) +
  
  # 2. 非球霸球员
  geom_point(
    data = subset(final_df, !is_leader),
    aes(fill = abbr), 
    shape = 21, color = "black", size = 4.5, alpha = 0.6, stroke = 0.3
  ) +
  
  # 3. 球霸头像
  geom_image(
    data = subset(final_df, is_leader & !is.na(headshot_path)),
    aes(image = headshot_path),
    size = 0.039, asp = 1.8
  ) +
  
  scale_fill_manual(values = nba_team_colors_light) +
  

  scale_y_continuous(
    breaks = seq(10, 70, 10), 
    labels = number_format(accuracy = 1)
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(), 
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(b = 5)), 
    plot.subtitle = element_text(color = "gray50", size = 13, hjust = 0.5, margin = margin(b = 5)),
    axis.title.y = element_text(face = "bold", size = 12),
    plot.margin = margin(t = 10, r = 10, b = 60, l = 10, unit = "pt"),
    legend.position = "none"
  ) +
  
  coord_cartesian(ylim = c(10, 75), clip = 'off') +
  
  labs(
    title = "NBA本赛季各队球权使用率分布图 谁是大球霸？谁又更团队？",
    subtitle = "头像代表队内球权使用率最高的球员其余球员以圆点表示|球权使用率为BBE的Offensive Load|截止1.28|制作：俗人罢了",
    x = NULL, y = "球权使用率"
  )

ggsave("NBA_2026_Fixed_Logo_Return.png", width = 18, height = 10, dpi = 600)