library(tidyverse)
library(ggimage)
library(ggrepel)
library(stringi)

# ==============================================================================
# 1. 颜色配置 (保持浅色系风格)
# ==============================================================================
nba_team_colors_light <- c(
  "ATL"="#F4A6A6", "BOS"="#66B266", "BKN"="#666666", "CHA"="#66B2C1", "CHI"="#F27A8E", 
  "CLE"="#C5667A", "DAL"="#66A3D1", "DEN"="#FFD76A", "DET"="#7893C9", "GSW"="#FFD36F", 
  "HOU"="#F27A8E", "IND"="#FED584", "LAC"="#667D96", "LAL"="#9D7FCA", "MEM"="#9DB4D6", 
  "MIA"="#D1667A", "MIL"="#4D8C4D", "MIN"="#5D7AAB", "NOP"="#bc8a5f", "NYK"="#F9B57D", 
  "OKC"="#66B8E6", "ORL"="#66B2E6", "PHI"="#66A3D1", "PHX"="#EF9C6C", "POR"="#F4A6A6", 
  "SAC"="#9D7FCA", "SAS"="#E1E7EB", "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)

# ==============================================================================
# 2. 数据处理 (确保排序逻辑正确)
# ==============================================================================
df_raw <- read_csv("D:/nba/nba_injury_2025.csv", show_col_types = FALSE)

df_injury <- df_raw %>%
  select(Player = 1, Team = 2, Cash_Raw = 3) %>%
  mutate(
    Player_Clean = stringi::stri_trans_general(Player, id = "Latin-ASCII") %>% str_trim(),
    Cash_Num = parse_number(as.character(Cash_Raw)),
    Lost_Salary_M = Cash_Num / 1000000
  ) %>%
  filter(!is.na(Lost_Salary_M), Lost_Salary_M > 0)

df_team_totals <- df_injury %>%
  group_by(Team) %>%
  summarise(Team_Total_M = sum(Lost_Salary_M, na.rm = TRUE)) %>%
  arrange(desc(Team_Total_M))

final_df <- df_injury %>%
  mutate(team_ordered = factor(Team, levels = df_team_totals$Team)) %>%
  group_by(Team) %>%
  mutate(
    is_leader = (Lost_Salary_M == max(Lost_Salary_M, na.rm = TRUE)),
    local_logo_path = paste0("D:/nba/球队logo/队标/", Team, ".png")
  ) %>%
  ungroup()

# ==============================================================================
# 3. 绘图 (无水印 + 标题左对齐版)
# ==============================================================================
y_max <- ceiling(max(final_df$Lost_Salary_M, na.rm = TRUE) / 5) * 5 + 10

p <- ggplot(final_df, aes(x = team_ordered, y = Lost_Salary_M)) +
  
  # 1. 队标 (紧贴 X 轴下方)
  geom_image(
    data = final_df %>% distinct(team_ordered, local_logo_path),
    aes(x = team_ordered, y = -1.5, image = local_logo_path), 
    size = 0.025, asp = 16/9
  ) +
  # 0. 全局专属背景水印 (底层)
  annotate("text", x = mid_point, y = y_max / 2, 
           label = "俗人罢了", alpha = 0.04, size = 50, angle = 30, family = "Microsoft YaHei") +
  
  # 2. 球队总额 (Consolas 数字)
  geom_text(
    data = df_team_totals %>% mutate(team_ordered = factor(Team, levels = Team)),
    aes(x = team_ordered, y = -3.5, label = paste0(round(Team_Total_M, 1), "M")),
    size = 3.5, fontface = "bold", color = "black", family = "Consolas"
  ) +
  
  # 3. 球员数据分布点
  geom_point(
    aes(fill = Team), 
    shape = 21, color = "black", size = 4, alpha = 0.7, stroke = 0.3
  ) +
  
  # 4. 标注损失最大的苦主姓名
  geom_text_repel(
    data = subset(final_df, is_leader),
    aes(label = Player_Clean),
    size = 4.5, fontface = "bold", color = "black", family = "Consolas",
    box.padding = 0.6, point.padding = 0.3,
    nudge_y = 3, direction = "y", segment.size = 0.2, segment.color = "gray50",
    max.overlaps = Inf
  ) +
  
  scale_fill_manual(values = nba_team_colors_light) +
  scale_y_continuous(
    breaks = seq(0, y_max, by = 10),
    labels = function(x) paste0("$", x, "M")
  ) +
  
  # 全局样式配置
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
    panel.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_text(family = "Consolas", face = "bold", size = 11),
    
    # 标题位置设置
    plot.title.position = "plot", 
    plot.title = element_text(size = 26, face = "bold", hjust = 0), 
    plot.subtitle = element_text(color = "#A1A1A1", size = 13, hjust = 0, margin = margin(b = 20)),
    
    axis.title.y = element_text(face = "bold", size = 12),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20, unit = "pt"),
    legend.position = "none"
  ) +
  
  coord_cartesian(ylim = c(-5, y_max), clip = 'off') +
  
  labs(
    title = "NBA 本赛季球员缺席导致薪资流失金额分布图",
    subtitle = "球队从左至右按损失总额降序排列 | 仅标注各队损失金额最高的球员|截止：2026.4.6|数据源：spotrac|制作：俗人罢了",
    x = NULL, y = "损失薪资估值 (单位: 百万美元)"
  )

# ==============================================================================
# 4. 保存图片
# ==============================================================================
ggsave("NBA_2026_Lost_Salary_NoWatermark.png", plot = p, width = 16, height = 9, dpi = 300)