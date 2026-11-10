library(tidyverse)
library(ggchicklet) 
library(gganimate) 
library(ggpath) 
library(showtext)

# 1. 挂载 Windows 字体
font_add("yahei", "msyh.ttc")  
showtext_auto()

# 2. 自定义主题 (彻底抛弃 axis.text.x = element_path)
# 2. 自定义主题：完美缝合 WSJ 质感与动态 Logo 需求
theme_wsj_custom <- function (font_size = 12) { 
  theme_minimal(base_size = font_size, base_family = "yahei") %+replace% 
    theme(
      # WSJ 标志性米黄色背景
      plot.background = element_rect(fill = "#F8F2E4", color = NA),
      panel.background = element_rect(fill = "#F8F2E4", color = NA),
      
      # 模仿 WSJ：隐藏垂直网格和次要网格，只保留 Y 轴水平点状网格
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(color = "#D1C4AB", linetype = "dotted", linewidth = 0.8),
      
      # 底部黑色坚实基准线
      axis.line.x = element_line(color = "black", linewidth = 0.8),
      
      # 字体颜色微调，贴合 WSJ 的深灰色和纯黑色搭配
      plot.title = element_text(hjust = 0, size = 18, face = 'bold', color = "black", margin = margin(10,0,5,0)), 
      plot.subtitle = element_text(color = '#444444', hjust = 0, margin = margin(0,0,20,0), size = 12), 
      plot.caption = element_text(color = '#666666', margin = margin(10,0,0,0), hjust = 1, size = 9),
      axis.text.y = element_text(color = "black", face = "bold", size = 10),
      
      # 💡 核心保留区：隐藏自带文本，并保留留白给底下画好的 Logo
      axis.text.x = element_blank(), 
      plot.margin = margin(5, 15, 25, 10) 
    )
}

# 3. 底层数据源 (保持不变)
violation_data <- data.frame(
  team = c("ATL", "TOR", "BKN", "OKC", "DET", "CLE", "NOP", "DAL", "MIN", "MIL",
           "MIA", "SAC", "LAL", "PHO", "WSH", "SAS", "IND", "PHI", "POR", "HOU",
           "LAC", "ORL", "BOS", "UTA", "CHI", "GSW", "MEM", "DEN", "NYK", "CHA"),
  avg_24violation = c(0.93, 0.86, 0.83, 0.82, 0.81, 0.80, 0.77, 0.76, 0.75, 0.74,
                      0.73, 0.69, 0.66, 0.62, 0.62, 0.62, 0.62, 0.61, 0.60, 0.60,
                      0.60, 0.59, 0.59, 0.58, 0.56, 0.54, 0.53, 0.50, 0.39, 0.37)
)

shot_data <- data.frame(
  team = c("MIA", "CLE", "TOR", "BKN", "ATL", "CHI", "SAS", "DET", "PHO", "OKC",
           "MIL", "LAL", "ORL", "CHA", "POR", "MEM", "DAL", "MIN", "LAC", "SAC",
           "NOP", "HOU", "DEN", "IND", "BOS", "NYK", "WSH", "GSW", "PHI", "UTA"),
  rush_shot_ratio_pct = c("11.49%", "11.41%", "11.22%", "10.92%", "10.57%", "10.52%", "10.34%", "10.05%", "10.03%", "9.86%",
                          "9.86%", "9.81%", "9.69%", "9.65%", "9.57%", "9.54%", "9.48%", "9.47%", "9.44%", "9.38%",
                          "9.3%", "9.3%", "9.27%", "9.07%", "9.04%", "8.71%", "8.21%", "8.16%", "8.04%", "7.8%")
)

nba_team_colors_light <- c(
  "ATL"="#E03A3E", "BOS"="#008348", "BKN"="#666666",
  "CHA"="#00788C", "CHI"="#F27A8E", "CLE"="#860038",
  "DAL"="#0066B5", "DEN"="#FFD76A", "DET"="#C8102E",
  "GSW"="#FFD36F", "HOU"="#CE1141", "IND"="#FEBC30",
  "LAC"="#12173F", "LAL"="#552583", "MEM"="#9DB4D6",
  "MIA"="#961A31", "MIL"="#4D8C4D", "MIN"="#266092",
  "NOP"="#B89A5B", "NYK"="#F9B57D", "OKC"="#007AC0",
  "ORL"="#66B2E6", "PHI"="#1D428A", "PHO"="#EF9C6C",
  "POR"="#F4A6A6", "SAC"="#592D81", "SAS"="#E1E7EB",
  "TOR"="#BC2126", "UTA"="#668BB2", "WAS"="#F07C8C"
)

team_colors_df <- tibble(
  team = names(nba_team_colors_light),
  team_color = unname(nba_team_colors_light)
) %>% 
  mutate(team = ifelse(team == "WAS", "WSH", team))

# 4. 数据合并与【动态Logo位置计算】
df_merged <- violation_data %>% 
  inner_join(shot_data, by = "team") %>% 
  left_join(team_colors_df, by = "team") %>% 
  mutate(
    rush_shot_val = as.numeric(gsub("%", "", rush_shot_ratio_pct)),
    logo_path = paste0("D:/nba/球队logo/球队logo方/", team, ".png"),
    team_color = ifelse(is.na(team_color), "#808080", team_color)
  )

df_plot <- df_merged %>%
  mutate(logo_fct = fct_reorder(logo_path, desc(avg_24violation))) %>% 
  pivot_longer(
    cols = c(avg_24violation, rush_shot_val),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric_label = case_when(
      metric == "avg_24violation" ~ "图1: 场均逼迫对手 24 秒违例次数",
      metric == "rush_shot_val"   ~ "图2: 逼迫对手压哨仓促出手 (≤4秒) 占比 (%)"
    ),
    # 🚀 魔法核心：为两个量级完全不同的图，分别计算 Logo 的 Y 轴高度！
    # 这样 Logo 就能永远乖乖呆在 0 刻度线下方
    y_logo = case_when(
      metric == "avg_24violation" ~ -0.07,  # 图1的量级是 0-1
      metric == "rush_shot_val"   ~ -0.8    # 图2的量级是 0-12
    )
  )

# 5. 绘图：手动植入 Logo 图层
p <- ggplot(df_plot, aes(x = logo_fct, y = value, group = logo_fct, fill = team_color)) + 
  geom_chicklet(color = 'black', alpha = 0.85, radius = grid::unit(2, "mm")) + 
  geom_text(aes(label = round(value, 2)), vjust = -0.6, size = 4.5, family = "yahei", fontface = "bold") +
  
  # 🚀 将 Logo 作为一个独立的图层画进去，彻底避开坐标轴 Bug
  # width 决定 Logo 的大小，0.035 适合 30 支球队排开
  geom_from_path(aes(y = y_logo, path = logo_fct), width = 0.035) + 
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  scale_fill_identity() + 
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  theme_f5() + 
  
  # 开启裁切外溢，让负数坐标的 Logo 完美显示
  coord_cartesian(clip = 'off') +
  theme(legend.position = 'none') +
  
  transition_states(
    metric_label,
    transition_length = 2,
    state_length = 3  
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') + 
  view_follow(fixed_x = TRUE) + 
  labs(
    x = "", 
    y = "", 
    title = "{closest_state}",
    subtitle = "数据包含：2025-26 赛季 | 反映了各队阵地战防守的“窒息感”",
    caption = "数据源: NBA PBP | 绘图: SuRen"
  )

# 6. 渲染
options(gganimate.dev_args = list(width = 11, height = 6.5, units = 'in', res = 300)) 
cat("正在渲染，这次 Logo 绝对跑不掉了...\n")
animate(p, end_pause = 30, nframes = 100, fps = 15, renderer = gifski_renderer("D:/nba/NBA_Defense_Suffocation_Fix.gif", loop = TRUE))
cat("✅ 成功！\n")