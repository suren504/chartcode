library(tidyverse)
library(ggpath)
library(shadowtext)

# 使用原代码的主题
theme_f5 <- function (font_size = 9) { 
  theme_minimal(base_size = font_size, base_family = "Roboto") %+replace% 
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      plot.title = element_text(hjust = 0, size = 14, face = 'bold'), 
      plot.subtitle = element_text(color = 'gray65', hjust = 0, margin=margin(2.5,0,5,0), size = 11), 
      plot.caption = element_text(color = 'gray65', margin=margin(0,0,0,0), hjust = 1, size = 6)
    )
}

# 创建快船队史得分数据
clippers_data <- tribble(
  ~rank, ~team, ~player, ~date, ~opponent, ~score,
  1, "洛杉矶快船1", "科怀·伦纳德", "2025.12.29", "底特律活塞", 55,
  1, "洛杉矶快船1", "詹姆斯·哈登", "2025.11.23", "夏洛特黄蜂", 55,
  3, "洛杉矶快船3", "查尔斯·史密斯", "1990.12.2", "丹佛掘金", 52,
  3, "布法罗勇敢者", "鲍勃·麦卡杜", "1974.2.23", "波士顿凯尔特人", 52,
  3, "布法罗勇敢者", "鲍勃·麦卡杜", "1976.3.18", "西雅图超音速", 52,
  6, "布法罗勇敢者", "鲍勃·麦卡杜", "1975.3.19", "休斯顿火箭", 51,
  6, "圣地亚哥快船", "弗里曼·威廉姆斯", "1980.1.20", "菲尼克斯太阳", 51,
  8, "洛杉矶快船2", "路威", "2018.1.11", "金州勇士", 50,
  8, "洛杉矶快船1", "詹姆斯·哈登", "2025.3.6", "底特律活塞", 50,
  8, "布法罗勇敢者", "鲍勃·麦卡杜", "1975.11.21", "克利夫兰骑士", 50
)

# 2. 创建本地文件路径映射及数据处理
df <- clippers_data %>%
  mutate(
    rank_display = paste0("#", rank),
    
    # 修改后的 team_logo 逻辑：使用 == 进行精确匹配，解决“包含关系”导致的错误
    team_logo = case_when(
      team == "洛杉矶快船1" ~ "D:/nba/临时照片/快船队史/洛杉矶快船1.png",
      team == "洛杉矶快船2" ~ "D:/nba/临时照片/快船队史/洛杉矶快船2.png",
      team == "洛杉矶快船3" ~ "D:/nba/临时照片/快船队史/洛杉矶快船3.png",
      team == "布法罗勇敢者" ~ "D:/nba/临时照片/快船队史/布法罗勇敢者.png",
      team == "圣地亚哥快船" ~ "D:/nba/临时照片/快船队史/圣地亚哥快船.png",
      TRUE ~ "D:/nba/临时照片/快船队史/洛杉矶快船2.png" # 默认兜底图
    ), # <--- 注意这里补上了逗号
    
    player_photo = paste0("D:/nba/临时照片/快船队史/", player, ".png"),
    opponent_line = opponent,
    date_line = date,
    
    # 【核心修改部分】
    # 使用 row_number() 确保即便同一个球员在同一天拿了同样分数（极端情况）也能区分
    unique_id = paste(row_number(), player, date, sep = "_"),
    
    # 使用 fct_inorder 确保图表顺序完全按照数据录入的顺序排列
    player_ordered = fct_inorder(unique_id)
  )
# 绘图
p <- df %>% 
  ggplot(aes(x = player_ordered, y = 1)) + 
  facet_wrap(~player_ordered, nrow = 10, scales = 'free_x') + 
  
  # 1. 得分渐变条 (大幅向左平移，消除中间空隙)
  geom_rect(aes(fill = score), 
            xmin = 1.25,    # 【核心修改】从 1.65 改为 1.15，紧贴对手信息
            xmax = 1.34, 
            ymin = .95, 
            ymax = 1.045) +
  
  # 2. 得分数字 (跟随向左平移)
  geom_shadowtext(aes(x = 1.27, label = score), # 【核心修改】从 1.67 改为 1.17 (紧随 xmin)
                  hjust = 0,    
                  fontface = 'bold', 
                  vjust = .65, 
                  family = "Consolas", 
                  size = 5.5) +
  
  # 3. 排名 (调整回 -0.42 保证可见且对齐)
  # 3. 排名 (调整垂直对齐，增大字号，应用 Consolas 字体)
  geom_text(aes(x = player_ordered, label = rank_display),
            hjust = 0, 
            nudge_x = -.52,      # 保持你确认的横向偏移
            vjust = 0.45,        # 【核心修改】从 0 改为 0.45，解决“视觉偏上”问题，实现行内垂直居中
            fontface = 'bold', 
            size = 5.5,          # 【核心修改】字号从 4 变大到 5.5
            family = "Consolas", # 【核心修改】指定 Consolas 字体
            color = 'black') +
  
  # 4. 球队logo
  geom_from_path(aes(path = team_logo), 
                 width = .12, 
                 hjust = 3.1, 
                 vjust = .5) +
  
  # 5. 球员头像 
  geom_from_path(aes(path = player_photo), 
                 width = .15, 
                 hjust = 2.25, 
                 vjust = .5) +
  
  # 6. 球员姓名
  geom_text(aes(x = player_ordered, label = player),
            hjust = 0, 
            nudge_x = -.25, 
            vjust = 0.5,
            fontface = 'bold', size = 4.2) +
  
  # 7. 对手 (放在中间空档)
  geom_text(aes(x = player_ordered, label = opponent_line),
            hjust = 0, 
            nudge_x = 0.05, 
            vjust = -0.1,
            color = 'gray30', size = 3.2, fontface = 'bold') +
  
  # 8. 日期
  geom_text(aes(x = player_ordered, label = date_line),
            hjust = 0, 
            nudge_x = 0.05, 
            vjust = 1.2,
            color = 'gray60', size = 2.7) +
  
  scale_fill_gradient(low = "#E6E9ED", high ="#002147") +
  coord_cartesian(clip = 'off') +
  theme_f5() + 
  theme(
    legend.position = 'none',
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.line.x.bottom = element_line(color = 'black', linewidth = .05),
    axis.line.x.top = element_line(color = 'black', linewidth = .05),
    strip.text = element_blank(), 
    panel.spacing.y = unit(0.1, "lines"), # 稍微拉开一点间距防止拥挤
    panel.grid = element_blank(),
    plot.title = element_text(size = 18.5, hjust = .5),
    plot.subtitle = element_text(size = 8, hjust = .5)
  ) +
  labs(title = "洛杉矶快船队史单场得分榜 伦纳德哈登共享第一", 
       subtitle = "包括布法罗勇敢者和圣地亚哥快船时期|从1970-71赛季统计")

# 保存
ggsave("Clippers_Scoring_Final_Fixed.png", p, width = 6, height = 5, units = 'in', dpi = 600)
browseURL("Clippers_Scoring_Final_Fixed.png")