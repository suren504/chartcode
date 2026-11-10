library(tidyverse)
library(gganimate)
library(ggtext)
library(showtext)
library(lubridate)

# ==============================================================================
# 1. 字体与配色 (保持一致)
# ==============================================================================
font_add(family = "Microsoft YaHei", regular = "msyh.ttc")
showtext_auto()

player_colors <- c(
  "亚历山大" = "#66B8E6",
  "约基奇"   = "#FFD76A",
  "东契奇"   = "#9D7FCA",
  "坎宁安"   = "#C5667A",
  "杰伦布朗" = "#66B266"
)

# ==============================================================================
# 2. 数据处理与变速逻辑构建 (关键修改点)
# ==============================================================================
data_path <- "D:/nba/mvp_top5_evolution.csv"
raw_data <- read_csv(data_path, show_col_types = FALSE)

# 定义关键日期
start_season_date <- as.Date("2025-10-21")
jokic_injury_date <- as.Date("2025-12-31")

plot_data <- raw_data %>%
  mutate(date = ymd(date)) %>%
  mutate(name_cn = case_when(
    str_detect(name, "Shai") ~ "亚历山大",
    str_detect(name, "Jokic") ~ "约基奇",
    str_detect(name, "Doncic") ~ "东契奇",
    str_detect(name, "Cunningham") ~ "坎宁安",
    str_detect(name, "Brown") ~ "杰伦布朗",
    TRUE ~ name
  )) %>%
  mutate(label_text = sprintf("%.1f%%", prob_percent)) %>%
  arrange(date) %>%
  
  # --- 核心黑科技：构建变速时间轴 ---
  mutate(day_diff = as.numeric(date - min(date))) %>%
  mutate(season_diff = as.numeric(date - start_season_date)) %>%
  mutate(anim_index = case_when(
    date <= start_season_date ~ as.numeric(date), 
    # --- 修改处：将倍率从 4 改为 3.5 ---
    # 这意味着：常规赛每天在动画里占用的时间，是季前赛每天的 3.5 倍
    TRUE ~ as.numeric(start_season_date) + (season_diff * 3.5) 
  ))

# ==============================================================================
# 3. 极致紧凑主题 (Ultra Tight Theme)
# ==============================================================================
theme_mvp_ultra <- function(font_size = 10) {
  theme_minimal(base_size = font_size, base_family = "Microsoft YaHei") %+replace%
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite'),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linetype = "dashed"),
      
      plot.title = element_markdown(hjust = 0, size = 18, face = "bold", margin = margin(5, 0, 5, 0)),
      plot.subtitle = element_markdown(hjust = 0, size = 10, color = "gray40", lineheight = 1.2, margin = margin(0, 0, 5, 0)),
      
      # 保持你满意的紧凑右边距 (35)
      plot.margin = margin(5, 35, 10, 15), 
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text = element_text(size = 9, color = "gray30"),
      axis.title = element_blank()
    )
}

# ==============================================================================
# 4. 绘图构建 (保持一致)
# ==============================================================================

p <- plot_data %>%
  ggplot(aes(x = date, y = prob_percent, color = name_cn, group = name_cn)) +
  
  # --- 事件 1: 常规赛开始 ---
  geom_vline(xintercept = start_season_date, linetype = "longdash", color = "gray30", linewidth = 0.8) +
  annotate("text", x = start_season_date, y = 88, label = "常规赛开始", 
           family = "Microsoft YaHei", color = "gray20", size = 3, hjust = -0.1) +
  
  # --- 事件 2: 约基奇受伤 ---
  geom_vline(xintercept = jokic_injury_date, linetype = "longdash", color = "gray30", linewidth = 0.8) +
  annotate("text", x = jokic_injury_date, y = 80, label = "约基奇受伤", 
           family = "Microsoft YaHei", color = "gray20", size = 3.5, hjust = -0.1) +
  
  # 线条层
  geom_line(alpha = 0.2, linewidth = 2.5) +
  geom_line(linewidth = 1.2) +
  
  # 标签层
  geom_label(aes(label = paste0(name_cn, " ", label_text)),
             hjust = 0,
             nudge_x = 1, 
             family = "Microsoft YaHei", 
             size = 3.8,
             alpha = 0.95,
             fontface = "bold",
             label.padding = unit(0.2, "lines"), 
             label.size = 0) +
  
  # 坐标轴与比例
  scale_y_continuous(
    limits = c(0, 90), 
    breaks = seq(0, 90, 10), 
    expand = c(0,0),
    labels = function(x) paste0(x, "%") 
  ) +
  scale_x_date(date_labels = "%m月", date_breaks = "1 month") +
  scale_color_manual(values = player_colors) +
  
  coord_cartesian(clip = "off") +
  
  theme_mvp_ultra() +
  theme(legend.position = "none") + 
  
  labs(
    title = "**NBA2025-26赛季五大球员每日MVP赔率变化 亚历山大断档领先**",
    subtitle = "<span style='color:#66B8E6;'>亚历山大</span> • 
                <span style='color:#FFD76A;'>约基奇</span> • 
                <span style='color:#9D7FCA;'>东契奇</span> • 
                <span style='color:#C5667A;'>坎宁安</span> • 
                <span style='color:#66B266;'>杰伦布朗</span>
                <br><span style='font-size:9pt; color:gray60;'>数据来源：CaesarsSportsbook（凯撒体育博彩）|数据截止：2026.2.4|制作：俗人罢了</span>",
    y = "", x = ""
  ) +
  
  transition_reveal(anim_index) 

# ==============================================================================
# 5. 渲染与保存 (变速版 v5)
# ==============================================================================

# 更新文件名为 v5
output_file <- "D:/nba/nba_mvp_race_v5_final_speed.gif"

print("正在渲染 V5 最终版动画 (常规赛速率修正为 3.5倍)...")

animate(p, 
        nframes = 450,      # 保持高帧数以确保流畅
        fps = 25,           
        duration = 15,
        end_pause = 50,     
        width = 1000,
        height = 600,
        res = 120,
        renderer = gifski_renderer(output_file, loop = FALSE))

try(utils::browseURL(output_file), silent = TRUE)
print(paste("最终成品已保存至:", output_file))