# ==============================================================================
# 1. 环境与主题设置
# ==============================================================================
library(tidyverse)
library(teamcolors)
library(ggpath)      # 处理图片
library(showtext)    # 处理字体
library(duckdb)      # 数据库
library(glue)

# --- 字体设置 ---
font_add_google("Noto Sans SC", "noto")
showtext_auto()

# --- 自定义主题 (F5 Style - FloralWhite版) ---
theme_f5_floral <- function (font_size = 9) {
  theme_minimal(base_size = font_size, base_family = "noto") %+replace%
    theme(
      # 背景色
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite'),
      panel.background = element_rect(fill = 'floralwhite', color = NA),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.3),
      
      plot.title = element_text(hjust = 0.5, size = 18, face = 'bold', color = "#111111"),

      plot.subtitle = ggtext::element_markdown(
        color = '#444444', 
        hjust = 0.5, 
        margin = margin(5, 0, 20, 0), 
        size = 11
      ),
      
      plot.caption = element_text(color = 'gray60', margin = margin(10, 0, 0, 0), hjust = 1, size = 7),
      
      # Logo 路径支持
      strip.text = element_path(size = 1, hjust = 0.5), 
      panel.spacing = unit(0.5, "lines"),
      
      axis.text.x = element_text(size = 7, color = "gray40"),
      axis.text.y = element_text(size = 7, color = "gray40")
    )
}

# ==============================================================================
# 2. 准备颜色元数据 (只取颜色，不用Logo链接了)
# ==============================================================================
nba_colors <- teamcolors::teamcolors %>%
  filter(league == "nba") %>%
  mutate(
    name = case_when(
      name == "Los Angeles Clippers" ~ "LA Clippers",
      TRUE ~ name
    )
  ) %>%
  select(name, primary, secondary) # 只需要颜色

# ==============================================================================
# 3. DuckDB 数据提取
# ==============================================================================
parquet_file <- "D:/nba/RStudio/hoopR-nba-data/nba/pbp/parquet/play_by_play_2026.parquet"
logo_base_dir <- "D:/nba/球队logo/队标/"  # 你的本地Logo文件夹路径

con <- dbConnect(duckdb::duckdb())
pbp_db <- tbl(con, paste0("'", parquet_file, "'"))

# 提取每场比赛结果和球队信息
game_logs <- pbp_db %>%
  group_by(game_id) %>%
  summarise(
    game_date = max(game_date),
    
    # 主队
    home_city = max(home_team_name),
    home_mascot = max(home_team_mascot), # 例如 "Nuggets"
    home_score = max(home_score, na.rm = TRUE),
    
    # 客队
    away_city = max(away_team_name),
    away_mascot = max(away_team_mascot), # 例如 "76ers"
    away_score = max(away_score, na.rm = TRUE)
  ) %>%
  collect() %>%
  ungroup()

dbDisconnect(con)

# ==============================================================================
# 4. 数据清洗与路径拼接
# ==============================================================================

# 构建球队全名和本地Logo路径
df_clean <- game_logs %>%
  mutate(
    game_date = as.Date(game_date),
    
    # 拼接全名用于匹配颜色
    home_full = paste(home_city, home_mascot),
    away_full = paste(away_city, away_mascot),
    
    # 修正特殊情况
    home_full = str_replace(home_full, "Los Angeles Clippers", "LA Clippers"),
    away_full = str_replace(away_full, "Los Angeles Clippers", "LA Clippers"),
    
    # 构建本地 Logo 路径
    home_logo_path = paste0(logo_base_dir, home_mascot, ".png"),
    away_logo_path = paste0(logo_base_dir, away_mascot, ".png")
  )

# 长格式转换
df_long <- bind_rows(
  df_clean %>% select(game_date, team = home_full, mascot = home_mascot, logo_path = home_logo_path, score = home_score, opp_score = away_score),
  df_clean %>% select(game_date, team = away_full, mascot = away_mascot, logo_path = away_logo_path, score = away_score, opp_score = home_score)
) %>%
  # ✨ 【核心修复语句】：强制核对户口本，只保留 teamcolors 里登记在册的 30 支正规军 ✨
  filter(team %in% nba_colors$name)

# 计算排名 (后面的代码完全不用动...)
daily_ranked <- df_long %>%
  arrange(team, game_date) %>%
  # ... 保持你原来的代码不变 ...
  group_by(team) %>%
  mutate(
    cum_score = cumsum(score),
    cum_opp_score = cumsum(opp_score),
    games_played = row_number()
  ) %>%
  ungroup() %>%
  complete(team, game_date = seq.Date(min(game_date), max(game_date), by = "day")) %>%
  arrange(team, game_date) %>%
  group_by(team) %>%
  fill(cum_score, cum_opp_score, games_played, mascot, logo_path, .direction = "down") %>%
  filter(!is.na(games_played)) %>%
  mutate(net_rating = (cum_score - cum_opp_score) / games_played) %>%
  ungroup() %>%
  group_by(game_date) %>%
  mutate(net_rating_rank = rank(-net_rating, ties.method = "min")) %>%
  ungroup() %>%
  # 合并颜色
  left_join(nba_colors, by = c("team" = "name"))

# ==============================================================================
# 5. 绘图准备
# ==============================================================================

# 1. 确定排序 (按最后一日排名)
last_day <- max(daily_ranked$game_date)
team_rank_order <- daily_ranked %>%
  filter(game_date == last_day) %>%
  arrange(net_rating_rank) %>%
  pull(logo_path) # 这里我们取 path 作为排序的因子

# 2. 设置因子水平
df_plot <- daily_ranked %>%
  mutate(
    logo_path = factor(logo_path, levels = team_rank_order),
    group_bg = logo_path # 用于背景灰线
  )


# ==============================================================================
# 6. 绘图
# ==============================================================================

p <- ggplot(df_plot, aes(x = game_date, y = net_rating_rank)) + 
  # 辅助线
  geom_hline(yintercept = 15, alpha = .3, linetype = 'dashed', color = "gray60") +
  
  # 背景灰线 (所有球队轨迹)
  stat_smooth(data = mutate(df_plot, logo_path = NULL), 
              aes(group = group_bg), 
              geom="line", alpha=0.08, linewidth=.2, span=0.2, colour = 'gray50', se=FALSE) +
  
  # 主队轨迹
  geom_step(aes(color = primary), linewidth = .6, alpha = .7) +
  geom_smooth(aes(color = primary), linewidth = .5, span = .2, se = FALSE, method = "loess", formula = "y ~ x") +
  
  # 坐标轴
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_reverse(breaks = c(1, 15, 30), labels = c("1st", "15th", "30th"), 
                  expand = c(0, 0), limits = c(31, 0)) + 
  scale_color_identity() + 
  
  # 分面：直接使用本地路径
  facet_wrap(~logo_path) +
  
  theme_f5_floral() + 
  labs(
    title = "NBA 2025-2026 赛季球队实力表现走势图", 
    # 将“俗人罢了”用<b>标签包起来
    subtitle = glue("数据截止全明星 | 数据来源: hoopR|制作：<b>俗人罢了</b>|按照球队目前净效率排序，左上角强队右下角弱队"),
    x = NULL, 
    y = "球队每日净效率排名 (Net Rating Rank)"
  )
# ==============================================================================
# 7. 保存
# ==============================================================================
ggsave("nrtg_rank_local_logo.png", p, h = 10, w = 10, dpi = 300)