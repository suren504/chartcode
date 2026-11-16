# ==============================================================================
# 1. 环境与主题设置
# ==============================================================================
library(tidyverse)
library(duckdb)      # 数据库

# --- 字体设置 ---
font_en <- "Menlo"
font_zh <- "Hiragino Sans GB"
pingfang_files <- Sys.glob(
  "/System/Library/AssetsV2/com_apple_MobileAsset_Font*/*.asset/AssetData/PingFang.ttc"
)
for (font_file in pingfang_files) {
  faces <- tryCatch(
    systemfonts::font_info(path = rep(font_file, 2), index = c(3L, 11L)),
    error = function(e) NULL
  )
  if (identical(faces$family, rep("PingFang SC", 2)) &&
      identical(faces$style, c("Regular", "Semibold"))) {
    font_zh <- "NBA PingFang"
    systemfonts::register_font(
      font_zh, plain = list(font_file, 3L), bold = list(font_file, 11L)
    )
    break
  }
}

# --- 自定义主题 (F5 Style - #f2f3f4版) ---
theme_f5_floral <- function (font_size = 9) {
  theme_minimal(base_size = font_size, base_family = font_en) %+replace%
    theme(
      # 背景色
      plot.background = element_rect(fill = '#f2f3f4', color = '#f2f3f4'),
      panel.background = element_rect(fill = '#f2f3f4', color = NA),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.3),
      
      plot.title = ggtext::element_markdown(
        family = font_zh, hjust = 0.5, size = 18, face = "bold", color = "#111111"
      ),

      plot.subtitle = ggtext::element_markdown(
        family = font_zh,
        color = '#444444',
        hjust = 0.5, 
        margin = margin(5, 0, 20, 0), 
        size = 11
      ),
      
      plot.caption = element_text(color = 'gray60', margin = margin(10, 0, 0, 0), hjust = 1, size = 7),
      
      # Logo 路径支持
      strip.text = ggpath::element_path(size = 1, hjust = 0.5),
      panel.spacing = unit(0.5, "lines"),
      
      axis.text.x = element_text(size = 7, color = "gray40"),
      axis.text.y = element_text(size = 7, color = "gray40")
    )
}

# ==============================================================================
# 2. 读取球队主色与队标 ID
# ==============================================================================
nba_site_dir <- "/Users/suren/Desktop/nba_site"
logo_base_dir <- file.path(nba_site_dir, "static/NBA/logo")
nba_teams <- jsonlite::fromJSON(file.path(nba_site_dir, "src/lib/data/teamcolor.json")) %>%
  transmute(abbr = team_abbreviation, logo_id = team_id, primary = team_color)

# ==============================================================================
# 3. DuckDB 数据提取
# ==============================================================================
parquet_file <- "/Users/suren/Downloads/play_by_play_2027.parquet"

con <- dbConnect(duckdb::duckdb(shared_home = FALSE))
parquet_path <- DBI::dbQuoteString(con, parquet_file)
pbp_db <- tbl(con, dbplyr::sql(paste0("SELECT * FROM read_parquet(", parquet_path, ")")))

# 提取每场比赛结果和球队信息
game_logs <- pbp_db %>%
  group_by(game_id) %>%
  summarise(
    game_date = max(game_date, na.rm = TRUE),
    
    # 主队
    home_city = max(home_team_name, na.rm = TRUE),
    home_mascot = max(home_team_mascot, na.rm = TRUE),
    home_abbr = max(home_team_abbrev, na.rm = TRUE),
    home_score = max(home_score, na.rm = TRUE),
    
    # 客队
    away_city = max(away_team_name, na.rm = TRUE),
    away_mascot = max(away_team_mascot, na.rm = TRUE),
    away_abbr = max(away_team_abbrev, na.rm = TRUE),
    away_score = max(away_score, na.rm = TRUE)
  ) %>%
  collect() %>%
  ungroup()

dbDisconnect(con)

# ==============================================================================
# 4. 数据清洗与排名
# ==============================================================================

# 构建球队全名
df_clean <- game_logs %>%
  mutate(
    game_date = as.Date(game_date) + 1L,
    
    # 拼接全名用于分组
    home_full = paste(home_city, home_mascot),
    away_full = paste(away_city, away_mascot),
    
    # 修正特殊情况
    home_full = str_replace(home_full, "Los Angeles Clippers", "LA Clippers"),
    away_full = str_replace(away_full, "Los Angeles Clippers", "LA Clippers")
  )

# 长格式转换
df_long <- bind_rows(
  df_clean %>% select(game_date, team = home_full, abbr = home_abbr, score = home_score, opp_score = away_score),
  df_clean %>% select(game_date, team = away_full, abbr = away_abbr, score = away_score, opp_score = home_score)
) %>%
  mutate(abbr = recode(abbr, GS = "GSW", NO = "NOP", NY = "NYK", SA = "SAS", UTAH = "UTA", WSH = "WAS")) %>%
  filter(abbr %in% nba_teams$abbr)

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
  fill(cum_score, cum_opp_score, games_played, abbr, .direction = "down") %>%
  filter(!is.na(games_played)) %>%
  mutate(net_rating = (cum_score - cum_opp_score) / games_played) %>%
  ungroup() %>%
  group_by(game_date) %>%
  mutate(net_rating_rank = rank(-net_rating, ties.method = "min")) %>%
  ungroup() %>%
  left_join(nba_teams, by = "abbr", relationship = "many-to-one")

# ==============================================================================
# 5. 绘图准备
# ==============================================================================

# 1. 确定排序 (按最后一日排名)
last_day <- max(daily_ranked$game_date)
team_strips <- daily_ranked %>%
  filter(game_date == last_day) %>%
  arrange(net_rating_rank) %>%
  mutate(logo_path = file.path(logo_base_dir, paste0(logo_id, ".webp")))
team_labels <- setNames(team_strips$logo_path, team_strips$team)

# 2. 设置因子水平
df_plot <- daily_ranked %>%
  mutate(team = factor(team, levels = team_strips$team), group_bg = team)


# ==============================================================================
# 6. 绘图
# ==============================================================================

p <- ggplot(df_plot, aes(x = game_date, y = net_rating_rank)) + 
  # 辅助线
  geom_hline(yintercept = 15, alpha = .3, linetype = 'dashed', color = "gray60") +
  
  # 背景灰线 (所有球队轨迹)
  stat_smooth(data = select(df_plot, -team),
              aes(group = group_bg), 
              geom="line", alpha=0.08, linewidth=.2, span=0.2, colour = 'gray50', se=FALSE) +
  
  # 主队轨迹
  geom_step(aes(color = primary), linewidth = .6, alpha = .7) +
  geom_smooth(aes(color = primary), linewidth = .5, span = .2, se = FALSE, method = "loess", formula = "y ~ x") +
  
  # 坐标轴
  scale_x_date(date_breaks = "1 month", labels = function(dates) month.abb[as.integer(format(dates, "%m"))]) +
  scale_y_reverse(breaks = c(1, 15, 30), labels = c("1st", "15th", "30th"), 
                  expand = c(0, 0), limits = c(31, 0)) + 
  scale_color_identity() + 
  
  facet_wrap(~team, labeller = as_labeller(team_labels)) +
  
  theme_f5_floral() + 
  labs(
    title = sprintf("<span style='font-family: %s;'>NBA 2026-2027</span> 赛季球队净胜分走势图", font_en),
    subtitle = sprintf(
      paste0(
        "数据截至11/11| ",
        "数据来源：<span style='font-family: %s;'>hoopR</span> | 制作：<b>俗人罢了</b> | ",
        "按照球队每日净效率排序，左上角强队右下角弱队"
      ),
      font_en, last_day, font_en
    ),
    x = NULL, 
    y = "Net Rating Rank"
  )
# ==============================================================================
# 7. 保存
# ==============================================================================
ggsave("nrtg_rank_local_logo.png", p, height = 10, width = 10, dpi = 300, device = ragg::agg_png)
