library(tidyverse)
library(lubridate)

# 1. 加载数据
pbp_2026 <- readRDS("D:/nba/RStudio/HoopR/nba/pbp/rds/play_by_play_2026.rds")

# 2. 计算每场比赛的总时长
game_durations <- pbp_2026 %>%
  group_by(game_id) %>%
  summarise(
    home_team = first(home_team_abbrev),
    away_team = first(away_team_abbrev),
    # 转换 wallclock 并计算差值
    start_time = min(ymd_hms(wallclock), na.rm = TRUE),
    end_time   = max(ymd_hms(wallclock), na.rm = TRUE),
    duration_mins = as.numeric(difftime(end_time, start_time, units = "mins"))
  ) %>%
  # 过滤掉异常数据（比如时长少于 90 分钟的可能记录不全）
  filter(duration_mins > 90)

# 3. 将数据按球队展开 (每场比赛主客队各算一次)
team_data <- bind_rows(
  game_durations %>% select(team = home_team, duration = duration_mins),
  game_durations %>% select(team = away_team, duration = duration_mins)
)

# 4. 计算平均值并排名
team_ranking <- team_data %>%
  group_by(team) %>%
  summarise(
    平均用时 = round(mean(duration, na.rm = TRUE), 1),
    比赛场次 = n()
  ) %>%
  arrange(desc(平均用时)) # 按时间从长到短排序

# 5. 直接打印输出
print("--- 2025-26 赛季 NBA 各球队平均每场比赛耗时排行榜 ---")
print(as.data.frame(team_ranking))