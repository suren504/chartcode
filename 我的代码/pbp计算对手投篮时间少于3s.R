library(tidyverse)
library(arrow)

cat("正在加载 PBP 数据并计算防守压迫时长...\n")

# 1. 极速读取本地 Parquet 数据
pbp_df <- read_parquet("D:/nba/RStudio/HoopR/nba/pbp/parquet/play_by_play_2026.parquet")

# 2. 核心清洗与时间差计算
late_shot_stats <- pbp_df %>%
  # 💡 极其重要：确保数据按照时间流逝严格倒序，防止 lag() 抓错数据
  arrange(game_id, period_number, desc(clock_minutes), desc(clock_seconds), sequence_number) %>%
  group_by(game_id, period_number) %>%
  mutate(
    # 将时钟转化为纯秒数
    total_sec = clock_minutes * 60 + clock_seconds,
    # 计算与上一个回合动作消耗的时间差
    time_consumed = lag(total_sec) - total_sec
  ) %>%
  ungroup() %>%
  
  # 3. 筛选真实投篮，并翻转防守方身份
  filter(shooting_play == TRUE) %>%
  mutate(
    defense_team = if_else(team_id == home_team_id, away_team_abbrev, home_team_abbrev)
  ) %>%
  # 过滤掉无法识别防守方的脏数据
  filter(!is.na(defense_team)) %>%
  
  # 4. 终极聚合：计算 <= 4秒出手的比重
  group_by(defense_team) %>%
  summarise(
    对手总出手次数 = n(),
    # 💡 核心逻辑：消耗 >= 20 秒，意味着 24 秒表只剩下 4 秒或更少
    防到四秒内出手次数 = sum(time_consumed >= 20, na.rm = TRUE),
    压哨仓促出手占比 = 防到四秒内出手次数 / 对手总出手次数
  ) %>%
  # 将占比转化为百分比格式，方便阅读
  mutate(
    压哨仓促出手占比_pct = paste0(round(压哨仓促出手占比 * 100, 2), "%")
  ) %>%
  arrange(desc(压哨仓促出手占比))

# 5. 打印排行榜
cat("====================================================\n")
cat("⏱️ 2025-26 赛季『逼迫对手仓促出手 (≤4秒)』排行榜\n")
cat("====================================================\n")
print(late_shot_stats, n = 30)