library(tidyverse)
library(tidyr)

# --- 1. 数据预处理 ---
pbp_clean <- pbp_data %>%
  # 必须严格按时间倒序排列
  arrange(game_id, period, desc(clock_minutes), desc(clock_seconds), desc(sequence_number)) %>%
  mutate(
    seconds_remaining = ifelse(is.na(clock_minutes), 0, clock_minutes * 60) + clock_seconds,
    clean_text = tolower(text),
    
    # A. 标记原始事件 (当前行发生了什么？)
    curr_def_reb = grepl("defensive rebound", clean_text),
    curr_turnover = grepl("turnover", clean_text),
    curr_score = scoring_play == TRUE
  )

# --- 2. 核心修复：使用 lag() 构建触发器 ---
early_offense_fixed <- pbp_clean %>%
  group_by(game_id, period) %>%
  mutate(
    # B. 真正的触发器 (Looking Backwards)
    # 逻辑：取“上一行”的事件作为当前回合的开始信号
    # 这样 Quickley 投进的那一行，读取的是“上一行”的时间锚点
    trigger_source = case_when(
      lag(curr_def_reb, default = FALSE) ~ "rebound",
      lag(curr_turnover, default = FALSE) ~ "turnover",
      lag(curr_score, default = FALSE) ~ "opponent_make",
      TRUE ~ NA_character_
    ),
    
    # 记录触发时间 (取上一行的时间)
    trigger_time_anchor = ifelse(!is.na(trigger_source), lag(seconds_remaining), NA)
  ) %>%
  
  # 向下填充：将最近一次触发时间延续下去
  fill(trigger_time_anchor, trigger_source, .direction = "down") %>%
  
  mutate(
    # 计算消耗时间
    time_elapsed = trigger_time_anchor - seconds_remaining
  ) %>%
  ungroup()

# --- 3. 筛选与统计 (Top 15) ---
fast_3pt_stats_fixed <- early_offense_fixed %>%
  filter(
    # 1. 是三分出手 (进或不进)
    (grepl("three point|3pt|3-pointer", clean_text) | (!is.na(points_attempted) & points_attempted == 3)),
    
    # 2. 极速筛选 (模拟进攻时间剩 18s+)
    # 篮板/抢断：推进 <= 6秒
    # 对手进球：推进 <= 8秒 (含发球缓冲)
    case_when(
      trigger_source %in% c("rebound", "turnover") ~ (time_elapsed <= 4 & time_elapsed > 0),
      trigger_source == "opponent_make" ~ (time_elapsed <= 6 & time_elapsed > 0),
      TRUE ~ FALSE
    )
  ) %>%
  group_by(athlete_id_1) %>%
  summarise(
    display_name = {
      txt_name <- first(str_extract(text, "^.*?(?=\\s+(makes|misses))"))
      if(is.na(txt_name)) paste("Player", first(athlete_id_1)) else str_trim(txt_name)
    },
    
    fast_attempts = n(),
    
    # 统计进球 (这次 scoring_play=TRUE 的行保留下来了！)
    fast_makes = sum(scoring_play == TRUE, na.rm = TRUE), 
    
    # 真实命中率
    fast_fg_pct = round(fast_makes / fast_attempts * 100, 1),
    
    # 平均消耗时间
    avg_seconds = round(mean(time_elapsed), 1)
  ) %>%
  filter(fast_attempts >= 20) %>%
  arrange(desc(fast_attempts)) %>%
  slice(1:15)

# --- 4. 输出结果 ---
cat("--- 🏎️ NBA 2026赛季“极速三分”(修正版) 榜单 ---\n")
print(fast_3pt_stats_fixed)