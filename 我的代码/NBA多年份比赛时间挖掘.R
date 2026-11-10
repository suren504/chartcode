library(tidyverse)
library(lubridate)

# ==============================================================================
# 1. 定义核心清洗函数 (统一算法 + 精确格式化)
# ==============================================================================
analyze_nba_duration_precise <- function(file_path, label) {
  message(paste0("正在处理: ", label, " ..."))
  
  # 读取本地 RDS
  pbp <- readRDS(file_path)
  
  # 统一清洗逻辑
  res <- pbp %>%
    # 强制将 period 转为数字 (解决部分年份数据结构为 list 的报错)
    mutate(period = as.numeric(unlist(period))) %>%
    # 排除加时赛，仅统计常规 4 节
    filter(period <= 4) %>%
    # 统一时间戳列名 (兼容 hoopR 不同版本的字段名)
    rename_with(~"wc", any_of(c("wallclock", "wall_clock", "time_actual"))) %>%
    mutate(wc_ts = ymd_hms(wc)) %>%
    filter(!is.na(wc_ts)) %>%
    # 计算每场比赛真实的物理时长 (秒)
    group_by(game_id) %>%
    summarise(
      duration_secs = as.numeric(difftime(max(wc_ts), min(wc_ts), units = "secs")),
      .groups = "drop"
    ) %>%
    # 剔除异常值 (时长在 90分钟到 4小时之间)
    filter(duration_secs > 5400 & duration_secs < 14400) %>%
    # 计算该赛季平均秒数
    summarise(
      season = label,
      avg_total_secs = mean(duration_secs, na.rm = TRUE),
      game_count = n()
    ) %>%
    # --- 核心转换逻辑：将秒数转化为 H:M:S ---
    mutate(
      hours = avg_total_secs %/% 3600,
      mins  = (avg_total_secs %% 3600) %/% 60,
      secs  = round(avg_total_secs %% 60),
      # 格式化为 0:00:00 样式
      precise_duration = sprintf("%d:%02d:%02d", hours, mins, secs)
    ) %>%
    select(season, precise_duration, avg_total_secs, game_count)
  
  return(res)
}

# ==============================================================================
# 2. 批量执行 (读取 D:/下载文件/ 目录下的 6 个文件)
# ==============================================================================
file_list <- tribble(
  ~path, ~label,
  "D:/下载文件/play_by_play_2021.rds", "2020-21",
  "D:/下载文件/play_by_play_2022.rds", "2021-22",
  "D:/下载文件/play_by_play_2023.rds", "2022-23",
  "D:/下载文件/play_by_play_2024.rds", "2023-24",
  "D:/下载文件/play_by_play_2025.rds", "2024-25",
  "D:/下载文件/play_by_play_2026.rds", "2025-26"
)

# 循环处理并合并结果
final_stats_table <- file_list %>%
  rowwise() %>%
  do(analyze_nba_duration_precise(.$path, .$label)) %>%
  ungroup()

# ==============================================================================
# 3. 打印最终精确结果
# ==============================================================================
cat("\n\n")
message("========================================================")
message("   NBA 过去六个赛季平均比赛用时 (常规 4 节 / 精确到秒)   ")
message("========================================================")
print(final_stats_table %>% select(season, precise_duration, game_count))
message("========================================================")