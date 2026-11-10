# 加载所需的核心包
library(tidyverse)
library(arrow)

# 允许 arrow 恢复 R 的元数据，消除读取 parquet 时的外部指针警告
options(arrow.unsafe_metadata = TRUE)

# 1. 设定杰伦·布朗职业生涯的赛季范围
seasons <- 2018:2026

# 2. 批量读取本地赛程文件并合并
schedule_df <- map_dfr(seasons, function(yr) {
  # 拼接本地赛程 Parquet 文件的绝对路径
  file_path <- paste0("D:/nba/RStudio/HoopR/nba/schedules/parquet/nba_schedule_", yr, ".parquet")
  
  # 检查文件是否存在，防止缺失单赛季文件导致代码中断
  if (file.exists(file_path)) {
    read_parquet(file_path)
  } else {
    warning(paste("未找到赛程文件，跳过:", file_path))
    return(NULL)
  }
})

# 3. 筛选波士顿凯尔特人队的常规赛与季后赛赛程，并判定每场胜负
celtics_schedule <- schedule_df %>%
  filter(
    # 修改点：使用 %in% 运算符，同时保留常规赛(2)和季后赛(3)
    season_type %in% c(2, 3), 
    # 筛选包含波士顿凯尔特人的比赛
    home_display_name == "Boston Celtics" | away_display_name == "Boston Celtics"
  ) %>%
  mutate(
    # 判断该场比赛凯尔特人是否为主场
    is_celtics_home = home_display_name == "Boston Celtics",
    
    # 将得分强制转换为数值型，防止由于数据类型引发的比对错误
    home_score = as.numeric(home_score),
    away_score = as.numeric(away_score),
    
    # 根据主客场和比分，判定凯尔特人是否赢下该场比赛
    celtics_won = case_when(
      is_celtics_home & home_score > away_score ~ TRUE,
      !is_celtics_home & away_score > home_score ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  # 保留核心列，此处特别增加了 season_type 以区分常规赛/季后赛
  select(game_id, season, season_type, game_date, celtics_won)

# 4. 批量读取本地球员技术统计文件
jb_box <- map_dfr(seasons, function(yr) {
  # 拼接本地个人技术统计 Parquet 文件的绝对路径
  player_file <- paste0("D:/nba/RStudio/HoopR/nba/player_box/parquet/player_box_", yr, ".parquet")
  
  if (file.exists(player_file)) {
    # 读取并在源头直接过滤，降低内存消耗
    read_parquet(player_file) %>%
      filter(
        athlete_display_name == "Jayson Tatum",
        # 修改点：与赛程表同步，同时提取常规赛和季后赛数据
        season_type %in% c(2, 3) 
      )
  } else {
    warning(paste("未找到球员数据文件，跳过:", player_file))
    return(NULL)
  }
}) %>%
  # 剔除大名单中但未出场 (DNP) 的无效记录
  filter(did_not_play == FALSE | is.na(did_not_play)) %>%
  # 仅保留 game_id 作为后续表关联的唯一键
  select(game_id) %>%
  # 为成功匹配的比赛标记出战状态为 TRUE
  mutate(played = TRUE)

# 5. 将赛程表(主表)与个人数据(副表)进行左连接
result_df <- celtics_schedule %>%
  left_join(jb_box, by = "game_id") %>%
  mutate(
    # 如果左连接后 played 字段为 NA，说明布朗缺席了该场比赛，赋值为 FALSE
    played = ifelse(is.na(played), FALSE, TRUE)
  )

# 6. 按赛季和赛事类型分组汇总，并构造自定义文本输出
win_rate_summary <- result_df %>%
  # 按年份和赛事类型进行多维分组
  group_by(season, season_type) %>%
  summarise(
    # 统计该赛季该类型比赛中，布朗出战的总场次
    played_games = sum(played == TRUE),
    # 统计出战时的获胜场次
    played_wins = sum(celtics_won[played == TRUE], na.rm = TRUE),
    # 统计该赛季该类型比赛中，布朗缺席的总场次
    missed_games = sum(played == FALSE),
    # 统计缺席时的获胜场次
    missed_wins = sum(celtics_won[played == FALSE], na.rm = TRUE),
    # 取消分组，防止后续 mutate 报错
    .groups = "drop"
  ) %>%
  mutate(
    # 计算出战胜率：如果出场次数大于0，则计算百分比并保留两位小数，否则显示 N/A
    played_rate = ifelse(played_games > 0, paste0(round((played_wins / played_games) * 100, 2), "%"), "N/A"),
    # 计算缺席胜率：如果缺席次数大于0，则计算百分比并保留两位小数，否则显示 N/A
    missed_rate = ifelse(missed_games > 0, paste0(round((missed_wins / missed_games) * 100, 2), "%"), "N/A"),
    
    # 转换文字标签：2 对应常规赛，3 对应季后赛
    season_label = ifelse(season_type == 2, "常规赛", "季后赛"),
    
    # 构造赛季年份字符串：例如把 2017 转化为 2016-17
    season_str = paste0(season - 1, "-", substr(season, 3, 4)),
    
    # 使用 sprintf 格式化函数，将变量拼接成你要求的一整段文本
    output_text = sprintf("%s%s 出战场次:%d 对应胜率:%s，缺席场次:%d 对应胜率:%s", 
                          season_str, season_label, played_games, played_rate, missed_games, missed_rate)
  ) %>%
  # 按照赛季年份正序、赛季类型正序(先常规赛后季后赛)排列
  arrange(season, season_type)

# 7. 在 Console 逐行打印最终结果
cat(paste(win_rate_summary$output_text, collapse = "\n"))