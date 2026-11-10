library(duckdb)
library(DBI)
library(tidyverse)
library(lubridate)

# 1. 建立 DuckDB 内存连接
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# 2. 编写 DuckDB 原生 SQL (直接在 Parquet 文件上做带时间窗的过滤)
query <- "
WITH game_starts AS (
  -- 第一步：扫描一遍，找出每场比赛的开球时间 (Tip-off)
  SELECT game_id, MIN(CAST(wallclock AS TIMESTAMP)) AS tipoff
  FROM read_parquet('D:/nba/RStudio/HoopR/nba/pbp/parquet/play_by_play_2026.parquet')
  WHERE wallclock IS NOT NULL
  GROUP BY game_id
),
valid_plays AS (
  -- 第二步：把开球时间拼回去，剔除距离开球超过 5 小时 (300分钟) 的赛后补录幽灵数据
  SELECT p.game_id, p.home_team_abbrev, p.away_team_abbrev, p.period_number, CAST(p.wallclock AS TIMESTAMP) AS play_time
  FROM read_parquet('D:/nba/RStudio/HoopR/nba/pbp/parquet/play_by_play_2026.parquet') p
  JOIN game_starts s ON p.game_id = s.game_id
  WHERE p.wallclock IS NOT NULL
    AND date_diff('minute', s.tipoff, CAST(p.wallclock AS TIMESTAMP)) < 300
)
-- 第三步：在干净的数据上，重新计算真实耗时
SELECT 
  game_id,
  MAX(home_team_abbrev) AS home,
  MAX(away_team_abbrev) AS away,
  MAX(period_number) AS max_per,
  MIN(play_time) AS start_time_utc,
  MAX(play_time) AS end_time_utc,
  date_diff('minute', MIN(play_time), MAX(play_time)) AS duration
FROM valid_plays
GROUP BY game_id
HAVING duration > 90
ORDER BY duration DESC
"

# 3. 让 DuckDB 执行这套复杂的逻辑，并把干净的最终结果拉回 R
clean_ranking_db <- dbGetQuery(con, query)

# 4. 在 R 里只做最简单的排版和时区转换 (此时数据只有几百行，毫无压力)
final_clean_ranking <- clean_ranking_db %>%
  mutate(
    # 将拉回来的 UTC 时间强制识别并转为北京时间
    start_bj = with_tz(force_tz(start_time_utc, "UTC"), "Asia/Shanghai"),
    end_bj   = with_tz(force_tz(end_time_utc, "UTC"), "Asia/Shanghai"),
    # 新增：提取比赛开始的日期（北京时间，格式：年-月-日）
    比赛日期_BJ = format(start_bj, "%Y-%m-%d"),
    交手双方 = paste0(away, " @ ", home),
    起止时刻_BJ = paste0(format(start_bj, "%H:%M"), " - ", format(end_bj, "%H:%M")),
    是否加时 = ifelse(max_per > 4, paste0(max_per - 4, "OT"), "常规时间")
  ) %>%
  # 调整列顺序：把比赛日期放在前面，更符合阅读习惯
  select(比赛ID = game_id, 比赛日期_BJ, 交手双方, 真实耗时_分钟 = duration, 起止时刻_BJ, 是否加时)

# 养成好习惯，用完关门
dbDisconnect(con, shutdown = TRUE)

# 直接以 Excel 表格形式查看！
View(final_clean_ranking)