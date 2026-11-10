library(duckdb)
library(DBI)
library(tidyverse)

# 1. 启动 DuckDB 内存引擎
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# 2. SQL 逻辑（保持不变，这是我们的核心算法）
query <- "
WITH base_pbp AS (
  SELECT 
    game_id, team_id, home_team_id, away_team_id, 
    home_team_abbrev, away_team_abbrev,
    LOWER(COALESCE(type_text, '')) AS type_text_l,
    LOWER(COALESCE(text, '')) AS text_l
  FROM read_parquet('D:/nba/RStudio/HoopR/nba/pbp/parquet/play_by_play_2026.parquet')
),
game_counts AS (
  SELECT team, COUNT(DISTINCT game_id) AS games_played
  FROM (
    SELECT home_team_abbrev AS team, game_id FROM base_pbp
    UNION
    SELECT away_team_abbrev AS team, game_id FROM base_pbp
  )
  WHERE team IS NOT NULL
  GROUP BY team
),
violations AS (
  SELECT 
    CASE 
      WHEN team_id = home_team_id THEN away_team_abbrev 
      ELSE home_team_abbrev 
    END AS defense_team
  FROM base_pbp
  WHERE type_text_l LIKE '%shot clock%' OR text_l LIKE '%shot clock%'
),
defense_stats AS (
  SELECT defense_team AS team, COUNT(*) AS forced_violations
  FROM violations
  WHERE defense_team IS NOT NULL
  GROUP BY defense_team
)
SELECT 
  g.team AS 球队,
  COALESCE(d.forced_violations, 0) AS 逼迫24秒违例总数,
  g.games_played AS 比赛场数,
  ROUND(CAST(COALESCE(d.forced_violations, 0) AS DOUBLE) / g.games_played, 2) AS 场均逼迫24秒违例
FROM game_counts g
LEFT JOIN defense_stats d ON g.team = d.team
ORDER BY 场均逼迫24秒违例 DESC
"

# 3. 获取数据
defense_ranking <- dbGetQuery(con, query)

# 4. 打印结果 (💡 这里是修复点：先转为 tibble 再 print)
cat("\n🛡️ NBA 2025-26 赛季『窒息防守』排行榜\n")
defense_ranking %>% 
  as_tibble() %>% 
  print(n = 30) 

# 养成好习惯，关闭连接
dbDisconnect(con, shutdown = TRUE)