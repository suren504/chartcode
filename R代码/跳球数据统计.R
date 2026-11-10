library(tidyverse)
library(duckdb)
library(stringr)

# ===============================
# 1️⃣ 使用 DuckDB 极速读取
# ===============================
con <- dbConnect(duckdb::duckdb())
query <- "
  SELECT 
    game_id, text, team_id AS win_team_id, 
    home_team_id, away_team_id, home_team_abbrev, away_team_abbrev
  FROM read_parquet('D:/nba/RStudio/HoopR/nba/pbp/parquet/play_by_play_2026.parquet')
  WHERE type_text = 'Jumpball'
"
jumpball_raw <- dbGetQuery(con, query)
dbDisconnect(con, shutdown = TRUE)

# ===============================
# 2️⃣ 定义标准的 30 支 NBA 球队缩写
# ===============================
# 参照你数据中的缩写习惯（例如马刺是 SA，鹈鹕是 NO，爵士是 UTAH 等）
nba_30_teams <- c(
  "ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GS",
  "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NO", "NY",
  "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SA", "TOR", "UTAH", "WSH"
)

# ===============================
# 3️⃣ 数据清洗与角色拆解
# ===============================
jumpball_clean <- jumpball_raw %>%
  mutate(
    player_away = str_trim(str_extract(text, "^.*?(?=\\s+vs\\.)")),
    player_home = str_trim(str_extract(text, "(?<=vs\\.\\s).*?(?=\\s*\\()"))
  ) %>%
  filter(!is.na(player_away) & !is.na(player_home)) %>%
  pivot_longer(
    cols = c(player_away, player_home),
    names_to = "role",
    values_to = "player"
  ) %>%
  mutate(
    team_abbrev = if_else(role == "player_away", away_team_abbrev, home_team_abbrev),
    team_id     = if_else(role == "player_away", away_team_id, home_team_id),
    is_win      = if_else(win_team_id == team_id, 1, 0)
  ) %>%
  # 🔥 新增：只保留 30 支常规 NBA 球队，剔除全明星等表演赛队伍
  filter(team_abbrev %in% nba_30_teams)

# ===============================
# 4️⃣ 汇总统计各队最强跳球手并排名 (精确 30 人)
# ===============================
team_top_jumpers <- jumpball_clean %>%
  group_by(team_abbrev, player) %>%
  summarise(
    Total_Jumpballs = n(),
    Jumpballs_Won = sum(is_win, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(team_abbrev) %>%
  arrange(desc(Total_Jumpballs)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    Win_Rate = round(Jumpballs_Won / Total_Jumpballs, 3)
  ) %>%
  arrange(desc(Win_Rate)) %>%
  select(
    `球队` = team_abbrev,
    `首发跳球手` = player,
    `总跳球次数` = Total_Jumpballs,
    `跳球获胜数` = Jumpballs_Won,
    `胜率` = Win_Rate
  )

# ===============================
# 5️⃣ 输出最终榜单
# ===============================
print(team_top_jumpers, n = 30)