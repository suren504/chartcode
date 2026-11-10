# 加载必备的包
library(tidyverse)
library(arrow)

# 1. 读取 PBP 数据并提取比分核心列
pbp_raw <- read_parquet("D:/nba/RStudio/HoopR/nba/pbp/parquet/play_by_play_2026.parquet") %>%
  # 剔除比分为空的无效行，只保留我们需要的列，极大提升运行速度
  filter(!is.na(home_score) & !is.na(away_score)) %>%
  select(game_id, sequence_number, home_team_abbrev, away_team_abbrev, home_score, away_score)

# 2. 【核心优化】为每场比赛注入 0-0 的起点
# 这是为了防止开局直接打出 10-0 时，底部的 min(score) 不是 0 的问题
game_starts <- pbp_raw %>%
  distinct(game_id, home_team_abbrev, away_team_abbrev) %>%
  mutate(sequence_number = 0, home_score = 0, away_score = 0)

# 合并并按发生顺序排列
pbp_full <- bind_rows(game_starts, pbp_raw) %>%
  arrange(game_id, sequence_number)

# 3. 【致敬欧文·菲利普斯】利用对手得分停滞期，计算攻击波
# 计算主队打出的攻击波 (Grouping by away_score)
home_runs <- pbp_full %>%
  group_by(game_id, home_team_abbrev, away_team_abbrev, away_score) %>%
  summarize(
    run_start = min(home_score, na.rm = TRUE),
    run_end = max(home_score, na.rm = TRUE),
    run_length = run_end - run_start,
    .groups = "drop"
  ) %>%
  # 筛选出 10 分或以上的一波流
  filter(run_length >= 10) %>%
  # 主队打出的，算作主队 Created，客队 Allowed
  select(game_id, team = home_team_abbrev, opponent = away_team_abbrev, run_length)

# 计算客队打出的攻击波 (Grouping by home_score)
away_runs <- pbp_full %>%
  group_by(game_id, home_team_abbrev, away_team_abbrev, home_score) %>%
  summarize(
    run_start = min(away_score, na.rm = TRUE),
    run_end = max(away_score, na.rm = TRUE),
    run_length = run_end - run_start,
    .groups = "drop"
  ) %>%
  filter(run_length >= 10) %>%
  # 客队打出的，算作客队 Created，主队 Allowed
  select(game_id, team = away_team_abbrev, opponent = home_team_abbrev, run_length)

# 4. 汇总所有的 10-0 波段
all_runs <- bind_rows(home_runs, away_runs)

# 分别统计每支球队“打出”和“被打”的次数
team_runs_created <- all_runs %>% count(team, name = "runs_created")
team_runs_allowed <- all_runs %>% count(opponent, name = "runs_allowed")

# 5. 计算每支球队的比赛场数 (Games Played)
games_played <- pbp_full %>%
  distinct(game_id, home_team_abbrev, away_team_abbrev) %>%
  pivot_longer(cols = c(home_team_abbrev, away_team_abbrev), values_to = "team") %>%
  count(team, name = "gp")

# 6. 生成最终的场均数据表，并剔除全明星等非标准球队
nba_30_teams <- c(
  "ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GS",
  "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NO", "NY",
  "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SA", "TOR", "UTAH", "WSH"
)

final_run_stats <- games_played %>%
  filter(team %in% nba_30_teams) %>%
  left_join(team_runs_created, by = "team") %>%
  left_join(team_runs_allowed, by = c("team" = "opponent")) %>%
  # 把没有打出过或没被打出过的次数补 0
  replace_na(list(runs_created = 0, runs_allowed = 0)) %>%
  mutate(
    `场均制造一波流` = round(runs_created / gp, 3),
    `场均被揍一波流` = round(runs_allowed / gp, 3)
  ) %>%
  select(
    `球队` = team,
    `出场数` = gp,
    `总制造次数` = runs_created,
    `总被揍次数` = runs_allowed,
    `场均制造一波流`,
    `场均被揍一波流`
  ) %>%
  # 按照净胜一波流能力排序 (场均制造 - 场均被揍)
  arrange(desc(`场均制造一波流` - `场均被揍一波流`))

# 查看最终结果
print(final_run_stats, n = 30)