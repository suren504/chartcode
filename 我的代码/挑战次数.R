rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(arrow)
  library(tidyverse)
})

# ==============================================================================
# 1. 读取 2025-26 NBA Play-by-Play Parquet
# ===============================Q===============================================

file_path <- "D:/nba/RStudio/HoopR/nba/pbp/parquet/play_by_play_2026.parquet"

pbp_data <- read_parquet(
  file_path,
  col_select = c(
    game_id,
    game_date,
    text
  )
)

cat("PBP rows:", format(nrow(pbp_data), big.mark = ","), "\n\n")


# ==============================================================================
# 2. 提取 Coach's Challenge 最终结果
# ==============================================================================

challenge_data <- pbp_data %>%
  
  # --------------------------------------------------------------------------
# A. 基础筛选
# --------------------------------------------------------------------------

filter(
  !is.na(text),
  as.Date(game_date) >= as.Date("2025-10-20")
) %>%
  
  # 必须包含 Challenge
  filter(
    str_detect(
      text,
      regex("Challenge", ignore_case = TRUE)
    )
  ) %>%
  
  # 只保留已经给出最终裁决的 Challenge
  filter(
    str_detect(
      text,
      regex(
        "OVERTURNED|REVERSED|STANDS|SUPPORTS|CONFIRMED|retain|charged",
        ignore_case = TRUE
      )
    )
  ) %>%
  
  # 排除裁判主动发起的 review
  filter(
    !str_detect(
      text,
      regex("REF-INITIATED", ignore_case = TRUE)
    )
  ) %>%
  
  
  # --------------------------------------------------------------------------
# B. 提取挑战球队、结果、时间
# --------------------------------------------------------------------------

mutate(
  
  # ESPN PBP Challenge 文本通常包含：
  # [Lakers] / [LAL] 等方括号内容
  Team_In_Bracket = str_extract(
    text,
    "(?<=\\[)[A-Za-z0-9 .'-]+(?=\\])"
  ),
  
  # 如果方括号里实际是时间，例如 [10:23]，则排除
  Team_Raw = if_else(
    str_detect(Team_In_Bracket, "\\d{1,2}:\\d{2}"),
    NA_character_,
    Team_In_Bracket
  ),
  
  # ------------------------------------------------------------------------
  # C. 球队名称标准化：统一输出 NBA 三字母缩写
  # ------------------------------------------------------------------------
  
  Team_Standard = case_when(
    
    str_detect(Team_Raw, regex("\\bLakers\\b|\\bLAL\\b", TRUE)) ~ "LAL",
    str_detect(Team_Raw, regex("\\bClippers\\b|\\bLAC\\b", TRUE)) ~ "LAC",
    str_detect(Team_Raw, regex("\\bWarriors\\b|\\bGSW\\b", TRUE)) ~ "GSW",
    str_detect(Team_Raw, regex("\\bKings\\b|\\bSAC\\b", TRUE)) ~ "SAC",
    str_detect(Team_Raw, regex("\\bSuns\\b|\\bPHX\\b", TRUE)) ~ "PHX",
    
    str_detect(Team_Raw, regex("\\bNuggets\\b|\\bDEN\\b", TRUE)) ~ "DEN",
    str_detect(Team_Raw, regex("\\bTimberwolves\\b|\\bMIN\\b", TRUE)) ~ "MIN",
    str_detect(Team_Raw, regex("\\bThunder\\b|\\bOKC\\b", TRUE)) ~ "OKC",
    str_detect(Team_Raw, regex("\\bTrail Blazers\\b|\\bBlazers\\b|\\bPOR\\b", TRUE)) ~ "POR",
    str_detect(Team_Raw, regex("\\bJazz\\b|\\bUTA\\b", TRUE)) ~ "UTA",
    
    str_detect(Team_Raw, regex("\\bMavericks\\b|\\bDAL\\b", TRUE)) ~ "DAL",
    str_detect(Team_Raw, regex("\\bRockets\\b|\\bHOU\\b", TRUE)) ~ "HOU",
    str_detect(Team_Raw, regex("\\bSpurs\\b|\\bSAS\\b", TRUE)) ~ "SAS",
    str_detect(Team_Raw, regex("\\bPelicans\\b|\\bNOP\\b", TRUE)) ~ "NOP",
    str_detect(Team_Raw, regex("\\bGrizzlies\\b|\\bMEM\\b", TRUE)) ~ "MEM",
    
    str_detect(Team_Raw, regex("\\bCeltics\\b|\\bBOS\\b", TRUE)) ~ "BOS",
    str_detect(Team_Raw, regex("\\bNets\\b|\\bBKN\\b", TRUE)) ~ "BKN",
    str_detect(Team_Raw, regex("\\bKnicks\\b|\\bNYK\\b", TRUE)) ~ "NYK",
    str_detect(Team_Raw, regex("\\b76ers\\b|\\bSixers\\b|\\bPHI\\b", TRUE)) ~ "PHI",
    str_detect(Team_Raw, regex("\\bRaptors\\b|\\bTOR\\b", TRUE)) ~ "TOR",
    
    str_detect(Team_Raw, regex("\\bBulls\\b|\\bCHI\\b", TRUE)) ~ "CHI",
    str_detect(Team_Raw, regex("\\bCavaliers\\b|\\bCavs\\b|\\bCLE\\b", TRUE)) ~ "CLE",
    str_detect(Team_Raw, regex("\\bPistons\\b|\\bDET\\b", TRUE)) ~ "DET",
    str_detect(Team_Raw, regex("\\bPacers\\b|\\bIND\\b", TRUE)) ~ "IND",
    str_detect(Team_Raw, regex("\\bBucks\\b|\\bMIL\\b", TRUE)) ~ "MIL",
    
    str_detect(Team_Raw, regex("\\bHawks\\b|\\bATL\\b", TRUE)) ~ "ATL",
    str_detect(Team_Raw, regex("\\bHornets\\b|\\bCHA\\b", TRUE)) ~ "CHA",
    str_detect(Team_Raw, regex("\\bHeat\\b|\\bMIA\\b", TRUE)) ~ "MIA",
    str_detect(Team_Raw, regex("\\bMagic\\b|\\bORL\\b", TRUE)) ~ "ORL",
    str_detect(Team_Raw, regex("\\bWizards\\b|\\bWAS\\b", TRUE)) ~ "WAS",
    
    TRUE ~ Team_Raw
  ),
  
  # ------------------------------------------------------------------------
  # D. Challenge 结果
  # ------------------------------------------------------------------------
  
  Outcome = case_when(
    
    # 成功挑战
    str_detect(
      text,
      regex(
        "OVERTURNED|REVERSED|retain",
        ignore_case = TRUE
      )
    ) ~ "Successful",
    
    # 挑战失败
    str_detect(
      text,
      regex(
        "STANDS|SUPPORTS|CONFIRMED|charged",
        ignore_case = TRUE
      )
    ) ~ "Unsuccessful",
    
    TRUE ~ "Unknown"
  ),
  
  
  # ------------------------------------------------------------------------
  # E. 提取比赛时间
  # ------------------------------------------------------------------------
  
  TimeStamp = str_extract(
    text,
    "\\d{1,2}:\\d{2}"
  )
) %>%
  
  
  # --------------------------------------------------------------------------
# F. 清洗
# --------------------------------------------------------------------------

filter(
  !is.na(Team_Standard),
  Team_Standard != "",
  Outcome != "Unknown"
) %>%
  
  # 同一次 Challenge 在 ESPN PBP 中有时会出现重复描述
  # 用比赛 + 球队 + 比赛时间 + 最终结果去重
  distinct(
    game_id,
    Team_Standard,
    TimeStamp,
    Outcome,
    .keep_all = TRUE
  )


# ==============================================================================
# 3. 全联盟 Challenge 榜单
# ==============================================================================

league_challenge_final <- challenge_data %>%
  group_by(Team_Standard) %>%
  summarise(
    Total = n(),
    Success = sum(Outcome == "Successful"),
    Fail = sum(Outcome == "Unsuccessful"),
    
    Success_Rate = round(
      Success / Total,
      3
    ),
    
    Rate_Display = paste0(
      round(Success / Total * 100, 1),
      "%"
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    desc(Total),
    desc(Success_Rate)
  )


# ==============================================================================
# 4. Console 输出
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("  2025-26 NBA 全联盟 Coach's Challenge 榜单\n")
cat("============================================================\n")
cat(
  "有效 Challenge：",
  nrow(challenge_data),
  "\n"
)
cat(
  "涉及比赛：",
  n_distinct(challenge_data$game_id),
  "\n"
)
cat("============================================================\n\n")

print(
  league_challenge_final,
  n = 35,
  width = Inf
)