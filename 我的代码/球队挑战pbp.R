# ===============================
# 🐝 夏洛特黄蜂队 (Hornets) 挑战记录提取版
# ===============================
library(tidyverse)

# 请将路径替换为你电脑上文件的实际位置
pbp_data <- read_rds("D:/下载文件/play_by_play_2026.rds")

hornets_challenge_log <- pbp_data %>%
  # --- A. 基础筛选 ---
  filter(as.Date(game_date) >= as.Date("2025-10-21")) %>%
  
  # --- B. 锁定挑战事件 ---
  filter(str_detect(type_text, "Overturned|Stands|Supported")) %>%
  filter(str_detect(type_text, "Coach's Challenge")) %>%
  
  # --- C. 提取发起球队 ---
  mutate(
    # 提取方括号里的内容，例如 [Hornets] -> Hornets
    Team_Initiator = str_extract(text, "(?<=\\[).+?(?=\\])"),
    
    # 结果汉化
    Result_CN = case_when(
      str_detect(type_text, "Overturned") ~ "成功 (改判)",
      str_detect(type_text, "Stands") ~ "失败 (维持原判)",
      str_detect(type_text, "Supported") ~ "失败 (确认无误)",
      TRUE ~ "未知"
    )
  ) %>%
  
  # --- D. 锁定黄蜂队 (核心修改点) ---
  filter(Team_Initiator == "Hornets") %>%
  
  # --- E. 整理与去重 ---
  distinct(game_id, period, clock_display_value, type_text, .keep_all = TRUE) %>%
  
  select(
    Date = game_date,         # 日期
    Team = Team_Initiator,    # 发起队
    Time = clock_display_value, # 剩余时间
    Result = Result_CN,       # 结果
    Detail = text             # 详细描述
  ) %>%
  arrange(desc(Date))

# --- F. 完整输出 ---
cat("\n=== 🐝 夏洛特黄蜂队 (Hornets) 本赛季挑战详实记录 ===\n")
print(hornets_challenge_log, n = Inf, width = Inf)

# --- G. 统计摘要 ---
if(nrow(hornets_challenge_log) > 0){
  cat("\n--- 统计摘要 ---\n")
  hornets_challenge_log %>% count(Result) %>% print()
} else {
  cat("\n⚠️ 警告：未提取到黄蜂队数据。请检查 Team_Initiator 是否匹配。 \n")
}