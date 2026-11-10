library(dplyr)
library(stringr)

# --- 1. 修正 Player ID 到 Name 的映射表创建 ---

# 尝试从最干净的得分事件中提取姓名
player_name_map_clean <- pbp %>%
  filter(!is.na(athlete_id_1), scoring_play == TRUE) %>% # 筛选出得分事件，通常文本最干净
  # 提取文本开头的姓名部分，直到遇到动作动词或数字
  mutate(
    # 正则表达式：提取行首的文本，直到遇到 ' made' 或 ' misses' 或 ' fouled' 等动词，
    # 或者直到文本结束。这通常能获取完整的球员姓名。
    Player_Name = str_extract(text, "^[A-Za-z\\s\\.\\-']+(?=\\s+(made|missed|fouled|turned|steals|passes))|^[A-Za-z\\s\\.\\-']+$") %>% str_trim()
  ) %>%
  filter(!is.na(Player_Name), Player_Name != "") %>%
  select(Z_Bound_Player_ID = athlete_id_1, Player_Name) %>%
  # 保留每个 ID 唯一的、最常见的姓名映射
  group_by(Z_Bound_Player_ID) %>%
  summarise(Player_Name = first(Player_Name))

# --- 2. 将 Z-Bound 榜单与干净的姓名映射表合并 ---

# 假设 z_bound_rankings 已经包含了所有 338 名球员的 ID 和次数
final_top_15_rankings_clean <- z_bound_rankings %>%
  # 确保只取前 15 名
  arrange(desc(Total_Z_Bounds)) %>%
  head(15) %>%
  
  # 合并干净的姓名
  left_join(player_name_map_clean, by = "Z_Bound_Player_ID") %>%
  arrange(desc(Total_Z_Bounds)) %>%
  
  # 重新排列列顺序
  select(Z_Bound_Player_ID, Total_Z_Bounds, Player_Name)

cat("\n--- NBA 本赛季自投自抢 (Z-Bound) Top 15 最终榜单 ---\n")
print(final_top_15_rankings_clean)