library(tidyverse)
library(stringr) # 确保加载字符串处理包

# 1. 加载本地 RDS 文件
file_path <- "D:/下载文件/球员pbp.rds"

if (!file.exists(file_path)) {
  stop("找不到文件，请确认路径是否正确：", file_path)
}

df_pbp <- readRDS(file_path)

# 2. 提取唯一的球员名单 (增加后缀清洗逻辑)
players_list <- df_pbp %>%
  rename_with(~ "FullName", any_of(c("athlete_display_name", "player_name", "name", "player"))) %>%
  distinct(FullName, .keep_all = TRUE) %>%
  filter(!is.na(FullName)) %>%
  mutate(
    # --- 关键修改开始 ---
    # 定义一个清洗后的临时名字字段
    # 正则逻辑：
    # (?i)  -> 忽略大小写
    # \\s+  -> 匹配后缀前的空格
    # (...) -> 匹配 Jr, Jr., Sr, Sr., II, III, IV, V 这些常见后缀
    # $     -> 必须出现在字符串末尾
    CleanName = str_remove(FullName, "(?i)\\s+(Jr\\.?|Sr\\.?|I{2,3}|IV|V)$"),
    
    # 基于清洗后的名字提取名和姓
    FirstName = word(CleanName, 1),
    LastName  = word(CleanName, -1)
    # --- 关键修改结束 ---
  )

# 3. 定义你的统计目标
target_first_names <- c("Jalen", "Isaiah", "Jordan", "Chris", "Josh", 
                        "Kevin", "Aaron", "Anthony", "Brandon", "Cam")

target_last_names <- c("Williams", "Jones", "Green", "Johnson", "Smith", 
                       "Jackson", "Martin", "Miller", "Murray", "Thompson")

# 4. 筛选并按照你的排名顺序排序
res_first <- players_list %>%
  filter(FirstName %in% target_first_names) %>%
  mutate(FirstName = factor(FirstName, levels = target_first_names)) %>%
  arrange(FirstName) %>%
  select(FirstName, FullName, everything()) # 保留原始 FullName 以便查看 Jr.

res_last <- players_list %>%
  filter(LastName %in% target_last_names) %>%
  mutate(LastName = factor(LastName, levels = target_last_names)) %>%
  arrange(LastName) %>%
  select(LastName, FullName, everything())

# 5. 打印结果
div <- strrep("=", 50)
cat("\n", div, "\n  修正后：名字符合排名的球员 (含后缀处理)\n", div, "\n")
print(res_first, n = 100)

cat("\n", div, "\n  修正后：姓氏符合排名的球员 (含后缀处理)\n", div, "\n")
print(res_last, n = 100)