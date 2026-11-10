library(tidyverse)
library(fs)       
library(lubridate) 

# ==============================================================================
# 1. 设置路径与目标 (Setup)
# ==============================================================================

# 数据源路径：根据你提供的文件位置
source_folder <- "D:/nba/RStudio/NBA-MVP-Odds/data" 

# 修改这里：将 Jaylen Brown 加入名单
# 注意：名字必须和 CSV 文件里的 "name" 列完全一致，一个字母都不能错
target_players <- c(
  "Shai Gilgeous-Alexander", 
  "Nikola Jokic", 
  "Luka Doncic", 
  "Cade Cunningham",
  "Jaylen Brown"  # <--- 新增了这位
)

# ==============================================================================
# 2. 批量读取与筛选 (Batch Read & Filter)
# ==============================================================================

print("正在处理 5 位球员的数据，请稍候...")

# 获取该目录下所有 CSV 文件
file_list <- dir_ls(source_folder, glob = "*.csv")

# 批量读取并清洗
mvp_evolution <- file_list %>%
  map_dfr(read_csv, col_types = cols(.default = "c"), .id = "filename") %>% 
  
  # 1. 提取日期
  mutate(date = ymd(str_extract(filename, "\\d{4}_\\d{2}_\\d{2}"))) %>%
  
  # 2. 筛选球员：只保留这 5 个人
  filter(name %in% target_players) %>%
  
  # 3. 选择关键列：日期、名字、球队、以及 Caesars 的赔率
  select(date, name, team, raw_odds = caesars_odds) %>%
  
  # 4. 数据类型转换
  mutate(raw_odds = parse_number(raw_odds))

# ==============================================================================
# 3. 计算隐含胜率 (Calculate Implied Probability)
# ==============================================================================

final_data <- mvp_evolution %>%
  filter(!is.na(raw_odds)) %>% 
  mutate(
    prob = case_when(
      # 负赔率计算公式 (例如 -330)
      raw_odds < 0 ~ abs(raw_odds) / (abs(raw_odds) + 100),
      
      # 正赔率计算公式 (例如 +5000)
      raw_odds > 0 ~ 100 / (raw_odds + 100),
      
      TRUE ~ 0
    )
  ) %>%
  # 转换为百分比，保留1位小数
  mutate(prob_percent = round(prob * 100, 1)) %>%
  # 按日期和概率降序排列，方便查看
  arrange(date, desc(prob))

# ==============================================================================
# 4. 导出结果 (Export)
# ==============================================================================

# 保存到 D:/nba 文件夹下
output_path <- "D:/nba/mvp_top5_evolution.csv"
write_csv(final_data, output_path)

print(paste("成功！包含 Jaylen Brown 的 5 人数据已保存至:", output_path))