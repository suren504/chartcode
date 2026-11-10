library(tidyverse)
library(gt)
library(gtExtras)
library(glue)

# ==============================================================================
# 1. 原始数据 (你的 Top 10 + 底部数据)
# ==============================================================================
df <- tribble(
  ~排名, ~日期, ~主场球队, ~客场球队, ~加时赛数, ~开始时间, ~结束时间, ~比赛用时, ~赛果, ~最佳球员, ~最差球员,
  "1", "2025-10-22", "OKC", "HOU", 2, "07:43", "10:59", 196.4, "OKC 125 - 124 HOU", "S. Gilgeous-Alexander", "R. Sheppard",
  "2", "2025-10-24", "IND", "OKC", 2, "07:45", "10:58", 192.9, "IND 135 - 141 OKC", "B. Mathurin", "A. Nesmith",
  "3", "2025-11-15", "DAL", "LAC", 2, "09:40", "12:50", 189.8, "DAL 127 - 133 LAC", "J. Harden", "D. Russell",
  "4", "2025-11-17", "UTAH", "CHI", 2, "09:11", "12:20", 189.1, "UTAH 150 - 147 CHI", "K. George", "K. Huerter",
  "5", "2025-11-30", "PHI", "ATL", 2, "07:11", "10:12", 181.5, "PHI 134 - 142 ATL", "T. Maxey", "O. Okongwu",
  "6", "2025-12-26", "DEN", "MIN", 1, "11:43", "14:43", 179.6, "DEN 141 - 138 MIN", "A. Edwards", "J. Valančiūnas",
  "7", "2025-11-06", "LAL", "SA", 0, "11:13", "14:09", 176.3, "LAL 118 - 116 SA", "D. Ayton", "J. Champagnie",
  "8", "2025-11-11", "DET", "WSH", 1, "08:13", "11:08", 175.9, "DET 137 - 135 WSH", "D. Jenkins", "A. Sarr",
  "9", "2025-11-11", "MIA", "CLE", 1, "08:43", "11:38", 174.9, "MIA 140 - 138 CLE", "J. Jaquez Jr", "J. Tyson",
  "10", "2025-12-19", "POR", "SAC", 1, "11:12", "14:05", 173.0, "POR 134 - 133 SAC", "D. Clingan", "R. Westbrook",
  "11-510", "...", "...", "...", NA, "...", "...", NA, "...", "...", "...",
  "511", "2025-11-25", "MEM", "DEN", 0, "09:10", "11:14", 123.9, "MEM 115 - 125 DEN", "J. Murray", "S. Jones",
  "512", "2025-11-13", "BOS", "MEM", 0, "08:41", "10:45", 123.8, "BOS 131 - 95 MEM", "N. Queta", "C. Coward",
  "513", "2025-12-28", "HOU", "CLE", 0, "09:11", "11:15", 123.6, "HOU 117 - 100 CLE", "K. Durant", "D. Hunter",
  "514", "2025-12-04", "HOU", "SAC", 0, "09:11", "11:14", 123.5, "HOU 121 - 95 SAC", "M. Raynaud", "K. Murray",
  "515", "2026-01-01", "ATL", "MIN", 0, "04:10", "06:14", 123.2, "ATL 126 - 102 MIN", "J. Johnson", "B. Hyland",
  "516", "2026-01-02", "DAL", "PHI", 0, "09:40", "11:43", 122.6, "DAL 108 - 123 PHI", "Q. Grimes", "C. Flagg",
  "517", "2025-12-08", "CHI", "GS", 0, "08:11", "10:14", 122.5, "CHI 91 - 123 GS", "J. Carter", "J. Giddey",
  "518", "2025-12-07", "BKN", "NO", 0, "06:10", "08:12", 121.5, "BKN 119 - 101 NO", "M. Porter Jr", "D. Queen",
  "519", "2025-11-27", "MIA", "MIL", 0, "08:41", "10:42", 120.9, "MIA 106 - 103 MIL", "T. Herro", "C. Anthony",
  "520", "2025-12-06", "MEM", "LAC", 0, "09:12", "11:13", 120.9, "MEM 107 - 98 LAC", "C. Coward", "S. Aldama"
)

# ==============================================================================
# 2. 建立“球员字典” (最重要的一步)
# ==============================================================================
# 我们必须手动告诉 R：缩写是谁，全名是什么(用于找文件)，中文名是什么
player_lookup <- tribble(
  ~abbr_name, ~full_filename, ~cn_name,
  "S. Gilgeous-Alexander", "Shai_Gilgeous-Alexander", "亚历山大",
  "R. Sheppard", "Reed_Sheppard", "谢泼德",
  "B. Mathurin", "Bennedict_Mathurin", "马瑟林",
  "A. Nesmith", "Aaron_Nesmith", "奈史密斯",
  "J. Harden", "James_Harden", "哈登",
  # --- 修正：匹配你本地带单引号的文件名 ---
  "D. Russell", "D'Angelo_Russell", "拉塞尔", 
  "K. George", "Keyonte_George", "乔治",
  "K. Huerter", "Kevin_Huerter", "许尔特",
  "T. Maxey", "Tyrese_Maxey", "马克西",
  "O. Okongwu", "Onyeka_Okongwu", "奥孔古",
  "A. Edwards", "Anthony_Edwards", "爱德华兹",
  "J. Valančiūnas", "Jonas_Valanciunas", "瓦兰", 
  "D. Ayton", "Deandre_Ayton", "艾顿",
  "J. Champagnie", "Julian_Champagnie", "香槟",
  "D. Jenkins", "Daniss_Jenkins", "詹金斯",
  "A. Sarr", "Alex_Sarr", "萨尔",
  "J. Jaquez Jr", "Jaime_Jaquez_Jr", "哈克斯",
  "J. Tyson", "Jaylon_Tyson", "泰森",
  "D. Clingan", "Donovan_Clingan", "克林根",
  "R. Westbrook", "Russell_Westbrook", "威少",
  "J. Murray", "Jamal_Murray", "穆雷",
  "S. Jones", "Spencer_Jones", "琼斯",
  "N. Queta", "Neemias_Queta", "克塔",
  "C. Coward", "Cedric_Coward", "考沃德",
  "K. Durant", "Kevin_Durant", "杜兰特",
  # --- 修正：匹配你本地带单引号的文件名 ---
  "D. Hunter", "De'Andre_Hunter", "亨特", 
  "M. Raynaud", "Maxime_Raynaud", "雷诺",
  "K. Murray", "Keegan_Murray", "默里",
  "J. Johnson", "Jalen_Johnson", "杰伦·约翰逊",
  "B. Hyland", "Bones_Hyland", "海兰德",
  "Q. Grimes", "Quentin_Grimes", "格莱姆斯",
  "C. Flagg", "Cooper_Flagg", "弗拉格",
  "J. Carter", "Jevon_Carter", "卡特",
  "J. Giddey", "Josh_Giddey", "基迪",
  "M. Porter Jr", "Michael_Porter_Jr", "波特",
  "D. Queen", "Derik_Queen", "奎因",
  "T. Herro", "Tyler_Herro", "希罗",
  "C. Anthony", "Cole_Anthony", "安东尼",
  "S. Aldama", "Santi_Aldama", "阿尔达马"
)

# 球队映射
team_map <- c(
  "OKC" = "雷霆", "HOU" = "火箭", "IND" = "步行者", "DAL" = "独行侠", "LAC" = "快船",
  "UTAH" = "爵士", "CHI" = "公牛", "PHI" = "76人", "ATL" = "老鹰", "DEN" = "掘金",
  "MIN" = "森林狼", "LAL" = "湖人", "SA" = "马刺", "DET" = "活塞", "WSH" = "奇才",
  "MIA" = "热火", "CLE" = "骑士", "POR" = "开拓者", "SAC" = "国王", "MEM" = "灰熊",
  "BOS" = "凯尔特人", "BKN" = "篮网", "NO" = "鹈鹕", "MIL" = "雄鹿", "GS" = "勇士",
  "..." = "..."
)

# ==============================================================================
# 3. 数据清洗与HTML构造 (修正版 - 只留头像)
# ==============================================================================
clean_df <- df %>%
  mutate(排名 = paste0("#", 排名)) %>%
  mutate(
    score_home = str_extract(赛果, "(?<=\\s)\\d+(?=\\s-)"),
    score_away = str_extract(赛果, "(?<=\\-\\s)\\d+(?=\\s)")
  ) %>%
  
  # --- 先生成赛果 HTML (此时主队名还是英文缩写，用于匹配LOGO文件名) ---
  mutate(
    赛果_html = case_when(
      排名 == "#11-510" ~ "...", 
      TRUE ~ glue(
        "<div style='display: flex; align-items: center; justify-content: center; gap: 10px;'>",
        "<img src='file:///D:/nba/球队logo/球队logo方/{主场球队}.png' style='height: 30px;'>", 
        "<span style='font-weight: bold; font-size: 14px;'>{score_home} - {score_away}</span>",
        "<img src='file:///D:/nba/球队logo/球队logo方/{客场球队}.png' style='height: 30px;'>",
        "</div>"
      )
    )
  ) %>%
  
  # --- 生成完图片后，再把球队名翻译成中文 ---
  mutate(
    主场球队 = ifelse(排名 == "#11-510", "...", team_map[主场球队]),
    客场球队 = ifelse(排名 == "#11-510", "...", team_map[客场球队])
  ) %>%
  
  # --- 关联球员字典 ---
  left_join(player_lookup, by = c("最佳球员" = "abbr_name")) %>%
  rename(best_file = full_filename, best_cn = cn_name) %>%
  left_join(player_lookup, by = c("最差球员" = "abbr_name")) %>%
  rename(worst_file = full_filename, worst_cn = cn_name) %>%
  
  # --- 【修改点1】生成球员 HTML (删除了名字 span) ---
  mutate(
    最佳球员_html = case_when(
      排名 == "#11-510" ~ "...",
      is.na(best_file) ~ "...", # 如果找不到，显示点点点
      TRUE ~ glue(
        # 只保留 img 标签，去掉后面的 span 标签
        "<img src=\"file:///D:/nba/球员头像/{best_file}.png\" style='height: 40px; width: 40px; border-radius: 50%; object-fit: cover; border: 2px solid #eee;'>"
      )
    ),
    最差球员_html = case_when(
      排名 == "#11-510" ~ "...",
      is.na(worst_file) ~ "...",
      TRUE ~ glue(
        # 只保留 img 标签
        "<img src=\"file:///D:/nba/球员头像/{worst_file}.png\" style='height: 40px; width: 40px; border-radius: 50%; object-fit: cover; border: 2px solid #ccc; filter: grayscale(20%);'>"
      )
    )
  ) %>%
  
  select(排名, 日期, 主场球队, 客场球队, 加时赛数, 开始时间, 结束时间, 比赛用时, 赛果_html, 最佳球员_html, 最差球员_html)

# ==============================================================================
# 4. GT 表格渲染 (修正版 - 加入渐变色)
# ==============================================================================
my_final_table <- clean_df %>%
  gt() %>%
  tab_header(
    title = md("**2025-26 赛季 NBA 用时最长与最短的单场比赛TOP10**"),
    subtitle = "统计截止：2026.1.4|数据源：hoopR|制作人：俗人罢了|以北京时间为准"
  ) %>%
  # 解析 HTML
  fmt_markdown(columns = c(赛果_html, 最佳球员_html, 最差球员_html)) %>%
  
  # --- 【修改点2】给“比赛用时”加渐变色 ---
  data_color(
    columns = 比赛用时,
    method = "numeric",
    palette = c('#F2F2F2', '#F7565A'), # 灰 -> 红
    na_color = "white" # 省略号那一行不填色
  ) %>%
  
  cols_label(
    赛果_html = "赛果",
    比赛用时 = "比赛用时 (分钟)",
    最佳球员_html = "最佳球员",
    最差球员_html = "最差球员"
  ) %>%
  tab_options(
    table.font.names = "Microsoft YaHei", 
    column_labels.font.weight = "bold",
    table.border.top.style = "none",
    data_row.padding = px(6), 
    table.width = px(1350) 
  ) %>%
  # 全局居中 (因为去掉了名字，头像居中更好看，所以去掉了之前专门的左对齐设置)
  cols_align(align = "center", columns = everything()) %>%
  
  tab_style(
    style = cell_text(font = "Consolas", weight = "bold"),
    locations = cells_body(columns = 排名)
  ) %>%
  cols_width(
    排名 ~ px(60),
    日期 ~ px(100),
    主场球队 ~ px(80),
    客场球队 ~ px(80),
    加时赛数 ~ px(80),
    开始时间 ~ px(80),
    结束时间 ~ px(80),
    比赛用时 ~ px(120),
    赛果_html ~ px(150),
    # 【修改点3】因为没了名字，宽度缩小到 70px 就够了
    最佳球员_html ~ px(60), 
    最差球员_html ~ px(60)
  )

# 预览
my_final_table

# 保存
gtsave(my_final_table, "D:/nba/NBA_Rank_Avatar_Only_Gradient.png", vwidth = 1400)