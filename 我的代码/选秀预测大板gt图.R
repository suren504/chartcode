# 加载必要的包
library(tidyverse)
library(gt)
library(glue)

# 1. 原始数据
raw_data <- "
#1 HawksAJ Dybantsa G BYU#2 PacersDarryn Peterson G Kansas#3 WizardsCameron Boozer F Duke#4 KingsCaleb Wilson F UNC#5 NetsKingston Flemings G Houston#6 JazzKoa Peat G Arizona#7 HornetsHannes Steinbach F Washington#8 MavericksJayden Quaintance C Kentucky#9 BucksBraylon Mullins F UConn#10 GrizzliesLabaron Philon G Alabama#11 SpursYaxel Lendeborg F Michigan#12 ThunderPatrick Ngongba II C Duke#13 BullsKeaton Wagler G Illinois#14 Trail BlazersThomas Haugh F Florida#15 HeatBennett Stirtz G Iowa#16 HawksMikel Brown Jr G Louisville#17 GrizzliesNate Ament F Tennessee#18 ThunderDariu Acuff Jr G Arkansas#19 RaptorsHenri Veesaar F UNC#20 WarriorsJoshua Jefferson F Iowa State#21 KnicksChristian Anderson Jr G Texas Tech#22 HornetsNeoklis Avdalas G Virginia Tech#23 LakersAday Mara C Michigan#24 CelticsTounde Yessoufou F Baylor#25 ThunderDailyn Swain G Texas#26 TimberwolvesCameron Carr G Baylor#27 NuggetsTyler Tanner G Vanderbilt#28 CavaliersKarim Lopez F NZ Breakers#29 PistonsBrayden Burries G Arizona#30 WizardsFlory Bidunga C Kansas#31 CelticsRichie Saunders G BYU#32 GrizzliesChris Cenac Jr F Houston#33 KnicksJuke Harris G Wake Forest#34 KingsIsaiah Evans G Duke#35 NetsMeleek Thomas G Arkansas#36 SpursKillyan Toure G Iowa State#37 KingsDash Daniels G Melbourne Unite#38 ThunderPryce Sandfort F Nebraska#39 CelticsAmari Allen F Alabama#40 ClippersEbuka Okorie G Stanford#41 NetsMorez Johnson Jr C Michigan#42 ClippersDe'Shayne Montgomery G Dayton#43 WizardsRyan Conwell G Louisville#44 SpursBraden Smith G Purdue#45 SpursMilan Momcilovic F Iowa State#46 CavaliersAlex Condon C Florida#47 MagicZuby Ejiofor F St John's#48 RocketsPaul McNeil Jr G NC State#49 HornetsAlex Karaban F UConn#50 RaptorsAlvaro Folgueiras F Iowa#51 WizardsMassamba Diop C Arizona State#52 WizardsTarris Reed Jr C UConn#53 RaptorsKwame Evans Jr F Oregon#54 HawksTomislav Ivisic C Illinois#55 RocketsCayden Boozer G Duke#56 KnicksMiles Uzan G Houston#57 SunsDavid Punch F TCU#58 TimberwolvesJT Toppin F NC State#59 KnicksPaulius Murauskas F Saint Mary's#60 WizardsZvonimir Ivisic C Illinois
"

# 2. NBA 球队列表 (用于切割字符串，顺序很重要，长名字在前)
nba_teams <- c(
  "Trail Blazers", "Timberwolves", "Cavaliers", "Mavericks", "Grizzlies", 
  "Clippers", "Warriors", "Pelicans", "Pistons", "Rockets", "Wizards", 
  "Hornets", "Thunder", "Celtics", "Raptors", "Pacers", "Nuggets", 
  "Lakers", "Sixers", "Knicks", "Spurs", "Kings", "Hawks", "Bucks", 
  "Bulls", "Heat", "Jazz", "Nets", "Suns", "Magic"
)

# 3. 数据清洗函数
parse_draft_data <- function(raw_text) {
  # 按 # 分割并去空
  items <- str_split(raw_text, "#")[[1]]
  items <- items[items != ""]
  
  # 创建空的数据框
  parsed_df <- tibble(
    rank = integer(),
    pick_html = character(),
    player = character(),
    pos = character(),
    school = character()
  )
  
  for (item in items) {
    # 提取排名 (数字) 和 剩余部分
    rank_match <- str_match(item, "^(\\d+)\\s*(.*)")
    if (any(is.na(rank_match))) next
    
    rank <- as.integer(rank_match[2])
    rest <- rank_match[3]
    
    # 匹配球队
    team <- "Unknown"
    player_part <- rest
    
    for (t in nba_teams) {
      if (str_starts(rest, t)) {
        team <- t
        # 切掉队名，保留后面
        player_part <- str_trim(str_sub(rest, start = nchar(t) + 1))
        break
      }
    }
    
    # 匹配 球员 位置 学校 (找最后一个 G/F/C)
    # 正则逻辑：(.+) 任意字符空格 ([GFC]) 位置空格 (.*) 学校
    info_match <- str_match(player_part, "(.*)\\s+([GFC])\\s+(.*)")
    
    if (!any(is.na(info_match))) {
      player <- str_trim(info_match[2])
      pos <- str_trim(info_match[3])
      school <- str_trim(info_match[4])
    } else {
      player <- player_part
      pos <- ""
      school <- ""
    }
    
    # 构建本地图片路径 (注意 R 在读取本地图片渲染 HTML 时，使用 file:/// 更稳妥)
    # 这里的路径是你的 D 盘路径
    logo_path <- glue("D:/nba/球队logo/队标_透明/{team}.png")
    
    # 构建 HTML (Flex布局实现排名+Logo并排)
    pick_html <- glue(
      "<div style='display: flex; align-items: center; gap: 10px;'>
         <span style='font-weight: bold; font-size: 14px; color: #333; width: 25px;'>#{rank}</span>
         <img src='file:///{logo_path}' style='height: 30px; width: auto;'>
       </div>"
    )
    
    parsed_df <- bind_rows(parsed_df, tibble(
      rank = rank,
      pick_html = pick_html,
      player = player,
      pos = pos,
      school = school
    ))
  }
  return(parsed_df)
}

# --- 执行清洗 ---
df_all <- parse_draft_data(raw_data)

# 4. 拆分首轮 (1-30) 和次轮 (31-60) 并横向拼接
df_left <- df_all %>% 
  filter(rank <= 30) %>% 
  select(pick_L = pick_html, player_L = player, pos_L = pos, school_L = school)

df_right <- df_all %>% 
  filter(rank > 30) %>% 
  select(pick_R = pick_html, player_R = player, pos_R = pos, school_R = school)

# 横向合并 (bind_cols 要求行数一致，这里刚好都是30行)
df_final <- bind_cols(df_left, df_right)

# 5. 制作 GT 表格
table <- df_final %>% 
  gt() %>% 
  # 设置表头
  tab_header(
    title = md("**Jstar 2026 NBA Mock Draft V.2**"),
    subtitle = "顺位截止至北京时间1.20|不代表主流行情，个别球员默认留校|制作：俗人罢了"
  ) %>% 
  # 设置列名
  cols_label(
    pick_L = "P I C K", player_L = "PLAYER", pos_L = "POS", school_L = "TEAM",
    pick_R = "P I C K", player_R = "PLAYER", pos_R = "POS", school_R = "TEAM"
  ) %>% 
  # 渲染 HTML 内容 (关键)
  fmt_markdown(columns = c(pick_L, pick_R)) %>% 
  # 居中对齐位置
  cols_align(align = "center", columns = c(pos_L, pos_R)) %>% 
  # 斑马纹
  opt_row_striping() %>% 
  # 样式微调
  tab_options(
    table.font.names = "Roboto Condensed",
    data_row.padding = px(5),
    column_labels.font.weight = "bold",
    column_labels.background.color = "white",
    table_body.hlines.color = "transparent",
    table.border.top.style = "none",
    heading.align = "center"
  ) %>% 
  # 中间加一条分割线
  tab_style(
    style = cell_borders(sides = "right", color = "lightgray", weight = px(2)),
    locations = list(
      cells_body(columns = school_L),
      cells_column_labels(columns = school_L)
    )
  )

# 6. 保存图片 (需要安装 webshot2 包)
gtsave(table, "2026_mock_draft_R.png", vwidth = 1200, expand = 20)
browseURL("2026_mock_draft_R.png")