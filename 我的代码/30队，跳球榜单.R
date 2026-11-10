library(tidyverse)
library(gt)
library(gtExtras)
library(glue)
library(magick)
library(webshot2)

# ================================
# 1. 球员英文名映射 (新增：用于生成头像文件路径)
# ================================
# ❗ 注意：您提供的中文名与我之前使用的英文名略有不同，
# ❗ 请根据您的头像文件名 (例如 Alperen_Sengun) 修正以下映射。
# ❗ 这里的映射是基于常见昵称和文件名的猜测，如果头像路径不对，请检查这里。
player_name_en <- c(
  "文班亚马" = "Victor_Wembanyama",
  "德拉蒙德" = "Andre_Drummond",
  "马克·威廉姆斯" = "Mark_Williams",
  "努尔基奇" = "Jusuf_Nurkic",
  "温德尔·卡特" = "Wendell_Carter_Jr",
  "艾顿" = "Deandre_Ayton",
  "波尔特尔" = "Jakob_Poeltl",
  "克拉克斯顿" = "Nic_Claxton",
  "贾莱特·阿伦" = "Jarrett_Allen",
  "克林根" = "Donovan_Clingan",
  "凯尔·维尔" = "Kel'el_Ware",
  "唐斯" = "Karl-Anthony_Towns",
  "切特" = "Chet_Holmgren",
  "戈贝尔" = "Rudy_Gobert",
  "萨尔" = "Alex_Sarr",
  "瑞安·卡尔克布伦纳" = "Ryan_Kalkbrenner",
  "约基奇" = "Nikola_Jokic",
  "加福德" = "Daniel_Gafford",
  "奥孔古" = "Onyeka_Okongwu",
  "奎塔" = "Neemias_Queta",
  "贾伦·杰克逊" = "Jaren_Jackson_Jr",
  "杰伦·杜伦" = "Jalen_Duren",
  "祖巴茨" = "Ivica_Zubac",
  "武切维奇" = "Nikola_Vucevic",
  "追梦格林" = "Draymond_Green",
  "申京" = "Alperen_Sengun",
  "以赛亚·杰克逊" = "Isaiah_Jackson",
  "萨博尼斯" = "Domantas_Sabonis",
  "德里克·奎因" = "Derik_Queen",
  "迈尔斯·特纳" = "Myles_Turner"
)

# 球队中文映射 (保持不变)
team_name_cn <- c(
  SAS="圣安东尼奥马刺", PHI="费城76人", PHX="菲尼克斯太阳", UTA="犹他爵士",
  ORL="奥兰多魔术", LAL="洛杉矶湖人", TOR="多伦多猛龙", BKN="布鲁克林篮网",
  CLE="克利夫兰骑士", POR="波特兰开拓者", MIA="迈阿密热火", NYK="纽约尼克斯",
  OKC="俄克拉荷马城雷霆", MIN="明尼苏达森林狼", WAS="华盛顿奇才",
  CHA="夏洛特黄蜂", DEN="丹佛掘金", DAL="达拉斯独行侠",
  ATL="亚特兰大老鹰", BOS="波士顿凯尔特人", MEM="孟菲斯灰熊",
  DET="底特律活塞", LAC="洛杉矶快船", CHI="芝加哥公牛",
  GSW="金州勇士", HOU="休斯顿火箭", IND="印第安纳步行者",
  SAC="萨克拉门托国王", NOP="新奥尔良鹈鹕", MIL="密尔沃基雄鹿"
)

# ================================
# 2. 处理 jumpball 数据
# ================================
jumpball <- tribble(
  ~球队全称, ~缩写, ~球员, ~跳球次数, ~成功率,
  "圣安东尼奥马刺","SAS","文班亚马",15,93.3,
  "费城76人","PHI","德拉蒙德",13,84.6,
  "菲尼克斯太阳","PHX","马克·威廉姆斯",20,75.0,
  "犹他爵士","UTA","努尔基奇",20,75.0,
  "奥兰多魔术","ORL","温德尔·卡特",27,74.1,
  "洛杉矶湖人","LAL","艾顿",24,66.7,
  "多伦多猛龙","TOR","波尔特尔",23,65.2,
  "布鲁克林篮网","BKN","克拉克斯顿",28,64.3,
  "克利夫兰骑士","CLE","贾莱特·阿伦",19,57.9,
  "波特兰开拓者","POR","克林根",23,56.5,
  "迈阿密热火","MIA","凯尔·维尔",18,55.6,
  "纽约尼克斯","NYK","唐斯",20,55.0,
  "俄克拉荷马城雷霆","OKC","切特",24,54.2,
  "明尼苏达森林狼","MIN","戈贝尔",26,53.8,
  "华盛顿奇才","WAS","萨尔",23,52.2,
  "夏洛特黄蜂","CHA","瑞安·卡尔克布伦纳",25,52.0,
  "丹佛掘金","DEN","约基奇",25,52.0,
  "达拉斯独行侠","DAL","加福德",16,50.0,
  "亚特兰大老鹰","ATL","奥孔古",19,42.1,
  "波士顿凯尔特人","BOS","奎塔",24,41.7,
  "孟菲斯灰熊","MEM","贾伦·杰克逊",17,41.2,
  "底特律活塞","DET","杰伦·杜伦",23,39.1,
  "洛杉矶快船","LAC","祖巴茨",29,37.9,
  "芝加哥公牛","CHI","武切维奇",23,34.8,
  "金州勇士","GSW","追梦格林",15,33.3,
  "休斯顿火箭","HOU","申京",18,33.3,
  "印第安纳步行者","IND","以赛亚·杰克逊",15,33.3,
  "萨克拉门托国王","SAC","萨博尼斯",15,33.3,
  "新奥尔良鹈鹕","NOP","德里克·奎因",12,25.0,
  "密尔沃基雄鹿","MIL","迈尔斯·特纳",29,24.1
) %>%
  mutate(
    成功率数 = 成功率 / 100,
    排名 = row_number(),
    球队 = team_name_cn[缩写],
    # 新增：英文名用于头像文件路径
    player_en = player_name_en[球员]
  )

# 高成功率：藏蓝 → 低成功率：深紫（冷色调，纯色相渐变）
pal <- colorRampPalette(c("#E64A19", "#4A148C"))  # 藏蓝→深紫
colors <- pal(100)

jumpball <- jumpball %>%
  mutate(
    rate_rank = rank(-成功率数),
    color = colors[ceiling(rate_rank / max(rate_rank) * 100)]
  )

# ================================
# 4. 两列布局
# ================================
n_cols <- 2
n_per_col <- 15
logo_dir <- "D:/nba/球队logo/球队logo方"
player_pic_dir <- "D:/nba/球员头像" # 新增头像文件夹路径

# 修正后的 Flexbox HTML 内容行
row_html <- "
<div style='display:flex; align-items:center; width:100%;'>
  <div style='width:34px; font-weight:bold; text-align:right;'>{rank}.</div>
  
    <div style='width:28px; margin:0 3px; text-align:center;'>{logo}</div> 
  
    <div style='width:30px; margin:0 3px; text-align:center;'>{player_pic}</div> 
  
  <div style='flex-grow:1; font-size:18px; font-weight:bold;'>{player}</div>
  <div style='width:70px; text-align:right; color:{color}; font-size:18px;'>{pct}</div>
  <div style='width:40px; text-align:right; color:#777;'>{n}</div>
</div>
"

jump_fmt <- jumpball %>%
  mutate(
    col = rep(1:2, each=n_per_col, length.out=n()),
    
    # 球队 Logo HTML (保持不变)
    logo_path = glue("{logo_dir}/{缩写}.png"),
    logo = map_chr(logo_path, ~ as.character(web_image(., height=26))),
    
    # ❗ 球员头像 HTML (新增)
    player_pic_path = glue("{player_pic_dir}/{player_en}.png"),
    player_pic = map_chr(player_pic_path, ~ as.character(web_image(., height=28))), # 头像略大
    
    formatted = glue(
      row_html,
      rank = 排名,
      logo = logo, 
      player_pic = player_pic, # 引用头像 HTML
      player = 球员,
      pct = paste0(sprintf("%.1f", 成功率), "%"),
      color = color,
      n = paste0("(", 跳球次数, ")")
    )
  ) %>%
  group_by(col) %>%
  summarise(block = paste0(formatted, collapse = ""), .groups="drop") %>%
  pivot_wider(names_from = col, values_from = block, names_prefix = "col_")

# ================================
# 5. 表格渲染
# ================================
# ❗ 修正后的列标题 HTML：新增头像占位符
header_html <- "
<div style='display:flex; align-items:center; width:100%; font-weight:bold;'>
  <div style='width:34px; text-align:right;'>排名</div>
  
    <div style='width:28px; margin:0 3px;'></div> 
  
    <div style='width:30px; margin:0 3px;'></div>
  
  <div style='flex-grow:1;'>球员</div>
  <div style='width:70px; text-align:right;'>成功率</div>
  <div style='width:40px; text-align:right;'>次数</div>
</div>
"

table <- jump_fmt %>%
  gt() %>%
  fmt_markdown(everything()) %>%
  cols_label(
    col_1 = md(header_html),
    col_2 = md(header_html)
  ) %>%
  tab_options(
    table.background.color = "#faf7f0",
    data_row.padding = px(4),
    table.width = px(1000)
  ) %>%
  tab_header(
    title = md("**NBA本赛季中锋跳球排行榜 文班亚马只输一次！**"),
    subtitle = "统计截止12.7 | 只统计NBA每支球队跳球次数最多的球员，做到30个球员来自不同的球队|数据源：hoopR|制作：俗人罢了"
  )

# ================================
# 6. 导出
# ================================
out <- "C:/Users/徐铭成/Documents/NBA_跳球排行榜_双列版.png"
gtsave_extra(table, out, vwidth=1200, vheight=1200, zoom=6)

image_read(out) %>%
  image_trim() %>%
  image_write(out, density=300)

# 检查文件是否生成
if (file.exists(out)) {
  shell.exec(out)
  cat("\n✅ 表格已生成并修正：已成功添加球员头像，并保持完美对齐。\n")
} else {
  cat("\n❌ 图片生成失败，请检查 webshot2, magick 配置和头像文件路径 ('player_pic_dir')。\n")
}