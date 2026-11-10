# ==============================================================================
# NBA 2025-26 大众姓名统计 - 高清完美版
# ==============================================================================

# 1. 加载必要的包
library(tidyverse)
library(gt)
library(gtExtras)
library(stringr)
library(webshot2)
library(htmltools)

# ==============================================================================
# 第一部分：原始数据录入
# ==============================================================================

# --- 1.1 姓氏数据 (基于你的最新准确名单) ---
player_full_sur <- tibble(
  FullName = c(
    "Cody Williams", "Vince Williams Jr.", "Brandon Williams", "Robert Williams III", 
    "Patrick Williams", "Malik Williams", "Jalen Williams", "Kenrich Williams", 
    "Ziaire Williams", "Mark Williams", "Amari Williams", "Grant Williams", "Jaylin Williams",
    "Derrick Jones Jr.", "Tre Jones", "Kam Jones", "Spencer Jones", 
    "Curtis Jones", "Tyus Jones", "Herbert Jones", "Isaac Jones", "Colby Jones",
    "Javonte Green", "Jeff Green", "Draymond Green", "AJ Green", "Josh Green", "Jalen Green",
    "Keldon Johnson", "Keshad Johnson", "Tre Johnson", "AJ Johnson", "Jalen Johnson", "Cameron Johnson",
    "Nick Smith Jr.", "Jalen Smith", "Jabari Smith Jr.", "Dru Smith", "Tyler Smith", "Tolu Smith",
    "Jaren Jackson Jr.", "GG Jackson", "Quenton Jackson", "Andre Jackson Jr.", "Isaiah Jackson",
    "Caleb Martin", "Tyrese Martin", "Alijah Martin", "Cody Martin",
    "Jordan Miller", "Leonard Miller", "Brandon Miller", "Emanuel Miller",
    "Keegan Murray", "Kris Murray", "Jamal Murray", "Dejounte Murray",
    "Ausar Thompson", "Klay Thompson", "Ethan Thompson", "Amen Thompson"
  )
)

df_surname <- player_full_sur %>%
  mutate(CleanName = str_remove(FullName, "(?i)\\s+(Jr\\.?|Sr\\.?|I{2,3}|IV|V)$")) %>%
  mutate(姓氏 = str_extract(CleanName, "\\w+$")) %>%
  count(姓氏, name = "人数") %>%
  mutate(
    中文姓氏 = case_when(
      姓氏 == "Williams" ~ "威廉姆斯", 姓氏 == "Jones" ~ "琼斯", 姓氏 == "Green" ~ "格林",
      姓氏 == "Johnson" ~ "约翰逊", 姓氏 == "Smith" ~ "史密斯", 姓氏 == "Jackson" ~ "杰克逊",
      姓氏 == "Martin" ~ "马丁", 姓氏 == "Miller" ~ "米勒", 姓氏 == "Murray" ~ "穆雷",
      姓氏 == "Thompson" ~ "汤普森", TRUE ~ 姓氏
    )
  ) %>%
  rowwise() %>%
  mutate(
    球员列表 = paste(player_full_sur$FullName[str_detect(
      str_remove(player_full_sur$FullName, "(?i)\\s+(Jr\\.?|Sr\\.?|I{2,3}|IV|V)$"), 
      paste0("\\b", 姓氏, "$")
    )], collapse = ",")
  ) %>%
  ungroup() %>%
  select(Label = 中文姓氏, 球员列表, 人数) %>%
  arrange(desc(人数))

# --- 1.2 名字数据 ---
player_full_first <- tibble(
  FullName = c(
    "Jalen Duren", "Jalen Smith", "Jalen Brunson", "Jalen Williams", "Jalen Wilson", "Jalen Pickett", "Jalen Johnson", "Jalen Suggs", "Jalen Green",
    "Isaiah Stewart", "Isaiah Collier", "Isaiah Joe", "Isaiah Livers", "Isaiah Hartenstein", "Isaiah Jackson", "Isaiah Crawford", "Isaiah Stevens",
    "Jordan Miller", "Jordan Walsh", "Jordan Clarkson", "Jordan Poole", "Jordan Hawkins", "Jordan McLaughlin", "Jordan Goodwin",
    "Chris Boucher", "Chris Youngblood", "Chris Manon", "Chris Livingston", "Chris Paul",
    "Josh Minott", "Josh Giddey", "Josh Okogie", "Josh Green", "Josh Hart",
    "Kevin Huerter", "Kevin Durant", "Kevin McCullar Jr.", "Kevin Porter Jr.", "Kevin Love",
    "Aaron Nesmith", "Aaron Holiday", "Aaron Wiggins", "Aaron Gordon",
    "Anthony Edwards", "Anthony Black", "Anthony Gill", "Anthony Davis",
    "Brandon Williams", "Brandon Ingram", "Brandon Miller", "Brandon Clarke",
    "Cam Christie", "Cam Spencer", "Cam Thomas", "Cam Whitmore"
  )
)

df_firstname <- player_full_first %>%
  mutate(名字 = str_extract(FullName, "^\\w+"), 名字 = str_trim(名字)) %>%
  count(名字, name = "人数") %>%
  mutate(
    中文名字 = case_when(
      名字 == "Jalen" ~ "杰伦", 名字 == "Isaiah" ~ "以赛亚", 名字 == "Jordan" ~ "乔丹",
      名字 == "Chris" ~ "克里斯", 名字 == "Josh" ~ "乔希", 名字 == "Kevin" ~ "凯文",
      名字 == "Aaron" ~ "艾伦", 名字 == "Anthony" ~ "安东尼", 名字 == "Brandon" ~ "布兰登",
      名字 == "Cam" ~ "卡姆", TRUE ~ 名字
    )
  ) %>%
  rowwise() %>%
  mutate(
    球员列表 = paste(player_full_first$FullName[str_detect(player_full_first$FullName, paste0("^", 名字, "\\s"))], collapse = ",")
  ) %>%
  ungroup() %>%
  select(Label = 中文名字, 球员列表, 人数) %>%
  arrange(desc(人数))

# ==============================================================================
# 第二部分：核心渲染函数 (修复比例 + 居中)
# ==============================================================================

generate_headshot_html <- function(names_string) {
  name_list <- str_split(names_string, ",")[[1]]
  img_tags <- sapply(name_list, function(nm) {
    nm <- str_trim(nm)
    clean_name <- nm %>%
      str_replace_all("č", "c") %>% str_replace_all("·", "") %>%
      str_replace_all("\\.", "") %>% str_replace_all("'", "") %>%
      str_replace_all(" ", "_")
    path <- paste0("D:/nba/球员头像/", clean_name, ".png")
    if (file.exists(path)) {
      return(paste0(
        "<img src='", path, "' style='",
        "height: 38px; width: 38px; object-fit: cover; object-position: top; ",
        "flex-shrink: 0; border-radius: 50%; margin-right: -12px; ",
        "border: 2px solid white; box-shadow: 2px 0 5px rgba(0,0,0,0.1);'>"))
    } else { return("") }
  })
  return(paste0("<div style='display: flex; justify-content: center; align-items: center; width: 100%;'>", paste(img_tags, collapse = ""), "</div>"))
}

# 应用函数
df_sur_final <- df_surname %>% rowwise() %>% mutate(头像组 = generate_headshot_html(球员列表)) %>% select(-球员列表) %>% ungroup()
df_first_final <- df_firstname %>% rowwise() %>% mutate(头像组 = generate_headshot_html(球员列表)) %>% select(-球员列表) %>% ungroup()

# ==============================================================================
# 第三部分：表格主题与输出逻辑
# ==============================================================================

apply_centered_theme <- function(gt_obj, title_text, subtitle_text, color_palette, domain_range) {
  gt_obj %>%
    tab_header(title = md(paste0("**", title_text, "**")), subtitle = subtitle_text) %>%
    fmt_markdown(columns = c(头像组)) %>%
    cols_label(Label = "类别", 头像组 = "球员名单", 人数 = "人数") %>%
    gt_color_rows(columns = 人数, palette = color_palette, domain = domain_range) %>%
    cols_width(Label ~ px(80), 头像组 ~ px(400), 人数 ~ px(60)) %>%
    cols_align(align = "center", columns = everything()) %>%
    gt_theme_espn() %>%
    opt_align_table_header(align = "center") %>%
    tab_options(data_row.padding = px(8), heading.title.font.size = px(22))
}

# 制作左右两表
gt_left <- df_first_final %>% gt() %>% 
  apply_centered_theme("NBA现役最大众化的名字", "Jalen | Isaiah | Jordan | Chris | Josh | Kevin | Aaron | Anthony | Brandon | Cam", "ggsci::blue_material", c(min(df_first_final$人数), max(df_first_final$人数)))

gt_right <- df_sur_final %>% gt() %>% 
  apply_centered_theme("NBA现役最大众化的姓氏", "Williams | Jones | Green | Johnson | Smith | Jackson | Martin | Miller | Murray | Thompson", "ggsci::red_material", c(min(df_sur_final$人数), max(df_sur_final$人数)))

# ==============================================================================
# 第四部分：高清智能导出 (核心解决清晰度与白边)
# ==============================================================================

# 1. 组合 HTML
combined_html <- gt_two_column_layout(tables = list(gt_left, gt_right), output = "html")

# 2. 定义路径
temp_h <- "D:/nba/temp_layout.html"
final_p <- "D:/nba/NBA_2026_大众姓名_高清完美版.png"

# 3. 渲染高清图 (zoom=3 提升清晰度，selector="body" 自动切掉白边)
save_html(combined_html, file = temp_h)
webshot(url = temp_h, file = final_p, vwidth = 1100, selector = "body", zoom = 3)

# 4. 提示
if(file.exists(temp_h)) file.remove(temp_h)
cat("\n--- 任务完成！---\n图片已存至: ", final_p, "\n")