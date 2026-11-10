# ==============================================================================
# 1. 库加载
# ==============================================================================
library(tidyverse)
library(vipor)      
library(ggimage)    
library(ggtext)
library(stringi)
library(magick)     

# ==============================================================================
# 2. 球队配色字典 & 教练中文映射表
# ==============================================================================
team_colors <- c(
  "ATL"="#E03A3E", "BOS"="#008348", "BKN"="#666666", "CHA"="#00788C", 
  "CHI"="#CD1F43", "CLE"="#860038", "DAL"="#0066B5", "DEN"="#FFD76A", 
  "DET"="#C8102E", "GSW"="#FFD36F", "HOU"="white", "IND"="#FEBC30",
  "LAC"="#12173F", "LAL"="#552583", "MEM"="#5D76A9", "MIA"="#961A31", 
  "MIL"="#EEE1C6", "MIN"="#266092", "NOP"="#B89A5B", "NYK"="#F9B57D", 
  "OKC"="#007AC0", "ORL"="#66B2E6", "PHI"="#1D428A", "PHX"="#FF6012", 
  "POR"="#D00A2C", "SAC"="#592D81", "SAS"="#E1E7EB", "TOR"="white", 
  "UTA"="#668BB2", "WAS"="#F07C8C"
)

coach_map <- c("DET" = "活塞", "BOS" = "凯尔特人", "SAS" = "马刺", "CHA" = "黄蜂", "PHX" = "太阳")

# ==============================================================================
# 3. 数据准备与合并 (防 Tab 乱码 + 极致降维版)
# ==============================================================================

# 核心黑科技函数：纯字母提取
clean_name_for_join <- function(name) {
  name %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>% 
    tolower() %>%                                       
    str_remove_all("[^a-z]") 
}

# 1. 建立极其安全的 ID 字典 (修复 Tab 分隔符识别问题)
# 先尝试按逗号读
df_id_raw <- read_csv("D:/nba/id查询.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
# 如果读出来发现只有 1 列，说明其实是用 Tab 分隔的
if (ncol(df_id_raw) == 1) {
  df_id_raw <- read_tsv("D:/nba/id查询.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
}

df_id_dict <- df_id_raw %>%
  select(raw_name = 1, player_id = 2) %>%
  filter(!is.na(raw_name)) %>%
  mutate(join_name = clean_name_for_join(raw_name)) %>%
  distinct(join_name, .keep_all = TRUE) 

# 2. 清洗主表并匹配
df_plot <- df_raw %>%
  mutate(
    team_abbr = str_extract(player_raw, "(?<=\\()[A-Z]{3}(?=\\))"),
    clean_name = str_trim(str_remove(player_raw, "\\(.*\\)")),
    join_name = clean_name_for_join(clean_name) 
  ) %>%
  left_join(df_id_dict, by = "join_name") %>%
  mutate(
    player_id = if_else(is.na(player_id), "0", player_id),
    award_fct = fct_rev(factor(award, levels = c("MVP", "DPOY", "ROY", "6MOY", "MIP", "COY"))),
    award_num = as.numeric(award_fct)
  ) %>%
  group_by(award_fct) %>%
  mutate(
    y_offset = vipor::offsetX(win_prob_pct, width = 0.35, method = "quasirandom"),
    plot_y = award_num + y_offset
  ) %>%
  ungroup()

# ==============================================================================
# 4. Magick 图像合成引擎 (修复教练误杀 Bug)
# ==============================================================================
temp_img_dir <- "D:/nba/temp_avatars/"
dir.create(temp_img_dir, showWarnings = FALSE)
unlink(list.files(temp_img_dir, full.names = TRUE)) 

cat("\n--- 开始合成图像，请留意下方是否有报错提示 ---\n")

generate_composite_avatar <- function(award, player_id, team_abbr, clean_name) {
  out_path <- paste0(temp_img_dir, award, "_", player_id, "_", team_abbr, ".png")
  
  bg_color <- team_colors[team_abbr]
  if (is.na(bg_color)) bg_color <- "#000000" 
  
  # ------ 第 1 层：所有人都要加载底层队标 ------
  logo_path <- paste0("D:/nba/球队logo/队标/", team_abbr, ".png")
  if(file.exists(logo_path)){
    logo_img <- image_read(logo_path) %>% image_resize("450x450")
  } else {
    logo_img <- image_blank(500, 500, "none")
  }
  
  # ------ 第 2 层：分别处理教练和球员 (核心修复点) ------
  if (award == "COY") {
    # 【教练分支】：只看图片在不在，不查 ID！
    coach_zh <- coach_map[team_abbr]
    portrait_path <- paste0("D:/nba/临时照片/教练/", coach_zh, ".png")
    
    if(file.exists(portrait_path)) {
      p_img <- image_read(portrait_path) %>% image_resize("x450")
    } else {
      cat("❌ [缺教练] 找不到:", coach_zh, "路径:", portrait_path, "\n")
      p_img <- image_blank(450, 450, "none")
    }
    
  } else {
    # 【球员分支】：必须满足图片在 且 ID 匹配成功 (!= 0)
    portrait_path <- paste0("D:/nba/www/", player_id, ".png")
    
    if(file.exists(portrait_path) && player_id != "0") {
      p_img <- image_read(portrait_path) %>% image_resize("x450")
    } else {
      cat("❌ [缺头像] 找不到:", clean_name, "ID:", player_id, "路径:", portrait_path, "\n")
      p_img <- image_blank(450, 450, "none")
    }
  }
  
  # ------ 第 3 层：合并图层 ------
  img_layer <- image_blank(500, 500, "none") %>%
    image_composite(logo_img, gravity = "center", operator = "Over") %>%
    image_composite(p_img, gravity = "south", operator = "Over") 
  
  # ------ 第 4 层：圆形裁剪 & 涂边 ------
  mask <- image_blank(500, 500, "none")
  mask <- image_draw(mask)
  symbols(250, 250, circles = 238, bg = "black", fg = "black", inches = FALSE, add = TRUE)
  dev.off()
  
  cut_layer <- image_composite(img_layer, mask, operator = "DstIn")
  
  final_canvas <- image_blank(500, 500, "none")
  final_canvas <- image_draw(final_canvas)
  symbols(250, 250, circles = 248, bg = bg_color, fg = bg_color, inches = FALSE, add = TRUE)
  dev.off()
  
  final_img <- image_composite(final_canvas, cut_layer, gravity = "center")
  image_write(final_img, out_path)
  
  return(out_path)
}

# 批量执行生成
df_plot$final_img_path <- pmap_chr(
  list(df_plot$award, df_plot$player_id, df_plot$team_abbr, df_plot$clean_name), 
  generate_composite_avatar
)
# ==============================================================================
# 5. 极简绘图与导出
# ==============================================================================
p <- df_plot %>% 
  ggplot(aes(x = win_prob_pct, y = plot_y)) + 
  
  annotate("text", x = 40, y = 3.5, label = "俗人罢了", 
           alpha = 0.04, size = 50, angle = 30, family = "Microsoft YaHei") +
  
  geom_image(aes(image = final_img_path), size = 0.055, asp = 16/9) +
  
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 20), labels = function(x) paste0(x, "%")) + 
  scale_y_continuous(breaks = 1:6, labels = levels(df_plot$award_fct), limits = c(0.2, 6.5)) + 
  
  annotate("text", x = 6, y = 0.4, label = "陪跑者", family = "Microsoft YaHei", fontface = "bold", color = "#D6484F", size = 6) + 
  annotate("segment", x = 20, y = 0.4, xend = 12, yend = 0.4, arrow = arrow(length = unit(0.2, "cm")), color = "#D6484F") + 
  annotate("text", x = 74, y = 0.4, label = "有力竞争者", family = "Microsoft YaHei", fontface = "bold", color = "#3FA34D", size = 6) + 
  annotate("segment", x = 60, y = 0.4, xend = 68, yend = 0.4, arrow = arrow(length = unit(0.2, "cm")), color = "#3FA34D") + 
  
  labs(title = "NBA2025-26赛季常规赛奖项竞选获奖图",
       subtitle = paste0("数据源：rotowire| 统计截止：2026.3.26|制作：俗人罢了"),
       x = "获奖概率 (%)", 
       y = "") + 
  
  theme_minimal(base_family = "Microsoft YaHei") + 
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot', 
    plot.title = element_text(face = 'bold', size = 24, family = "Microsoft YaHei"),
    plot.subtitle = element_text(size = 14, color = "gray40", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 16, family = "Microsoft YaHei"),
    axis.text.x = element_text(size = 12, family = "Consolas"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 5), family = "Microsoft YaHei"),
    plot.margin = margin(20, 10, 20, 20)
  )

ggsave("2026_NBA_Awards_Prob_16x9_Pro.png", p, width = 16, height = 9, dpi = 300)