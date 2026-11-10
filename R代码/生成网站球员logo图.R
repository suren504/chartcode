# ==============================================================================
# 🚀 终极球员专属 LOGO 批量合成流水线 (极速压缩版)
# ==============================================================================
library(magick)
library(dplyr)
library(readr)

# --- 1. 路径与环境配置 ---
input_csv  <- "D:/nba/player_latest_team.csv"
player_dir <- "D:/nba/www/"
logo_dir   <- "D:/nba/球队logo/队标/"
output_dir <- "D:/nba/headshot/"

# 确保输出文件夹存在
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# --- 2. 球队配色字典 (完美继承，无改动) ---
team_colors <- c(
  "ATL"="#E9072B", "BOS"="#008348", "BKN"="black", "CHA"="#00788C", 
  "CHI"="#CD1F43", "CLE"="#860038", "DAL"="#0066B5", "DEN"="#FFC600", 
  "DET"="#E9072B", "GSW"="#00408D", "HOU"="#CE1141", "IND"="#FED51F",
  "LAC"="#12173F", "LAL"="#360072", "MEM"="#5D76A9", "MIA"="#961A31", 
  "MIL"="#004C26", "MIN"="#A0A2A3", "NOP"="#C69857", "NYK"="#FF8512", 
  "OKC"="#007AC0", "ORL"="#1F55A6", "PHI"="white", "PHX"="#FF6012", 
  "POR"="#FF373C", "SAC"="#592D81", "SAS"="#E1E7EB", "TOR"="white", 
  "UTA"="#668BB2", "WAS"="#C1CFD5"
)

# --- 3. 核心合成函数 (保留完美坐标，增加终极压缩) ---
make_circular_avatar <- function(id, team) {
  # 🚨 规范命名：直接使用 player_id.png，方便 Shiny 端直接无脑拼接 URL
  out_path <- paste0(output_dir, id, ".png")
  
  # 如果图片已经存在，直接跳过 (增量更新，极其节省下次跑脚本的时间)
  if (file.exists(out_path)) return(out_path)
  
  bg_color <- team_colors[team]
  if (is.na(bg_color)) bg_color <- "#000000"
  
  # ------ 1. 加载队标 ------
  logo_path <- paste0(logo_dir, team, ".png")
  if(file.exists(logo_path)) {
    logo_img <- image_read(logo_path) %>% 
      image_repage() %>%       
      image_trim() %>%         
      image_resize("520x520")  
  } else {
    logo_img <- image_blank(500, 500, "none")
  }
  
  # ------ 2. 加载头像 ------
  portrait_path <- paste0(player_dir, id, ".png")
  if(file.exists(portrait_path)) {
    p_img <- image_read(portrait_path) %>% image_resize("x450")
  } else {
    # 遇到没有头像的双向合同球员，直接跳过不生成，防止生成丑陋的空图
    cat("⚠️ 跳过缺失素材的球员 ID:", id, "\n")
    return(NULL) 
  }
  
  # ------ 3. 三层合体 ------
  combined <- image_blank(500, 500, "none") %>%
    image_composite(logo_img, gravity = "center", operator = "Over") %>%
    image_composite(p_img, gravity = "south", operator = "Over")
  
  # ------ 4. 圆形裁剪蒙版 ------
  mask <- image_blank(500, 500, "none")
  mask <- image_draw(mask)
  symbols(250, 250, circles = 248, bg = bg_color, fg = bg_color, inches = FALSE, add = TRUE)
  dev.off()
  cut_content <- image_composite(combined, mask, operator = "DstIn")
  
  # ------ 5. 生成底盘 ------
  final_canvas <- image_blank(500, 500, "none")
  final_canvas <- image_draw(final_canvas)
  symbols(250, 250, circles = 248, bg = bg_color, fg = bg_color, inches = FALSE, add = TRUE)
  dev.off()
  final_img <- image_composite(final_canvas, cut_content, gravity = "center")
  
  # ==============================================================================
  # 🚨 修复版：保留原生 sRGB 色彩，只做物理缩放
  # ==============================================================================
  final_compressed <- final_img %>%
    # 只做尺寸缩小，保留所有色彩细节
    image_scale("120x120") %>%
    # 强制声明色彩空间为 sRGB，防止写入 PNG 时发生偏色
    image_convert(colorspace = "sRGB") 
  
  # 保存输出 (利用 PNG 默认的无损压缩算法，体积依然会在 20KB 左右)
  image_write(final_compressed, path = out_path, format = "png")
  
  return(out_path)
}

# --- 4. 批量执行引擎 ---
print("Status: 启动自动化头像合成流水线...")

df_players <- read_csv(input_csv, show_col_types = FALSE)
total_players <- nrow(df_players)

for (i in seq_len(total_players)) {
  p_id <- as.character(df_players$player_id[i])
  p_team <- df_players$team_abbreviation[i]
  
  if (i %% 50 == 0) cat(sprintf("进度: %d / %d...\n", i, total_players))
  
  # 捕捉可能的异常，防止某一张图的坏点导致整个脚本崩溃
  tryCatch({
    make_circular_avatar(id = p_id, team = p_team)
  }, error = function(e) {
    cat("❌ 生成失败 ID:", p_id, "错误原因:", e$message, "\n")
  })
}

print("✅ 所有球员专属 LOGO 压缩合成完毕！快去看看文件夹里的体积有多小！")