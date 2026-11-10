library(tidyverse)
library(ggchicklet)
library(prismatic)
library(ggimage)
library(teamcolors)
library(scales)

# ==============================================================================
# 1. 修复数据准备 - 正确的 tribble 格式
# ==============================================================================
ranking_df <- tribble(
  ~team_abbr, ~avg_duration, ~games_count,
  "DET", 148.0, 33,
  "ORL", 144.0, 33,
  "NY",  144.0, 36,
  "POR", 143.0, 36,
  "HOU", 143.0, 31,
  "IND", 143.0, 35,
  "DAL", 143.0, 36,
  "UTAH",143.0, 34,
  "LAL", 142.0, 31,
  "MIN", 142.0, 33,
  "CLE", 142.0, 35,
  "SA",  142.0, 36,
  "GS",  142.0, 35,
  "PHI", 142.0, 32,
  "PHX", 141.0, 33,
  "DEN", 140.0, 33,
  "NO",  140.0, 36,
  "LAC", 140.0, 33,
  "OKC", 140.0, 35,
  "TOR", 140.0, 35,
  "CHA", 139.0, 33,
  "MIL", 139.0, 35,
  "BOS", 139.0, 33,
  "SAC", 138.0, 35,
  "CHI", 138.0, 35,
  "MEM", 138.0, 33,
  "MIA", 138.0, 34,
  "ATL", 138.0, 36,
  "BKN", 137.0, 31,
  "WSH", 136.0, 32
)

# ==============================================================================
# 2. 数据处理 - 添加文件存在性检查
# ==============================================================================
plot_data <- ranking_df %>%
  # 修正缩写
  mutate(team_abbr_clean = case_match(team_abbr,
                                      "GS"   ~ "GSW",
                                      "NY"   ~ "NYK",
                                      "NO"   ~ "NOP",
                                      "SA"   ~ "SAS",
                                      "UTAH" ~ "UTA",
                                      "WSH"  ~ "WAS",
                                      .default = team_abbr
  )) %>%
  # 匹配全名
  mutate(full_name = case_match(team_abbr_clean,
                                "DET" ~ "Detroit Pistons",
                                "ORL" ~ "Orlando Magic",
                                "NYK" ~ "New York Knicks",
                                "POR" ~ "Portland Trail Blazers",
                                "HOU" ~ "Houston Rockets",
                                "IND" ~ "Indiana Pacers",
                                "DAL" ~ "Dallas Mavericks",
                                "UTA" ~ "Utah Jazz",
                                "LAL" ~ "Los Angeles Lakers",
                                "MIN" ~ "Minnesota Timberwolves",
                                "CLE" ~ "Cleveland Cavaliers",
                                "SAS" ~ "San Antonio Spurs",
                                "GSW" ~ "Golden State Warriors",
                                "PHI" ~ "Philadelphia 76ers",
                                "PHX" ~ "Phoenix Suns",
                                "DEN" ~ "Denver Nuggets",
                                "NOP" ~ "New Orleans Pelicans",
                                "LAC" ~ "Los Angeles Clippers",
                                "OKC" ~ "Oklahoma City Thunder",
                                "TOR" ~ "Toronto Raptors",
                                "CHA" ~ "Charlotte Hornets",
                                "MIL" ~ "Milwaukee Bucks",
                                "BOS" ~ "Boston Celtics",
                                "SAC" ~ "Sacramento Kings",
                                "CHI" ~ "Chicago Bulls",
                                "MEM" ~ "Memphis Grizzlies",
                                "MIA" ~ "Miami Heat",
                                "ATL" ~ "Atlanta Hawks",
                                "BKN" ~ "Brooklyn Nets",
                                "WAS" ~ "Washington Wizards"
  )) %>%
  # 构造logo路径
  mutate(logo_path = paste0("D:/nba/球队logo/球队logo圆/", team_abbr_clean, ".png")) %>%
  # 检查文件是否存在，如果不存在就用占位符
  mutate(logo_exists = file.exists(logo_path)) %>%
  # 添加排序因子
  arrange(desc(avg_duration)) %>%
  mutate(team_rank = row_number(),
         team_factor = fct_reorder(team_abbr_clean, -avg_duration))

# 检查哪些logo文件不存在
missing_logos <- plot_data %>% 
  filter(!logo_exists) %>% 
  select(team_abbr_clean, logo_path)

if(nrow(missing_logos) > 0) {
  cat("⚠️ 以下logo文件不存在:\n")
  print(missing_logos)
  
  # 检查文件夹是否存在
  logo_dir <- "D:/nba/球队logo/球队logo圆/"
  if(!dir.exists(logo_dir)) {
    cat("❌ 目录不存在:", logo_dir, "\n")
    cat("📁 请检查路径是否正确\n")
  } else {
    cat("📁 目录存在，检查其中的文件:\n")
    existing_files <- list.files(logo_dir, pattern = "*.png")
    cat("找到", length(existing_files), "个PNG文件\n")
    if(length(existing_files) > 0) {
      cat("前几个文件:", paste(head(existing_files, 5), collapse = ", "), "\n")
    }
  }
}

# 合并队伍颜色（如果需要的话）
if("teamcolors" %in% (.packages())) {
  plot_data <- plot_data %>%
    left_join(teamcolors %>% filter(league == "nba"), by = c("full_name" = "name"))
}

# ==============================================================================
# 3. 绘图 - 修复版本（已按要求修改）
# ==============================================================================
p <- ggplot(plot_data, aes(x = team_factor, y = avg_duration)) +
  
  # 柱子 - 使用条件颜色
  {if("primary" %in% names(plot_data)) {
    geom_chicklet(
      aes(fill = primary, color = after_scale(clr_darken(fill, 0.3))),
      alpha = 0.9, width = 0.65, size = 0.8,
      radius = grid::unit(4, "pt")
    )
  } else {
    geom_chicklet(
      fill = "steelblue", color = "darkblue",
      alpha = 0.9, width = 0.65, size = 0.8,
      radius = grid::unit(4, "pt")
    )
  }} +
  
  # 只显示存在的logo
  {if(any(plot_data$logo_exists)) {
    geom_image(
      data = filter(plot_data, logo_exists),
      aes(image = logo_path, y = -8), 
      size = 0.045,
      asp = 1.8
    )
  }} +
  
  # 对于不存在logo的球队，显示缩写
  {if(any(!plot_data$logo_exists)) {
    geom_text(
      data = filter(plot_data, !logo_exists),
      aes(label = team_abbr_clean, y = -8),
      size = 3, fontface = "bold", color = "red"
    )
  }} +
  
  # 辅助线
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # 数字标签
  geom_text(
    aes(label = round(avg_duration, 0)), 
    vjust = -0.5, size = 4, fontface = "bold", 
    family = "Consolas", color = "gray20"
  ) +
  
  # 坐标轴
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    limits = c(-20, 160),
    breaks = seq(0, 150, 25)
  ) +
  
  # 如果有颜色数据就使用，否则忽略
  {if("primary" %in% names(plot_data)) scale_fill_identity()} +
  
  coord_cartesian(clip = "off") +
  
  # 主题（核心修改：标题和副标题居中）
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    plot.background = element_rect(fill = 'floralwhite', color = NA), 
    panel.background = element_rect(fill = 'floralwhite', color = NA),
    axis.text.x = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold", color = "gray20", margin = margin(t = 25)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linetype = "dashed"),
    # 关键修改1：标题居中
    plot.title = element_text(face = 'bold', size = 22, hjust = 0.5, margin = margin(b = 5)),
    # 关键修改2：副标题居中
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30", margin = margin(b = 25)),
    # 关键修改3：删除caption相关样式（原本的caption样式已移除）
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  labs(
    x = "比赛用时长 <<--   ---   -->> 比赛用时短", 
    y = "平均时长 (分钟)", 
    title = "2025-26 赛季 NBA 球队平均比赛耗时排行榜", 
    # 关键修改4：替换副标题文本
    subtitle = "数据源：hoopR|制作人：俗人罢了|统计截止：2026.1.4"
    # 关键修改5：删除caption参数
  )

# ==============================================================================
# 4. 保存
# ==============================================================================
ggsave("NBA_Duration_Fixed.png", p, width = 15, height = 7.5, dpi = 300, bg = 'floralwhite')
browseURL("NBA_Duration_Fixed.png")