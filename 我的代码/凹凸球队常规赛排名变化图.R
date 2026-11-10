# 加载必备的包
library(tidyverse)
library(arrow)
library(ggbump)
library(ggimage)
library(showtext)

# 1. 读取本地 2026 赛季 Boxscore Parquet 数据
df_team_raw <- arrow::read_parquet("D:/nba/RStudio/HoopR/nba/team_box/parquet/team_box_2026.parquet")

# 定义 30 支常规赛球队名称
valid_teams <- c(
  "Celtics", "Bucks", "76ers", "Cavaliers", "Knicks", 
  "Heat", "Hawks", "Raptors", "Bulls", "Pacers", 
  "Nets", "Magic", "Hornets", "Wizards", "Pistons",
  "Nuggets", "Grizzlies", "Kings", "Suns", "Clippers", 
  "Warriors", "Lakers", "Timberwolves", "Pelicans", "Thunder", 
  "Mavericks", "Jazz", "Trail Blazers", "Spurs", "Rockets"
)

# 2. 定义时间节点与对应的 X 轴标签
snapshots <- as.Date(c(
  "2025-10-31", "2025-11-30", "2025-12-31", 
  "2026-01-31", "2026-02-28", "2026-03-31"
))
x_labels <- c("11.1", "12.1", "元旦", "2.1", "3.1", "愚人节")

# 3. 统计函数
get_snapshot_standings <- function(target_date) {
  df_team_raw %>%
    mutate(game_date = as.Date(game_date)) %>%
    filter(game_date <= target_date, team_name %in% valid_teams) %>%
    filter(!is.na(team_color)) %>%
    group_by(team_abbreviation, team_color, team_logo) %>%
    summarise(
      W = sum(team_winner == TRUE, na.rm = TRUE),
      L = sum(team_winner == FALSE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Win_Pct = if_else((W + L) == 0, 0, W / (W + L))) %>%
    arrange(desc(Win_Pct), desc(W), L) %>%
    mutate(
      Rank = row_number(),
      snapshot_date = target_date
    )
}

# 4. 合并数据
df_bump <- map_dfr(snapshots, get_snapshot_standings) %>%
  mutate(
    time_idx = match(snapshot_date, snapshots),
    color_hex = paste0("#", team_color)
  )

# 提取关键节点：起点(1)、元旦(3)、终点(6)
df_nodes <- df_bump %>% filter(time_idx %in% c(1, 3, 6))

# 5. 绘图
p <- df_bump %>%
  ggplot(aes(x = time_idx, y = Rank, group = team_abbreviation)) +
  
  # 专属版权水印（居中在 30 支球队的中心第 15 行）
  annotate("text", x = 3.5, y = 15, label="俗人罢了", alpha=0.04, size=60, angle=30, family="Microsoft YaHei") +
  
  # 【核心新加项】：在图表最左侧（11.1 节点的左侧留白区）批量加入 #1 到 #30 的名次标签
  annotate(
    "text",
    x = 0.65,                  # 精准定位于左侧边缘与第一个节点的黄金分割线上
    y = 1:30,                  # 垂直对齐全量 30 个排名网格
    label = paste0("#", 1:30), # 自动生成 #1, #2 ... #30
    family = "Consolas",       # 强制指定数字英文特性的 Consolas 字体
    fontface = "bold",         # 粗体让排名更醒目
    size = 6,                  # 适配 30 行大长图的字号
    color = "#333333"          # 统一质感深灰
  ) +
  
  # 核心流线：无缝连接
  geom_bump(aes(color = color_hex), linewidth = 2.5, smooth = 8, lineend = "round", alpha = 0.85) +
  
  # 圆点底座：消除缝隙，平衡 Logo 大小不一的问题
  geom_point(data = df_nodes, aes(x = time_idx, color = color_hex), size = 15) +
  
  # 网络 Logo 叠加：使用原生 team_logo 链接
  geom_image(data = df_nodes, aes(x = time_idx, image = team_logo), size = 0.032) +
  
  scale_color_identity() +
  scale_y_reverse(breaks = 1:30) +
  
  # X 轴拉长：左侧界扩展到 0.1，为最左侧名次框 #1~#30 腾出完美排版空间
  scale_x_continuous(
    breaks = 1:length(x_labels),
    labels = x_labels,
    limits = c(0.1, 6.3)
  ) +
  
  labs(
    title = "NBA2025-26常规赛球队战绩排名变化图",
    subtitle = "北京时间 | 截止2026.4.1 | 制作：俗人罢了 | 数据源：HoopR",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linetype = "dashed"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 18, face = "bold", family = "Consolas", color = "#333333"),
    
    # 【核心调整】：通过极度压缩 margin 边距，拉近标题、副标题与图表主体的垂直空间
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 2)),
    plot.subtitle = element_text(size = 16, color = "#A1A1A1", hjust = 0.5, margin = margin(b = 4)),
    plot.margin = margin(10, 0, 10, 0)
  )

# 6. 自定义保存指令：30 支全量常规赛图表，强制维持 16:20 纵向大长图输出
ggsave("NBA2025-26常规赛球队战绩排名变化图.png", p, width = 16, height = 20, dpi = 300)