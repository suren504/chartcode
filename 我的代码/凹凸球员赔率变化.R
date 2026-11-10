# 加载必备的包
library(tidyverse)
library(arrow)
library(ggbump)
library(ggimage)
library(showtext)

# 1. 构建这 13 位 MVP 历史前 8 巨头的专属颜色与本地圆形头像映射字典
df_player_dict <- tibble(
  name = c(
    "Nikola Jokic", "Stephen Curry", "Kevin Durant", "Anthony Edwards", 
    "Luka Doncic", "Jaylen Brown", "Jalen Brunson", "Tyrese Maxey", 
    "Victor Wembanyama", "Cade Cunningham", "Shai Gilgeous-Alexander", 
    "Donovan Mitchell", "Giannis Antetokounmpo"
  ),
  img_path = c(
    "D:/nba/headshot/203999.png", "D:/nba/headshot/201939.png", "D:/nba/headshot/201142.png", "D:/nba/headshot/1630162.png",
    "D:/nba/headshot/1629029.png", "D:/nba/headshot/1627759.png", "D:/nba/headshot/1628973.png", "D:/nba/headshot/1630178.png",
    "D:/nba/headshot/1641705.png", "D:/nba/headshot/1630595.png", "D:/nba/headshot/1628983.png", "D:/nba/headshot/1628378.png",
    "D:/nba/headshot/203507.png"
  ),
  color_hex = c(
    "#FFC600", "#00408D", "#CE1141", "#266092", 
    "#360072", "#008442", "#FF8512", "#00408D", 
    "#E1E7EB", "#E9072B", "#007AC0", "#860038", 
    "#004C26"
  )
)

# 2. 定义时间节点：包含国庆和劳动节，总计 8 个时间锚点
snapshots <- as.Date(c(
  "2025-09-30", "2025-10-31", "2025-11-30", "2025-12-31", 
  "2026-01-31", "2026-02-28", "2026-03-31", "2026-04-30"
))
x_labels <- c("国庆", "11.1", "12.1", "元旦", "2.1", "3.1", "愚人节", "劳动节")

# 3. 统计清洗函数：循环读取各个月末的 MVP 赔率 CSV 文件
get_mvp_snapshot <- function(target_date) {
  date_str = format(target_date, "%Y_%m_%d")
  file_path = paste0("D:/nba/RStudio/NBA-MVP-Odds/data/mvp_odds_", date_str, ".csv")
  
  if (file.exists(file_path)) {
    read_csv(file_path, show_col_types = FALSE) %>%
      select(name, contains("winPct")) %>%
      rename(win_pct_val = 2) %>%
      # 严格保留全部 13 位配跑人员，用于底层连线
      filter(name %in% df_player_dict$name) %>%
      arrange(desc(win_pct_val)) %>%
      mutate(
        Rank = row_number(),
        snapshot_date = target_date
      )
  } else {
    NULL
  }
}

# 4. 循环合并数据并建立时间轴索引
df_bump <- map_dfr(snapshots, get_mvp_snapshot) %>%
  mutate(time_idx = match(snapshot_date, snapshots)) %>%
  left_join(df_player_dict, by = "name")

# 提取指定节点：在 1(国庆), 3(12.1), 4(元旦), 6(3.1), 8(劳动节) 节点渲染头像
df_nodes <- df_bump %>% filter(time_idx %in% c(1, 3, 4, 6, 8))

# 5. 开始绘制标准的 16:9 MVP 赔率走势图
p <- df_bump %>%
  ggplot(aes(x = time_idx, y = Rank, group = name)) +
  
  # 居中的全局防盗背景水印（聚焦在 Top 10 的中心第 5.5 行）
  annotate("text", x = 4.5, y = 5.5, label = "俗人罢了", alpha = 0.04, size = 60, angle = 30, family = "Microsoft YaHei") +
  
  # 【新加项】：在图表最左侧加入 Consolas 字体的 #1 到 #10 名次标签
  annotate(
    "text", 
    x = 0.55,                  # 放在国庆节点（x=1）左侧的黄金留白区
    y = 1:10,                  # 严格对应 1 到 10 名的垂直高度
    label = paste0("#", 1:10), # 自动生成 #1, #2 ... #10
    family = "Consolas",       # 强制指定 Consolas 字体
    fontface = "bold",         # 粗体显示
    size = 7,                  # 字号大小
    color = "#333333"          # 深灰色，高级且不刺眼
  ) +
  
  # 核心走势流线：线宽保持 3，顺滑度为 8
  geom_bump(aes(color = color_hex), linewidth = 3, smooth = 8, lineend = "round", alpha = 0.85) +
  
  # 头像叠加：彻底删除了 geom_point 圆点底座，直接大图渲染圆形头像
  geom_image(data = df_nodes, aes(x = time_idx, image = img_path), size = 0.1) +
  
  # 映射颜色
  scale_color_identity() +
  
  # Y轴反转：只保留 1-10 的刻度标签
  scale_y_reverse(breaks = 1:10) +
  
  # X轴拉长：左侧界扩展到 0.1，为左侧的名次标签 #1~#10 留出绝对充足的空间
  scale_x_continuous(
    breaks = 1:length(x_labels),
    labels = x_labels,
    limits = c(0.1, 8.5)
  ) +
  
  # 视窗裁剪到 10.4，确保第 10 名的头像完整，同时第 11-13 名的数据继续在视窗下方绘制
  coord_cartesian(ylim = c(10.4, 0.4), expand = FALSE) +
  
  labs(
    title = "NBA2025-26赛季球员MVP赔率排名变化图",
    subtitle = "北京时间 | 截止2026.5.1 | 数据源：rotowire | 制作：俗人罢了",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    
    # 彻底清除横向网格干扰线，仅保留纵向日期节点的虚线辅助线
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linetype = "dashed"),
    
    # 隐藏原生数字 Y 轴
    axis.text.y = element_blank(),
    
    # X 轴时间标签粗体与特定字体配置
    axis.text.x = element_text(size = 18, face = "bold", family = "Consolas", color = "#333333"),
    
    # 标题与副标题留边调整
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 8)),
    plot.subtitle = element_text(size = 16, color = "#A1A1A1", hjust = 0.5, margin = margin(b = 10)),
    
    # 调整整体画布的外边距
    plot.margin = margin(5, 0, 10, 0)
  )

# 6. 最简调用本地重写后的自定义保存指令，强制输出 16:9 高清大图
ggsave("NBA2025-26赛季MVP赔率排名变化图.png", p, width = 16, height = 9, dpi = 300)