library(tidyverse)
library(ggrepel)
library(ggimage)  # 🌟 必须加载 ggimage 包来渲染头像
library(stringi)  # 🌟 必须加载 stringi 用于姓名清洗

# ==========================================
# 0. 定义核心清洗函数 (防报错护城河)
# ==========================================
clean_name_for_join <- function(name) {
  name %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("[^a-z0-9]", "")
}

# ==========================================
# 1. 强健的 ID 库处理
# ==========================================
df_id <- readRDS("D:/nba/id查询.rds")
df_id_clean <- df_id %>%
  # 用列索引 [[1]] 定位姓名列
  mutate(join_name = clean_name_for_join(.[[1]])) %>% 
  # 强制保留 join_name 和 第二列（并重命名为 player_id）
  select(join_name, player_id = 2) 

# ==========================================
# 2. 读取、清洗与合并本地数据
# ==========================================
df_load <- read_csv("D:/nba/球权占比.csv", show_col_types = FALSE) %>%
  select(1:2) %>%
  set_names(c("Player", "Load"))

df_def <- read_csv("D:/nba/对位.csv", show_col_types = FALSE) %>%
  select(1:2) %>%
  set_names(c("Player", "Difficulty"))

df_merged <- inner_join(df_load, df_def, by = "Player") %>%
  mutate(
    # 生成用于 Join 的干净姓名
    join_name = clean_name_for_join(Player)
  ) %>%
  # 🌟 关联 ID 库并生成头像路径
  left_join(df_id_clean, by = "join_name") %>%
  mutate(
    player_id = replace_na(as.character(player_id), "0"),
    headshot  = paste0("D:/nba/headshot/", player_id, ".png")
  )

# ==========================================
# 3. 计算四个角落的 Top 10 球员标签
# ==========================================
df_scored <- df_merged %>%
  mutate(
    z_load = scale(Load)[, 1],
    z_diff = scale(Difficulty)[, 1],
    score_top_right = z_load + z_diff,      # 右上
    score_top_left = -z_load + z_diff,      # 左上
    score_bottom_right = z_load - z_diff,   # 右下
    score_bottom_left = -z_load - z_diff    # 左下
  )

corner_players <- bind_rows(
  df_scored %>% top_n(10, score_top_right),
  df_scored %>% top_n(10, score_top_left),
  df_scored %>% top_n(10, score_bottom_right),
  df_scored %>% top_n(10, score_bottom_left)
) %>%
  distinct(Player)

label_data <- df_merged %>% filter(Player %in% corner_players$Player)

# ==========================================
# 4. 绘图主题设定 (取消了无用的图例代码)
# ==========================================
theme_owen <- function () { 
  theme_minimal(base_family = "Microsoft YaHei", base_size = 12) +
    theme(
      plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
      panel.background = element_rect(fill = "floralwhite", color = "floralwhite"),
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      plot.subtitle = element_text(color = "gray20", size = 11, hjust = 0.5, margin = margin(b = 10)),
      plot.caption = element_text(color = "gray60", size = 9, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray85", linewidth = 0.2), 
      axis.text = element_text(family = "Consolas", color = "black", face = "bold")
    )
}

# ==========================================
# 5. 可视化绘制
# ==========================================
p <- ggplot(df_merged, aes(x = Load, y = Difficulty)) +
  
  # 趋势线（置于底层）
  stat_smooth(
    method = "lm", color = "black", linetype = "dashed",
    se = TRUE, fill = "gray70", alpha = 0.15, linewidth = 1
  ) +
  
  # 🌟 核心替换：使用真实头像绘制散点
  # size 控制头像大小，0.05 是个比较适中的散点图比例，你可以微调 (0.04 - 0.08)
  geom_image(aes(image = headshot), size = 0.08) +
  

  # 标题与坐标轴（移除了残差颜色的说明文字）
  labs(
    title = "NBA2025-26常规赛核心球员球权占比 (Load) 与 防守对位难度趋势图",
    subtitle = "场均至少出场30分钟 | 头像位置代表该球员真实水平",
    x = "球权占比 (Load %)",
    y = "防守对位难度 (Matchup Difficulty)"
  ) +
  
  # 载入主题
  theme_owen()

# ==========================================
# 6. 保存图表
# ==========================================
ggsave("球员_球权与防守_头像版.png", p, width = 16, height = 9, dpi = 300)