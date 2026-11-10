library(tidyverse)
library(ggrepel)
library(showtext)
library(ggthemes) # 引入主题包

# 字体加载
font_add_google("Noto Sans SC", "noto")
showtext_auto()

# 1. 数据处理 (保持一致)
df_raw <- read_csv("D:/nba/三分.csv", show_col_types = FALSE)
df_plot <- df_raw %>%
  select(1, 10, 11) %>%
  setNames(c("Player", "Exp", "Diff")) %>%
  mutate(Exp = as.numeric(Exp), Diff = as.numeric(Diff)) %>%
  drop_na() %>%
  mutate(
    Exp_norm = (max(Exp) - Exp) / (max(Exp) - min(Exp)),
    Diff_norm = (Diff - min(Diff)) / (max(Diff) - min(Diff)),
    Score = Exp_norm + Diff_norm
  )

# 标签数据筛选 (包含库里)
labels_df <- bind_rows(
  df_plot %>% arrange(desc(Score)) %>% slice(1:10),
  df_plot %>% arrange(Score) %>% slice(1:5),
  df_plot %>% filter(str_detect(Player, "Curry"))
) %>% distinct(Player, .keep_all = TRUE)

# 2. 绘制 WSJ 质感图表
p <- ggplot(df_plot, aes(x = Exp, y = Diff, color = Score)) +
  geom_point(size = 3, alpha = 0.8) +
  
  # 渐变配色
  scale_color_gradient(low = "#F5C4AE", high = "#DE6658") +
  
  # X轴反转
  scale_x_reverse() +
  
  # --- 核心优化：ggrepel 标签层 ---
  geom_text_repel(
    data = labels_df, 
    aes(label = Player, color = Score), # 颜色继承 Score，实现线条和文字配色统一
    family = "noto", 
    size = 4, 
    fontface = "bold",
    # 线条柔化处理
    segment.alpha = 0.5,        # 线条半透明，不突兀
    segment.size = 0.4, 
    segment.curvature = -0.1,    # 线条轻微弧度，增加灵动感
    arrow = arrow(length = unit(0.01, "npc")), # 可选：给线条加个极小的箭头指回点位
    
    # 防遮挡算法参数
    force = 30,                  # 增大斥力，把名字推得更开
    force_pull = 0.1,            # 减小对原点位的引力
    box.padding = 1.2,           # 增加文字周边的空白填充
    point.padding = 0.6,         # 增加文字距离数据点的最小距离
    max.overlaps = Inf,
    bg.color = "white",          # 你的白色描边建议
    bg.r = 0.12
  ) +
  
  # --- WSJ 风格定制 ---
  theme_minimal(base_family = "noto") +
  theme(
    # 全局背景：#F8F2E4
    plot.background = element_rect(fill = "#F8F2E4", color = NA),
    panel.background = element_rect(fill = "#F8F2E4", color = NA),
    
    # 模仿 WSJ：只保留 Y 轴水平点状网格
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#D1C4AB", linetype = "dotted", size = 0.8),
    
    # 底部基准线
    axis.line.x = element_line(color = "black", linewidth  = 0.6),
    
    # 隐藏不必要的元素
    legend.position = "none",
    plot.title = element_blank(),
    axis.title = element_text(face = "bold", size = 12, color = "#444444")
  ) +
  labs(
    x = "投篮难度 (Exp%) → [越靠右越难]",
    y = "进球能力/三分命中率-预期命中率 (Diff%) →"
  )

# 渲染输出
ggsave("3pt_difficulty_ability.png", plot = p, width = 10, height = 8, dpi = 300, bg = "#F8F2E4")