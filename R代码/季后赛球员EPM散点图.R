# ==============================================================================
# 1. 加载核心包
# ==============================================================================
library(tidyverse)
library(arrow)    # 读取本地 Parquet 数据
library(ggtext)   # 渲染富文本标签
library(ggimage)  # 渲染本地球员头像
library(scales)   # 比例尺工具
library(MASS)     # 核心：用于 2D 核密度估计 (kde2d) 生成流体星云

# ==============================================================================
# 2. 数据载入与极简化清洗
##==============================================================================
# 直接读取徐大帅用 VIP Cookie 抓取的季后赛专属数据
df_raw <- arrow::read_parquet("D:/nba/epm_stats/2025-26NBA_Playoffs_EPM_stats.parquet")

# 筛选并映射头像路径
df <- df_raw %>%
  # 确保 off 和 def 是数值型
  mutate(
    off = as.numeric(off),
    def = as.numeric(def)
  ) %>%
  # 剔除存在 NA 的无效数据
  filter(!is.na(off) & !is.na(def)) %>%
  # 直连本地头像库
  mutate(headshot_path = paste0("D:/nba/headshot/", player_id, ".png"))

# 设定基准十字准星（EPM 的基准线为 0 代表联盟平均水平）
# 也可以用季后赛平均值，这里采用 0 绝对标准
baseline_off <- 0
baseline_def <- 0

# ==============================================================================
# 3. 🌟 核心：流体星云渲染矩阵 (KDE)
# ==============================================================================
# 动态计算边界与 15% 的留白 (Padding)，防止边缘头像被切掉
pad_x <- (max(df$off) - min(df$off)) * 0.01
pad_y <- (max(df$def) - min(df$def)) * 0.01

x_seq <- seq(min(df$off) - pad_x, max(df$off) + pad_x, length.out = 200)
y_seq <- seq(min(df$def) - pad_y, max(df$def) + pad_y, length.out = 200)

grid_data <- expand.grid(off = x_seq, def = y_seq)

x_min <- min(df$off); x_max <- max(df$off)
y_min <- min(df$def); y_max <- max(df$def)

# 【修复核心1】重构颜色 Score 逻辑：直接采用 进攻 + 防守 (即总 EPM 影响力)
# 这样文班亚马这种防守逆天、进攻稍弱的球员，总分依然是正数，完美吃满绿色加成
grid_data <- grid_data %>%
  mutate(Score = off + def)

# 【修复核心2】调大带宽 (h) 的分母，从 3.5 降到 2
# 作用是让星云的“光晕”扩散得更远，能完美包裹住外围的极端数据球员
kde <- kde2d(df$off, df$def, n = 200, 
             lims = c(min(x_seq), max(x_seq), min(y_seq), max(y_seq)),
             h = c((x_max - x_min)/4, (y_max - y_min)/4)) 

grid_data$density <- as.vector(kde$z)
max_dens <- max(grid_data$density)

# 【修复核心3】修改透明度算法，赋予保底底色
# 降低指数让颜色更加浓郁，并 +0.1 给出保底透明度，防止离群怪物完全隐身
grid_data <- grid_data %>%
  mutate(alpha_val = pmin(1, (density / max_dens)^0.4))

# ==============================================================================
# 4. 顶级视觉绘图执行
# ==============================================================================
p <- ggplot() +
  
  # Layer 0: 核心铁律 - 全局防盗水印 (最底层正中心)
  annotate("text", x = mean(x_seq), y = mean(y_seq), label = "俗人罢了", 
           alpha = 0.04, size = 50, angle = 30, family = "Microsoft YaHei", fontface = "bold") +
  
  # Layer 1: 🌌 星云像素网格底色
  geom_tile(data = grid_data, aes(x = off, y = def, fill = Score, alpha = alpha_val)) +
  # 【修复核心4】使用 gradient2，将 0 严格锚定为底色，正数为绿，负数为红，极具张力
  scale_fill_gradient2(low = "#D54650", mid = "floralwhite", high = "#3FA34D", midpoint = 0, guide = "none") +
  scale_alpha_continuous(range = c(0, 0.5), guide = "none") + 
  
  # Layer 2: 平均线十字坐标轴 
  geom_vline(xintercept = baseline_off, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = baseline_def, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # Layer 4: 四象限富文本标签 
  # 【修复核心5】将所有的 fill = "floralwhite" 替换为 fill = NA，实现真正的纯文字去底色效果
  annotate("richtext", x = min(x_seq), y = max(y_seq), 
           label = "<b style='font-size:22pt; color:#546E7A'>防守大闸</b><br><span style='font-size:13pt; color:#666666'></span>", 
           fill = NA, label.color = NA, family = "Microsoft YaHei", hjust = 0, vjust = 0.9) +
  
  annotate("richtext", x = max(x_seq), y = max(y_seq), 
           label = "<b style='font-size:22pt; color:#1B5E20'>攻防一体</b><br><span style='font-size:13pt; color:#666666'>季后赛真神 | 降维打击</span>", 
           fill = NA, label.color = NA, family = "Microsoft YaHei", hjust = 1, vjust = 0.9) +
  
  annotate("richtext", x = min(x_seq), y = min(y_seq), 
           label = "<b style='font-size:22pt; color:#C62828'>战犯</b><br><span style='font-size:13pt; color:#666666'>攻防全拉</span>", 
           fill = NA, label.color = NA, family = "Microsoft YaHei", hjust = 0, vjust = 0) +
  
  annotate("richtext", x = max(x_seq), y = min(y_seq), 
           label = "<b style='font-size:22pt; color:#546E7A'>进攻尖刀</b><br><span style='font-size:13pt; color:#666666'></span>", 
           fill = NA, label.color = NA, family = "Microsoft YaHei", hjust = 1, vjust = 0)+
  # Layer 3: 球员头像直出
  geom_image(data = df, aes(x = off, y = def, image = headshot_path), 
             size = 0.05, by = "width", asp = 16/9) +
  
  # Layer 5: 坐标轴与极致排版
  labs(
    title = "NBA2025-26赛季季后赛球员EPM攻防影响力对比图",
    subtitle = paste0("数据源：Dunks & Threes|制作:俗人罢了"),
    x = "← 进攻拖后腿          季后赛进攻 EPM           顶尖进攻 →",
    y = "← 防守提款机          季后赛防守 EPM           防守大闸 →"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    
    text = element_text(family = "Consolas"), 
    plot.title = element_text(family = "Microsoft YaHei", face = "bold", size = 26, 
                              color = "#222222", hjust = 0, margin = margin(b=8)),
    plot.subtitle = element_text(family = "Microsoft YaHei", size = 13, color = "#777777", 
                                 lineheight = 1.2, margin = margin(b=12)),
    
    # 按照铁律：完全移除图表底部的常规 Caption
    plot.caption = element_blank(),
    
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    axis.title.x = element_text(family = "Microsoft YaHei", size = 14, face = "bold", margin = margin(t=12)),
    axis.title.y = element_text(family = "Microsoft YaHei", size = 14, face = "bold", margin = margin(r=12)),
    axis.text = element_text(size = 12, color = "gray30"),
    
    plot.margin = margin(15, 15, 15, 15)
  )

# ==============================================================================
# 5. 自定义极简保存 (强制 16:9 横版宽幅，适合社交媒体阅读)
# ==============================================================================
ggsave("2026_Playoffs_EPM_Nebula.png", p, width = 12, height = 8, dpi = 300)