# 加载必要的包
library(tidyverse)
library(ggrepel)
library(showtext)
library(sysfonts)
library(grid) 
library(patchwork)

# ---------------------------------------------------------
# 1. 云端字体环境设置
# ---------------------------------------------------------
font_add_google("Noto Sans SC", "noto")
font_add_google("Roboto", "roboto")
showtext_auto()

# 定义全局通用的 F5 主题
theme_f5_base <- function(font_size = 10) { 
  theme_minimal(base_size = font_size) %+replace% 
    theme(
      text = element_text(family = "noto"),
      plot.background = element_rect(fill = 'floralwhite', color = NA), 
      panel.background = element_rect(fill = 'floralwhite', color = NA),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
      
      axis.text = element_text(family = "roboto", color = "gray30", size = 9),
      axis.title = element_text(family = "noto", face = "bold", size = 10),
      plot.title = element_text(family = "noto", face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
      
      legend.position = "none",
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ---------------------------------------------------------
# 2. 数据清洗与多模型计算
# ---------------------------------------------------------
df_raw <- read_csv("D:/nba/三分.csv", show_col_types = FALSE)

df <- df_raw %>%
  select(1, 7, 8, 10, 14) %>% 
  setNames(c("Player", "PA3", "P3_pct", "Exp_pct", "Open_P3_pct")) %>%
  mutate(across(c(PA3, P3_pct, Exp_pct, Open_P3_pct), ~parse_number(as.character(.)))) %>%
  drop_na()

# 建立模型与残差
m_pa3 <- lm(Exp_pct ~ PA3, data = df)
m_p3 <- lm(Exp_pct ~ P3_pct, data = df)
m_open <- lm(Exp_pct ~ Open_P3_pct, data = df)

df <- df %>%
  mutate(
    res_pa3 = Exp_pct - predict(m_pa3, df),
    res_p3 = Exp_pct - predict(m_p3, df),
    res_open = Exp_pct - predict(m_open, df)
  )

# 提取标签数据函数
get_labels <- function(data, res_col) {
  top_p <- data %>% arrange(desc(!!sym(res_col))) %>% slice(1:5)
  top_n <- data %>% arrange(!!sym(res_col)) %>% slice(1:5)
  curry <- data %>% filter(str_detect(Player, "(?i)Stephen Curry"))
  bind_rows(top_p, top_n, curry) %>% distinct(Player, .keep_all = TRUE)
}

labels_pa3 <- get_labels(df, "res_pa3")
labels_p3 <- get_labels(df, "res_p3")
labels_open <- get_labels(df, "res_open")

# --- P4 专用的归一化数据 ---
# 将 3PA, 3P%, Open 3P% 全部压缩到 0~1 的标准区间，用于重合对比
df_norm <- df %>%
  mutate(
    PA3_norm = (PA3 - min(PA3)) / (max(PA3) - min(PA3)),
    P3_norm = (P3_pct - min(P3_pct)) / (max(P3_pct) - min(P3_pct)),
    Open_norm = (Open_P3_pct - min(Open_P3_pct)) / (max(Open_P3_pct) - min(Open_P3_pct))
  )

# ---------------------------------------------------------
# 3. 绘制四个子图
# ---------------------------------------------------------
# 统一设置置信区间透明度，避免 P4 重叠太脏
ci_alpha <- 0.2 

# --- P1: 产量 vs 预期 (橙) ---
p1 <- ggplot(df, aes(x = PA3, y = Exp_pct)) +
  geom_smooth(method = "lm", color = "#E6550D", fill = "#FDAE6B", alpha = ci_alpha, linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(color = res_pa3), size = 2.5, alpha = 0.8) +
  scale_color_gradient(low = "#8C2D04", high = "#FDD0A2") +
  scale_y_reverse() +
  geom_text_repel(data = labels_pa3, aes(label = Player, color = res_pa3), family = "noto", fontface = "bold", size = 3, bg.color = "white", bg.r = 0.15, segment.color = "#A63603", max.overlaps = Inf) +
  theme_f5_base() +
  labs(title = "单位时间三分出手", x = "3PA", y = "投篮难度 (Exp%) —→ [越靠上越难]")

# --- P2: 命中率 vs 预期 (绿) ---
p2 <- ggplot(df, aes(x = P3_pct, y = Exp_pct)) +
  geom_smooth(method = "lm", color = "#238B45", fill = "#A1D99B", alpha = ci_alpha, linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(color = res_p3), size = 2.5, alpha = 0.8) +
  scale_color_gradient(low = "#00441B", high = "#E5F5E0") +
  scale_y_reverse() +
  geom_text_repel(data = labels_p3, aes(label = Player, color = res_p3), family = "noto", fontface = "bold", size = 3, bg.color = "white", bg.r = 0.15, segment.color = "#006D2C", max.overlaps = Inf) +
  theme_f5_base() +
  labs(title = "三分命中率", x = "3P%", y = "") 

# --- P3: 空位命中率 vs 预期 (紫) ---
p3 <- ggplot(df, aes(x = Open_P3_pct, y = Exp_pct)) +
  geom_smooth(method = "lm", color = "#6A51A3", fill = "#BCBDDC", alpha = ci_alpha, linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(color = res_open), size = 2.5, alpha = 0.8) +
  scale_color_gradient(low = "#3F007D", high = "#EFEDF5") +
  scale_y_reverse() +
  geom_text_repel(data = labels_open, aes(label = Player, color = res_open), family = "noto", fontface = "bold", size = 3, bg.color = "white", bg.r = 0.15, segment.color = "#54278F", max.overlaps = Inf) +
  theme_f5_base() +
  labs(title = "空位三分命中率", x = "Open 3P%", y = "投篮难度 (Exp%) —→ [越靠上越难]")

# --- P4: 三线对比 (核心修复 Legend 冗余及位置问题) ---
p4 <- ggplot(df_norm) +
  # 绘制三条线及置信区间
  geom_smooth(aes(x = PA3_norm, y = Exp_pct, color = "三分出手产量", fill = "三分出手产量"), method = "lm", se = TRUE, alpha = 0.15, linewidth = 1.2, linetype = "solid") +
  geom_smooth(aes(x = P3_norm, y = Exp_pct, color = "整体三分命中率", fill = "整体三分命中率"), method = "lm", se = TRUE, alpha = 0.15, linewidth = 1.2, linetype = "solid") +
  geom_smooth(aes(x = Open_norm, y = Exp_pct, color = "空位三分命中率", fill = "空位三分命中率"), method = "lm", se = TRUE, alpha = 0.15, linewidth = 1.2, linetype = "solid") +
  
  scale_y_reverse() +
  
  # 1. 设定线条颜色 (保留这个图例)
  scale_color_manual(
    values = c("三分出手产量" = "#E6550D", "整体三分命中率" = "#238B45", "空位三分命中率" = "#6A51A3")
  ) +
  
  # 2. 设定置信区间颜色，并强制关闭它的图例 (guide = "none")
  scale_fill_manual(
    values = c("三分出手产量" = "#FDAE6B", "整体三分命中率" = "#A1D99B", "空位三分命中率" = "#BCBDDC"),
    guide = "none" 
  ) +
  
  theme_f5_base() +
  theme(
    # 3. 将图例放到左上角 (x=0.05, y=0.95 处)
    legend.position = c(0.05, 0.95), 
    legend.justification = c(0, 1), # 锚点设为图例框的左上角，防止坐标变动导致跑偏
    
    # 4. 图例美化：透明背景，去掉按键灰底
    legend.background = element_rect(fill = alpha("floralwhite", 0.7), color = NA),
    legend.key = element_blank(), 
    legend.title = element_blank(),
    legend.text = element_text(family = "noto", size = 10, face = "bold")
  ) +
  labs(title = "线性对比 (趋势归一化)", x = "各项指标相对水平 (0 = 最低, 1 = 最高)", y = "")
# ---------------------------------------------------------
# 4. Patchwork 拼图与渲染
# ---------------------------------------------------------
final_plot <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "floralwhite", color = NA)
    )
  )

ggsave("3pt_dashboard_fixed.png", plot = final_plot, width = 16, height = 12, dpi = 300, bg = "floralwhite")