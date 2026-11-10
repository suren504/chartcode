# ==============================================================================
# 亚历山大 (SGA) 专属：对位防守人头像象限图
# ==============================================================================

# 加载必备包
library(tidyverse)
library(ggimage)    # 🚨 核心包：用于在散点图上绘制本地图片
library(showtext)   # 用于支持中文字体

# 开启中文字体支持
showtext_auto()

# ----------------- 1. 数据读取与处理 -----------------
# 读取抓取的原始对位数据
df <- read_csv("D:/nba/matchup.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# 核心清洗逻辑
sga_matchups <- df %>%
  # 筛选 指定 的进攻对位 (ID: 1628983)，且对位回合数不小于 20
  filter(off_player_id == 203507, partial_poss >= 30) %>%
  mutate(
    # 计算真实回合数分母 (投篮 + 0.44*罚球 + 失误)
    true_1v1_poss = matchup_fga + 0.44 * matchup_fta + matchup_tov,
    
    # 计算 True PPP (真实对位回合得分)
    true_ppp = if_else(true_1v1_poss > 0, player_pts / true_1v1_poss, 0),
    
    # 🎯 核心魔法：直接根据防守人 ID 拼接出本地头像的绝对路径
    # 注意：R语言里路径用正斜杠 "/"
    headshot_path = paste0("D:/nba/www/", def_player_id, ".png")
  )

# 计算 X 轴和 Y 轴的联盟/样本平均值（用于画十字虚线）
mean_poss <- mean(sga_matchups$partial_poss, na.rm = TRUE)
mean_ppp <- mean(sga_matchups$true_ppp, na.rm = TRUE)

# ----------------- 2. 核心绘图 (翻转 Y 轴版) -----------------
p <- ggplot(sga_matchups, aes(x = partial_poss, y = true_ppp)) +
  
  # 【十字虚线基准线】
  geom_hline(yintercept = mean_ppp, linetype = "dashed", color = "#E36C0A", linewidth = 1, alpha = 0.7) +
  geom_vline(xintercept = mean_poss, linetype = "dashed", color = "#E36C0A", linewidth = 1, alpha = 0.7) +
  
  # 【🎯 替换为头像！】
  geom_image(aes(image = headshot_path), size = 0.1, by = "width") +
  
  # 🚨🚨🚨 【核心改动：翻转 Y 轴】 🚨🚨🚨
  # 使用 reverse 之后，坐标轴上方是 0，下方是大数值
  scale_y_reverse() + 
  
  # 【象限文字标注：因为坐标轴翻转了，文字位置也得跟着调】
  # 现在的右上角 (max X, min Y) 是“防守大闸”
  annotate("text", x = max(sga_matchups$partial_poss), y = min(sga_matchups$true_ppp), 
           label = "对位大锁\n防守效果显著！", hjust = 1, vjust = 0.5, size=6,color = "#065F46", fontface = "bold", family = "Microsoft YaHei") +
  # 现在的右下角 (max X, max Y) 是“提款机”
  annotate("text", x = max(sga_matchups$partial_poss), y = max(sga_matchups$true_ppp), 
           label = "对位玩具\n防守端被打爆！", hjust = 1, vjust = -0.2, size=6,color = "#7F1D1D", fontface = "bold", family = "Microsoft YaHei") +
  
  # 【坐标轴与标题】
  labs(
    title = "SGA 亚历山大：对位防守人质检报告 (2025-26赛季)",
    subtitle = "X轴：对位折合总回合数 | Y轴：对位真实回合得分(True PPP，已翻转，越往上越强) | 十字虚线为均值",
    x = "对位负荷 (Partial Possessions)",
    y = "Shai Gilgeous-Alexander面对不同防守人的单打效率"
  ) +
  
  # 【主题与正方形限制】
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "grey40", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, family = "Consolas"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

# ----------------- 3. 保存输出 -----------------
# 保存为 8x8 英寸的正方形图片
ggsave("对位杀手象限图.png", p, width = 8, height = 8, dpi = 300)