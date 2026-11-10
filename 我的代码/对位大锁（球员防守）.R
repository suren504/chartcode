# ==============================================================================
# 亚历山大 (SGA) 专属：个人防守质检报告（对手进攻头像版）
# ==============================================================================

# 加载必备包
library(tidyverse)
library(ggimage)    
library(showtext)   

# 开启中文字体支持
showtext_auto()

# ----------------- 1. 数据读取与处理 -----------------
# 读取抓取的原始对位数据
df <- read_csv("D:/nba/matchup.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# 核心清洗逻辑：这次 SGA 是防守人！
sga_def_matchups <- df %>%
  # 🚨 核心切换：SGA 是防守者 (def_player_id == 1628983)，且对位至少 20 回合
  filter(def_player_id == 1628983, partial_poss >= 20) %>%
  mutate(
    # 计算真实回合数分母 (投篮 + 0.44*罚球 + 失误)
    true_1v1_poss = matchup_fga + 0.44 * matchup_fta + matchup_tov,
    
    # 计算 True PPP (SGA 在面对该球员时的每回合失分)
    true_ppp = if_else(true_1v1_poss > 0, player_pts / true_1v1_poss, 0),
    
    # 🎯 核心魔法：这次去拿进攻球员 (off_player_id) 的头像！
    headshot_path = paste0("D:/nba/www/", off_player_id, ".png")
  )

# 计算 X 轴和 Y 轴的均值（用于画十字虚线）
mean_poss <- mean(sga_def_matchups$partial_poss, na.rm = TRUE)
mean_ppp <- mean(sga_def_matchups$true_ppp, na.rm = TRUE)

# ----------------- 2. 核心绘图 -----------------
p <- ggplot(sga_def_matchups, aes(x = partial_poss, y = true_ppp)) +
  
  # 【十字虚线基准线】
  geom_hline(yintercept = mean_ppp, linetype = "dashed", color = "#E36C0A", linewidth = 1, alpha = 0.7) +
  geom_vline(xintercept = mean_poss, linetype = "dashed", color = "#E36C0A", linewidth = 1, alpha = 0.7) +
  
  # 【🎯 替换为进攻人头像】
  geom_image(aes(image = headshot_path), size = 0.08, by = "width") +
  
  # 🚨 【翻转 Y 轴：越往上说明 SGA 防得越好（失分越低）】
  scale_y_reverse() + 
  
  # 【象限文字标注】
  # 右上角：防得多，且 PPP 极低（SGA 把他拿捏了）
  annotate("text", x = max(sga_matchups$partial_poss), y = min(sga_matchups$true_ppp), 
           label = "对位大锁\n防守效果显著！", hjust = 1, vjust = 1, size=6,color = "#065F46", fontface = "bold", family = "Microsoft YaHei") +
  # 现在的右下角 (max X, max Y) 是“提款机”
  annotate("text", x = max(sga_matchups$partial_poss), y = max(sga_matchups$true_ppp), 
           label = "对位玩具\n防守端被打爆！", hjust = 1, vjust = -1, size=6, color = "#7F1D1D", fontface = "bold", family = "Microsoft YaHei") +
  
  # 【坐标轴与标题】
  labs(
    title = "SGA 亚历山大：个人防守质检报告 (2025-26赛季)",
    subtitle = "X轴：对位折合总回合数 | Y轴：SGA防守该进攻人的真实失分(True PPP，已翻转) | 十字虚线为均值",
    x = "对位负荷 (Partial Possessions)",
    y = "Shai Gilgeous-Alexander防守不同球员的单打效率"
  ) +
  
  # 【主题与正方形限制 (保持纯净样式)】
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
ggsave("SGA_防守质检报告象限图.png", p, width = 8, height = 8, dpi = 300)