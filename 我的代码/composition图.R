# ===============================
# 1️⃣ 加载必要的包
# ===============================
library(tidyverse)
library(janitor)
library(paletteer)
library(cowplot)
library(ggimage)   # ← 新增：用于绘制 LOGO

# ===============================
# 2️⃣ 自定义主题
# ===============================
theme_owen <- function () { 
  theme_minimal(base_size = 12, base_family = "Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "#F0EEE9", color = "#F0EEE9")
    )
}

# ===============================
# 3️⃣ 手动输入的阵容数据（中文球队名）
# ===============================
team_data <- tibble(
  team = c(
    "亚特兰大老鹰", "波士顿凯尔特人", "布鲁克林篮网", "夏洛特黄蜂", 
    "芝加哥公牛", "克利夫兰骑士", "达拉斯独行侠", "丹佛掘金", 
    "底特律活塞", "金州勇士", "休斯顿火箭", "印第安纳步行者", 
    "洛杉矶快船", "洛杉矶湖人", "孟菲斯灰熊", "迈阿密热火", 
    "密尔沃基雄鹿", "明尼苏达森林狼", "新奥尔良鹈鹕", "纽约尼克斯", 
    "俄克拉荷马城雷霆", "奥兰多魔术", "费城76人", "菲尼克斯太阳", 
    "波特兰开拓者", "萨克拉门托国王", "圣安东尼奥马刺", "多伦多猛龙", 
    "犹他爵士", "华盛顿奇才z"
  ),
  选秀 = c(7, 6, 9, 8, 7, 4, 3, 9, 9, 8, 5, 7, 3, 3, 7, 7, 2, 6, 9, 5, 7, 9, 6, 5, 6, 4, 7, 6, 9, 7),
  交易 = c(3, 3, 4, 5, 6, 5, 8, 3, 1, 2, 3, 5, 5, 4, 1, 5, 3, 4, 3, 4, 2, 2, 1, 5, 6, 5, 3, 5, 5, 5),
  裁员认领 = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  自由签约 = c(4, 5, 2, 2, 2, 5, 4, 2, 3, 5, 6, 3, 6, 7, 7, 2, 10, 4, 3, 5, 6, 3, 7, 3, 3, 6, 5, 3, 1, 3)
)

# ===============================
# 4️⃣ 中文球队名 → LOGO 路径映射
# ===============================
logo_map <- tibble(
  team = team_data$team,
  abbr = c(
    "ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
    "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK",
    "OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTA","WAS"
  )
) %>%
  mutate(
    logo_path = paste0("D:/nba/球队logo/球队logo圆/", abbr, ".png")
  )

# ===============================
# 5️⃣ 转长表 + 因子顺序控制
# ===============================
df <- team_data %>% 
  pivot_longer(-team, names_to = "name", values_to = "value") %>% 
  mutate(name = factor(name, levels = c("裁员认领", "交易", "自由签约", "选秀"))) %>%
  left_join(logo_map, by = "team")

# ===============================
# 6️⃣ 添加分区信息
# ===============================
df <- df %>% 
  mutate(
    conf = case_when(
      team %in% c(
        "亚特兰大老鹰", "波士顿凯尔特人", "布鲁克林篮网", "夏洛特黄蜂",
        "芝加哥公牛", "克利夫兰骑士", "底特律活塞", "印第安纳步行者",
        "迈阿密热火", "密尔沃基雄鹿", "纽约尼克斯", "奥兰多魔术",
        "费城76人", "多伦多猛龙", "华盛顿奇才z"
      ) ~ "东部",
      TRUE ~ "西部"
    )
  )

# ===============================
# 7️⃣ 按选秀人数排序
# ===============================
df <- df %>% 
  group_by(team) %>% 
  mutate(draft_val = sum(value[name == "选秀"])) %>% 
  ungroup()

# LOGO 专用数据（每队一行）
logo_df <- df %>%
  distinct(team, draft_val, conf, logo_path) %>%
  mutate(team = fct_reorder(team, draft_val))

# ===============================
# 8️⃣ 绘图
# ===============================
p <- ggplot(
  df,
  aes(x = value, y = fct_reorder(team, draft_val), fill = name)
) +
  geom_col(width = 0.85) +
  geom_vline(xintercept = seq(1, 15, 1), linewidth = 0.5, color = "floralwhite") +
  facet_wrap(~ fct_rev(conf), scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = c(-1.2, 15), expand = c(0, 0)) +
  scale_fill_manual(
    values = paletteer_d("Redmonder::qPBI"),
    breaks = c("选秀", "自由签约", "交易", "裁员认领")
  ) +
  geom_image(
    data = logo_df,
    aes(x = -0.6, y = team, image = logo_path),
    inherit.aes = FALSE,
    size = 0.065,
    by = "width"
  ) +
  theme_owen() +
  theme(
    legend.position = "top",
    
    # ===============================
    # 标题系统（重点）
    # ===============================
    plot.title = element_text(
      hjust = 0.5,          # 居中
      face  = "bold",       # 加粗
      size  = 18,           # 变大
      family = "Microsoft YaHei"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size  = 11,
      margin = margin(t = 4),
      family = "Microsoft YaHei"
    ),
    plot.caption = element_text(
      hjust = 0.5,
      size  = 9,
      margin = margin(t = 6),
      family = "Microsoft YaHei"
    ),
    
    # ===============================
    # 轴 & LOGO
    # ===============================
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title   = element_blank(),
    
    # 分区标题
    strip.text = element_text(
      face = "bold",
      size = 12,
      family = "Consolas"
    )
  )+
  labs(
    title = "NBA 2025–2026 赛季球队人员组成名单",
    subtitle = "每个色块代表一名球员及其被球队获得的方式",
    fill = "获得方式",
    caption = "数据源：basketball.realgm | 制作人：俗人罢了"
  )

# ===============================
# 9️⃣ 保存
# ===============================
ggsave(
  "RosterComp_2025_2026_logo.png",
  p,
  width = 10, height = 9, dpi = 300,
  bg = "aliceblue"
)

browseURL("RosterComp_2025_2026_logo.png")
