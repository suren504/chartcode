# ===============================
# 1️⃣ 加载必要的包
# ===============================
library(tidyverse)
library(janitor)
library(paletteer)
library(cowplot)

# ===============================
# 2️⃣ 自定义主题
# ===============================
theme_owen <- function () { 
  theme_minimal(base_size = 12, base_family = "Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "lavenderblush1", color = "lavenderblush1")
    )
}

# ===============================
# 3️⃣ 数据（列名已为中文）
# ===============================
# ===============================
# 3️⃣ 手动输入的阵容数据（球队名称改为中文）
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
  选秀 = c(7, 6, 9, 8, 7, 4, 3, 9, 9, 8, 5, 7, 3, 3, 7, 7, 2, 6, 9, 5,7, 9, 6, 5, 6, 4, 7, 6, 9, 7),
  交易 = c(3, 3, 4, 5, 6, 5, 8, 3, 1, 2, 3, 5, 5, 4, 1, 5, 3, 4, 3, 4,2, 2, 1, 5, 6, 5, 3, 5, 5, 5),
  裁员认领 = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  自由签约 = c(4, 5, 2, 2, 2, 5, 4, 2, 3, 5,6, 3, 6, 7, 7, 2, 10, 4, 3, 5,6, 3, 7, 3, 3, 6, 5, 3, 1, 3)
)

# ===============================
# 4️⃣ 转长表 + 关键：因子水平反向（保证选秀在最左）
# ===============================
df <- team_data %>% 
  pivot_longer(cols = -team, names_to = "name", values_to = "value") %>% 
  # 因子水平顺序：[裁员认领, 交易, 自由签约, 选秀]（与目标顺序反向）
  mutate(name = factor(name, levels = c("裁员认领", "交易", "自由签约", "选秀")))

# ===============================
# 5️⃣ 添加东西部信息（球队名称已改为中文，匹配对应中文队名）
# ===============================
df <- df %>% 
  mutate(
    conf = case_when(
      team %in% c(
        "亚特兰大老鹰", "波士顿凯尔特人", "布鲁克林篮网", "夏洛特黄蜂",
        "芝加哥公牛", "克利夫兰骑士", "底特律活塞", "印第安纳步行者",
        "迈阿密热火", "密尔沃基雄鹿", "纽约尼克斯", "奥兰多魔术",
        "费城76人", "多伦多猛龙", "华盛顿奇才"
      ) ~ "东部", # 同时将分区名称改为中文，更协调
      TRUE ~ "西部"
    )
  )

# ===============================
# 6️⃣ 球队排序依据（选秀数量）
# ===============================
df <- df %>% 
  group_by(team) %>% 
  mutate(draft_val = sum(value[name == "选秀"], na.rm = TRUE)) %>% 
  ungroup()

# ===============================
# 7️⃣ 绘图（核心：scale_fill_manual匹配因子水平）
# ===============================
# （前面的包加载、数据定义、数据处理代码保持不变，此处展示后续的绘图代码）

# ===============================
# 7️⃣ 绘图（核心：给球队名称添加加粗）
# ===============================
p <- ggplot(
  df,
  aes(x = value, y = fct_reorder(team, draft_val), fill = name)
) +
  geom_col(width = 0.85) +
  geom_vline(xintercept = seq(1, 15, 1), linewidth = 0.5, color = "floralwhite") +
  facet_wrap(~ fct_rev(conf), scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = c(0, 15), expand = c(0, 0)) +
  # 填充色与因子水平对应，图例顺序修正为目标顺序
  scale_fill_manual(
    values = paletteer_d("Redmonder::qPBI"),
    breaks = c("选秀", "自由签约", "交易", "裁员认领"), # 图例显示顺序
    labels = c("选秀", "自由签约", "交易", "裁员认领")
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.75, keyheight = 0.2, default.unit = "inch",
      title.position = "top", label.position = "bottom",
      title.hjust = 0.5, label.hjust = 0.5, nrow = 1
    )
  ) +
  # 这里使用修改后的主题（冷调背景，比如aliceblue）
  theme_owen() +
  theme(
    plot.margin = margin(t = 10, r = 10, b = 15, l = 10),
    plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 18, family = "Microsoft YaHei"),
    plot.subtitle = element_text(hjust = 0.5, vjust = 1, size = 10, family = "Microsoft YaHei"),
    plot.caption = element_text(hjust = 0.5, size = 9, family = "Microsoft YaHei"),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.title = element_text(family = "Consolas"),
    legend.text = element_text(family = "Consolas"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    # ===============================
    # 核心修改：球队名称（y轴文字）加粗 + 可微调字号增强显示
    # ===============================
    axis.text.y = element_text(
      size = 10, 
      family = "Consolas", 
      face = "bold", # 加粗样式（关键）
      color = "black" # 可选：设置为黑色，进一步提升辨识度（若仍觉得浅可添加）
    ),
    # 分区标签（东部/西部）也可同步加粗，保持风格统一
    strip.text = element_text(
      size = 12, 
      face = "bold", 
      family = "Consolas"
    )
  ) +
  labs(
    title = "NBA2025-2026赛季球队人员组成名单",
    subtitle = "每个色块代表一名球员及其被球队获得的方式",
    fill = "获得方式",
    caption = "数据源：basketball.realgm|制作人：俗人罢了"
  )

# 保存图片
ggsave(
  "RosterComp_2025_2026.png",
  p,
  width = 10, height = 9, dpi = 300,
  bg = "aliceblue" # 与冷调背景色保持一致
)

browseURL("RosterComp_2025_2026.png")