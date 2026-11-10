# ==========================================
# 2026 NBA 选秀顺位概率图 (绝对定位直标 + Logo 终极版)
# ==========================================

# 1. 加载必备包
library(tidyverse)
library(ggchicklet)
library(ggtext)
library(shadowtext)
library(prismatic)
library(ggimage)

# 2. 构造数据
df_wide <- tribble(
  ~team, ~p1, ~p2, ~p3, ~p4, ~p5, ~p6, ~p7, ~p8, ~p9, ~p10, ~p11, ~p12, ~p13, ~p14, ~avg,
  "WAS", 14.0, 13.4, 12.7, 12.0, 47.9,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,  3.7,
  "IND", 14.0, 13.4, 12.7, 12.0, 27.8, 20.1,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,  3.9,
  "BKN", 14.0, 13.4, 12.7, 12.0, 14.8, 26.0,  7.0,   NA,   NA,   NA,   NA,   NA,   NA,   NA,  4.1,
  "UTA", 11.5, 11.4, 11.2, 11.0,  4.75, 22.65, 21.7,  5.45,  0.3,   NA,   NA,   NA,   NA,   NA,  4.7,
  "SAC", 11.5, 11.4, 11.2, 11.0,  4.75, 22.65, 21.7,  5.45,  0.3,   NA,   NA,   NA,   NA,   NA,  4.7,
  "MEM",  9.0,  9.2,  9.4,  9.6,   NA,  8.6, 29.7, 20.6,  3.7,  0.2,   NA,   NA,   NA,   NA,  5.5,
  "NOP", 6.75, 7.05, 7.45, 7.85,  NA,  NA,  9.9, 34.25, 22.45, 4.0, 0.25, 0.05,   NA,   NA,  6.65,
  "DAL", 6.75, 7.05, 7.45, 7.85,  NA,  NA,  9.9, 34.25, 22.45, 4.0, 0.25, 0.05,   NA,   NA,  6.65,
  "CHI",  4.5,  4.8,  5.2,  5.7,   NA,   NA,   NA,   NA, 50.8, 25.9,  3.0,  0.1,   -1,   NA,  8.0,
  "MIL",  3.0,  3.3,  3.6,  4.0,   NA,   NA,   NA,   NA,   NA, 65.9, 19.0,  1.2,   -1,   -1,  9.2,
  "GSW",  2.0,  2.2,  2.4,  2.8,   NA,   NA,   NA,   NA,   NA,   NA, 77.6, 12.6,  0.4,   -1, 10.3,
  "LAC",  1.5,  1.7,  1.9,  2.1,   NA,   NA,   NA,   NA,   NA,   NA,   NA, 86.1,  6.7,  0.1, 11.4,
  "MIA",  1.0,  1.1,  1.2,  1.4,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA, 92.9,  2.3, 12.5,
  "CHA",  0.5,  0.6,  0.6,  0.7,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA, 97.6, 13.7
) 

# （这里承接你上面原封不动的 df_wide 数据代码）

# ================= 修改开始 =================

# 【修复与优化】：直接从 df_wide 提取真实球队顺序，避免错位
team_levels <- df_wide$team 

df <- df_wide %>%
  pivot_longer(cols = starts_with("p"), names_to = "pick", values_to = "odds") %>%
  filter(odds > 0) %>%
  mutate(
    pick = as.numeric(str_remove(pick, "p")),
    team = factor(team, levels = rev(team_levels)) # 反转后：WAS=14, IND=13...NOP=8...LAC=3
  ) %>%
  group_by(team) %>%
  arrange(pick, .by_group = TRUE) %>%
  mutate(
    label_pos = cumsum(odds) - (odds / 2),
    
    # 新版文本显示逻辑
    label_text = case_when(
      odds <= 10 ~ "", # 概率 <= 10% 的不显示
      team == "IND" & pick %in% c(5, 6) ~ "", 
      team == "NOP" & pick %in% c(1, 2, 3, 4, 6, 7, 8, 9) ~ "",
      team == "LAC" ~ "", # 【新增】：LAC 的签全部变成 OKC，隐藏原有文字防止遮挡 Logo
      TRUE ~ paste0("#", pick, " (", odds, "%)") 
    )
  ) %>%
  ungroup()

df_logo_base <- df %>%
  distinct(team) %>%
  mutate(logo_path = paste0("D:/nba/球队logo/队标/", as.character(team), ".png"))

# 【新增】：包含 OKC 的交易 Logo 坐标表
# team_y 对应因子化后的绝对坐标：IND = 13, NOP = 8, LAC = 3
df_trade_logos <- tibble(
  team_y = c(13, 8, 3), 
  odds_x = c(76, 50, 50), # LAC的签全给OKC，Logo居中放在 50 的位置
  logo_path = c("D:/nba/球队logo/队标/LAC.png", 
                "D:/nba/球队logo/队标/ATL.png",
                "D:/nba/球队logo/队标/OKC.png")
)

my_colors <- setNames(
  colorRampPalette(c("#008348", "#FFFFFF", "#CE1141"))(14),
  nm = as.character(1:14)
)

p <- ggplot() +
  
  annotate("text", x = 7.5, y = 50, label = "俗人罢了", 
           alpha = 0.04, size = 65, angle = 30, family = "Microsoft YaHei") +
  
  geom_chicklet(
    data = df, 
    aes(x = team, y = odds, fill = as.factor(pick), 
        color = after_scale(clr_darken(fill, 0.2))),
    width = 0.65, radius = grid::unit(1.5, "mm"), size = 0.5
  ) +
  
  geom_shadowtext(
    data = df,
    aes(x = team, y = label_pos, label = label_text),
    color = "white", bg.colour = "black", bg.r = 0.1,
    family = "Consolas", fontface = "bold", size = 4.5 
  ) +
  
  # 侧边悬浮 Logo
  geom_image(data = df_logo_base, aes(x = team, y = -2, image = logo_path), size = 0.06) +
  
  # ================= 高亮矩形 & Logo: 快船去步行者 (#07103E) =================
annotate("rect", xmin = 13 - 0.35, xmax = 13 + 0.35, ymin = 52.1, ymax = 100, 
         linewidth = 1.8, color = "white", fill = "transparent") +
  annotate("rect", xmin = 13 - 0.35, xmax = 13 + 0.35, ymin = 52.1, ymax = 100, 
           linewidth = 0.8, color = "#07103E", fill = "transparent") +
  geom_image(data = df_trade_logos %>% filter(str_detect(logo_path, "LAC")),
             aes(x = team_y, y = odds_x, image = logo_path), size = 0.045) +
  
  # ================= 高亮矩形 & Logo: 鹈鹕去老鹰 (#E9072B) =================
annotate("rect", xmin = 8 - 0.35, xmax = 8 + 0.35, ymin = 0, ymax = 99.8, 
         linewidth = 1.8, color = "white", fill = "transparent") +
  annotate("rect", xmin = 8 - 0.35, xmax = 8 + 0.35, ymin = 0, ymax = 99.8, 
           linewidth = 0.8, color = "#E9072B", fill = "transparent") +
  geom_image(data = df_trade_logos %>% filter(str_detect(logo_path, "ATL")),
             aes(x = team_y, y = odds_x, image = logo_path), size = 0.045) +
  
  # ================= 高亮矩形 & Logo: 快船去雷霆 (#007AC1) =================
annotate("rect", xmin = 3 - 0.35, xmax = 3 + 0.35, ymin = 0, ymax = 100, 
         linewidth = 1.8, color = "white", fill = "transparent") +
  annotate("rect", xmin = 3 - 0.35, xmax = 3 + 0.35, ymin = 0, ymax = 100, 
           linewidth = 0.8, color = "#007AC1", fill = "transparent") +
  geom_image(data = df_trade_logos %>% filter(str_detect(logo_path, "OKC")),
             aes(x = team_y, y = odds_x, image = logo_path), size = 0.045) +
  
  scale_fill_manual(values = my_colors) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, by = 10), labels = function(x) paste0(x, "%")) +
  
  # 全局翻转
  coord_flip(ylim = c(-7, 100), clip = "off") +
  
  # 主题部分
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dashed"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 12, family = "Consolas", color = "grey30"),
    plot.title = element_markdown(size = 28, face = "bold", hjust = 0.5, family = "Microsoft YaHei"),
    
    plot.subtitle = element_markdown(
      size = 13, 
      color = "#A1A1A1", 
      hjust = 0.5, 
      family = "Microsoft YaHei",
      lineheight = 1.2,
      margin = margin(b = 15)
    ),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 0),
    legend.position = "none" 
  ) +
  labs(
    y = "中签概率 %",
    title = "NBA 2025-26 各支球队乐透选秀中签概率",
    subtitle = "数据源：tankathon  |  制作：<span style='font-size:18px; color:#333333;'>**俗人罢了**</span>  |  制作时间：2026.4.18"
  )

# 4. 保存输出
ggsave("2026_Draft_Lottery_Final_Logo.png", p, width = 16, height = 9, dpi = 300)