# ==========================================
# Tobias Harris 职业生涯 USG% (回合占有率) 走势图
# ==========================================

library(tidyverse)
library(ggimage)

# 1. 构建常规赛数据 (清洗了 2TM 与 Dec-11)
df_reg <- tibble::tribble(
  ~Season,   ~Team, ~USG,
  "2025-26", "DET", 18.8,
  "2024-25", "DET", 17.6,
  "2023-24", "PHI", 20.7,
  "2022-23", "PHI", 18.2,
  "2021-22", "PHI", 21.7,
  "2020-21", "PHI", 23.9,
  "2019-20", "PHI", 23.9,
  "2018-19", "PHI", 22.8,  # 原2TM，季后赛代表PHI
  "2017-18", "LAC", 23.3,  # 原2TM，被交易至LAC
  "2016-17", "DET", 21.7,
  "2015-16", "DET", 19.3,  # 原2TM，季后赛代表DET
  "2014-15", "ORL", 22.5,
  "2013-14", "ORL", 22.2,
  "2012-13", "ORL", 22.1,  # 原2TM，主要代表ORL
  "2011-12", "MIL", 20.6   # 原Dec-11，规范为2011-12
) %>% mutate(Type = "常规赛")

# 2. 构建季后赛数据
df_po <- tibble::tribble(
  ~Season,   ~Team, ~USG,
  "2025-26", "DET", 23.1,
  "2024-25", "DET", 15.5,
  "2023-24", "PHI", 11.8,
  "2022-23", "PHI", 17.5,
  "2021-22", "PHI", 19.1,
  "2020-21", "PHI", 24.6,
  "2019-20", "PHI", 21.5,
  "2018-19", "PHI", 19.0,
  "2015-16", "DET", 18.6
) %>% mutate(Type = "季后赛")

# 3. 数据整合与连续索引构建
df_all <- bind_rows(df_reg, df_po) %>%
  arrange(Season)

# 提取唯一的赛季并赋予连续的 X 轴坐标索引
seasons <- df_all %>% 
  distinct(Season) %>% 
  arrange(Season) %>% 
  mutate(Season_Index = row_number())

df_plot <- df_all %>%
  left_join(seasons, by = "Season") %>%
  mutate(
    # 构建本地球队方标 Logo 路径
    Logo_Path = paste0("D:/nba/球队logo/球队logo圆/", Team, ".png"),
    # 强制图例顺序：常规赛在前，季后赛在后
    Type = factor(Type, levels = c("常规赛", "季后赛"))
  )

# 计算水印绝对中心点
mid_x <- mean(seasons$Season_Index)
mid_y <- mean(df_plot$USG)

# 4. 绘制核心图表
p <- ggplot() +
  
  # 【底层】全局防盗水印
  annotate("text", x = mid_x, y = mid_y, label = "俗人罢了", 
           alpha = 0.04, size = 50, angle = 30, family = "Microsoft YaHei") +
  
  # 【中层】常规赛与季后赛折线 (隔离图层以免影响Logo)
  geom_line(data = df_plot, aes(x = Season_Index, y = USG, color = Type, group = Type), 
            linewidth = 2.5, alpha = 0.85) +
  
  # 【顶层】将数据点替换为对应赛季效力的球队 Logo
  geom_image(data = df_plot, aes(x = Season_Index, y = USG, image = Logo_Path), size = 0.08) +
  
  # 颜色比例尺设定
  scale_color_manual(values = c("常规赛" = "#0A9C9C", "季后赛" = "#E24C1D")) +
  
  # X轴显式声明连续轴，并将数字映射换成对应的赛季文本
  scale_x_continuous(breaks = seasons$Season_Index, labels = seasons$Season) +
  
  # Y轴格式化 (添加百分号)
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  # 标题与坐标轴标签
  labs(
    title = "Tobias Harris 职业生涯 USG% (回合占有率)球权走势图",
    subtitle = "数据点显示为其当赛季效力(或主代表)球队LOGO | 制作：俗人罢了",
    x = "赛季 (Season)",
    y = "回合占有率 (USG%)"
  ) +
  
  # 5. 社交媒体视觉排版 (Theme)
  theme_minimal(base_family = "Microsoft YaHei") +
  theme(
    # 背景色设为高级暖白
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    text = element_text(family = "Microsoft YaHei", color = "#333333"),
    
    # 标题排版
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5, margin = margin(t = 20, b = 10)),
    plot.subtitle = element_text(size = 15, color = "#A1A1A1", hjust = 0.5, margin = margin(b = 20)),
    
    # 图例设置 (放置在上方居中)
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16, face = "bold", margin = margin(r = 20)),
    legend.key.width = unit(3, "cm"),
    
    # 坐标轴与字体分离: X轴文字倾斜防重叠，Y轴数字强制 Consolas
    axis.text.x = element_text(family = "Consolas", size = 13, angle = 45, hjust = 1, vjust = 1, color = "#555555"),
    axis.text.y = element_text(family = "Consolas", size = 15, color = "#333333"),
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 15)),
    
    # 网格线设置
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed")
  )

# 6. 自定义 ggsave 极简导出 (严格遵守本地规则)
ggsave("Tobias_Harris_生涯USG走势.png", p, width = 16, height = 9, dpi = 300)