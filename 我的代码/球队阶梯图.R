library(tidyverse) # 数据处理
library(ggimage)   # 处理图片logo
library(magick)    # 图片透明度处理
library(ggtext)    # 必须加载这个包

# 修改后的主题函数
theme_f5_fixed <- function (font_size = 10) { 
  theme_minimal(base_size = font_size, base_family = "Microsoft YaHei") %+replace% 
    theme(
      plot.title.position = "plot", # 【在此添加：使标题相对于整张图左对齐】
      
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = 0, size = 13, face = 'bold'), 
      # 【修改这里】：必须把 element_text 改为 ggtext::element_markdown
      plot.subtitle = ggtext::element_markdown(
        color = 'gray35', 
        hjust = 0, 
        margin = margin(2.5, 0, 10, 0), 
        size = 9,
        lineheight = 1.2 # markdown模式下通常建议设置行高
      ),
      plot.caption = element_text(color = 'gray65', margin=margin(5,0,0,0), hjust = 1, size = 8),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      plot.margin = margin(t = 5, r = 0, b = 0, l = 10) 
    )}

# 2. 定义图片透明化函数 (保持不变)
transparent <- function(img) {
  magick::image_fx(img, expression = "0.8*a", channel = "alpha") 
}

# 3. 录入数据 
# (已将你提供的球队全名转换为对应的文件名简写，如 Bulls -> CHI)
df <- tibble::tribble(
  ~Rank, ~TEAM, ~Rank_Variance,
  1, "CHI", 16.98,
  2, "PHO", 12.82,
  3, "ORL", 12.72,
  4, "MIL", 12.26,
  5, "BOS", 12.23,
  6, "CHA", 11.88,
  7, "POR", 9.25,
  8, "LAC", 8.98,
  9, "MIN", 8.77,
  10, "TOR", 8.61,
  11, "GSW", 8.26,
  12, "NYK", 7.61,
  13, "MIA", 6.91,
  14, "PHI", 5.88,
  15, "MEM", 5.78,
  16, "DEN", 5.02,
  17, "ATL", 4.92,
  18, "LAL", 4.71,
  19, "UTA", 4.39,
  20, "DET", 4.35,
  21, "CLE", 3.98,
  22, "SAC", 2.89,
  23, "DAL", 2.17,
  24, "SAS", 2.12,
  25, "BKN", 1.98,
  26, "NOP", 1.67,
  27, "HOU", 1.33,
  28, "IND", 1.04,
  29, "WAS", 0.23,
  30, "OKC", 0.19
)

# 4. 构建本地Logo路径 (保持不变)
base_path <- "D:\\nba\\球队logo\\球队logo圆\\"

df <- df %>% 
  mutate(
    logo = paste0(base_path, TEAM, ".png")
  )

plot <- df %>% 
  # 修改 Y 轴数据映射
  ggplot(aes(x = Rank, y = Rank_Variance)) +
  
  # 绘制Logo：保持参数不变 (0.085)
  geom_image(aes(image = logo), 
             size = 0.085, 
             image_fun = transparent) +
  
  # Y轴设置：根据方差数值范围(0-17)调整，但保持原来的样式逻辑
  # 注意：方差是数值越大越波动，所以我使用 continuous 而不是 reverse
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.1)), 
    breaks = seq(0, 18, 2), # 根据数据最大值16.98调整刻度
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # X轴设置：保持原本的 limits 逻辑
  scale_x_continuous(limits = c(-1, 32), expand = c(0, 0)) +
  
  # 保持关闭裁剪
  coord_cartesian(clip = "off") + 
  
  # 应用修正字体后的主题
  theme_f5_fixed() + 
  
  # 更新标题文字
  labs(
    title = "NBA本赛季球队实力排名浮动指数——左上角代表球队水平起伏大，右下角代表球队水平稳定",
    subtitle = "数据截止全明星|数据源 hoopR|制作：<b>俗人罢了</b>",
    y = "每日净效率排名方差 (Rank Variance)"
  )

# 保存
ggsave("nba_rank_volatility_fixed.png", plot = plot, width = 8.5, height = 5.5, dpi = 300)