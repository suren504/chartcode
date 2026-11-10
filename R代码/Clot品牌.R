# ===============================
# 0. 加载必要的库与字体设置
# ===============================
library(tidyverse)
library(ggimage)
library(ggtext)
library(showtext)

# 【核心修改 1】加载系统自带的微软雅黑字体 (Windows系统)
# "msyh.ttc" 是微软雅黑的标准文件名，如果报错找不到，请检查 C:/Windows/Fonts/
font_add("Microsoft YaHei", regular = "msyh.ttc", bold = "msyhbd.ttc")

# 开启字体自动渲染
showtext_auto()

# ===============================
# 1. 定义市场数据
# ===============================
market_df <- tribble(
  ~Brand,      ~X_Aesthetics, ~Y_Positioning, ~Label_Color,
  "CLOT",       7.5,           6.5,           "#D32F2F", 
  "Supreme",   -8.0,           8.0,           "#333333",
  "Palace",    -6.5,           6.0,           "#333333",
  "Li-Ning",    8.0,          -4.0,           "#333333",
  "Zara",      -3.0,          -7.0,           "#333333",
  "Uniqlo",    -1.5,          -8.0,           "#333333",
  "Nike",      -4.0,          -2.0,           "#333333"
)

# ===============================
# 2. 图片路径映射
# ===============================
logo_base_dir <- "D:/nba/临时照片/品牌"

plot_df <- market_df %>%
  mutate(
    logo_path = file.path(logo_base_dir, paste0(tolower(Brand), ".png"))
  )

# ===============================
# 3. 设定坐标范围
# ===============================
limit_range <- 10 

# ===============================
# 4. 绘图 (ggplot2)
# ===============================
p <- ggplot(plot_df, aes(x = X_Aesthetics, y = Y_Positioning)) +
  
  # ---- A. 象限背景色 ----
annotate("rect", xmin = 0, xmax = limit_range, ymin = 0, ymax = limit_range,
         fill = "#e8f5e9", alpha = 0.6) +
  annotate("rect", xmin = -limit_range, xmax = 0, ymin = -limit_range, ymax = 0,
           fill = "#fff3e0", alpha = 0.6) +
  annotate("rect", xmin = -limit_range, xmax = 0, ymin = 0, ymax = limit_range,
           fill = "#f5f5f5", alpha = 0.4) +
  annotate("rect", xmin = 0, xmax = limit_range, ymin = -limit_range, ymax = 0,
           fill = "#f5f5f5", alpha = 0.4) +
  
  # ---- B. 坐标轴虚线 ----
geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  
  # ---- C. 象限策略标签 (核心修改：指定 family) ----
# 右上：蓝海
annotate("text", x = 9.5, y = 9.5, label = "东方奢潮\n(稀缺蓝海)", 
         hjust = 1, vjust = 1, color = "#2e7d32", fontface = "bold", size = 9, lineheight = 1.2,
         family = "Microsoft YaHei") + # 【关键】指定字体
  
  # 左下：红海
  annotate("text", x = -9.5, y = -9.5, label = "西式快时尚\n(竞争红海)", 
           hjust = 0, vjust = 0, color = "#ef6c00", fontface = "bold", size = 9, lineheight = 1.2,
           family = "Microsoft YaHei") + # 【关键】指定字体
  
  # ---- D. 品牌 Logo 层 ----
geom_image(
  aes(image = logo_path),
  size = 0.13,   
  by = "width",
  asp = 1.25 
) +
  
  # ---- E. 品牌名称标签 (优化：增加半透明白色背景，解决重叠模糊) ----
geom_label(
  aes(label = Brand, color = Label_Color),
  vjust = -1.6,           # 将标签位置向上移动，拉开与 Logo 的距离
  fontface = "bold",
  size = 8,
  family = "Microsoft YaHei",
  fill = "white",         # 【关键】设置标签背景为白色
  label.size = 0,         # 【关键】将背景框的边框粗细设为0（去掉边框线）
  label.padding = unit(0.2, "lines"), # 设置文字与背景框的边距
  alpha = 0.8             # 设置背景半透明，既能看清字，又不会完全遮挡背景
) +
  
  # CLOT 的红色聚焦光环
  geom_point(data = filter(plot_df, Brand == "CLOT"),
             aes(x = X_Aesthetics, y = Y_Positioning),
             color = "red", size = 30, shape = 1, stroke = 2, alpha = 0.6) +
  
  # ---- F. 颜色映射 ----
scale_color_identity() +
  
  # ---- G. 坐标轴与主题 (核心修改：base_family) ----
scale_x_continuous(limits = c(-limit_range, limit_range), expand = c(0,0)) +
  scale_y_continuous(limits = c(-limit_range, limit_range), expand = c(0,0)) +
  
  labs(
    title = "CLOT的品牌市场定位图：东西方美学的比较",
    subtitle = "CLOT 凭借东方文化底蕴与高端定价，成功占据了独特的生态位",
    x = "← 西式美学 (Western)                            东方美学 (Eastern) →",
    y = "← 大众/快时尚 (Mass)                            高端/奢华 (Luxury) →",
    caption = "**数据分析：俗人 | 课程：Fashion Marketing**"
  ) +
  
  # 【关键】将主题的基础字体设为 Microsoft YaHei
  theme_minimal(base_family = "Microsoft YaHei") + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(), 
    
    # 标题字号极大化
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5, margin = margin(t=20, b = 15)),
    # 副标题字号加大
    plot.subtitle = element_text(size = 22, color = "gray30", hjust = 0.5, margin = margin(b = 30)),
    # 脚注字号加大
    plot.caption = element_markdown(size = 14, color = "gray50", hjust = 0.98, margin = margin(t=10, b=10)),
    
    # 坐标轴标题字号加大
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15, b = 10)),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15, l = 10)),
    axis.text = element_blank() 
  )

# ===============================
# 5. 保存图片
# ===============================
ggsave("CLOT_Market_Positioning_YaHei.png", p, width = 10, height = 8, dpi = 300)

# 自动打开预览
if (file.exists("CLOT_Market_Positioning_YaHei.png")) {
  browseURL("CLOT_Market_Positioning_YaHei.png")
}