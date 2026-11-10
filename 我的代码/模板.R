library(ggplot2)
library(dplyr)
library(ggimage)  
library(scales)   

# --- 2. 坐标轴与双极渐变背景计算 (核心样式，请直接复用) ---
# 动态计算边界，确保背景铺满
y_mid <- 0  # 视新数据分布调整中心点
x_mid <- 20 # 视新数据分布调整中心点
xmin <- min(df$x_var) - 2
xmax <- max(df$x_var) + 2
ymin <- min(df$y_var) - 1.5
ymax <- max(df$y_var) + 1.5

# 创建全图网格
bg_data <- expand.grid(
  x = seq(xmin, xmax, length.out = 200),
  y = seq(ymin, ymax, length.out = 200)
)

# 颜色定义 (马卡龙色系)
c_bl  <- col2rgb("#FCD2DC") # 左下
c_tr  <- col2rgb("#DCFBDB") # 右上
c_mid <- col2rgb("#F7F4D3") # 中间过渡

# 向量化计算渐变色
bg_data <- bg_data %>%
  mutate(
    x_norm = rescale(x, from = c(xmin, xmax)),
    y_norm = rescale(y, from = c(ymin, ymax)),
    diag_score = (x_norm + y_norm) / 2,
    w_bl = ifelse(diag_score < 0.5, 1 - (diag_score * 2), 0),
    w_tr = ifelse(diag_score >= 0.5, (diag_score - 0.5) * 2, 0),
    w_mid = 1 - w_bl - w_tr,
    r = w_bl * c_bl[1] + w_mid * c_mid[1] + w_tr * c_tr[1],
    g = w_bl * c_bl[2] + w_mid * c_mid[2] + w_tr * c_tr[2],
    b = w_bl * c_bl[3] + w_mid * c_mid[3] + w_tr * c_tr[3],
    fill_color = rgb(r, g, b, maxColorValue = 255)
  )

# --- 3. 绘图代码 (核心框架，请保持图层顺序和 theme 设置) ---
p <- ggplot(df, aes(x = x_var, y = y_var)) +
  
  # Layer 1: 背景渐变
  geom_tile(data = bg_data, aes(x = x, y = y, fill = fill_color), alpha = 0.6) +
  scale_fill_identity() +
  
  # Layer 2: 象限辅助虚线
  geom_vline(xintercept = x_mid, linetype = "twodash", color = "gray25", size = 0.5) +
  geom_hline(yintercept = y_mid, linetype = "twodash", color = "gray25", size = 0.5) +
  
  # Layer 3: 球队 Logo 散点
  geom_image(aes(image = logo_path), size = 0.15, by = "width") + 
  
  # Layer 4: 坐标轴留白控制
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  
  # Layer 5: 文本信息 (实际生成时由指令填充具体文字)
  labs(
    title = "此处替换为主标题",
    subtitle = "此处替换为副标题与说明",
    x = "此处替换为X轴说明",
    y = "此处替换为Y轴说明",
    caption = "制作：俗人罢了 | 数据源：待填"
  ) +
  
  # Layer 6: 全局排版主题 (严格保留)
  theme_minimal(base_family = "sans") + 
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 32, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 24, color = "gray30", hjust = 0),
    axis.title = element_text(face = "bold", size = 16), 
    axis.text = element_text(size = 15),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#0000001A"),
    plot.caption = element_text(size = 28, color = "gray15", hjust = 0.5),
    plot.background = element_rect(fill = "#ffffff", colour = NA)
  )