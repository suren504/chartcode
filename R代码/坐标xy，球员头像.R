# 1. 加载包
library(reticulate) 
library(ggplot2)
library(dplyr)
library(ggtext)
library(ggimage)
library(stringr)
library(scales) 
library(showtext) 

# 2. 字体配置
showtext_auto() 
font_add("MYH", "msyh.ttc") 

# 3. 读取与清洗数据
raw_df <- py$result

df <- raw_df %>%
  mutate(
    x = TwoPtShootingFoulsDrawnPct / 100,      
    y = ThreePtShootingFoulsDrawnPct / 100
  ) %>%
  mutate(
    clean_name_str = Name %>%
      str_replace_all("č", "c") %>%      
      str_replace_all("ć", "c") %>% 
      str_replace_all("ū", "u") %>%    
      str_replace_all("š", "s") %>%
      str_replace_all("ö", "o") %>%  
      str_replace_all("(?i)Jr\\b\\.", "Jr") %>%       
      str_replace_all(" ", "_"),
    
    local_path = paste0("D:/nba/球员头像/", clean_name_str, ".png"),
    has_local = file.exists(local_path),
    headshot = ifelse(has_local, local_path, "https://cdn.nba.com/headshots/nba/latest/260x190/placeholder.png")
  )

# 4. 绘图范围设定
x_mid <- median(df$x, na.rm = TRUE)          
y_mid <- median(df$y, na.rm = TRUE)   

view_xmin <- min(df$x, na.rm = TRUE) - 0.01 
view_xmax <- max(df$x, na.rm = TRUE) + 0.015 
view_ymin <- min(df$y, na.rm = TRUE) - 0.005
view_ymax <- max(df$y, na.rm = TRUE) + 0.005

bg_xmin <- view_xmin - 0.02
bg_xmax <- view_xmax + 0.02
bg_ymin <- view_ymin - 0.02
bg_ymax <- view_ymax + 0.02

# 5. 准备高级 RGB 渐变背景
bg_data <- expand.grid(
  x_bg = seq(bg_xmin, bg_xmax, length.out = 200),
  y_bg = seq(bg_ymin, bg_ymax, length.out = 200)
)

c_bl  <- col2rgb("#F3C8F5") 
c_tr  <- col2rgb("#C2FDD1") 
c_mid <- col2rgb("floralwhite") 

bg_data <- bg_data %>%
  mutate(
    x_norm = rescale(x_bg, from = c(view_xmin, view_xmax)),
    y_norm = rescale(y_bg, from = c(view_ymin, view_ymax)),
    diag_score = (x_norm + y_norm) / 2,
    w_bl = ifelse(diag_score < 0.5, 1 - (diag_score * 2), 0),
    w_tr = ifelse(diag_score >= 0.5, (diag_score - 0.5) * 2, 0),
    w_mid = 1 - w_bl - w_tr,
    r = pmin(pmax(w_bl * c_bl[1] + w_mid * c_mid[1] + w_tr * c_tr[1], 0), 255),
    g = pmin(pmax(w_bl * c_bl[2] + w_mid * c_mid[2] + w_tr * c_tr[2], 0), 255),
    b = pmin(pmax(w_bl * c_bl[3] + w_mid * c_mid[3] + w_tr * c_tr[3], 0), 255),
    fill_color = rgb(r, g, b, maxColorValue = 255)
  )

# 6. 正式绘图 (一气呵成版)
p <- ggplot() + 
  geom_tile(data = bg_data, aes(x = x_bg, y = y_bg, fill = fill_color), alpha = 0.8) +
  scale_fill_identity() + 
  geom_vline(xintercept = x_mid, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  geom_hline(yintercept = y_mid, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  geom_image(
    data = df, 
    aes(x = x, y = y, image = headshot, size = I(FTA / max(FTA) * 0.08 + 0.04)), 
    alpha = 0.9      
  ) +
  annotate("richtext", x = view_xmax, y = view_ymax, hjust = 1, vjust = 1, 
           label.margin = unit(c(8, 8, 8, 8), "pt"), 
           label = "<b style='font-size:22pt; color:#1b5e20'>造犯规真神</b><br><span style='font-size:13pt; color:#333'>内外线通吃</span><br><span style='font-size:13pt; color:#333'>罚球机器</span>",
           fill = alpha("#C2FDD1", 0.85), 
           label.color = "#69F0AE", 
           label.padding = unit(0.6, "lines"), 
           label.r = unit(0.4, "lines"),
           family = "MYH") +
  annotate("richtext", x = view_xmin, y = view_ymax, hjust = 0, vjust = 1, 
           label.margin = unit(c(8, 8, 8, 8), "pt"),
           label = "<b style='font-size:22pt; color:#E65100'>专造三分罚球</b><br><span style='font-size:13pt; color:#333'>两分造犯规少</span><br><span style='font-size:13pt; color:#333'>但擅长三分造犯规</span>",
           fill = alpha("#FFEBDB", 0.85), 
           label.color = "#FFCC80", 
           label.padding = unit(0.6, "lines"),
           label.r = unit(0.4, "lines"),
           family = "MYH") +
  annotate("richtext", x = view_xmin, y = view_ymin, hjust = 0, vjust = 0, 
           label.margin = unit(c(8, 8, 8, 8), "pt"),
           label = "<span style='font-size:13pt; color:#333'>几乎造不到犯规</span><br><span style='font-size:13pt; color:#333'>得分全靠运动战进球</span>",
           fill = alpha("#F3C8F5", 0.85), 
           label.color = "#CE93D8", 
           label.padding = unit(0.6, "lines"),
           label.r = unit(0.4, "lines"),
           family = "MYH") +
  annotate("richtext", x = view_xmax, y = view_ymin, hjust = 1, vjust = 0, 
           label.margin = unit(c(8, 8, 8, 8), "pt"),
           label = "<b style='font-size:22pt; color:#BF360C'>禁区推土机</b><br><span style='font-size:13pt; color:#333'>三分线外造不到</span><br><span style='font-size:13pt; color:#333'>疯狂冲击篮筐造杀伤</span>",
           fill = alpha("#FFEBDB", 0.85), 
           label.color = "#FFCC80", 
           label.padding = unit(0.6, "lines"),
           label.r = unit(0.4, "lines"),
           family = "MYH") +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = c(0,0),
    labels = scales::percent
  ) + 
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6), 
    expand = c(0,0),
    labels = scales::percent
  ) +
  coord_cartesian(
    xlim = c(view_xmin, view_xmax), 
    ylim = c(view_ymin, view_ymax),
    expand = FALSE,
    clip = "off" 
  ) +
    labs(
      title = "NBA2025-26赛季 球员造罚球能力 两罚vs三罚对比图",
      subtitle = "X轴=两分造犯规率 Y轴=三分造犯规率 | <span style='color:black;'>头像越大代表总罚球出手数(FTA)越多</span> | 筛选标准：罚球出手 > 100，本赛季有造到过三分犯规",
      x = "两分投篮造犯规率\n← 越靠左代表进了三分线就越没哨 ----------------------------------- 越靠右代表造两罚能力强 →",
      y = "三分投篮造犯规率 越靠上代表在三分线外罚球越多----->>>",
      caption = "日期：2026.2.20| 制作：<span style='font-family:MYH; font-size:14pt; color:black;'>**俗人罢了**</span> | 数据源：PBPStats"
    ) +
  theme_minimal() + 
  theme(
    text = element_text(family = "MYH"), 
    plot.title.position = "plot",
    plot.caption.position = "plot", 
    
    plot.title = element_text(size = 24, face = "bold", hjust = 0, family = "MYH"),
    
    # 【核心修改】把 element_text 改成 element_markdown，这样才能解析 HTML 标签
    plot.subtitle = element_markdown(size = 15, color = "gray50", hjust = 0, family = "MYH", margin = margin(b = 15)),
    
    axis.title = element_text(face = "bold", size = 12, family = "MYH"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 10, family = "MYH"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#00000015"),
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15, unit = "pt"),
    plot.caption = element_markdown(size = 10, color = "gray50", hjust = 0.5, family = "MYH", lineheight = 1.2, margin = margin(t = 10))
  )
# 保存图片
ggsave("NBA_造犯规排行榜_破框溢出版.png", p, width = 14, height = 11, dpi = 300, bg = "floralwhite")