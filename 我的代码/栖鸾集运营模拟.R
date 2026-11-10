suppressPackageStartupMessages({
  library(tidyverse)
})

# ============================================================
# 0. 输出目录
# ============================================================

out_dir <- "D:/nba"
dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# 使用时间戳文件名，避免旧图片正在被PPT或图片查看器占用
output_time <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

png_path <- file.path(
  out_dir,
  paste0(
    "重点客群活动时段_最终版_",
    output_time,
    ".png"
  )
)

# ============================================================
# 1. 创建时间序列
# ============================================================

hours <- seq(
  from = 6,
  to = 22,
  by = 0.1
)

# 高斯峰函数：
# center 控制高峰时间
# width 控制持续时间
# height 控制相对强度
peak <- function(
    x,
    center,
    width,
    height = 1
) {
  height * exp(
    -((x - center)^2) /
      (2 * width^2)
  )
}

# ============================================================
# 2. 构建五类客群活动强度
# 说明：这是运营情景模拟，并非实测客流数据
# ============================================================

activity_data <- bind_rows(
  
  # 当地居民：
  # 早晨散步、中午日常休闲、傍晚及晚间活动
  tibble(
    客群 = "当地居民",
    时间 = hours,
    原始强度 =
      peak(
        hours,
        center = 7.5,
        width = 0.90,
        height = 0.65
      ) +
      peak(
        hours,
        center = 12.0,
        width = 1.20,
        height = 0.35
      ) +
      peak(
        hours,
        center = 18.5,
        width = 1.55,
        height = 1.00
      )
  ),
  
  # 亲子家庭：
  # 上午和下午为主要活动时段
  tibble(
    客群 = "亲子家庭",
    时间 = hours,
    原始强度 =
      peak(
        hours,
        center = 10.0,
        width = 1.25,
        height = 1.00
      ) +
      peak(
        hours,
        center = 15.5,
        width = 1.30,
        height = 0.85
      )
  ),
  
  # 青年社交：
  # 午后开始增加，傍晚达到高峰
  tibble(
    客群 = "青年社交",
    时间 = hours,
    原始强度 =
      peak(
        hours,
        center = 12.5,
        width = 1.50,
        height = 0.50
      ) +
      peak(
        hours,
        center = 17.5,
        width = 2.00,
        height = 1.00
      )
  ),
  
  # 企业及社群：
  # 上午与下午分别形成活动高峰
  tibble(
    客群 = "企业及社群",
    时间 = hours,
    原始强度 =
      peak(
        hours,
        center = 10.5,
        width = 1.10,
        height = 0.80
      ) +
      peak(
        hours,
        center = 15.5,
        width = 1.40,
        height = 1.00
      )
  ),
  
  # 周末微度假：
  # 从上午延续到夜间，午后及傍晚最活跃
  tibble(
    客群 = "周末微度假",
    时间 = hours,
    原始强度 =
      peak(
        hours,
        center = 11.5,
        width = 2.10,
        height = 0.55
      ) +
      peak(
        hours,
        center = 16.5,
        width = 2.60,
        height = 1.00
      ) +
      peak(
        hours,
        center = 19.0,
        width = 1.20,
        height = 0.45
      )
  )
) |>
  group_by(客群) |>
  mutate(
    相对活跃度 =
      原始强度 /
      max(原始强度) *
      100
  ) |>
  ungroup()

# ============================================================
# 3. 设置客群顺序和纵向位置
# ============================================================

group_order <- c(
  "当地居民",
  "亲子家庭",
  "青年社交",
  "企业及社群",
  "周末微度假"
)

activity_data <- activity_data |>
  mutate(
    客群 = factor(
      客群,
      levels = group_order
    ),
    
    基准线 = case_when(
      客群 == "当地居民"   ~ 5,
      客群 == "亲子家庭"   ~ 4,
      客群 == "青年社交"   ~ 3,
      客群 == "企业及社群" ~ 2,
      客群 == "周末微度假" ~ 1
    ),
    
    波幅 =
      相对活跃度 /
      100 *
      0.34,
    
    上边界 =
      基准线 +
      波幅,
    
    下边界 =
      基准线 -
      波幅
  )

# 水平参考线数据
horizontal_lines <- tibble(
  y = 1:5,
  x = 6,
  xend = 22
)

# ============================================================
# 4. 配色
# ============================================================

group_colors <- c(
  "当地居民"   = "#EEE8DA",
  "亲子家庭"   = "#E5D9C5",
  "青年社交"   = "#D7C7AD",
  "企业及社群" = "#C8B89A",
  "周末微度假" = "#B5A17D"
)

text_color <- "black"
grid_color <- "#CFC5B3"
outline_color <- "#A48B63"

# ============================================================
# 5. 绘图
# ============================================================

p <- ggplot(
  activity_data,
  aes(x = 时间)
) +
  
  # 每小时垂直虚线
  geom_vline(
    xintercept = seq(
      6,
      22,
      by = 1
    ),
    linewidth = 0.34,
    linetype = "dashed",
    color = grid_color,
    alpha = 0.72
  ) +
  
  # 水平虚线只从6:00开始，不穿过左侧客群标签
  geom_segment(
    data = horizontal_lines,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = y
    ),
    inherit.aes = FALSE,
    linewidth = 0.34,
    linetype = "dashed",
    color = grid_color,
    alpha = 0.78
  ) +
  
  # 对称活动强度带
  geom_ribbon(
    aes(
      ymin = 下边界,
      ymax = 上边界,
      fill = 客群,
      group = 客群
    ),
    alpha = 0.96,
    color = NA
  ) +
  
  # 上侧轮廓线
  geom_line(
    aes(
      y = 上边界,
      group = 客群
    ),
    linewidth = 0.52,
    color = outline_color,
    alpha = 0.88
  ) +
  
  # 下侧轮廓线
  geom_line(
    aes(
      y = 下边界,
      group = 客群
    ),
    linewidth = 0.52,
    color = outline_color,
    alpha = 0.88
  ) +
  
  scale_fill_manual(
    values = group_colors
  ) +
  
  # 时间轴
  scale_x_continuous(
    limits = c(6, 22),
    breaks = seq(
      6,
      22,
      by = 2
    ),
    labels = function(x) {
      paste0(x, ":00")
    },
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  
  # 使用正式Y轴标签，避免手动geom_text被裁剪
  scale_y_continuous(
    limits = c(0.45, 5.55),
    breaks = c(
      5,
      4,
      3,
      2,
      1
    ),
    labels = group_order,
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    caption = paste0(
      "运营情景模拟｜",
      "曲线表示各类客群在不同时段的相对活跃程度"
    )
  ) +
  
  theme_minimal(
    base_family = "Microsoft YaHei"
  ) +
  
  theme(
    legend.position = "none",
    
    panel.grid = element_blank(),
    
    # 横轴时间标签
    axis.text.x = element_text(
      size = 25,
      face = "bold",
      color = text_color,
      margin = margin(
        t = 10
      )
    ),
    
    # 左侧客群标签
    axis.text.y = element_text(
      size = 25,
      face = "bold",
      color = text_color,
      hjust = 1,
      margin = margin(
        r = 14
      )
    ),
    
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
    # 注释放在右下角
    plot.caption = element_text(
      size = 9.5,
      face = "plain",
      color = "black",
      hjust = 1,
      margin = margin(
        t = 12
      )
    ),
    
    plot.caption.position = "plot",
    
    # 为左侧标签保留充足空间
    plot.margin = margin(
      t = 20,
      r = 24,
      b = 20,
      l = 40
    ),
    
    plot.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "transparent",
      color = NA
    )
  )



# ============================================================
# 6. 导出透明背景PNG
# 16 × 6.4英寸，比例约为5:2
# 300dpi已经足够用于PPT，像素为4800 × 1920
# ============================================================

ggsave(
  filename = png_path,
  plot = p,
  width = 16,
  height = 6.4,
  units = "in",
  dpi = 300,
  bg = "transparent"
)