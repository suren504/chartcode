p <- ggplot(
  need_data,
  aes(x = 需求, y = 客群, fill = 强度)
) +
  
  # 背景参考网格（淡虚线）
  geom_vline(
    xintercept = seq_along(need_order),
    linewidth = 0.32,
    linetype = "dashed",
    color = "#D8CDBA",
    alpha = 0.55
  ) +
  
  geom_hline(
    yintercept = seq_along(customer_order),
    linewidth = 0.32,
    linetype = "dashed",
    color = "#D8CDBA",
    alpha = 0.55
  ) +
  
  # 热力图方块
  geom_tile(
    width = 0.86,
    height = 0.78,
    color = "#A48B63",
    linewidth = 0.55,
    alpha = 0.98
  ) +
  
  # 方块中的评分数字
  geom_text(
    aes(label = 强度),
    family = "Microsoft YaHei",
    fontface = "bold",
    size = 15,
    color = "#5F492C"
  ) +
  
  # 色阶
  scale_fill_gradientn(
    colors = c(
      "#F3EFE6",
      "#E9E0D0",
      "#DDD0B9",
      "#D0BEA0",
      "#C0AA84",
      "#AD9165"
    ),
    limits = c(1, 5),
    guide = "none"
  ) +
  
  scale_x_discrete(
    expand = expansion(add = c(0.18, 0.18))
  ) +
  
  scale_y_discrete(
    expand = expansion(add = c(0.12, 0.12))
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_family = "Microsoft YaHei") +
  
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      size = 25,
      face = "bold",
      color = "black",
      margin = margin(t = 10)
    ),
    
    axis.text.y = element_text(
      size = 25,
      face = "bold",
      color = "black",
      margin = margin(r = 10)
    ),
    
    axis.ticks = element_blank(),
    
    plot.margin = margin(
      t = 10,
      r = 14,
      b = 12,
      l = 18
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
# 4. 导出透明背景 PNG
# 比例接近 7:3
# ============================================================

ggsave(
  filename = file.path(out_dir, "客群核心需求热力图.png"),
  plot = p,
  width = 10,
  height = 4.3,
  units = "in",
  dpi = 600,
  bg = "transparent"
)