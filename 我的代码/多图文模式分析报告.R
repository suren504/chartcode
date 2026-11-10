suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(patchwork)
  library(ragg)
  library(scales)
})


# ============================================================
# 1. 路径
# ============================================================

data_file <- "D:/nba/微信公众号数据.xlsx"

out_file <- "D:/nba/第一财经与新闻坊_头条次条阅读差异.png"


# ============================================================
# 2. 颜色
# ============================================================

account_colors <- c(
  "第一财经" = "#0098DB",
  "新闻坊"   = "#005E9C"
)

position_colors <- c(
  "单条"       = "#0098DB",
  "多图文整体" = "#66BCE3",
  "头条"       = "#003588",
  "次条"       = "#ADB9CA"
)

summary_color <- "#003588"


# ============================================================
# 3. 阅读量转换函数
#
# 1.5w  → 15000
# 1.5万 → 15000
# 10w+  → 100000
# ============================================================

parse_read_count <- function(x) {
  
  x_clean <- x %>%
    as.character() %>%
    str_squish() %>%
    str_to_lower() %>%
    str_replace_all(",", "")
  
  number_part <- suppressWarnings(
    parse_number(x_clean)
  )
  
  if_else(
    str_detect(x_clean, "w|万"),
    number_part * 10000,
    number_part
  )
}


# ============================================================
# 4. 读取全部数据
# ============================================================

wx_all <- read_excel(
  path = data_file,
  sheet = 1,
  col_types = "text"
) %>%
  
  rename_with(str_trim) %>%
  
  mutate(
    across(
      everything(),
      ~ na_if(str_squish(as.character(.x)), "")
    )
  ) %>%
  
  mutate(
    
    # 保留Excel原始顺序
    原始顺序 = row_number(),
    
    阅读量_num = parse_read_count(阅读量),
    
    内容位置 = case_when(
      
      头次条 == "单条" ~ "单条",
      
      头次条 %in% c(
        "头条",
        "主条"
      ) ~ "头条",
      
      头次条 %in% c(
        "次条",
        "副条"
      ) ~ "次条",
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(
    !is.na(账号),
    !is.na(时间),
    !is.na(阅读量_num),
    !is.na(内容位置)
  )


# ============================================================
# 5. 左图数据
#
# 绝对阅读量只统计第一财经：
# 单条、多图文整体、头条、次条
# ============================================================

wx_fcj <- wx_all %>%
  filter(
    账号 == "第一财经"
  )


position_summary <- bind_rows(
  
  wx_fcj %>%
    filter(
      内容位置 == "单条"
    ) %>%
    summarise(
      平均阅读量 = mean(
        阅读量_num,
        na.rm = TRUE
      ) / 10000,
      样本量 = n()
    ) %>%
    mutate(
      类别 = "单条"
    ),
  
  
  wx_fcj %>%
    filter(
      内容位置 %in% c(
        "头条",
        "次条"
      )
    ) %>%
    summarise(
      平均阅读量 = mean(
        阅读量_num,
        na.rm = TRUE
      ) / 10000,
      样本量 = n()
    ) %>%
    mutate(
      类别 = "多图文整体"
    ),
  
  
  wx_fcj %>%
    filter(
      内容位置 == "头条"
    ) %>%
    summarise(
      平均阅读量 = mean(
        阅读量_num,
        na.rm = TRUE
      ) / 10000,
      样本量 = n()
    ) %>%
    mutate(
      类别 = "头条"
    ),
  
  
  wx_fcj %>%
    filter(
      内容位置 == "次条"
    ) %>%
    summarise(
      平均阅读量 = mean(
        阅读量_num,
        na.rm = TRUE
      ) / 10000,
      样本量 = n()
    ) %>%
    mutate(
      类别 = "次条"
    )
  
) %>%
  
  mutate(
    类别 = factor(
      类别,
      levels = c(
        "单条",
        "多图文整体",
        "头条",
        "次条"
      )
    )
  )


# ============================================================
# 6. 右图数据
#
# 合并第一财经和新闻坊
# 相同“账号＋发布时间”视为同一批多图文
# 每批必须同时包含头条和次条
# ============================================================

batch_data <- wx_all %>%
  
  filter(
    账号 %in% c(
      "第一财经",
      "新闻坊"
    ),
    内容位置 %in% c(
      "头条",
      "次条"
    )
  ) %>%
  
  group_by(
    账号,
    时间
  ) %>%
  
  filter(
    any(内容位置 == "头条"),
    any(内容位置 == "次条")
  ) %>%
  
  arrange(
    原始顺序,
    .by_group = TRUE
  ) %>%
  
  mutate(
    
    # 每个账号、每个发布时间构成独立批次
    批次ID = paste(
      账号,
      时间,
      sep = "_"
    ),
    
    # 获取该批多图文的头条阅读量
    头条阅读量 = first(
      阅读量_num[内容位置 == "头条"]
    ),
    
    # 按Excel原始顺序确定次条1、次条2
    次条序号 = cumsum(
      内容位置 == "次条"
    ),
    
    批次位置 = case_when(
      
      内容位置 == "头条" ~ "头条",
      
      次条序号 == 1 ~ "次条1",
      
      次条序号 >= 2 ~ "次条2"
    ),
    
    # 头条设为100%
    相对头条占比 =
      阅读量_num /
      头条阅读量 *
      100
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    账号 = factor(
      账号,
      levels = c(
        "第一财经",
        "新闻坊"
      )
    ),
    
    批次位置 = factor(
      批次位置,
      levels = c(
        "头条",
        "次条1",
        "次条2"
      )
    )
  )


if (nrow(batch_data) == 0) {
  stop("没有找到同时包含头条和次条的多图文批次。")
}


# ============================================================
# 7. 合并两个账号后的典型阅读保留率
#
# 使用中位数：
# 避免个别爆款次条或异常批次影响整体结果
# ============================================================

batch_summary <- batch_data %>%
  
  group_by(
    批次位置
  ) %>%
  
  summarise(
    
    典型占比 = median(
      相对头条占比,
      na.rm = TRUE
    ),
    
    平均占比 = mean(
      相对头条占比,
      na.rm = TRUE
    ),
    
    波动程度 = sd(
      相对头条占比,
      na.rm = TRUE
    ),
    
    样本量 = n(),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    标签 = case_when(
      
      批次位置 == "头条" ~
        paste0(
          "基准100%\n",
          "n=",
          样本量
        ),
      
      典型占比 < 100 ~
        paste0(
          round(典型占比),
          "%\n下降",
          round(100 - 典型占比),
          "%"
        ),
      
      TRUE ~
        paste0(
          round(典型占比),
          "%\n上升",
          round(典型占比 - 100),
          "%"
        )
    )
  )


# ============================================================
# 8. 分账号辅助统计
#
# 只输出至Console，便于检查两个账号是否存在明显差异
# ============================================================

account_batch_summary <- batch_data %>%
  
  group_by(
    账号,
    批次位置
  ) %>%
  
  summarise(
    
    典型占比 = median(
      相对头条占比,
      na.rm = TRUE
    ),
    
    平均占比 = mean(
      相对头条占比,
      na.rm = TRUE
    ),
    
    样本量 = n(),
    
    .groups = "drop"
  )


# ============================================================
# 9. 统一主题
# ============================================================

theme_report <- theme_minimal(
  base_family = "Microsoft YaHei",
  base_size = 12
) +
  
  theme(
    
    plot.title = element_text(
      size = 14,
      face = "bold",
      color = "#333333",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    
    axis.title = element_blank(),
    
    axis.text.x = element_text(
      size = 11,
      color = "#4A4A4A",
      margin = margin(t = 7)
    ),
    
    axis.text.y = element_text(
      size = 10.5,
      color = "#555555"
    ),
    
    axis.ticks = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "#DCE2E8",
      linewidth = 0.45
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    
    legend.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    
    legend.key = element_rect(
      fill = "transparent",
      color = NA
    ),
    
    plot.margin = margin(
      t = 18,
      r = 18,
      b = 12,
      l = 12
    )
  )


# ============================================================
# 10. 左图
#
# 第一财经不同推送位置的平均阅读量
# ============================================================

p_position <- ggplot(
  position_summary,
  aes(
    x = 类别,
    y = 平均阅读量,
    fill = 类别
  )
) +
  
  geom_col(
    width = 0.67,
    alpha = 0.92
  ) +
  
  geom_text(
    aes(
      label = paste0(
        round(平均阅读量, 1),
        "万\n",
        "n=",
        样本量
      )
    ),
    vjust = -0.45,
    size = 4.8,
    lineheight = 1.15,
    fontface = "bold",
    color = "#333333"
  ) +
  
  scale_fill_manual(
    values = position_colors
  ) +
  
  scale_y_continuous(
    labels = function(x) {
      paste0(
        number(
          x,
          accuracy = 0.1
        ),
        "万"
      )
    },
    expand = expansion(
      mult = c(
        0,
        0.28
      )
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    title = "第一财经不同推送位置的平均阅读量"
  ) +
  
  theme_report +
  
  theme(
    legend.position = "none"
  )


# ============================================================
# 11. 右图
#
# 第一财经＋新闻坊
# 同批多图文中的阅读衰减
# ============================================================

ratio_upper <- max(
  125,
  ceiling(
    max(
      batch_data$相对头条占比,
      na.rm = TRUE
    ) / 25
  ) * 25
)


p_batch <- ggplot() +
  
  geom_hline(
    yintercept = 100,
    color = "#8A949F",
    linewidth = 0.65,
    linetype = "dashed"
  ) +
  
  # 每一批多图文的实际阅读变化
  # 不同账号用不同颜色区分
  geom_line(
    data = batch_data,
    aes(
      x = 批次位置,
      y = 相对头条占比,
      group = 批次ID,
      color = 账号
    ),
    linewidth = 0.55,
    alpha = 0.25
  ) +
  
  geom_point(
    data = batch_data,
    aes(
      x = 批次位置,
      y = 相对头条占比,
      color = 账号
    ),
    size = 1.8,
    alpha = 0.38
  ) +
  
  # 两个账号合并后的典型水平
  geom_line(
    data = batch_summary,
    aes(
      x = 批次位置,
      y = 典型占比,
      group = 1
    ),
    color = summary_color,
    linewidth = 1.8
  ) +
  
  geom_point(
    data = batch_summary,
    aes(
      x = 批次位置,
      y = 典型占比
    ),
    color = summary_color,
    size = 4.5
  ) +
  
  geom_text(
    data = batch_summary,
    aes(
      x = 批次位置,
      y = 典型占比,
      label = 标签
    ),
    vjust = -0.85,
    size = 4.5,
    lineheight = 1.05,
    fontface = "bold",
    color = "#333333"
  ) +
  
  scale_color_manual(
    values = account_colors,
    name = NULL
  ) +
  
  scale_y_continuous(
    limits = c(
      0,
      ratio_upper
    ),
    breaks = seq(
      0,
      ratio_upper,
      by = 25
    ),
    labels = function(x) {
      paste0(
        x,
        "%"
      )
    },
    expand = expansion(
      mult = c(
        0,
        0.07
      )
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    title = "两账号同批多图文中的阅读衰减"
  ) +
  
  theme_report +
  
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(
      size = 10.5,
      color = "#4A4A4A"
    )
  )


# ============================================================
# 12. 横向拼接
# ============================================================

final_plot <- p_position + p_batch +
  
  plot_layout(
    nrow = 1,
    widths = c(
      1,
      1.45
    )
  )


# ============================================================
# 13. 输出透明PNG
#
# 不运行print(final_plot)，不会显示在RStudio右下角
# ============================================================

ggsave(
  filename = out_file,
  plot = final_plot,
  width = 14,
  height = 4.2,
  units = "in",
  dpi = 450,
  bg = "transparent",
  device = ragg::agg_png
)


# ============================================================
# 14. Console输出关键结果
# ============================================================

cat("\n第一财经不同推送位置的平均阅读量：\n")
print(position_summary)

cat("\n两账号合并后的阅读保留率：\n")
print(batch_summary)

cat("\n分账号阅读保留率：\n")
print(account_batch_summary)