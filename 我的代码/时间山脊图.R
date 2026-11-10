suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(ggridges)
  library(ragg)
  library(scales)
})


# ============================================================
# 1. 路径
# ============================================================

data_file <- "D:/nba/微信公众号数据.xlsx"

out_file <- "D:/nba/微信公众号_发文时间山脊图.png"


# ============================================================
# 2. 三家公众号颜色
# ============================================================

platform_colors <- c(
  "第一财经" = "#0098DB",
  "新华社"   = "#003588",
  "新闻坊"   = "#005E9C"
)



# ============================================================
# 3. 读取数据
# ============================================================

wx_raw <- read_excel(
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
  )



# ============================================================
# 4. 时间解析函数
# 输出：
# 例如 07:30 → 7.5
# 23:45 → 23.75
# ============================================================

parse_time_decimal <- function(x){
  
  x <- str_trim(as.character(x))
  
  result <- rep(NA_real_, length(x))
  
  
  # ----------------------------
  # 标准时间格式
  # ----------------------------
  
  is_time_text <- !is.na(x) &
    str_detect(
      x,
      "^\\d{1,2}:\\d{2}(:\\d{2})?$"
    )
  
  
  normalized_time <- x[is_time_text]
  
  
  normalized_time <- if_else(
    str_count(normalized_time, ":") == 1,
    paste0(normalized_time, ":00"),
    normalized_time
  )
  
  
  parsed_time <- suppressWarnings(
    hms(normalized_time)
  )
  
  
  result[is_time_text] <-
    hour(parsed_time) +
    minute(parsed_time) / 60 +
    second(parsed_time) / 3600
  
  
  
  # ----------------------------
  # Excel 时间小数
  # ----------------------------
  
  is_excel_number <- !is.na(x) &
    is.na(result) &
    str_detect(
      x,
      "^\\d*\\.?\\d+$"
    )
  
  
  result[is_excel_number] <-
    as.numeric(x[is_excel_number]) * 24
  
  
  result
}




# ============================================================
# 5. 清洗
# ============================================================

wx_clean <- wx_raw %>%
  
  mutate(
    
    账号 = factor(
      账号,
      levels = c(
        "第一财经",
        "新华社",
        "新闻坊"
      )
    ),
    
    发布时间 = parse_time_decimal(时间)
    
  ) %>%
  
  filter(
    !is.na(账号),
    !is.na(发布时间)
  )





# ============================================================
# 6. 山脊密度图
# ============================================================

p_ridge <- ggplot(
  
  wx_clean,
  
  aes(
    x = 发布时间,
    y = 账号,
    fill = 账号,
    color = 账号
  )
  
) +
  
  
  geom_density_ridges(
    
    scale = 1.35,
    
    rel_min_height = 0.01,
    
    alpha = 0.30,
    
    linewidth = 0.9,
    
    bandwidth = 1.0
    
  ) +
  
  
  
  # 发布时间散点
  
  geom_point(
    
    aes(
      x = 发布时间,
      y = 账号
    ),
    
    position = position_jitter(
      height = 0.035,
      width = 0
    ),
    
    size = 1.5,
    
    alpha = 0.55
    
  ) +
  
  
  
  scale_fill_manual(
    
    values = platform_colors
    
  ) +
  
  
  scale_color_manual(
    
    values = platform_colors
    
  ) +
  
  
  
  # ===============================
# 完整一天 00-24
# ===============================

scale_x_continuous(
  
  limits = c(0,24),
  
  breaks = seq(
    0,
    24,
    by = 2
  ),
  
  labels = function(x){
    
    sprintf(
      "%02d:00",
      x
    )
    
  },
  
  expand = expansion(
    mult = c(
      0.01,
      0.01
    )
  )
  
) +
  
  
  
  labs(
    
    x = NULL,
    
    y = NULL
    
  ) +
  
  
  
  guides(
    
    fill = "none",
    
    color = "none"
    
  ) +
  
  
  
  theme_minimal(
    
    base_family = "Microsoft YaHei",
    
    base_size = 13
    
  ) +
  
  
  
  theme(
    
    # --------------------------
    # 删除标题
    # --------------------------
    
    plot.title = element_blank(),
    
    plot.subtitle = element_blank(),
    
    plot.caption = element_blank(),
    
    
    
    # --------------------------
    # 坐标文字
    # --------------------------
    
    axis.text.x = element_text(
      
      size = 11,
      
      color = "#555555"
      
    ),
    
    
    axis.text.y = element_text(
      
      size = 14,
      
      face = "bold",
      
      color = "#333333"
      
    ),
    
    
    
    # --------------------------
    # 网格
    # --------------------------
    
    panel.grid.major.x = element_line(
      
      color = "#D5DCE5",
      
      linewidth = 0.5,
      
      linetype = "dashed"
      
    ),
    
    
    panel.grid.major.y = element_blank(),
    
    
    panel.grid.minor = element_blank(),
    
    
    
    # --------------------------
    # 透明背景
    # --------------------------
    
    panel.background = element_rect(
      
      fill = "transparent",
      
      color = NA
      
    ),
    
    
    plot.background = element_rect(
      
      fill = "transparent",
      
      color = NA
      
    ),
    
    
    
    plot.margin = margin(
      
      t = 12,
      
      r = 25,
      
      b = 15,
      
      l = 25
      
    )
    
  )





# ============================================================
# 7. 输出 PNG
# ============================================================

ggsave(
  
  filename = out_file,
  
  plot = p_ridge,
  
  
  width = 14,
  
  height = 3.8,
  
  units = "in",
  
  
  dpi = 450,
  
  
  bg = "transparent",
  
  
  device = ragg::agg_png
  
)



print(p_ridge)




# ============================================================
# 8. 输出高峰时间段统计
# ============================================================

time_summary <- wx_clean %>%
  
  mutate(
    
    小时 = floor(发布时间),
    
    时段 = case_when(
      
      小时 >= 0 &
        小时 < 6 ~ "凌晨",
      
      小时 >= 6 &
        小时 < 9 ~ "早间",
      
      小时 >= 9 &
        小时 < 12 ~ "上午",
      
      小时 >= 12 &
        小时 < 15 ~ "午后",
      
      小时 >= 15 &
        小时 < 18 ~ "下午",
      
      小时 >= 18 &
        小时 < 21 ~ "晚间",
      
      小时 >= 21 &
        小时 < 24 ~ "夜间"
      
    )
    
  ) %>%
  
  count(
    账号,
    时段,
    name = "发文数量"
  ) %>%
  
  group_by(
    账号
  ) %>%
  
  mutate(
    
    占比 = 发文数量 /
      sum(发文数量)
    
  ) %>%
  
  arrange(
    账号,
    desc(发文数量)
  )


print(time_summary)