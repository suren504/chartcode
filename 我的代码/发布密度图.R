suppressPackageStartupMessages({
  
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(ragg)
  
})


# ============================================================
# 1. 路径
# ============================================================

data_file <- "D:/nba/微信公众号数据.xlsx"

out_file <- "D:/nba/第一财经_30分钟发布密度与阅读量.png"



# ============================================================
# 2. 颜色
# ============================================================

color_fcj <- "#0098DB"



# ============================================================
# 3. 读取数据
# ============================================================

wx <- read_excel(
  
  data_file,
  
  sheet = 1,
  
  col_types = "text"
  
) %>%
  
  rename_with(str_trim) %>%
  
  filter(
    
    账号 == "第一财经"
    
  )



# ============================================================
# 4. 数据清洗
# ============================================================

wx <- wx %>%
  
  mutate(
    
    时间_num =
      hour(hms(时间)) +
      minute(hms(时间)) / 60,
    
    
    阅读量_num =
      parse_number(
        
        str_replace_all(
          
          str_to_lower(阅读量),
          
          ",",
          
          ""
          
        )
        
      ),
    
    
    阅读量_num =
      if_else(
        
        str_detect(
          
          str_to_lower(阅读量),
          
          "w|万"
          
        ),
        
        阅读量_num * 10000,
        
        阅读量_num
        
      )
    
  ) %>%
  
  filter(
    
    !is.na(时间_num),
    
    !is.na(阅读量_num)
    
  ) %>%
  
  arrange(
    
    时间_num
    
  )



# ============================================================
# 5. 计算：
# 当前文章发布前30分钟已有多少文章
#
# 30分钟 = 0.5小时
# ============================================================

times <- wx$时间_num


wx <- wx %>%
  
  mutate(
    
    前30分钟已有文章数 = map_int(
      
      时间_num,
      
      function(t){
        
        sum(
          
          times < t &
            
            times >= t - 0.5
          
        )
        
      }
      
    )
    
  )



summary_data <- wx %>%
  
  mutate(
    
    发布密度 = case_when(
      
      前30分钟已有文章数 == 0 ~ "0篇",
      
      前30分钟已有文章数 == 1 ~ "1篇",
      
      前30分钟已有文章数 == 2 ~ "2篇",
      
      前30分钟已有文章数 == 3 ~ "3篇",
      
      前30分钟已有文章数 == 4 ~ "4篇",
      
      前30分钟已有文章数 >= 5 ~ "5篇及以上"
      
    )
    
  ) %>%
  
  group_by(
    
    发布密度
    
  ) %>%
  
  summarise(
    
    阅读量 = median(
      阅读量_num,
      na.rm = TRUE
    ) / 10000,
    
    样本量 = n(),
    
    .groups = "drop"
    
  ) %>%
  
  mutate(
    
    发布密度 = factor(
      
      发布密度,
      
      levels = c(
        "0篇",
        "1篇",
        "2篇",
        "3篇",
        "4篇",
        "5篇及以上"
      )
      
    )
    
  )

# ============================================================
# 7. 绘图
# ============================================================

p <- ggplot(
  
  summary_data,
  
  aes(
    
    x = 发布密度,
    
    y = 阅读量
    
  )
  
) +
  
  
  geom_col(
    
    fill = color_fcj,
    
    width = 0.6,
    
    alpha = 0.85
    
  ) +
  
  
  geom_text(
    
    aes(
      
      label = paste0(
        
        round(阅读量, 1),
        
        "万\n",
        
        "n=",
        
        样本量
        
      )
      
    ),
    
    vjust = -0.45,
    
    size = 6,
    
    fontface = "bold",
    
    color = "#333333"
    
  ) +
  
  
  scale_y_continuous(
    
    labels = function(x){
      
      paste0(
        
        x,
        
        "万"
        
      )
      
    },
    
    
    expand = expansion(
      
      mult = c(
        
        0,
        
        0.35
        
      )
      
    )
    
  ) +
  
  
  coord_cartesian(
    
    clip = "off"
    
  ) +
  
  
  labs(
    
    x = NULL,
    
    y = NULL
    
  ) +
  
  
  theme_minimal(
    
    base_family = "Microsoft YaHei"
    
  ) +
  
  
  theme(
    
    plot.margin = margin(
      
      20,
      
      20,
      
      15,
      
      20
      
    ),
    
    
    axis.text = element_text(
      
      size = 14,
      
      color = "#555555"
      
    ),
    
    
    panel.grid.major.x = element_blank(),
    
    
    panel.grid.minor = element_blank(),
    
    
    panel.background = element_rect(
      
      fill = "transparent",
      
      color = NA
      
    ),
    
    
    plot.background = element_rect(
      
      fill = "transparent",
      
      color = NA
      
    )
    
  )



# ============================================================
# 8. 输出
# 不print，因此不会显示RStudio Plot
# ============================================================

ggsave(
  
  filename = out_file,
  
  plot = p,
  
  width = 8,
  
  height = 4,
  
  units = "in",
  
  dpi = 450,
  
  bg = "transparent",
  
  device = ragg::agg_png
  
)