suppressPackageStartupMessages({
  
  library(tidyverse)
  library(readxl)
  library(patchwork)
  library(ragg)
  
})


# ============================================================
# 1. 路径
# ============================================================

data_file <- "D:/nba/微信公众号数据.xlsx"

out_file <- "D:/nba/微信公众号_单条多图文占比.png"



# ============================================================
# 2. 颜色
# ============================================================

type_colors <- c(
  
  "单条" = "#ADB9CA",
  
  "多图文" = "#0098DB"
  
)



platform_colors <- c(
  
  "第一财经"="#0098DB",
  
  "新华社"="#003588",
  
  "新闻坊"="#005E9C"
  
)



# ============================================================
# 3. 读取
# ============================================================

wx_raw <- read_excel(
  
  data_file,
  
  sheet = 1,
  
  col_types = "text"
  
) %>%
  
  rename_with(str_trim)



# ============================================================
# 4. 找最后一列
# ============================================================

type_col <- names(wx_raw)[ncol(wx_raw)]



# ============================================================
# 5. 清洗
# ============================================================

wx <- wx_raw %>%
  
  mutate(
    
    消息类型 = .data[[type_col]],
    
    
    消息类型 = case_when(
      
      消息类型 == "单条" ~
        
        "单条",
      
      
      消息类型 %in% c(
        "头条",
        "次条"
      ) ~
        
        "多图文",
      
      
      TRUE ~
        
        NA_character_
      
    )
    
  ) %>%
  
  filter(
    
    !is.na(账号),
    
    !is.na(消息类型)
    
  )



# ============================================================
# 6. 统计函数
# ============================================================

make_ratio_plot <- function(data, title){
  
  
  plot_data <- data %>%
    
    count(
      
      消息类型
      
    ) %>%
    
    mutate(
      
      百分比 =
        n / sum(n)
      
    )
  
  
  
  ggplot(
    
    plot_data,
    
    aes(
      
      x="",
      
      y=百分比,
      
      fill=消息类型
      
    )
    
  ) +
    
    
    geom_col(
      
      width=0.55
      
    ) +
    
    
    
    geom_text(
      
      aes(
        
        label=paste0(
          round(百分比*100),
          "%"
        )
        
      ),
      
      position=position_stack(
        vjust=0.5
      ),
      
      size=6,
      
      fontface="bold",
      
      color="white"
      
    ) +
    
    
    
    scale_fill_manual(
      
      values=type_colors
      
    ) +
    
    
    
    scale_y_continuous(
      
      labels=scales::percent,
      
      expand=c(0,0)
      
    ) +
    
    
    
    labs(
      
      title=title,
      
      x=NULL,
      
      y=NULL
      
    ) +
    
    
    
    theme_minimal(
      
      base_family="Microsoft YaHei"
      
    ) +
    
    
    
    theme(
      
      plot.title = element_text(
        
        size=14,
        
        face="bold",
        
        hjust=0.5
        
      ),
      
      
      axis.text.x = element_blank(),
      
      
      axis.text.y = element_text(
        
        size=10,
        
        color="#555555"
        
      ),
      
      
      panel.grid.major.x =
        element_blank(),
      
      
      panel.grid.minor =
        element_blank(),
      
      
      legend.position =
        "none",
      
      
      panel.background =
        element_rect(
          fill="transparent",
          color=NA
        ),
      
      
      plot.background =
        element_rect(
          fill="transparent",
          color=NA
        )
      
    )
  
}





# ============================================================
# 7. 四张图
# ============================================================


p1 <- make_ratio_plot(
  
  wx %>%
    
    filter(
      账号=="第一财经"
    ),
  
  "第一财经"
  
)



p2 <- make_ratio_plot(
  
  wx %>%
    
    filter(
      账号=="新华社"
    ),
  
  "新华社"
  
)



p3 <- make_ratio_plot(
  
  wx %>%
    
    filter(
      账号=="新闻坊"
    ),
  
  "新闻坊"
  
)



p4 <- make_ratio_plot(
  
  wx,
  
  "整体"
  
)



# ============================================================
# 8. 横向拼接
# ============================================================


final_plot <-
  
  p1 + p2 + p3 + p4 +
  
  plot_layout(
    
    nrow=1
    
  )



# ============================================================
# 9. 输出
# ============================================================

ggsave(
  
  filename = out_file,
  
  plot = final_plot,
  
  width = 14,
  
  height = 4,
  
  units="in",
  
  dpi=450,
  
  bg="transparent",
  
  device=ragg::agg_png
  
)