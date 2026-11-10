library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# 原数据 + 新增 ORtg
nba_offense_data <- data.frame(
  Season = c("2025-26", "2024-25", "2023-24", "2022-23", "2021-22", "2020-21",
             "2019-20", "2018-19", "2017-18", "2016-17", "2015-16", "2014-15",
             "2013-14", "2012-13", "2011-12", "2010-11", "2009-10", "2008-09",
             "2007-08", "2006-07", "2005-06", "2004-05", "2003-04", "2002-03",
             "2001-02", "2000-01", "1999-00"),
  eFG_percent = c(0.545, 0.543, 0.547, 0.545, 0.532, 0.538, 0.529, 0.524,
                  0.521, 0.514, 0.502, 0.496, 0.501, 0.496, 0.487, 0.498,
                  0.501, 0.5, 0.497, 0.496, 0.49, 0.482, 0.471, 0.474,
                  0.477, 0.473, 0.478),
  TOV_percent = c(13, 12.6, 12.1, 12.5, 12.3, 12.4, 12.8, 12.4, 13, 12.7,
                  13.2, 13.3, 13.6, 13.7, 13.8, 13.4, 13.3, 13.3, 13.2, 14.2,
                  13.7, 13.6, 14.2, 14, 13.6, 14.1, 14.2),
  ORB_percent = c(26.2, 25.2, 24.2, 24, 23.2, 22.2, 22.5, 22.9, 22.3, 23.3,
                  23.8, 25.1, 25.5, 26.5, 27, 26.4, 26.3, 26.7, 26.7, 27.1,
                  27.3, 28.7, 28.6, 28.5, 28.9, 28.2, 28.9),
  FT_FGA = c(0.219, 0.189, 0.192, 0.208, 0.192, 0.192, 0.201, 0.198, 0.193,
             0.209, 0.209, 0.205, 0.215, 0.204, 0.208, 0.229, 0.228, 0.236,
             0.231, 0.246, 0.248, 0.245, 0.228, 0.229, 0.221, 0.231, 0.231),
  ORtg = c(115.8, 114.5, 115.3, 114.8, 112, 112.3, 110.6, 110.4, 108.6, 108.8, 
           106.4, 105.6, 106.6, 105.8, 104.6, 107.3, 107.6, 108.3, 107.5, 106.5, 
           106.2, 106.1, 102.9, 103.6, 104.5, 103, 104.1)
)

# 数据处理
nba_long <- nba_offense_data %>%
  mutate(
    eFG_percent = eFG_percent * 100,
    FT_FGA = FT_FGA * 100,
    ORtg_scaled = ORtg / 2,  # 除以2，让它落在合理范围
    Season_Simplified = sapply(strsplit(Season, "-"), function(x) {
      year_suffix <- as.integer(x[2])
      prefix <- ifelse(year_suffix < 50, "20", "19")
      paste0(prefix, x[2])
    })
  ) %>%
  tidyr::pivot_longer(
    cols = c(eFG_percent, TOV_percent, ORB_percent, FT_FGA, ORtg_scaled),
    names_to = "Factor",
    values_to = "Value"
  ) %>%
  mutate(
    Factor = case_when(
      Factor == "eFG_percent" ~ "Effective FG% (eFG%)",
      Factor == "TOV_percent" ~ "Turnover Rate (TOV%)",
      Factor == "ORB_percent" ~ "Offensive Rebound Rate (ORB%)",
      Factor == "FT_FGA" ~ "Free Throw Rate (FT/FGA)",
      Factor == "ORtg_scaled" ~ "Offensive Rating (ORtg / 2)"
    ),
    Factor = factor(Factor, levels = c("Offensive Rating (ORtg / 2)", 
                                       "Effective FG% (eFG%)", 
                                       "Turnover Rate (TOV%)", 
                                       "Offensive Rebound Rate (ORB%)", 
                                       "Free Throw Rate (FT/FGA)")),  # 我把ORtg放最上面，便于突出主趋势
    Season_Simplified = factor(
      Season_Simplified,
      levels = sort(unique(as.integer(Season_Simplified)))
    )
  )

# 绘图
p <- ggplot(nba_long, aes(x = Season_Simplified, y = Value, color = Factor, group = Factor)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(
    limits = c(10, 60),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "NBA Offensive Four Factors + Offensive Rating Trends (1999-2025)",
    subtitle = "eFG% and FT/FGA scaled to %; ORtg divided by 2 for comparable scaling; all on same scale | Source: Basketball-Reference",
    x = "Season (ending year)",
    y = "Value (%)",
    color = "Metrics"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # 标签倾斜，避免拥挤
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "right"
  )

# 显示并保存
print(p)

img_path <- "NBA_Offensive_Four_Factors_plus_ORtg_Trends.png"
ggsave(
  filename = img_path,
  plot = p,
  width = 16,
  height = 10,
  dpi = 300
)
browseURL(file.path(getwd(), img_path))