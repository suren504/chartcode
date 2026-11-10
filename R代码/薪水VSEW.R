library(tidyverse)
library(ggrepel)
library(scales)

# ==========================================
# 0. 定义新秀合同“黑名单”
# ==========================================
rookie_list <- c(
  "Paolo Banchero", "Cooper Flagg", "Chet Holmgren", "Victor Wembanyama", "Zaccharie Risacher",
  "Dylan Harper", "Jabari Smith Jr.", "Brandon Miller", "Alexandre Sarr", "VJ Edgecombe",
  "Reed Sheppard", "Kon Knueppel", "Amen Thompson", "Stephon Castle", "Bennedict Mathurin",
  "Ace Bailey", "Ausar Thompson", "Ron Holland II", "Shaedon Sharpe", "Tre Johnson",
  "Anthony Black", "Dyson Daniels", "Jeremiah Fears", "Bilal Coulibaly", "Donovan Clingan",
  "Egor Demin", "Ousmane Dieng", "Jarace Walker", "Rob Dillingham", "Jalen Duren",
  "Ochai Agbaji", "Collin Murray-Boyles", "Mark Williams", "Taylor Hendricks", "Khaman Maluach",
  "Cason Wallace", "Cody Williams", "Cedric Coward", "Tari Eason", "Jett Howard",
  "Matas Buzelis", "Jake LaRavia", "Derik Queen", "Gradey Dick", "Christian Braun",
  "Carter Bryant", "Jordan Hawkins", "Carlton Carrington", "Nikola Jovic", "Kel'el Ware",
  "Hansen Yang", "Peyton Watson", "Keyonte George", "Jared McCain", "Dalton Knecht","Toumani Camara",
  "Walter Clayton Jr.", "Jaime Jaquez Jr.", "Nolan Traore", "Tristan Da Silva", "Brandin Podziemski",
  "Kasparas Jakucionis", "Ja'Kobe Walter", "Will Riley", "Jaylon Tyson", "Noah Clowney",
  "Drake Powell", "Yves Missi", "Asa Newell", "Kris Murray", "Nique Clifford",
  "A.J. Johnson", "Jase Richardson", "Kyshawn George", "Ben Saraf", "Danny Wolf",
  "Ben Sheppard", "Hugo Gonzalez", "Yanic Niederhauser", "Brice Sensabaugh", "Julian Strawther","Bub Carrington",
  "Terrence Shannon Jr.", "Ryan Dunn", "Kobe Brown", "Isaiah Collier", "Baylor Scheierman"
)

# ==========================================
# 1. 数据加载与双向清洗 
# ==========================================
df_salary <- read_csv("D:/nba/salaries_final.csv", show_col_types = FALSE) %>%
  select(Player, Cap_Hit) %>%
  mutate(Player = str_trim(Player))

df_epm <- read_csv("D:/nba/export (6).csv", show_col_types = FALSE) %>%
  mutate(
    Player_Clean = stringi::stri_trans_general(.[[1]], id = "Latin-ASCII") %>% str_trim(),
    Attendance = as.numeric(.[[2]]), 
    EPM_Wins_Num = as.numeric(.[[3]]) 
  ) %>%
  select(Player = Player_Clean, Attendance, EPM_Wins = EPM_Wins_Num)

# ==========================================
# 2. 数据合并、过滤与回归分析
# ==========================================
df_final <- inner_join(df_salary, df_epm, by = "Player") %>% 
  filter(
    !is.na(Cap_Hit), 
    !is.na(EPM_Wins), 
    !is.na(Attendance),
    Cap_Hit > 0, 
    Attendance >= 50,
    # 【核心过滤】：剔除在 rookie_list 里面的球员
    !(Player %in% rookie_list) 
  )

# 重新建立线性回归模型 (此时的基准线不再受新秀干扰！)
fit <- lm(EPM_Wins ~ Cap_Hit, data = df_final)
df_final$residual <- resid(fit)

# 提取正逆差各 Top 5 用于打标签
label_data <- df_final %>% 
  arrange(desc(residual)) %>% head(10) %>%
  bind_rows(df_final %>% arrange(residual) %>% head(10))

# ==========================================
# 3. 绘图主题设定 (双字体精修)
# ==========================================
theme_owen <- function () { 
  theme_minimal(base_family = "Microsoft YaHei", base_size = 12) +
    theme(
      plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
      panel.background = element_rect(fill = "floralwhite", color = "floralwhite"),
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(color = "gray40", size = 11, hjust = 0.5, margin = margin(b = 15)),
      plot.caption = element_text(color = "gray60", size = 9, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray85", linewidth = 0.2), 
      legend.position = "top",
      legend.background = element_rect(fill = "floralwhite", color = NA),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      # 坐标轴数字使用 Consolas
      axis.text = element_text(family = "Consolas", color = "black", face = "bold")
    )
}

# ==========================================
# 4. 可视化绘制
# ==========================================
p <- ggplot(df_final, aes(x = Cap_Hit, y = EPM_Wins)) +
  
  stat_smooth(
    method = "lm", color = "black", linetype = "dashed", 
    se = TRUE, fill = "gray70", alpha = 0.15
  ) +
  
  geom_point(
    aes(fill = residual), 
    shape = 21, size = 4, color = "white", stroke = 0.6, alpha = 0.9
  ) +
  
  scale_fill_gradient2(
    low = "#DA4450", mid = "white", high = "#40A34D", midpoint = 0,
    name = "溢价合同(红) <<-------->> 超值合同(绿)"
  ) + 
  
  guides(fill = guide_colorbar(
    title.position = "top", title.hjust = 0.5,
    barwidth = unit(20, "lines"), barheight = unit(0.6, "lines"), ticks = FALSE
  )) +
  
  # 球员标签使用 Consolas
  geom_text_repel(
    data = label_data, aes(label = Player), 
    size = 4.5, fontface = "bold", color = "black", family = "Consolas",
    box.padding = 0.8, point.padding = 0.4,
    force = 40, max.overlaps = Inf, min.segment.length = 0,
    segment.color = "grey40", segment.size = 0.6,
    arrow = arrow(length = unit(0.015, "npc"), type = "closed")
  ) +
  
  scale_x_continuous(
    labels = scales::label_dollar(scale_cut = cut_short_scale()),
    limits = c(0, 60000000),
    breaks = seq(0, 60000000, by = 10000000)
  ) +
  scale_y_continuous(
    limits = c(-3, 19.5),
    breaks = seq(-3, 19.5, by = 3)
  ) +
  
  theme_owen() +
  labs(
    title = "NBA 2025-26 赛季球员超值合同VS溢价薪水【EPM WINS且无新秀合同版】",
    subtitle = "过滤条件：出场率 >= 50% | 趋势线上方为顺差，下方为逆差",
    x = "2025-26 赛季占用工资帽空间 (Cap Hit)", 
    y = "EPM Wins (累计胜场贡献)",
    caption = "数据源: Spotrac & dunksandthrees | 制作：俗人罢了"
  )


ggsave("NBA_2026_Salary_vs_EPM_NonRookie.png", plot = p, width = 10, height = 8, dpi = 300)
# ==========================================
# 6. 控制台硬核审计报告 (Non-Rookie Edition)
# ==========================================

# 提取超值 TOP 10
top_10_real <- df_final %>% 
  arrange(desc(residual)) %>% 
  head(10) %>%
  mutate(Rank = paste0("TOP ", row_number()), Status = "超值合同(顺差)")

# 提取溢价 Bottom 10
bottom_10_real <- df_final %>% 
  arrange(residual) %>% 
  head(10) %>%
  mutate(Rank = paste0("Bottom ", row_number()), Status = "溢价合同(逆差)")

# 合并并精简表格
audit_report <- bind_rows(top_10_real, bottom_10_real) %>%
  select(Rank, Player, Cap_Hit, EPM_Wins, Residual = residual) %>%
  mutate(
    # 薪水转为更直观的 $XX.XM 格式
    Salary = paste0("$", round(Cap_Hit / 1000000, 1), "M"),
    # 残差保留两位小数
    Residual = round(Residual, 2)
  ) %>%
  select(Rank, Player, Salary, EPM_Wins, Residual)


print(as.data.frame(audit_report), row.names = FALSE)
