library(tidyverse)
library(pdftools)
library(ggrepel)

# ==========================================
# 1. 定义 PDF 解析器
# ==========================================
parse_voting_pdf <- function(path) {
  txt <- pdf_text(path)
  lines <- unlist(strsplit(txt, "\n"))
  tibble(raw = lines) %>%
    filter(str_detect(raw, "\\("), str_detect(raw, ",")) %>%
    mutate(raw = str_trim(raw)) %>%
    extract(raw, into = c("name_raw", "fan_votes"), 
            regex = "^(.*\\))\\s+\\d+\\s+\\d+\\s+([\\d,]+)", remove = FALSE) %>%
    mutate(
      player_temp = str_trim(str_remove(name_raw, "\\s*\\(.*\\)")),
      player = paste(str_split_i(player_temp, ", ", 2), str_split_i(player_temp, ", ", 1)),
      fan_votes = parse_number(fan_votes)
    ) %>%
    mutate(player = ifelse(player == "Hansen Yang", "Yang Hansen", player)) %>%
    select(player, fan_votes)
}

# ==========================================
# 2. 数据加载与合并 (使用第 10 列：epm_wins)
# ==========================================
df_votes <- bind_rows(parse_voting_pdf("D:/nba/东部.pdf"), parse_voting_pdf("D:/nba/西部.pdf"))
df_epm <- read_csv("D:/nba/BBE_Data_Standard.csv", show_col_types = FALSE) %>%
  select(player = 1, epm_wins = 10) %>%
  mutate(player = str_trim(player))

df_final <- left_join(df_votes, df_epm, by = "player") %>% filter(!is.na(epm_wins))

# ==========================================
# 3. 统计审计与中文名字映射
# ==========================================
fit <- lm(log(fan_votes) ~ poly(epm_wins, 2), data = df_final)
df_final$residual <- resid(fit)

# 自动筛选：最被高估与最被低估的各 3 位
label_data_raw <- df_final %>% 
  arrange(desc(residual)) %>% head(3) %>%          # 最被高估
  bind_rows(df_final %>% arrange(residual) %>% head(3)) # 最被低估

# --- 关键修改：建立这 6 位球员的映射词典 ---
nba_dict <- c(
  "LeBron James" = "湖人-勒布朗·詹姆斯",
  "Adem Bona" = "76人-博纳",
  "Yang Hansen" = "开拓者-杨瀚森",
  "Julian Champagnie" = "马刺-朱利安·尚帕尼",
  "Dru Smith" = "热火-德鲁·史密斯",
  "Yves Missi" = "鹈鹕-米西"
)

# 应用映射：如果在词典里就换中文，不在就保留原名
label_data <- label_data_raw %>%
  mutate(player_cn = ifelse(player %in% names(nba_dict), nba_dict[player], player))

# ==========================================
# 4. 可视化绘制 (中文定制版)
# ==========================================
theme_owen <- function () { 
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(color = "gray40", size = 10, hjust = 0.5, margin = margin(b = 10)),
      plot.caption = element_text(color = "gray60", size = 8, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8)
    )
}

p <- ggplot(df_final, aes(x = epm_wins, y = fan_votes)) +
  # 1. 趋势线
  stat_smooth(
    method = "lm", 
    formula = y ~ poly(x, 2), 
    color = "black", 
    linetype = "dashed", 
    
    # --- 关键修改 ---
    se = TRUE,          # 打开阴影
    fill = "gray70",    # 设置阴影颜色 (深灰/浅灰)
    alpha = 0.2         # 设置阴影透明度 (越小越淡，建议0.15-0.2)
    # ----------------
  ) +
  
  # 2. 散点 (颜色映射残差)
  geom_point(aes(fill = -residual), shape = 21, size = 3.5, color = "white", stroke = 0.5) +
  
  # 3. 标签 (映射 player_cn 列)
  geom_text_repel(
    data = label_data,
    aes(label = player_cn), # <--- 这里改成了映射后的中文名字
    size = 4, fontface = "bold",
    box.padding = 0.8, point.padding = 0.3,
    force = 20, max.overlaps = Inf, min.segment.length = 0,
    segment.color = "grey30", segment.size = 0.5,
    arrow = arrow(length = unit(0.015, "npc"), type = "closed")
  ) +
  
  # 4. 颜色与图例设计
  scale_fill_distiller(palette = "RdYlGn", direction = 1) + 
  
  guides(fill = guide_colorbar(
    title.position = "bottom", title.hjust = 0.5,
    barwidth = unit(15, "lines"), barheight = unit(0.5, "lines"), ticks = FALSE
  )) +
  
  scale_y_log10(labels = scales::comma) +
  theme_owen() +
  
  # 5. 中文文案
  labs(
    title = "NBA最被球迷高估/低估的球员（EPM累计版）",
    subtitle = "X轴：EPM的累计数据，综合了单位时间与出勤 | Y轴：2026赛季全明星球迷总票数",
    x = "EPM Wins (累计胜场贡献)", 
    y = "球迷全明星投票总数",
    fill = "最被高估<<---------->>最被低估", 
    caption = "数据源：dunksandthrees&NBA官网|制作：俗人罢了"
  )

# 显示并保存
print(p)
ggsave("NBA_2026_EPMwins_Chinese.png", width = 9, height = 7, dpi = 300)