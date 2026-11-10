library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
library(ragg)

# ================= 1. 配色方案 (保持不变) =================
nba_team_colors <- c(
  "ATL"="#E03A3E", "BOS"="#007A33", "BKN"="#000000",
  "CHA"="#00788C", "CHI"="#CE1141", "CLE"="#860038",
  "DAL"="#00538C", "DEN"="#FEC524", "DET"="#1D428A",
  "GSW"="#FFB81C", "HOU"="#CE1141", "IND"="#FDBB30",
  "LAC"="#002147", "LAL"="#552583", "MEM"="#5D76A9",
  "MIA"="#98002E", "MIL"="#00471B", "MIN"="#0C2340",
  "NOP"="#8C4A24", "NYK"="#F58426", "OKC"="#007AC1",
  "ORL"="#0077C0", "PHI"="#006BB6", "PHX"="#E56020",
  "POR"="#E03A3E", "SAC"="#5A2D81", "SAS"="#C4CED4",
  "TOR"="#CE1141", "UTA"="#002B5C", "WAS"="#E31837"
)

nba_team_colors_light <- c(
  "ATL"="#F4A6A6", "BOS"="#66B266", "BKN"="#666666",
  "CHA"="#66B2C1", "CHI"="#F27A8E", "CLE"="#C5667A",
  "DAL"="#66A3D1", "DEN"="#FFD76A", "DET"="#7893C9",
  "GSW"="#FFD36F", "HOU"="#F27A8E", "IND"="#FED584",
  "LAC"="#667D96", "LAL"="#9D7FCA", "MEM"="#9DB4D6",
  "MIA"="#D1667A", "MIL"="#4D8C4D", "MIN"="#5D7AAB",
  "NOP"="#bc8a5f", "NYK"="#F9B57D", "OKC"="#66B8E6",
  "ORL"="#66B2E6", "PHI"="#66A3D1", "PHX"="#EF9C6C",
  "POR"="#F4A6A6", "SAC"="#9D7FCA", "SAS"="#E1E7EB",
  "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)

nba_team_colors_extra_light <- c(
  "ATL"="#FEEEEE", "BOS"="#EEF8EE", "BKN"="#F2F2F2",
  "CHA"="#F0F8F9", "CHI"="#FEEEEE", "CLE"="#F8EEF0",
  "DAL"="#EEF5FC", "DEN"="#FFFBF0", "DET"="#EEF2F9",
  "GSW"="#FFF9E6", "HOU"="#FEEEEE", "IND"="#FFFBF3",
  "LAC"="#E6E9ED", "LAL"="#F5F0FC", "MEM"="#F0F5FC",
  "MIA"="#F9F0F2", "MIL"="#F3F6F4", "MIN"="#F3F4F6",
  "NOP"="#F9F2ED", "NYK"="#FEF6EF", "OKC"="#F0F8FC",
  "ORL"="#F0F8FC", "PHI"="#EEF5FC", "PHX"="#FDF3EE",
  "POR"="#FEEEEE", "SAC"="#F5F0FC", "SAS"="#F9FAFB",
  "TOR"="#FEEEEE", "UTA"="#F3F4F6", "WAS"="#FEEEEE"
)

# 文件名清洗函数 (保持不变，用于找头像)
make_filename <- function(name) {
  name %>%
    str_replace_all("\\.", "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("'", "") %>%
    str_replace_all(" ", "_")
}

# ================= 2. 数据处理 =================
df <- tribble(
  ~athlete_id_1, ~name_en, ~fast_attempts, ~fast_makes, ~fast_fg_pct, ~avg_seconds,
  4395724, "Immanuel Quickley", 52, 16, 30.8, 3.0,
  4278039, "Nickeil Alexander-Walker", 44, 15, 34.1, 3.2,
  2583632, "Royce O'Neale", 42, 14, 33.3, 3.0,
  4431678, "Tyrese Maxey", 41, 19, 46.3, 3.3,
  6475,    "Klay Thompson", 38, 12, 31.6, 3.4,
  4433627, "Keyonte George", 36, 8, 22.2, 2.8,
  2595516, "Norman Powell", 35, 16, 45.7, 3.4,
  4397688, "Trey Murphy III", 33, 9, 27.3, 3.5,
  5238230, "Tre Johnson", 33, 11, 33.3, 3.2,
  5061575, "Kon Knueppel", 32, 15, 46.9, 3.1,
  3136193, "Devin Booker", 30, 10, 33.3, 3.2,
  4278585, "Collin Gillespie", 30, 12, 40.0, 3.2,
  3992,    "James Harden", 29, 14, 48.3, 3.7,
  4845374, "Bub Carrington", 29, 11, 37.9, 3.4,
  3908809, "Donovan Mitchell", 28, 7, 25.0, 3.1
) %>%
  mutate(
    # 1. 球队映射 (严格遵守你提供的上一版代码，绝对不改)
    team = case_when(
      name_en == "Immanuel Quickley" ~ "TOR",
      name_en == "Nickeil Alexander-Walker" ~ "ATL",
      name_en == "Royce O'Neale" ~ "PHX",
      name_en == "Tyrese Maxey" ~ "PHI",
      name_en == "Klay Thompson" ~ "DAL",
      name_en == "Keyonte George" ~ "UTA",
      name_en == "Norman Powell" ~ "MIA",
      name_en == "Trey Murphy III" ~ "NOP",
      name_en == "Tre Johnson" ~ "WAS", 
      name_en == "Kon Knueppel" ~ "CHA", 
      name_en == "Devin Booker" ~ "PHX",
      name_en == "Collin Gillespie" ~ "PHX",
      name_en == "James Harden" ~ "LAC",
      name_en == "Bub Carrington" ~ "WAS",
      name_en == "Donovan Mitchell" ~ "CLE",
      TRUE ~ "NBA"
    ),
    
    # 2. 英文名缩写处理 (First Initial. Last Name)
    # 正则逻辑：取第一个字母，加点和空格，然后取最后一个空格后的所有内容(保留III等后缀)
    # 示例: "Immanuel Quickley" -> "I. Quickley"
    # 示例: "Trey Murphy III" -> "T. Murphy III"
    name = str_replace(name_en, "^([A-Za-z])[^\\s]*\\s+(.*)$", "\\1. \\2"),
    
    # 3. 构造最后一列显示文本
    pct_label = paste0("(", sprintf("%.1f", fast_fg_pct), "%)"),
    
    # 4. 头像文件名 (必须用原始 name_en 找文件，不能用缩写)
    filename_safe = make_filename(name_en),
    filename_safe = case_when(
      name_en == "Trey Murphy III" ~ "Trey_Murphy_III",
      name_en == "Klay Thompson" ~ "Klay_Thompson",
      TRUE ~ filename_safe
    ),
    
    headshot = paste0("D:/nba/球员头像/", filename_safe, ".png"),
    team_logo = paste0("D:/nba/球队logo/球队logo圆/", team, ".png"),
    
    color = nba_team_colors[team],
    bar_color = nba_team_colors_light[team],
    bg_color = nba_team_colors_extra_light[team]
  ) %>%
  arrange(desc(fast_attempts)) %>%
  mutate(
    name = fct_reorder(name, fast_attempts, .desc = TRUE),
    row = row_number() * 1.25,
    ymin = row - 0.45,
    ymax = row + 0.45
  )

# ================= 3. 绘图 ==================
p <- ggplot(df) +
  # 背景和装饰条 (保持不变)
  geom_rect(aes(xmin = 0.2, xmax = 7.17, ymin = ymin, ymax = ymax, fill = bg_color), color = NA) +
  geom_rect(aes(xmin = 0.2, xmax = 5.25, ymin = ymin, ymax = ymax, color = color), fill = NA, linewidth = 1.5) +
  geom_rect(aes(xmin = 5.33, xmax = 7.17, ymin = row - 0.4, ymax = row + 0.4, fill = bar_color), color = NA) +
  geom_rect(aes(xmin = 5.25, xmax = 7.25, ymin = row - 0.53, ymax = row + 0.53, color = color), fill = NA, linewidth = 1.4) +
  
  # --- 数据标签 (保持你的分层设计) ---
  geom_shadowtext(aes(x = 6.05, y = row, label = fast_attempts),
                  size = 9, fontface = "bold", colour = "white", bg.colour = "black",
                  family = "consolas", hjust = 1) + # 改用 consolas 字体
  
  geom_shadowtext(aes(x = 6.15, y = row, label = pct_label),
                  size = 5, fontface = "bold", colour = "#DDDDDD", bg.colour = "black",
                  family = "consolas", hjust = 0) + # 改用 consolas 字体
  
  scale_fill_identity() + scale_color_identity() +
  
  # 图标
  geom_image(aes(x = 0.62, y = row, image = team_logo), size = 0.08) +
  geom_image(aes(x = 1.56, y = row - 0.27, image = headshot), size = 0.1) +
  
  # --- 球员名字 (优化字体) ---
  # 1. 使用 consolas 字体 (系统默认无衬线字体，类似 Arial)
  # 2. 名字变短了(缩写)，所以字号可以保持较大，这里设为 8.5 让它更醒目
  geom_text(aes(x = 3.43, y = row, label = name),
            size = 8.5, fontface = "bold", family = "consolas") +
  
  coord_cartesian(clip = "off", xlim = c(0, 8)) +
  scale_y_reverse() +
  
  # --- 英文列名 ---
  annotate("text", x = 0.62, y = min(df$row) - 0.95, label = "TEAM",
           size = 8, fontface = "bold", family = "consolas", color = "#111111") +
  annotate("text", x = 3.43, y = min(df$row) - 0.95, label = "PLAYER",
           size = 8, fontface = "bold", family = "consolas", color = "#111111") +
  annotate("text", x = 6.25, y = min(df$row) - 0.95, label = "3PA (3P%)",
           size = 7, fontface = "bold", family = "consolas", color = "#111111") +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.margin = margin(8, -10, -20, -10),
    
    # 标题字体也改为 sans
    plot.title = element_markdown(
      family = "sans", 
      face = "bold",
      colour = "black",
      hjust = 0.5,
      lineheight = 1.25,
      margin = margin(b = 5)
    ),
    
    plot.subtitle = element_text(
      family = "sans",
      size = 11.5,
      colour = "gray50",
      hjust = 0.5,
      margin = margin(b = 5)
    )
  ) +
  labs(
    title = "<span style='font-size:26pt;'>NBA 2026 Season 'Fast Break 3-Point' Leaders</span>",
    subtitle = "Data Source: hoopR | Criteria: <4s after Reb/TO or <6s after Score | Viz: 俗人罢了"
  )

# ================= 4. 保存图片 ==================
ggsave("NBA_FastBreak3s_TOP15_EN.png",
       width = 9, height = 12.5, dpi = 340,
       bg = "floralwhite", device = agg_png)

shell.exec("NBA_FastBreak3s_TOP15_EN.png")