library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)

# -----------------------------
# 基础参数设置
# -----------------------------
col_x_left <- -0.2 
col_x_right <- 12 
col_rank <- 0.6
col_player <- 6.5
col_value <- 10.0
bg_dark <- "floralwhite" 

# 球队主色映射 
nba_team_colors <- c(
  "ATL"="#E03A3E", "BOS"="#007A33", "BKN"="#000000", "CHA"="#00788C", "CHI"="#CE1141", 
  "CLE"="#860038", "DAL"="#00538C", "DEN"="#0D2240", "DET"="#1D428A", "GSW"="#FFB81C", 
  "HOU"="#CE1141", "IND"="#FDBB30", "LAC"="#002147", "LAL"="#552583", "MEM"="#5D76A9", 
  "MIA"="#98002E", "MIL"="#00471B", "MIN"="#0C2340", "NOP"="#8C4A24", "NYK"="#F58426", 
  "OKC"="#007AC1", "ORL"="#0077C0", "PHI"="#006BB6", "PHO"="#E56020", "POR"="#E03A3E", 
  "SAC"="#5A2D81", "SAS"="#C4CED4", "TOR"="#CE1141", "UTA"="#002B5C", "WAS"="#E31837"
)

nba_team_colors_light <- c(
  "ATL"="#F4A6A6", "BOS"="#66B266", "BKN"="#666666", "CHA"="#66B2C1", "CHI"="#F27A8E", 
  "CLE"="#C5667A", "DAL"="#66A3D1", "DEN"="#4A5870", "DET"="#7893C9", "GSW"="#FFD36F", 
  "HOU"="#F27A8E", "IND"="#FED584", "LAC"="#667D96", "LAL"="#FDB927", "MEM"="#9DB4D6", # LAL的浅色改为湖人金
  "MIA"="#D1667A", "MIL"="#4D8C4D", "MIN"="#5D7AAB", "NOP"="#bc8a5f", "NYK"="#F9B57D", 
  "OKC"="#66B8E6", "ORL"="#66B2E6", "PHI"="#66A3D1", "PHO"="#EF9C6C", "POR"="#F4A6A6", 
  "SAC"="#9D7FCA", "SAS"="#E1E7EB", "TOR"="#F27A8E", "UTA"="#668BB2", "WAS"="#F07C8C"
)

# -----------------------------
# 数据录入
# -----------------------------
quarter1 <- tibble(
  球队 = c("洛杉矶湖人队（LAL）", "纽约尼克斯队（NYK）", "克利夫兰骑士队（CLE）", "明尼苏达森林狼队（MIN）", "费城76人队（PHI）"),
  球员 = c("卢卡·东契奇（Luka Dončić）", "杰伦·布伦森（Jalen Brunson）", "詹姆斯·哈登（James Harden）", "安东尼·爱德华兹（Anthony Edwards）", "泰雷斯·马克西（Tyrese Maxey）"),
  Value_Raw = c(11.6, 9.3, 8.6, 8.6, 8.3), panel = "Top-Left", relative_row = 1:5)

quarter2 <- tibble(
  球队 = c("洛杉矶快船队（LAC）", "费城76人队（PHI）", "洛杉矶湖人队（LAL）", "密尔沃基雄鹿队（MIL）", "犹他爵士队（UTA）"),
  球员 = c("科怀·伦纳德（Kawhi Leonard）", "乔尔·恩比德（Joel Embiid）", "卢卡·东契奇（Luka Dončić）", "扬尼斯·阿德托昆博（Giannis Antetokounmpo）", "劳里·马尔卡宁（Lauri Markkanen）"),
  Value_Raw = c(8.3, 7.8, 7.3, 7.1, 7.1), panel = "Bottom-Left", relative_row = 1:5)

quarter3 <- tibble(
  球队 = c("俄克拉荷马城雷霆队（OKC）", "洛杉矶湖人队（LAL）", "明尼苏达森林狼队（MIN）", "丹佛掘金队（DEN）", "金州勇士队（GSW）"),
  球员 = c("谢伊·吉尔杰斯-亚历山大（Shai Gilgeous-Alexander）", "卢卡·东契奇（Luka Dončić）", "安东尼·爱德华兹（Anthony Edwards）", "尼古拉·约基奇（Nikola Jokić）", "斯蒂芬·库里（Stephen Curry）"),
  Value_Raw = c(11.5, 9.8, 9.4, 9.3, 8.9), panel = "Top-Right", relative_row = 1:5)

quarter4 <- tibble(
  球队 = c("密尔沃基雄鹿队（MIL）", "克利夫兰骑士队（CLE）", "洛杉矶湖人队（LAL）", "费城76人队（PHI）", "俄克拉荷马城雷霆队（OKC）"),
  球员 = c("扬尼斯·阿德托昆博（Giannis Antetokounmpo）", "多诺万·米切尔（Donovan Mitchell）", "奥斯汀·里夫斯（Austin Reaves）","泰雷斯·马克西（Tyrese Maxey）",  "谢伊·吉尔杰斯-亚历山大（Shai Gilgeous-Alexander）"),
  Value_Raw = c(8.8, 8.1, 7.4, 7.4, 7.3), panel = "Bottom-Right", relative_row = 1:5)

# -----------------------------
# 数据合并与【高亮逻辑处理】
# -----------------------------
y_offset_bottom <- 7

df <- bind_rows(quarter1, quarter2, quarter3, quarter4) %>%
  mutate(
    team_en = str_extract(球队, "（([A-Z]+)）") %>% str_remove_all("（|）"),
    姓名 = str_extract(球员, "^[^（]+"),
    姓名_en = str_extract(球员, "（([^）]+)）") %>% str_remove_all("（|）"),
    排名 = paste0("#", relative_row),
    col_x = ifelse(str_detect(panel, "Left"), col_x_left, col_x_right),
    base_y = relative_row * 1.3,
    row_y = ifelse(str_detect(panel, "Bottom"), base_y + y_offset_bottom, base_y),
    ymin = row_y - 0.45, ymax = row_y + 0.45,
    
    headshot = 姓名_en %>%
      str_replace_all("č", "c") %>% str_replace_all("ć", "c") %>% 
      str_replace_all("ū", "u") %>% str_replace_all("š", "s") %>%
      str_replace_all("·", "") %>% str_replace_all("\\.", "") %>%       
      str_replace_all(" ", "_") %>%
      paste0("D:/nba/球员头像/", ., ".png"),
    headshot = ifelse(file.exists(headshot), headshot, NA),
    team_logo = paste0("D:/nba/球队logo/球队logo方/", team_en, ".png"),
    
    # 🔥 核心逻辑：识别是否为湖人球员
    is_target = ifelse(team_en == "LAL", TRUE, FALSE),
    
    # 根据是否为目标球队，动态分配颜色和透明度
    border_color = ifelse(is_target, nba_team_colors[team_en], "#E0E0E0"), # 非湖人变浅灰边框
    fill_color = ifelse(is_target, nba_team_colors_light[team_en], "#F5F5F5"), # 非湖人数值条变极浅灰
    bg_alpha = ifelse(is_target, 0.15, 0.03), # 湖人底色加深，非湖人极淡
    
    # 动态分配文字颜色
    text_color_name = ifelse(is_target, "black", "#888888"), # 非湖人名字变灰
    text_color_rank = ifelse(is_target, nba_team_colors[team_en], "#B0B0B0"), # 湖人排名用主队色
    
    # 动态分配头像/Logo透明度 (ggimage支持直接把透明度映射为一列数据，但需用color="white"骗过它，这里采用更简化的图层法)
    # 因为 ggimage 处理动态透明度较复杂，我们将用 alpha 参数在绘图时统一处理
    
    数值_label = sprintf("%.1f", Value_Raw)
  )

# =======================================================================
# 绘图部分 (湖人定制版)
# =======================================================================
p <- ggplot(df) +
  # 背景与边框层 (应用动态颜色映射)
  geom_rect(aes(xmin = col_x + 0.2, xmax = col_x + 9.1, ymin = row_y - 0.45, ymax = row_y + 0.45, fill = border_color, alpha = bg_alpha), color = NA) +
  geom_rect(aes(xmin = col_x + 0.2, xmax = col_x + 9, ymin = ymin, ymax = ymax, color = border_color), fill = NA, linewidth = 1.4) +
  
  # 右侧数值条背景
  geom_rect(aes(xmin = col_x + 9.1, xmax = col_x + 10.9, ymin = row_y - 0.45, ymax = row_y + 0.45, fill = fill_color), color = NA) +
  geom_rect(aes(xmin = col_x + 9.0, xmax = col_x + 11.0, ymin = row_y - 0.53, ymax = row_y + 0.53, color = border_color), fill = NA, linewidth = 1.4) +
  
  # 排名与姓名 (应用动态文字颜色)
  geom_text(aes(x = col_x + 0.7, y = row_y, label = 排名, color = text_color_rank), size = 8, fontface = "bold", family = "Consolas") +
  geom_text(aes(x = col_x + col_player, y = row_y, label = 姓名, color = text_color_name), size = 8.2, fontface = "bold", family = "Microsoft YaHei") +
  
  # 非湖人球员的头像和Logo层 (设置静态透明度 0.3)
  geom_image(data = df %>% filter(!is_target), aes(x = col_x + 1.8, y = row_y, image = team_logo), size = 0.07, alpha = 0.3) +
  geom_image(data = df %>% filter(!is_target), aes(x = col_x + 3.5, y = row_y - 0.1 , image = headshot), size = 0.105, alpha = 0.3) +
  
  # 湖人球员的头像和Logo层 (火力全开，无透明度)
  geom_image(data = df %>% filter(is_target), aes(x = col_x + 1.8, y = row_y, image = team_logo), size = 0.08) + # Logo稍微放大一点点
  geom_image(data = df %>% filter(is_target), aes(x = col_x + 3.5, y = row_y - 0.1 , image = headshot), size = 0.115) + # 头像也微微放大
  
  # 数值阴影 (动态处理：湖人用纯黑边+白底，非湖人用浅灰字无黑边)
  geom_shadowtext(data = df %>% filter(is_target), aes(x = col_x + col_value, y = row_y, label = 数值_label), 
                  size = 11.5, fontface = "bold", colour = "black", bg.colour = "white", family = "Consolas") +
  geom_text(data = df %>% filter(!is_target), aes(x = col_x + col_value, y = row_y, label = 数值_label), 
            size = 10.8, fontface = "plain", colour = "#888888", family = "Consolas") +
  
  # 必须使用这两个函数，让图层能直接读取 df 里的颜色和透明度数值
  scale_fill_identity() + 
  scale_color_identity() +
  scale_alpha_identity() +
  
  # --- 表头保持原样黑色 ---
  annotate("text", x = col_x_left + col_rank, y = +0.4, label = "排名", size = 8.2, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_player, y = +0.4, label = "球员", size = 8, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_value, y = +0.4, label = "第一节场均得分", size = 7.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  annotate("text", x = col_x_left + col_rank, y = y_offset_bottom + 0.4, label = "排名", size = 8.2, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_player, y = y_offset_bottom + 0.4, label = "球员", size = 8, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_left + col_value, y = y_offset_bottom + 0.4, label = "第二节场均得分", size = 7.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  annotate("text", x = col_x_right + col_rank, y = +0.4, label = "排名", size = 8.2, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_right + col_player, y = +0.4, label = "球员", size = 8, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_right + col_value, y = +0.4, label = "第三节场均得分", size = 7.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  annotate("text", x = col_x_right + col_rank, y = y_offset_bottom + 0.4, label = "排名", size = 8.2, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_right + col_player, y = y_offset_bottom + 0.4, label = "球员", size = 8, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  annotate("text", x = col_x_right + col_value, y = y_offset_bottom + 0.4, label = "第四节场均得分", size = 7.5, fontface = "bold", family = "Microsoft YaHei", color = "black") +
  
  coord_cartesian(clip = "off", xlim = c(-2, 24.5)) +
  scale_y_reverse() + theme_void() +
  # ... 前面代码保持不变 ...
  
  labs(
    title = "<span style='font-size:38pt;'>NBA本赛季各单节场均得分王TOP5（湖人版）</span>",
    subtitle = "数据源：statmuse | 制图：<span style='color:black;'>**俗人罢了**</span> | 截止：2026.3.11"
  ) +
  theme(
    panel.background = element_rect(fill = bg_dark, colour = NA),
    plot.background = element_rect(fill = bg_dark, colour = NA),
    plot.margin = margin(20, -120, 20, -120),
    # 增加 title 的 margin，避免删掉副标题后标题太贴着内容
    plot.title = element_markdown(family = "Microsoft YaHei", color = "black", face = "bold", 
                                  hjust = 0.5, size = 20, margin = margin(b = 5)), 
    plot.subtitle = element_markdown(family = "Microsoft YaHei", color = "#A1A1A1", 
                                     size = 14, face = "bold", hjust = 0.5)
  )

# 保存
ggsave("NBA_Quarter_Scoring_LAL_Highlight_Clean.png", p, width = 18, height = 13.8, dpi = 340)