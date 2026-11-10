# ================= 0. 加载所需包 =================
library(tidyverse)
library(stringr)
library(ggpath)
library(teamcolors)

# ================= 1. 精简数据准备 (简约版 Tibble) ================
players <- c("Kevin Durant", "Al Horford", "Mike Conley", "Marc Gasol", "Jeff Green", 
             "Joakim Noah", "Tiago Splitter", "Thaddeus Young", "Marco Belinelli", 
             "Glen Davis", "Jared Dudley", "Corey Brewer", "Carl Landry", "Aaron Brooks",
             "Rodney Stuckey", "Josh McRoberts", "Spencer Hawes", "Arron Afflalo",
             "Rudy Fernandez", "Ramon Sessions", "Daequan Cook", "Nick Young",
             "Wilson Chandler", "Julian Wright", "Brandan Wright", "Jason Smith",
             "Greg Oden", "Acie Law", "Yi Jianlian", "Aaron Gray", "Dominic McGuire",
             "Chris Richard")

draft_2007 <- tibble(
  Player = players,
  # 2008-2010 (按照 players 顺序填充数据)
  Y2008 = c(NA, "4.3(ATL)", NA, NA, NA, NA, NA, "3.9(PHI)", NA, "1.4(BOS)", NA, NA, "3.1(HOU)", NA, "8.7(DET)", NA, NA, "1.5(DET)", NA, NA, NA, NA, NA, "3.8(NOP)", NA, "1.6(PHI)", NA, "0.8(ATL)", NA, NA, "1(WAS)", NA),
  Y2009 = c(NA, "5.4(ATL)", NA, NA, NA, "6.5(CHI)", NA, "2.9(PHI)", NA, "9(BOS)", NA, NA, "3.3(HOU)", "9.3(HOU)", "2.2(DET)", NA, NA, "0.5(DET)", "3.3(POR)", NA, "0.4(MIA)", NA, NA, "0.3(NOP)", NA, NA, "0.4(POR)", "1.3(ATL)", NA, NA, NA, NA),
  Y2010 = c("5(OKC)", "11.2(ATL)", NA, NA, "2.9(OKC)", "7(CHI)", NA, NA, NA, "10.4(BOS)", "13.9(PHX)", NA, NA, NA, NA, "0.9(IND)", NA, "3.1(DEN)", "2.7(POR)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "0.3(CHI)"),
  
  # 2011-2013
  Y2011 = c("30(OKC)", "8(ATL)", "8.9(MEM)", "13.4(MEM)", "2.9(BOS)", "14.1(CHI)", "3.1(SAS)", "1.4(PHI)", "1.6(NOP)", "1(BOS)", NA, "1.89(DAL)", "3.3(NOP)", NA, NA, NA, "0.5(PHI)", "1.3(DEN)", "0.9(POR)", NA, "3.4(OKC)", NA, "0.8(DEN)", NA, NA, "0.4(NOP)", NA, NA, NA, "1.1(NOP)", NA, NA),
  Y2012 = c("43.3(OKC)", "4.89(ATL)", "5.5(MEM)", "5.6(MEM)", NA, "9.3(CHI)", "6.2(SAS)", "4.3(PHI)", NA, "4.2(ORL)", NA, "3.5(DEN)", NA, NA, NA, NA, "4.9(PHI)", "2.9(DEN)", NA, "2.3(LAL)", "3(OKC)", "5(LAC)", NA, NA, NA, NA, NA, NA, "1.4(DAL)", NA, NA, NA),
  Y2013 = c("22(OKC)", "6(ATL)", "20.7(MEM)", "20.8(MEM)", "4.6(BOS)", "7.6(CHI)", "9.2(SAS)", NA, "6.3(CHI)", NA, NA, "1.7(DEN)", "6.5(GS)", "0.6(HOU)", NA, NA, NA, NA, NA, NA, NA, NA, "3.1(DEN)", NA, NA, NA, NA, NA, NA, NA, NA, NA),
  
  # 2014-2016
  Y2014 = c("28.8(OKC)", NA, "7.1(MEM)", "5.8(MEM)", NA, "4.5(CHI)", "17(SAS)", NA, "8(SAS)", "2.4(LAC)", "1.1(LAC)", NA, NA, NA, NA, "6.1(CHA)", NA, NA, NA, NA, NA, NA, NA, NA, "3.3(DAL)", NA, "1.7(MIA)", NA, NA, NA, NA, NA),
  Y2015 = c(NA, "17.7(ATL)", "11.6(MEM)", "15.8(MEM)", "3(MEM)", "1.8(CHI)", "0.5(SAS)", "1.8(BKN)", "4.1(SAS)", "0.7(LAC)", "4.5(MIL)", "7.6(HOU)", NA, "0.9(CHI)", NA, NA, "2.3(LAC)", NA, NA, "4.6(WAS)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Y2016 = c("22.3(OKC)", "13(ATL)", NA, NA, "3.8(LAC)", NA, NA, NA, NA, NA, NA, NA, NA, NA, "2(IND)", "3.2(MIA)", "1.4(CHA)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  
  # 2017-2019 (核心修正：Y2019 精准对位 Marc Gasol)
  Y2017 = c("46.1(GS)", "22.9(BOS)", "11.7(MEM)", "6(MEM)", NA, NA, NA, "6(IND)", NA, NA, NA, NA, NA, "1.9(IND)", NA, NA, "0.4(MIL)", NA, NA, NA, NA, "1.4(GS)", NA, NA, "0.4(MEM)", "1.1(WAS)", NA, NA, NA, NA, NA, NA),
  Y2018 = c("45.6(GS)", "23.8(BOS)", NA, NA, "11.8(CLE)", NA, NA, "6(IND)", NA, NA, "3.8(OKC)", NA, NA, "0.3(MIN)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Y2019 = c("29.1(GS)", "11(BOS)", NA, "22.2(TOR)", NA, NA, NA, "6.8(IND)", "8.9(PHI)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "0.3(LAC)", NA, NA, NA, NA, NA, NA, NA, NA, NA),
  
  # 2020-2022
  Y2020 = c(NA, "3(PHI)", "10.9(UTA)", "3.4(TOR)", "9.1(HOU)", NA, NA, NA, "1.9(SAS)", NA, "1.3(BKN)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Y2021 = c("27.7(BKN)", NA, "4.2(UTA)", "4.5(LAL)", "7.6(BKN)", NA, NA, NA, NA, NA, "1.2(LAL)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Y2022 = c("5.3(BKN)", "30.5(BOS)", "2.1(UTA)", NA, "1.8(DEN)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  
  # 2023-2025
  Y2023 = c("21.3(PHX)", "17.2(BOS)", "6.2(MIN)", NA, "4.9(DEN)", NA, NA, "2.3(TOR)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Y2024 = c("10.1(PHX)", "24.5(BOS)", "18.8(MIN)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  Y2025 = c(NA, "9.5(BOS)", "8.5(MIN)", NA, "0.3(HOU)", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
)
# 数据转换
image_folder <- "D:/nba/临时照片/球员"

playoff_df <- draft_2007 %>%
  pivot_longer(
    cols = -Player,
    names_to = "season",
    values_to = "raw_value"
  ) %>%
  filter(!is.na(raw_value)) %>%
  mutate(
    season = as.numeric(str_remove(season, "Y")),
    # 提取分数
    score = as.numeric(str_extract(raw_value, "^[0-9.]+")),
    # 提取球队缩写，并去除可能存在的空格
    team = str_extract(raw_value, "(?<=\\().+?(?=\\))") %>% str_trim()
  )

# 1.1 准备球队信息库 (teamcolors)
nba_teams_info <- teamcolors %>%
  filter(league == "nba") %>%
  select(name, mascot, primary, secondary, logo) %>%
  mutate(team_abbr_map = case_when(
    mascot == "Warriors" ~ "GS",
    mascot == "Trail Blazers" ~ "POR", # 修复波特兰 (POR)
    mascot == "Wizards"       ~ "WAS", # 修复华盛顿 (WAS)
    mascot == "Magic"         ~ "ORL",
    mascot == "Thunder"  ~ "OKC",
    mascot == "Nuggets"  ~ "DEN",
    mascot == "Rockets"  ~ "HOU",
    mascot == "Knicks"   ~ "NYK",
    mascot == "Pistons"  ~ "DET",
    mascot == "Celtics"  ~ "BOS",
    mascot == "Hawks"    ~ "ATL",
    mascot == "Grizzlies" ~ "MEM",
    mascot == "Bulls"    ~ "CHI",
    mascot == "76ers"    ~ "PHI",
    mascot == "Suns"     ~ "PHX", 
    mascot == "Mavericks" ~ "DAL",
    mascot == "Heat"     ~ "MIA",
    mascot == "Lakers"   ~ "LAL",
    mascot == "Clippers" ~ "LAC",
    mascot == "Pacers"   ~ "IND",
    mascot == "Nets"     ~ "BKN",
    mascot == "Spurs"    ~ "SAS",
    mascot == "Hornets"  ~ "CHA",
    mascot == "Timberwolves" ~ "MIN",
    mascot == "Raptors"  ~ "TOR",
    mascot == "Jazz"     ~ "UTA",
    mascot == "Bucks"    ~ "MIL",
    mascot == "Pelicans" ~ "NOP",
    mascot == "Cavaliers" ~ "CLE",
    TRUE ~ mascot
  ))

# 1.2 关联数据与头像路径
playoff_final <- playoff_df %>%
  left_join(nba_teams_info, by = c("team" = "team_abbr_map")) %>%
  mutate(
    # 修正点：将球员名中的空格换成下划线以匹配文件名，例如 Jeff_Green.png
    image_path = file.path(image_folder, paste0(str_replace_all(Player, " ", "_"), ".png"))
  )

# 分离绘图层
main_dots <- playoff_final %>% filter(score <= 10)
player_faces <- playoff_final %>% filter(score > 10)

# ================= 2. 绘图 =================
p <- ggplot() +
  # 1. 底层：分值较小的点，显示球队主色
  geom_point(data = main_dots, 
             aes(x = season, y = score, color = primary),
             size = 7, alpha = 0.8) +
  
  # 2. 中层：球队 Logo (仅针对高分球员)
  geom_from_path(data = player_faces, 
                 aes(x = season, y = score, path = logo),
                 width = 0.065, alpha = 0.7) + 
  
  # 3. 顶层：球员头像 (仅针对高分球员)
  geom_from_path(data = player_faces, 
                 aes(x = season, y = score, path = image_path),
                 width = 0.05) + 
  
  # 颜色识别设置
  scale_color_identity() + 
  guides(color = "none") +
  
  # 坐标轴与比例
  scale_x_continuous(breaks = 2008:2025) +
  scale_y_continuous(breaks = seq(0, 40, 10)) +
  coord_cartesian(ylim = c(0, 55), clip = "off") + 
  
  # FMVP 标注
  annotate("label", x = 2016, y = 43.1, label = "FMVP 46.1",
           fill = "#1D428A", color = "#FFC72C", size = 10,
           fontface = "bold", hjust = 0.5, vjust = -0.8) +
  annotate("label", x = 2019, y = 42.6, label = "FMVP 45.6",
           fill = "#1D428A", color = "#FFC72C", size = 10,
           fontface = "bold", hjust = 0.5, vjust = -0.8) +
  
  # 图表内文字说明
  annotate("text", x = 2007, y = 53, 
           label = "NBA 2007届选秀球员各赛季季后赛影响力得分",
           hjust = 0, vjust = 1, 
           size = 40,             # 大幅提升主标题大小
           fontface = "bold", 
           color = "black") +
  
  # 副标题：size 增加到 18 左右，并调整 y 坐标防止重叠
  annotate("text", x = 2007, y = 48, 
           label = "小于10分球员仅显示代表当时所处球队颜色 | 大于10分显示Logo与头像",
           hjust = 0, vjust = 1, 
           color = "gray30", 
           size = 18) +
  # 主题美化：隐藏竖线
  theme_minimal(base_size = 50) + 
  theme(
    plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # 隐藏竖轴线
    panel.grid.major.y = element_line(color = "gray90", linewidth = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 50, color = "black"),
    axis.text.y = element_text(size = 50, color = "black"),
    axis.title = element_text(size = 60, face = "bold"),
    plot.margin = margin(t = 5, r = 60, b = 20, l = 60)
  ) +
  labs(x = "赛季（季后赛）", y = "季后赛综合分数")

# ================= 3. 保存 =================
ggsave("playoff_final_pro.png", plot = p, width = 18, height = 12, dpi = 300)
browseURL("playoff_final_pro.png")