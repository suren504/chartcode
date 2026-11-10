library(tidyverse)
library(teamcolors)
library(ggpath)
library(prismatic)
library(shadowtext)
library(ggtext)
library(stringr)

# =========================================================================
# 1. 原始数据与映射表配置
# =========================================================================

rookie_coaches_data <- tibble::tribble(
  ~教练, ~场次, ~胜场, ~负场, ~赛季, ~球队缩写,
  "掘金",    3L,  3L,  0L, "2024-2025", "DEN",
  "骑士",  82L, 20L, 62L, "2016-2017", "BKN",
  "活塞", 71L, 37L, 34L, "2015-2016", "HOU",
  "鹈鹕",   30L, 10L, 20L, "2014-15",   "ORL",
  "尼克斯",       82L, 50L, 32L, "2005-06",   "CLE",
  "步行者",    82L, 50L, 32L, "2001-02",   "DET",
  "国王",    51L, 27L, 24L, "2004-05",   "SAC",
  "雷霆",  72L, 22L, 50L, "2020-21",   "OKC",
  "公牛",    82L, 55L, 27L, "2015-16",   "OKC",
  "篮网",  82L, 26L, 56L, "2024-25",   "BKN",
  "森林狼",      41L, 16L, 25L, "2020-21",   "MIN",
  "爵士",       82L, 37L, 45L, "2022-23",   "UTA",
  "灰熊",    82L, 25L, 57L, "2024-25",   "MEM",
  "马刺",    77L, 32L, 45L, "2024-25",   "SAS",
  "奇才",      39L, 8L,  31L, "2023-24",   "WAS",
  "勇士",       82L, 67L, 15L, "2014-15",   "GSW",
  "独行侠",       82L, 44L, 38L, "2013-14",   "BKN",
  "黄蜂",      82L, 19L, 63L, "2024-25",   "CHA",
  "快船",       41L, 27L, 14L, "2015-16",   "CLE",
  "凯尔特人",     82L, 57L, 25L, "2022-23",   "BOS",
  "魔术",    82L, 22L, 60L, "2021-22",   "ORL",
  "76人",       82L, 58L, 24L, "2018-19",   "TOR",
  "太阳",       82L, 45L, 37L, "2025-26",   "PHX",
  "猛龙",  82L, 25L, 57L, "2023-24",   "TOR",
  "湖人",        82L, 50L, 32L, "2024-25",   "LAL",
  "雄鹿",       82L, 41L, 41L, "1999-2000","ORL",
  "老鹰",      82L, 38L, 44L, "2014-15",   "UTA",
  "开拓者",   81L, 42L, 39L, "2025-26",   "POR",
  "热火",   82L, 43L, 39L, "2008-09",   "MIA",
  "火箭",        82L, 51L, 31L, "2021-22",   "BOS"
)

coach_name_map <- tribble(
  ~team_cn, ~coach_en,
  "掘金", "David Adelman", "骑士", "Kenny Atkinson", "活塞", "J.B. Bickerstaff",
  "鹈鹕", "James Borrego", "尼克斯", "Mike Brown", "步行者", "Rick Carlisle",
  "国王", "Doug Christie", "雷霆", "Mark Daigneault", "公牛", "Billy Donovan",
  "篮网", "Jordi Fernandez", "森林狼", "Chris Finch", "爵士", "Will Hardy",
  "灰熊", "Tuomas Iisalo", "马刺", "Mitch Johnson", "奇才", "Brian Keefe",
  "勇士", "Steve Kerr", "独行侠", "Jason Kidd", "黄蜂", "Charles Lee",
  "快船", "Tyronn Lue", "凯尔特人", "Joe Mazzulla", "魔术", "Jamahl Mosley",
  "76人", "Nick Nurse", "太阳", "Jordan Ott", "猛龙", "Darko Rajakovic",
  "湖人", "JJ Redick", "雄鹿", "Doc Rivers", "老鹰", "Quin Snyder",
  "开拓者", "Tiago Splitter", "热火", "Erik Spoelstra", "火箭", "Ime Udoka"
)

team_abbr_map <- tribble(
  ~abbr, ~team_en,
  "DEN", "Nuggets", "BKN", "Nets", "HOU", "Rockets", "ORL", "Magic", 
  "CLE", "Cavaliers", "DET", "Pistons", "SAC", "Kings", "OKC", "Thunder", 
  "MIN", "Timberwolves", "UTA", "Jazz", "MEM", "Grizzlies", "SAS", "Spurs", 
  "WAS", "Wizards", "GSW", "Warriors", "CHA", "Hornets", "BOS", "Celtics", 
  "TOR", "Raptors", "PHX", "Suns", "LAL", "Lakers", "POR", "Trail Blazers", 
  "MIA", "Heat"
)

# =========================================================================
# 2. 数据处理与计算排名
# =========================================================================

plot_data <- rookie_coaches_data %>% 
  left_join(coach_name_map, by = c("教练" = "team_cn")) %>%
  mutate(win_pct = round((胜场 / 场次) * 100, 1)) %>%
  arrange(desc(win_pct), desc(胜场)) %>%
  mutate(rn = row_number()) %>%
  left_join(team_abbr_map, by = c("球队缩写" = "abbr")) %>%
  left_join(teamcolors %>% filter(league == "nba"), by = c("team_en" = "mascot")) %>%
  rowwise() %>%
  mutate(
    local_headshot_path = paste0("D:/nba/临时照片/教练/", 教练, ".png"),
    local_team_path = paste0("D:/nba/球队logo/队标/", 球队缩写, ".png"),
    stats_label = paste0(赛季, "赛季", win_pct, "% (", 胜场, "-", 负场, ")")
  ) %>%
  ungroup() %>%
  mutate(coach_factor = factor(coach_en, levels = unique(coach_en))) %>%
  # 核心保底：确保每个主教练只有一条记录，杜绝任何多重 join 导致的图层翻倍
  distinct(coach_factor, .keep_all = TRUE)

# =========================================================================
# 3. 主题与绘图构建 (彻底修复卡死问题)
# =========================================================================

theme_f5 <- function () { 
  theme_minimal(base_size = 9) %+replace% 
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      plot.title = element_markdown(face = 'bold', size = 26, hjust = 0, family = 'Microsoft YaHei'),
      plot.subtitle = element_markdown(family = 'Microsoft YaHei', size = 12, color = '#A1A1A1', hjust = 0, margin=margin(5,0,15,0)),
      text = element_text(family = "Microsoft YaHei")
    )
}

final_plot <- plot_data %>% 
  ggplot(aes(x = coach_factor, y = 1)) + 
  
  # --- 布局：5行6列 ---
  facet_wrap(~coach_factor, nrow = 5, scales = 'free_x') +
  
  # --- 背景色块 ---
  geom_rect(aes(fill = primary, color = after_scale(clr_darken(fill, 0.3))),
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.12)  +
 
  # --- 左上角排名装饰 ---
  # 用 geom_point 随数据分面，避免 annotate 广播卡死
  geom_point(x = -Inf, y = Inf, shape = 23, size = 13, fill = "#5F6B6DFF") +
  geom_shadowtext(aes(x = -Inf, y = Inf, label = rn),
                  size = 3.8, vjust = 1.25, hjust = -0.15,
                  bg.colour = "black", color = 'white', bg.r = .15,
                  family = "Consolas", fontface = 'bold') + 
  
  # --- 球队 Logo ---
  geom_from_path(aes(path = local_team_path), width = 1, vjust = 0.2, alpha = 0.5) +
  
  # --- 教练头像 ---
  geom_from_path(aes(path = local_headshot_path), width = 0.9, vjust = 0.45) +
  
  scale_fill_identity() +
  theme_f5() + 
  
  # --- 底部数据条: 赛季 + 胜率与战绩 ---
  geom_label(aes(x = coach_factor, label = stats_label),
             vjust = 2.4, hjust = 0.5, 
             family = "Microsoft YaHei",
             fontface = "bold", 
             linewidth = 0,
             fill = 'floralwhite',
             size = 4,
             label.padding = unit(0.4, "lines")) + 
  
  # --- 教练名字: 英文全名回显 ---
  geom_shadowtext(aes(x = coach_factor, label = coach_en), 
                  vjust = 3.5, hjust = 0.5, 
                  fontface = 'bold', size = 4.5,
                  family = "Consolas", color = "white",
                  bg.colour = "#000", bg.r = 0.25) + 
  
  # --- 主题细节微调 ---
  theme(axis.text = element_blank(), 
        strip.text = element_blank(), 
        panel.grid = element_blank(),
        plot.margin = unit(c(1.5, 1, 1, 1), units="line"),
        legend.position = 'none') + 
  
  # --- 标题与副标题 ---
  labs(x = "", y = "", 
       title = "现役30位主教练：菜鸟赛季常规赛战绩盘点", 
       subtitle = "根据菜鸟赛季常规赛胜率由高到低排序 | 教练下方数据为菜鸟赛季胜率与 (胜-负) | 背景及Logo为其执教的首支球队 | 数据源：BBR",
       caption = NULL)

# =========================================================================
# 4. 极简保存
# =========================================================================
ggsave("rookie_coaches_ranking.png", final_plot, width = 16, height = 9, dpi = 300)