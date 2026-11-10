# ==============================================================================
# 1. 环境与包加载
# ==============================================================================
library(tidyverse)
library(arrow)
library(ggtext)
library(ggimage)    # 用于在图内嵌本地 Logo

# ==============================================================================
# 2. 注入专属色卡、映射字典与自定义主题
# ==============================================================================
# 徐大帅专属色卡
nba_team_colors_light <- c(
  "ATL"="#E03A3E", "BOS"="#008348", "BKN"="#666666",
  "CHA"="#00788C", "CHI"="#CD1F43", "CLE"="#860038",
  "DAL"="#0066B5", "DEN"="#FFD76A", "DET"="#C8102E",
  "GSW"="#FFD36F", "HOU"="#CE1141", "IND"="#FEBC30",
  "LAC"="#12173F", "LAL"="#552583", "MEM"="#5D76A9",
  "MIA"="#961A31", "MIL"="#004C26", "MIN"="#266092",
  "NOP"="#B89A5B", "NYK"="#F9B57D", "OKC"="#007AC0",
  "ORL"="#66B2E6", "PHI"="#1D428A", "PHO"="#EF9C6C",
  "POR"="#D00A2C", "SAC"="#592D81", "SAS"="black",
  "TOR"="#BC2126", "UTA"="#668BB2", "WAS"="#F07C8C"
)

# 终极映射字典：搞定 ESPN缩写 -> 颜色缩写 -> 中文名 -> 本地Logo文件名(吉祥物)
nba_dict <- tribble(
  ~espn_abbr, ~color_abbr, ~cn_name, ~mascot,
  "ATL", "ATL", "老鹰",   "Hawks",
  "BOS", "BOS", "凯尔特人", "Celtics",
  "BKN", "BKN", "篮网",   "Nets",
  "CHA", "CHA", "黄蜂",   "Hornets",
  "CHI", "CHI", "公牛",   "Bulls",
  "CLE", "CLE", "骑士",   "Cavaliers",
  "DAL", "DAL", "独行侠", "Mavericks",
  "DEN", "DEN", "掘金",   "Nuggets",
  "DET", "DET", "活塞",   "Pistons",
  "GS",  "GSW", "勇士",   "Warriors",      # 修正 ESPN 的 GS
  "HOU", "HOU", "火箭",   "Rockets",
  "IND", "IND", "步行者", "Pacers",
  "LAC", "LAC", "快船",   "Clippers",
  "LAL", "LAL", "湖人",   "Lakers",
  "MEM", "MEM", "灰熊",   "Grizzlies",
  "MIA", "MIA", "热火",   "Heat",
  "MIL", "MIL", "雄鹿",   "Bucks",
  "MIN", "MIN", "森林狼", "Timberwolves",
  "NO",  "NOP", "鹈鹕",   "Pelicans",      # 修正 ESPN 的 NO
  "NY",  "NYK", "尼克斯", "Knicks",        # 修正 ESPN 的 NY
  "OKC", "OKC", "雷霆",   "Thunder",
  "ORL", "ORL", "魔术",   "Magic",
  "PHI", "PHI", "76人",   "76ers",         # 匹配本地 76ers.png
  "PHX", "PHO", "太阳",   "Suns",          # 修正 ESPN 的 PHX
  "POR", "POR", "开拓者", "Trail Blazers", # 匹配本地 Trail Blazers.png
  "SAC", "SAC", "国王",   "Kings",
  "SA",  "SAS", "马刺",   "Spurs",         # 修正 ESPN 的 SA
  "TOR", "TOR", "猛龙",   "Raptors",
  "UTAH","UTA", "爵士",   "Jazz",          # 修正 ESPN 的 UTAH
  "WSH", "WAS", "奇才",   "Wizards"        # 修正 ESPN 的 WSH
)

# 方形紧凑主题
theme_f5_square <- function () {
  theme_minimal(base_family = "Microsoft YaHei") %+replace%
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite'),
      panel.background = element_rect(fill = 'floralwhite', color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
      
      plot.title = element_text(hjust = 0.5, size = 22, face = 'bold', color = "#111111", margin = margin(b = 10)),
      plot.subtitle = element_text(color = '#444444', hjust = 0.5, margin = margin(0, 0, 20, 0), size = 12),
      plot.caption = element_blank(),
      
      # 标题设为中文，极致压缩上下间距
      strip.text = element_text(family = "Microsoft YaHei", face = "bold", size = 12, color = "#111111"), 
      strip.background = element_blank(),
      panel.spacing.y = unit(0.3, "lines"), 
      panel.spacing.x = unit(0.5, "lines"),
      
      axis.text.x = element_text(family = "Consolas", size = 8, color = "gray40"),
      axis.text.y = element_text(family = "Consolas", size = 8, color = "gray40"),
      axis.title = element_text(family = "Microsoft YaHei", size = 11, color = "#333333", face = "bold")
    )
}

# ==============================================================================
# 3. 数据处理与累计差计算
# ==============================================================================
df_raw <- read_parquet("D:/nba/RStudio/HoopR/nba/team_box/parquet/team_box_2026.parquet") %>%
  filter(season_type == 2) %>% 
  # 拦截全明星及非正规军
  filter(team_abbreviation %in% nba_dict$espn_abbr) %>% 
  
  group_by(game_id) %>%
  mutate(game_total_fta = sum(free_throws_attempted, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    opp_fta = game_total_fta - free_throws_attempted,
    fta_diff = free_throws_attempted - opp_fta
  ) %>%
  arrange(team_abbreviation, game_date) %>%
  group_by(team_abbreviation) %>%
  mutate(
    game_num = row_number(),
    cum_fta_diff = cumsum(fta_diff)
  ) %>%
  ungroup() %>%
  # 合并字典获取中文名、颜色和吉祥物名称
  left_join(nba_dict, by = c("team_abbreviation" = "espn_abbr")) %>%
  mutate(
    primary_color = nba_team_colors_light[color_abbr],
    group_bg = team_abbreviation
  )

# ==============================================================================
# 4. 🔥 绝对防退化的排名排序逻辑 🔥
# ==============================================================================
# 1. 抓取最后一场，严格计算排名并生成标题
team_ranks <- df_raw %>%
  group_by(team_abbreviation) %>%
  slice_tail(n = 1) %>% 
  ungroup() %>%
  arrange(desc(cum_fta_diff)) %>%  # 严格按罚球差从大到小排序
  mutate(
    rank = row_number(),
    facet_title = paste0(cn_name, " 第", rank)
  )

# 2. 提取出绝对有序的因子层级 (从 第1 到 第30)
ordered_titles <- team_ranks$facet_title

# 3. 强制把因子赋予主绘图数据框
df_plot <- df_raw %>%
  left_join(team_ranks %>% select(team_abbreviation, rank, facet_title), by = "team_abbreviation") %>%
  mutate(facet_title = factor(facet_title, levels = ordered_titles)) # 核心：锁死顺序！

# 4. 生成 Logo 悬浮坐标 (左上角)
df_labels <- team_ranks %>%
  mutate(
    facet_title = factor(facet_title, levels = ordered_titles), # 同样锁死顺序
    logo_path = paste0("D:/nba/球队logo/队标/", mascot, ".png"),
    label_x = max(df_plot$game_num, na.rm = TRUE) * 0.15,
    label_y = max(df_plot$cum_fta_diff, na.rm = TRUE) * 0.85
  )

center_x <- max(df_plot$game_num, na.rm = TRUE) / 2

# ==============================================================================
# 5. 暴力美学出图
# ==============================================================================
p <- ggplot(df_plot, aes(x = game_num, y = cum_fta_diff)) + 
  # 1. 全屏防盗水印
  annotate("text", x = center_x, y = 0, label = "俗人罢了", 
           alpha = 0.04, size = 10, angle = 30, family = "Microsoft YaHei", fontface = "bold") +
  
  # 2. 0 轴基准线
  geom_hline(yintercept = 0, alpha = 0.5, linetype = 'dashed', color = "gray50") +
  
  # 3. 幽灵大盘底色
  geom_line(data = mutate(df_plot, facet_title = NULL), 
            aes(group = group_bg), alpha = 0.08, linewidth = 0.4, color = 'gray60') +
  
  # 4. 球队高亮主线
  geom_line(aes(color = primary_color), linewidth = 1.2, alpha = 0.9) +
  geom_point(data = df_plot %>% group_by(team_abbreviation) %>% slice_tail(n = 1),
             aes(color = primary_color), size = 2) +
  
  # ✨ 5. 图内嵌本地 Logo ✨
  geom_image(data = df_labels, 
             aes(x = label_x, y = label_y, image = logo_path), 
             size = 0.18, asp = 1) +
  
  scale_color_identity() + 
  facet_wrap(~facet_title, ncol = 6) + 
  
  # 留足上下空间，并开启右侧次坐标轴
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.25)),
    sec.axis = dup_axis(name = NULL) # ✨ dup_axis 代表“复制主轴”，name=NULL 表示不需要右侧再写一遍标题
  ) +
  
  theme_f5_square() + 
  labs(
    # ✨ 核心修正：
    # 1. 在 "2025-26" 和 "赛季" 之间使用 &nbsp; 强制拉开间距
    # 2. 红色 span 标签紧贴前后汉字，确保无缝衔接
    title = "NBA 2025-26 赛季球队累计罚球差走势图", 
    
    # 副标题同步美化：加粗“俗人罢了”
    subtitle = "截止：2026/3/12 | 数据源：hoopR | 制作：俗人罢了",
    
    x = "比赛场次", 
    y = "累计罚球差总和"
  )

# ==============================================================================
# 6. 极简保存
# ==============================================================================
ggsave("nba_cum_fta_square_sorted.png", p, width = 11, height = 11, dpi = 300)