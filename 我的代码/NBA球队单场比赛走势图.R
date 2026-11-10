library(tidyverse)
library(ggpath)
library(extrafont)
library(teamcolors)

# --- 1. 自定义主题 (您的 F5 风格) ---
theme_f5 <- function (font_size = 9) {
  theme_minimal(base_size = font_size, base_family = "Microsoft YaHei") %+replace%
    theme(
      plot.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
      panel.background = element_rect(fill = '#FFFFFF', color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95", linewidth = 0.2),
      
      # 主标题：纯黑，更粗，更醒目
      plot.title = element_text(
        hjust = 0.5, 
        size = 18,        # 稍微放大一点
        face = 'bold', 
        color = "#111111"
      ),
      
      # 副标题：深灰色 #444444，清晰但不夺目
      plot.subtitle = element_text(
        color = '#444444', 
        hjust = 0.5, 
        margin = margin(5, 0, 20, 0), # 增加下方边距，给图表留白
        size = 11,
        face = 'plain',    # 确保是常规体
        lineheight = 1.2   # 增加行高
      ),
      
      # 注脚：保持较浅，因为它最不重要
      plot.caption = element_text(
        color = 'gray70', 
        margin = margin(10, 0, 0, 0), 
        hjust = 1, 
        size = 7
      ),
      
      # 分面标签美化
      strip.text = element_text(size = 8, face = "bold", color = "#222222"),
      panel.spacing = unit(1.2, "lines")
    )
}
# --- 2. 准备元数据 (Teamcolors + HTTPS 修复) ---
# 这一步是 Logo 能显示的关键
nba_meta <- teamcolors::teamcolors %>%
  filter(league == "nba") %>%
  mutate(
    # 1. 强制将 http 替换为 https，防止网络拦截
    logo = str_replace(logo, "http://", "https://"),
    # 2. 修正名称以匹配 ESPN 数据源 (特别是快船队)
    name = case_when(
      name == "Los Angeles Clippers" ~ "LA Clippers",
      TRUE ~ name
    )
  ) %>%
  # 选取我们需要的列：全名、主色、副色、Logo链接
  select(name, primary, secondary, logo)

# --- 3. 加载本地数据 ---
pbp_file <- "D:/下载文件/play_by_play_2026.rds"
if (!file.exists(pbp_file)) stop("找不到文件，请检查路径: ", pbp_file)
pbp_2026 <- readRDS(pbp_file)

# --- 4. 球队中英文映射表 ---
team_cn_lookup <- c(
  "Atlanta Hawks"          = "亚特兰大老鹰",
  "Boston Celtics"         = "波士顿凯尔特人",
  "Brooklyn Nets"          = "布鲁克林篮网",
  "Charlotte Hornets"      = "夏洛特黄蜂",
  "Chicago Bulls"          = "芝加哥公牛",
  "Cleveland Cavaliers"    = "克利夫兰骑士",
  "Dallas Mavericks"       = "达拉斯独行侠",
  "Denver Nuggets"         = "丹佛掘金",
  "Detroit Pistons"        = "底特律活塞",
  "Golden State Warriors"  = "金州勇士",
  "Houston Rockets"        = "休斯顿火箭",
  "Indiana Pacers"         = "印第安纳步行者",
  "LA Clippers"            = "洛杉矶快船",
  "Los Angeles Lakers"     = "洛杉矶湖人",
  "Memphis Grizzlies"      = "孟菲斯灰熊",
  "Miami Heat"             = "迈阿密热火",
  "Milwaukee Bucks"        = "密尔沃基雄鹿",
  "Minnesota Timberwolves" = "明尼苏达森林狼",
  "New Orleans Pelicans"   = "新奥尔良鹈鹕",
  "New York Knicks"        = "纽约尼克斯",
  "Oklahoma City Thunder"  = "俄克拉荷马城雷霆",
  "Orlando Magic"          = "奥兰多魔术",
  "Philadelphia 76ers"     = "费城76人",
  "Phoenix Suns"           = "菲尼克斯太阳",
  "Portland Trail Blazers" = "波特兰开拓者",
  "Sacramento Kings"       = "萨克拉门托国王",
  "San Antonio Spurs"      = "圣安东尼奥马刺",
  "Toronto Raptors"        = "多伦多猛龙",
  "Utah Jazz"              = "犹他爵士",
  "Washington Wizards"     = "华盛顿奇才"
)

# --- 5. 数据处理与合并 ---
df <- pbp_2026 %>%
  # 5.1 构建球队全名
  mutate(
    home_full = paste(home_team_name, home_team_mascot),
    away_full = paste(away_team_name, away_team_mascot)
  ) %>%
  # 5.2 15秒分箱计算逻辑
  select(game_id, start_game_seconds_remaining, home_score, away_score, home_full, away_full) %>%
  mutate(
    game_minutes = ceiling((start_game_seconds_remaining / 60) * 4) / 4,
    home_margin = home_score - away_score,
    away_margin = away_score - home_score
  ) %>%
  pivot_longer(cols = c(home_full, away_full), names_to = "team_type", values_to = "team") %>%
  mutate(margin = if_else(team_type == "home_full", home_margin, away_margin)) %>%
  group_by(team, game_minutes) %>%
  summarize(avg_margin = mean(margin, na.rm = TRUE), .groups = "drop") %>%
  
  # 5.3 关键步骤：合并 teamcolors 元数据
  left_join(nba_meta, by = c("team" = "name")) %>%
  
  # 5.4 映射中文名
  mutate(
    team_cn = if_else(team %in% names(team_cn_lookup), team_cn_lookup[team], team)
  )

# --- 6. 排序逻辑 ---
team_order <- df %>%
  group_by(team) %>%
  summarize(total_avg = mean(avg_margin, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_avg)) %>%
  pull(team)

df$team <- factor(df$team, levels = team_order)
# 确保中文名也按同一顺序排列
df$team_cn <- factor(df$team_cn, levels = team_cn_lookup[team_order])

# --- 7. 辅助绘图层 ---
quarters <- data.frame(
  avg_margin = rep(-17.5, 4),
  game_minutes = seq(42, 6, -12),
  labels = c("1Q", "2Q", "3Q", "4Q")
)

# 提取 Logo 层 (每队只保留一行用于绘图)
tm_logos <- df %>%
  distinct(team_cn, logo) %>%
  filter(!is.na(logo)) %>% # 安全过滤
  mutate(avg_margin = 15, game_minutes = 42)

# --- 8. 绘图 ---
p <- ggplot(df, aes(x = game_minutes, y = avg_margin)) +
  # 网格辅助线
  geom_vline(xintercept = 24, linewidth = 0.25, color = "gray80") +
  geom_hline(yintercept = 0, linewidth = 0.25, color = "gray80") +
  
  facet_wrap(~team_cn) +
  
  # 散点层：使用 secondary (副色) 填充
  geom_point(alpha = 0.4, size = 0.6, shape = 21, aes(fill = secondary), color = "transparent") +
  
  # 龙形曲线背景 (白色光晕，制造层次感)
  stat_smooth(geom = "line", se = FALSE, linewidth = 1.2, span = 0.4,
              color = "floralwhite", alpha = 0.8) +
  
  # 龙形曲线主体：使用 primary (主色)
  stat_smooth(aes(color = primary), geom = "line",
              se = FALSE, linewidth = 0.8, span = 0.4) +
  
  # 季度标识
  geom_text(data = quarters, aes(label = labels),
            size = 1.8, color = "gray50") +
  
  # Logo 层：使用 ggpath 解析 HTTPS 链接
  geom_from_path(data = tm_logos, aes(path = logo), width = 0.2) +
  
  # 坐标轴与比例尺
  scale_x_reverse(breaks = seq(48, 0, -12)) +
  scale_y_continuous(limits = c(-20, 20), breaks = c(-10, 0, 10)) +
  
  # 启用颜色识别 (直接读取十六进制码)
  scale_color_identity() +
  scale_fill_identity() +
  
  theme_f5() +
  theme(
    strip.text = element_text(size = 7, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 6),
    panel.spacing = unit(0.8, "lines")
  ) +
  labs(
    title = "2025-26赛季 NBA30支球队比赛走势图 ",
    subtitle = "数据源：hoopR|数据截止：2025.12.29|制作人：俗人罢了|球队排序按照场均净胜分排序",
    x = "比赛过程（第一节开始 → 第四节结束）",
    y = "平均净胜分"
  )

# --- 9. 保存 ---
# 建议保存为 PNG，尺寸设大一点以保证 Logo 清晰
ggsave("nba_dragon_chart_2026_final.png", p, height = 8, width = 8, dpi = 400)
browseURL("nba_dragon_chart_2026_final.png")