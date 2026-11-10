# 加载所有必需的包
library(tidyverse)
library(ggimage)
library(ggtext)
library(shadowtext)
library(ragg)

# 球队中文名映射
team_cn <- c(
  "NUN" = "新泽西篮网",
  "DAL" = "达拉斯小牛",
  "GSW" = "金州勇士",
  "HOU" = "休斯顿火箭",
  "SEA" = "西雅图超音速",
  "SDC" = "圣地亚哥快船",
  "SAS" = "圣安东尼奥马刺",
  "PHI" = "费城76人"
)

# 球队主色（用于边框）
# 球队主色（LOGO核心色，保持不变）
# 球队主色（LOGO核心色，差异化调整，避免雷同）
team_primary_color <- c(
  "NUN" = "#CC0000",     # 篮网：深红（替代纯红，与火箭红区分）
  "DAL" = "#006400",     # 达拉斯小牛：深森林绿（替代纯绿，独特色系）
  "GSW" = "#1D428A",     # 勇士：深蓝（保留，无雷同）
  "HOU" = "#CE1141",     # 火箭：火箭红（保留，品牌核心色）
  "SEA" = "#00A86B",     # 西雅图超音速：青绿色/湖绿（替代纯绿，与小牛深绿明显区分）
  "SDC" = "#0066FF",     # 圣地亚哥快船：亮蓝（替代纯蓝，与勇士深蓝、76人蓝区分）
  "SAS" = "#000000",     # 马刺：黑色（保留，独特色系）
  "PHI" = "#006BB6"      # 76人：76人蓝（保留，与快船亮蓝、勇士深蓝区分）
)

# 球队副色（用于数据条填充，与主色同色系浅化，全差异化，无接近）
team_secondary_color <- c(
  "NUN" = "#F8D7DA",     # 篮网：浅玫红（对应主色深红，与火箭浅红区分）
  "DAL" = "#E6FFE6",     # 达拉斯小牛：浅森林绿（对应主色深森林绿，独特色系）
  "GSW" = "#E6EDF7",     # 勇士：浅深蓝（对应主色深蓝，独特色系）
  "HOU" = "#FDE6EA",     # 火箭：浅火箭红（对应主色火箭红，与篮网浅玫红区分）
  "SEA" = "#E6FFF2",     # 西雅图超音速：浅青绿（对应主色青绿色，与小牛浅绿明显区分）
  "SDC" = "#E6F0FF",     # 圣地亚哥快船：浅亮蓝（对应主色亮蓝，独特色系）
  "SAS" = "#F0F2F5",     # 马刺：浅灰（对应主色黑色，独特色系）
  "PHI" = "#E6F0F9"      # 76人：浅76人蓝（对应主色76人蓝，与快船浅亮蓝区分）
)

# LOGO尺寸映射
logo_size_map <- c(
  "NUN" = 0.07,
  "DAL" = 0.1,
  "GSW" = 0.075,
  "HOU" = 0.075,
  "SEA" = 0.08,
  "SDC" = 0.075,
  "SAS" = 0.12,
  "PHI" = 0.07
)

df <- tribble(
  ~排名, ~赛季, ~球队缩写, ~进攻篮板率,
  1, "1991-1992", "NUN", 39.1,
  2, "1994-1995", "DAL", 38.9,
  3, "1980-1981", "GSW", 38.8,
  4, "2025-2026", "HOU", 38.6,
  5, "1988-1989", "SEA", 38.3,
  6, "1981-1982", "HOU", 37.8,
  7, "1983-1984", "SDC", 37.7,
  8, "1986-1987", "SEA", 37.6,
  9, "1979-1980", "HOU", 37.6,
  10, "1987-1988", "DAL", 37.5,
  11, "1989-1990", "SEA", 37.5,
  12, "1978-1979", "SDC", 37.5,
  13, "1980-1981", "SAS", 37.5,
  14, "1984-1985", "PHI", 37.4,
  15, "1984-1985", "HOU", 37.4
) %>%
  mutate(
    排名带号 = paste0("#", 排名),
    rank_num = as.integer(str_remove(排名带号, "#")),
    赛季后两位 = str_sub(赛季, nchar(赛季)-1, nchar(赛季)),
    team_logo = paste0("D:/nba/临时球队logo/processed/", 赛季后两位, 球队缩写, ".png"),
    球队中文 = team_cn[球队缩写],
    进攻篮板率带百分号 = paste0(进攻篮板率, "%"),
    row = row_number() * 1.2,
    ymin = row - 0.45,
    ymax = row + 0.45,
    logo_size = logo_size_map[球队缩写],
    final_logo_size = ifelse(
      (赛季后两位 == "95" & 球队缩写 == "DAL") | 
        (赛季后两位 == "88" & 球队缩写 == "DAL") | 
        (赛季后两位 == "91" & 球队缩写 == "SAS"),
      logo_size,
      logo_size * (2/3)
    ),
    color = team_primary_color[球队缩写],
    bar_color = team_secondary_color[球队缩写]
  )

# 验证数据
print(df[, c("排名", "球队缩写", "球队中文", "进攻篮板率带百分号")])

# ================= 2. 绘图 =================
# 布局：排名(0.8) | LOGO(2.0) | 赛季(3.5) | 球队(5.0) || 进攻篮板率(7.0)
# 外层矩形到球队截止(xmax=6.0)，数据条包含进攻篮板率(6.0-8.0)
p <- ggplot(df) +
  # 外层边框矩形（到球队截止）
  geom_rect(aes(xmin = 0.2, xmax = 6.0, ymin = row - 0.5, ymax = row + 0.5, color = color),
            fill = NA, linewidth = 1.5) +
  # 数据条填充（包含进攻篮板率）
  geom_rect(aes(xmin = 6.08, xmax = 7.92, ymin = row - 0.4, ymax = row + 0.4, fill = bar_color),
            color = NA) +
  # 数据条边框（包含进攻篮板率）
  geom_rect(aes(xmin = 6.0, xmax = 8.0, ymin = row - 0.53, ymax = row + 0.53, color = color),
            fill = NA, linewidth = 1.5) +
  # 球队LOGO
  geom_image(aes(x = 2.0, y = row, image = team_logo), 
             size = I(df$final_logo_size)) +
  # 1. 排名
  geom_shadowtext(aes(x = 0.8, y = row, label = 排名带号),
                  size = 8, fontface = "bold", colour = "#111111",
                  bg.colour = "white", family = "Consolas") +
  # 2. 赛季
  geom_text(aes(x = 3.5, y = row, label = 赛季),
            size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  # 3. 球队中文名
  geom_text(aes(x = 5.0, y = row, label = 球队中文),
            size = 6.5, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  # 4. 进攻篮板率（在数据条内）
  geom_shadowtext(aes(x = 7.0, y = row, label = 进攻篮板率带百分号),
                  size = 8, fontface = "bold", colour = "#111111",
                  bg.colour = "white", family = "Consolas") +
  coord_cartesian(clip = "off", xlim = c(0, 9)) +
  scale_y_reverse() +
  scale_color_identity() +
  scale_fill_identity() +
  # 表头标签
  annotate("text", x = 0.8, y = min(df$row) - 0.95, label = "排名",
           size = 8, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  annotate("text", x = 3.5, y = min(df$row) - 0.95, label = "赛季",
           size = 8, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  annotate("text", x = 5.0, y = min(df$row) - 0.95, label = "球队",
           size = 8, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  annotate("text", x = 7.0, y = min(df$row) - 0.95, label = "进攻篮板率",
           size = 8, fontface = "bold", family = "Microsoft YaHei", color = "#111111") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "floralwhite", colour = NA),
    plot.margin = margin(8, 15, 8, 15),
    plot.title = element_markdown(
      family = "Microsoft YaHei",
      face = "bold",
      colour = "black",
      hjust = 0.5,
      lineheight = 1.25,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      family = "Microsoft YaHei",
      size = 11.5,
      colour = "gray50",
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    plot.caption = element_text(
      family = "Microsoft YaHei",
      size = 14.5,
      face = "bold",
      colour = "black",
      hjust = 0.5,
      margin = margin(t = 6)
    )
  ) +
  labs(
    title = "<span style='font-size:32pt;'>NBA 进攻篮板率最强球队 本赛季火箭登榜！</span>",
    subtitle = "统计指标：进攻篮板率|只统计常规赛，本赛季火箭只有24场样本
  进攻篮板率指的是一支球队能拼抢到进攻篮板的概率，由进攻篮板数除以进攻篮板机会",
    caption = "制作：俗人罢了|制作时间12.20|数据源：BBR|从1973-1974赛季开始统计（并非历史）"
  )

# ================= 3. 保存图片 =================
ggsave("NBA_进攻篮板率TOP15球队.png",
       plot = p,
       width = 11, height = 12.5, dpi = 340,
       bg = "floralwhite", device = agg_png)

shell.exec("NBA_进攻篮板率TOP15球队.png")
