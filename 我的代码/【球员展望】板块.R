library(tidyverse)
library(arrow)
library(shadowtext)
library(prismatic)
library(ggpath)

target_id <- 1631105
headshot_path <- paste0("D:/nba/俗人的篮球场代码/headshots/", target_id, ".webp")

# 2. 读取本地 Parquet 数据源
df_raw <- arrow::read_parquet("D:/nba/2025-26NBA_EPM_stats.parquet")

# 提取球员基础信息与名字
player_info <- df_raw %>% filter(player_id == target_id)
p_name <- player_info$player_name[1]

# 3. 核心指标精选与计算逻辑字典 (新增抢断、篮板及各区域命中率，共17项)
# 注：失误(tov)的 z-score 越高说明失误越多，因此 invert = TRUE，让 100% 代表失误最少
metrics_dict <- tribble(
  ~stat_name, ~raw_col, ~z_col, ~invert, ~is_pct,
  "总 EPM", "tot", "tot_z", FALSE, FALSE,
  "进攻 EPM", "off", "off_z", FALSE, FALSE,
  "防守 EPM", "def", "def_z", FALSE, FALSE,
  
  "球权使用率", "p_usg", "p_usg_z", FALSE, TRUE,
  "真实命中率", "p_tspct", "p_tspct_z", FALSE, TRUE,
  "百回合得分", "p_pts_100", "p_pts_100_z", FALSE, FALSE,
  "百回合助攻", "p_ast_100", "p_ast_100_z", FALSE, FALSE,
  "百回合失误", "p_tov_100", "p_tov_100_z", TRUE, FALSE, 
  "百回合抢断", "p_stl_100", "p_stl_100_z", FALSE, FALSE,
  "百回合后场板", "p_drb_100", "p_drb_100_z", FALSE, FALSE,
  "百回合前场板", "p_orb_100", "p_orb_100_z", FALSE, FALSE,
  
  "篮下出手频次", "p_fga_rim_100", "p_fga_rim_100_z", FALSE, FALSE,
  "中距离出手频次", "p_fga_mid_100", "p_fga_mid_100_z", FALSE, FALSE,
  "三分出手频次", "p_fg3a_100", "p_fg3a_100_z", FALSE, FALSE,
  
  "篮下命中率", "p_fgpct_rim", "p_fgpct_rim_z", FALSE, TRUE,
  "中距离命中率", "p_fgpct_mid", "p_fgpct_mid_z", FALSE, TRUE,
  "三分命中率", "p_fg3pct", "p_fg3pct_z", FALSE, TRUE
)

# 4. 数据重塑与百分位计算 (pnorm 魔法)
dat <- metrics_dict %>%
  rowwise() %>%
  mutate(
    raw_val = player_info[[raw_col]],
    z_val = player_info[[z_col]],
    # 核心大招：利用 Z-score 精准映射 0-100 百分位 (正态分布 CDF)
    ntile = if_else(invert, round(pnorm(-z_val) * 100, 0), round(pnorm(z_val) * 100, 0)),
    # 防止极值越界
    ntile = case_when(ntile > 99 ~ 99, ntile < 1 ~ 1, TRUE ~ ntile),
    # 格式化原始数据标签 (百分比 vs 小数)
    raw_label = if_else(is_pct, sprintf("%.1f%%", raw_val * 100), sprintf("%.1f", raw_val))
  ) %>%
  ungroup() %>%
  # 按字典顺序反转为因子，以确保画图时从上到下按分类排列
  mutate(stat_name = factor(stat_name, levels = rev(metrics_dict$stat_name)))

# 获取指标总数，用于计算顶部文字的相对 Y 轴位置，彻底解决重叠问题
n_metrics <- nrow(metrics_dict)

# 5. 绘制 Baseball Savant 风格图表
p <- ggplot(dat, aes(x = ntile, y = stat_name)) +

  # 分割线：精确划分 命中率 / 出手分布 / 基础得分面板 / EPM影响力
  # 根据 factor 的顺序（从下往上），目前有17个指标
  geom_hline(yintercept = c(3.5, 6.5, 14.5), linetype = 'dashed', linewidth = 0.4, color = "gray65") +
  
  # 滑块底层背景伪条形 (灰色轨道)
  geom_col(aes(x = 100, y = stat_name), fill = "#CFCFCFFF", color = "transparent", width = 0.25, alpha = 0.4) +
  
  # 真实数据条 (按百分位映射颜色)
  geom_col(aes(fill = ntile, color = after_scale(clr_darken(fill, 0.15))), alpha = 0.9, width = 0.6) +
  
  # 左侧指标名 (中文，YaHei) & 右侧原始数值 (数字，Consolas)
  geom_text(aes(label = stat_name, x = -3), hjust = 1, family = "Microsoft YaHei", size = 5.5, color = "gray25", fontface = 'bold') +
  geom_text(aes(label = raw_label, x = 104), hjust = 0, family = "Consolas", size = 5.5, color = "gray25", fontface = 'bold') +
  
  # 百分位气泡处理
  geom_point(data = . %>% filter(ntile >= 5), shape = 21, size = 11,  
             aes(fill = ntile, x = ntile - 3, color = after_scale(clr_darken(fill, 0.3)))) +
  geom_shadowtext(data = . %>% filter(ntile >= 5), aes(x = ntile - 3, y = stat_name, label = ntile), 
                  family = "Consolas", bg.r = 0.15, fontface = 'bold', bg.colour = "black", color = 'white', size = 4.5) +
  
  geom_point(data = . %>% filter(ntile < 5), shape = 21, size = 11,  
             aes(fill = ntile, x = ntile + 3.5, color = after_scale(clr_darken(fill, 0.3)))) +
  geom_shadowtext(data = . %>% filter(ntile < 5), aes(x = ntile + 3.5, y = stat_name, label = ntile), 
                  family = "Consolas", bg.r = 0.15, fontface = 'bold', bg.colour = "black", color = 'white', size = 4.5) +
  
  # 顶部坐标轴指引针 (High / Avg / Low) —— Y轴基于指标总数 + 1 抬高
  annotate('text', x = 99, y = n_metrics + 0.8, label = "优秀\n▼", family = 'Microsoft YaHei', color = '#40A34D', fontface = 'bold', lineheight = 0.95, size = 4.5) +
  annotate('text', x = 50, y = n_metrics + 0.8, label = "平均\n▼", family = 'Microsoft YaHei', color = 'gray45', fontface = 'bold', lineheight = 0.95, size = 4.5) +
  annotate('text', x = 1, y = n_metrics + 0.8, label = "较差\n▼", family = 'Microsoft YaHei', color = '#DA4450', fontface = 'bold', lineheight = 0.95, size = 4.5) +
  
  # 标题与副标题处理 —— 将 Y 轴显著拉高，杜绝重叠
  annotate('text', x = 50, y = n_metrics + 2.4, label = paste0(p_name,"未来数据期望与影响力预测"), family = 'Microsoft YaHei', color = 'black', fontface = 'bold', hjust = 0.5, size = 10) +
  annotate('text', x = 50, y = n_metrics + 1.6, label = "www.surennba.com", family = 'Consolas', color = '#A1A1A1', fontface = 'bold', hjust = 0.5, size = 5) +
  
  # X 轴加宽以容纳文字，渐变色配置 (红-灰-绿)
  # 因为是 1:1 尺寸，横向空间被相对压缩，所以左侧 limits 用 -28 就足够宽敞，不会死白
  scale_x_continuous(expand = c(0,0), limits = c(-28, 115)) + 
  # Y 轴顶部再多留 22% 的空间，保证头像和标题绝对安全
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.18))) +
  scale_fill_gradient2(low = "#DA4450", mid = "#EBE5E0", high = "#40A34D", midpoint = 50) +
  
  # 主题设定
  coord_cartesian(clip = 'off') +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = 'none',
    # 调用头像的核心魔法：
    plot.tag = element_path(size = 2.0),
    # 【头像位置微调】：将头像移至 X=0.15 左右的位置，精确悬浮在左侧中文字体的一列上方
    plot.tag.position = c(0.14, 0.9) 
  ) +
  labs(tag = headshot_path) # 绑定本地球员头像路径

# 6. 一键出图，强制 1:1 正方形比例 (12 x 12, 适配新增的多行数据)
ggsave(paste0(p_name, "_Savant_Percentile.png"), p, width = 12, height = 12, dpi = 300)