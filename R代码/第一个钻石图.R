library(tidyverse)
library(ggtext)  # 用于处理副标题的加粗 (Markdown语法)
library(ggimage)

# --- 2. 核心数据录入 (CTG数据 - 去除垃圾时间) ---
# 注意：已移除第一行 Average，将其单独作为变量处理
ctg_data <- tribble(
  ~Team_Name, ~Off_Rtg, ~Def_Rtg,
  "Oklahoma City", 119.5, 107.4,
  "Detroit", 117.6, 109.0,
  "Boston", 120.9, 113.6,
  "San Antonio", 118.2, 112.0,
  "New York", 120.6, 114.4,
  "Houston", 118.3, 112.3,
  "Minnesota", 118.9, 113.1,
  "Cleveland", 118.0, 114.1,
  "Denver", 122.1, 118.2,
  "Toronto", 115.6, 113.1,
  "Phoenix", 116.1, 113.6,
  "Miami", 114.7, 112.4,
  "Charlotte", 117.4, 115.5,
  "Golden State", 115.6, 113.7,
  "Philadelphia", 115.9, 116.0,
  "LA Lakers", 117.5, 117.6,
  "Orlando", 114.1, 115.0,
  "LA Clippers", 116.4, 117.4,
  "Atlanta", 114.2, 116.0,
  "Portland", 114.5, 116.5,
  "Memphis", 114.3, 116.5,
  "Milwaukee", 114.6, 118.2,
  "Dallas", 111.0, 114.7,
  "Chicago", 114.6, 118.4,
  "New Orleans", 113.3, 118.9,
  "Indiana", 109.9, 117.5,
  "Brooklyn", 111.5, 119.4,
  "Utah", 113.9, 122.1,
  "Sacramento", 110.8, 121.0,
  "Washington", 110.3, 121.4
)

# 设定 CTG 的联盟平均值 (作为原点)
avg_off <- 115.7
avg_def <- 115.7

# --- 3. 数据处理与坐标变换 ---

# 3.1 映射球队全名到缩写 (用于匹配本地 Logo 文件名)
# 请确保您的 Logo 文件名与下列缩写一致 (如 OKC.png, GSW.png)
df_processed <- ctg_data %>%
  mutate(
    Team = case_when(
      Team_Name == "Oklahoma City" ~ "OKC",
      Team_Name == "Detroit" ~ "DET",
      Team_Name == "Boston" ~ "BOS",
      Team_Name == "San Antonio" ~ "SAS",
      Team_Name == "New York" ~ "NYK",
      Team_Name == "Houston" ~ "HOU",
      Team_Name == "Minnesota" ~ "MIN",
      Team_Name == "Cleveland" ~ "CLE",
      Team_Name == "Denver" ~ "DEN",
      Team_Name == "Toronto" ~ "TOR",
      Team_Name == "Phoenix" ~ "PHO", # 有些Logo包是PHO，请根据实际修改
      Team_Name == "Miami" ~ "MIA",
      Team_Name == "Charlotte" ~ "CHA",
      Team_Name == "Golden State" ~ "GSW",
      Team_Name == "Philadelphia" ~ "PHI",
      Team_Name == "LA Lakers" ~ "LAL",
      Team_Name == "Orlando" ~ "ORL",
      Team_Name == "LA Clippers" ~ "LAC",
      Team_Name == "Atlanta" ~ "ATL",
      Team_Name == "Portland" ~ "POR",
      Team_Name == "Memphis" ~ "MEM",
      Team_Name == "Milwaukee" ~ "MIL",
      Team_Name == "Dallas" ~ "DAL",
      Team_Name == "Chicago" ~ "CHI",
      Team_Name == "New Orleans" ~ "NOP",
      Team_Name == "Indiana" ~ "IND",
      Team_Name == "Brooklyn" ~ "BKN",
      Team_Name == "Utah" ~ "UTA",
      Team_Name == "Sacramento" ~ "SAC",
      Team_Name == "Washington" ~ "WAS",
      TRUE ~ str_to_upper(str_sub(Team_Name, 1, 3)) # 兜底逻辑
    )
  )

# 3.2 计算菱形坐标
df_diamond <- df_processed %>%
  mutate(
    # 计算相对值
    off_rel = Off_Rtg - avg_off,
    def_rel = avg_def - Def_Rtg, # 防守效率越低越好，所以用 平均减去实际
    
    # 旋转坐标系 (X=攻-防, Y=攻+防)
    x_plot = off_rel - def_rel,
    y_plot = off_rel + def_rel,
    
    # 构建 Logo 路径
    logo_path = paste0("D:/nba/球队logo/球队logo方/", Team, ".png")
  )

# --- 4. 辅助绘图数据 ---

# 提取特定球队坐标用于注释 (根据新数据提取)
get_coord <- function(team_code) {
  df_diamond %>% filter(Team == team_code) %>% select(x_plot, y_plot)
}

pos_OKC <- get_coord("OKC") # 攻防俱佳
pos_DEN <- get_coord("DEN") # 进攻极强
pos_WAS <- get_coord("WAS") # 攻防黑洞
pos_MIA <- get_coord("MIA") # 防守出色
pos_SAS <- get_coord("SAS") # 防守出色
pos_CHI <- get_coord("CHI") # 防守出色
# 设定绘图边界 (根据数据极值动态调整，保证背景覆盖)
max_val <- max(abs(c(df_diamond$off_rel, df_diamond$def_rel))) + 2
L <- ceiling(max_val)

# 构建背景菱形
bg_all <- bind_rows(
  data.frame(group="Top",    fill="#a6dba0", x=c(0, L, 0, -L), y=c(0, L, 2*L, L)),
  data.frame(group="Bottom", fill="#c2a5cf", x=c(0, L, 0, -L), y=c(0, -L, -2*L, -L)),
  data.frame(group="Right",  fill="#f7f7f7", x=c(0, 2*L, L, L), y=c(0, 0, -L, L)),
  data.frame(group="Left",   fill="#f7f7f7", x=c(0, -2*L, -L, -L), y=c(0, 0, L, -L))
)

# --- 5. 绘图 ---
p_final <- ggplot() +
  # A. 背景多边形
  geom_polygon(data = bg_all, aes(x=x, y=y, group=group, fill=fill), alpha=0.5) +
  scale_fill_identity() +
  
  # B. 球队 Logo
  geom_image(data = df_diamond, aes(x=x_plot, y=y_plot, image=logo_path), size=0.06) +
  
  # C. 象限文字
  annotate("label", x=0, y=1.8*L, label="攻守兼备", fill="#1b7837", color="white", fontface="bold", size=8) +
  annotate("label", x=0, y=-1.8*L, label="啥也不行", fill="#762a83", color="white", fontface="bold", size=8) +
  annotate("label", x=1.8*L, y=0, label="极致进攻", fill="white", color="black", fontface="bold", size=7) +
  annotate("label", x=-1.8*L, y=0, label="铁血防守", fill="white", color="black", fontface="bold", size=7) +
  
  # D. 轴标识
  annotate("text", x = 1.8 * L, y = 0.3 * L, label = "进攻更好 →", 
           angle = 45, size = 3, color = "gray40", fontface = "bold") +
  annotate("text", x = -1.8 * L, y = 0.3 * L, label = "← 防守更好", 
           angle = -45, size = 3, color = "gray40", fontface = "bold") +
  
  # --- E. 动态注释 (根据CTG新数据) ---
  
  # 1. OKC: 独一档的攻防
  annotate("text", x = pos_OKC$x_plot, y = pos_OKC$y_plot + 1.5, 
           label = "#雷霆断档第一\n#攻防效率皆为前五", 
           color = "#EF3B24", fontface = "bold", size = 3.5, vjust = 0, lineheight = 0.9) +
  
  # 2. DEN: 进攻天花板
  annotate("text", x = pos_DEN$x_plot + 1.5, y = pos_DEN$y_plot, 
           label = "#掘金进攻效率即是联盟第一\n#也是当前历史单赛季第二", 
           color = "#1D428A", fontface = "bold", size = 3.5, hjust = 0, lineheight = 0.9) +
  

  annotate("text", x = pos_WAS$x_plot, y = pos_WAS$y_plot - 1.5, 
           label = "#奇才是反向雷霆\n#攻防效率都是联盟29名", 
           color = "#E41231", fontface = "bold", size = 3.5, vjust = 1, lineheight = 0.9) +
  
  annotate("text", x = pos_MIA$x_plot-1.5, y = pos_MIA$y_plot , 
           label = "#猛龙被遮挡\n#在热火和太阳的下面", 
           color = "#F9A01B", fontface = "bold", size = 3.5, hjust = 1, lineheight = 0.9) +
  
  annotate("text", x = pos_SAS$x_plot-1.5, y = pos_SAS$y_plot , 
           label = "#马刺与火箭攻防接近\n#因此前者被后者遮挡", 
           color = "black", fontface = "bold", size = 3.5, hjust = 1, lineheight = 0.9) +
  # 2. DEN: 进攻天花板
  annotate("text", x = pos_CHI$x_plot + 1.5, y = pos_CHI$y_plot, 
           label = "#部分球队攻防数据接近\n#雄鹿被公牛遮挡\n#开拓者、老鹰被灰熊遮挡", 
           color = "#CD1F43", fontface = "bold", size = 3.5, hjust = 0, lineheight = 0.9) +

  
  coord_fixed(ratio = 1, clip = "off") +
  
  labs(
    title = "NBA本赛季球队攻防效率对比图",
    # 【修改】把 ** 改为 <b>，并建议加个 span 确保字体
    subtitle = "数据源：CTG (垃圾时间已剔除) | 制图：<span style='font-family:Microsoft YaHei; font-size:22px; color:black;'>**俗人罢了**</span> | 更新于全明星",
    caption = NULL  
  )+
  theme_void() +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "in"),
    plot.title = element_text(face="bold", size=25, hjust=0.5, margin=margin(t=10, b=10), family = "Microsoft YaHei"),
    
    # 【修改】加入 family = "Microsoft YaHei"
    plot.subtitle = element_markdown(size=15, hjust=0.5, color="gray50", margin=margin(b=10), family = "Microsoft YaHei")
  )

# --- 6. 保存与浏览 ---
file_name <- "NBA_CTG_Efficiency_Diamond.png"
ggsave(file_name, p_final, width = 10, height = 10, dpi = 300)