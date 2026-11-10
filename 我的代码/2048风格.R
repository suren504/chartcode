# --- 1. 加载工具 ---
library(tidyverse)
library(ragg) 

# --- 2. 准备数据 ---
csv_text <- "
Pick,MVP,FMVP,一阵,最佳阵容,全明星,最佳防守阵容,DPOY,总冠军
状元,14.67%,10.67%,24.00%,38.67%,65.33%,21.33%,4.00%,32.00%
榜眼,6.67%,6.67%,18.67%,28.00%,42.67%,13.33%,5.33%,25.33%
探花,4.00%,2.67%,17.33%,26.67%,45.33%,12.00%,1.33%,21.33%
第4顺位,3.95%,0.00%,5.26%,14.47%,34.21%,9.21%,2.63%,23.68%
第5顺位,2.63%,1.32%,10.53%,22.37%,35.53%,11.84%,2.63%,26.32%
第6顺位,1.30%,1.30%,3.90%,6.49%,19.48%,6.49%,1.30%,20.78%
第7顺位,1.41%,2.82%,5.63%,9.86%,16.90%,7.04%,1.41%,23.94%
第8顺位,1.35%,1.35%,2.70%,12.16%,20.27%,5.41%,0.00%,14.86%
第9顺位,1.33%,4.00%,5.33%,12.00%,24.00%,5.33%,1.33%,14.67%
第10顺位,0.00%,1.37%,2.74%,12.33%,24.66%,12.33%,0.00%,28.77%
第11顺位,0.00%,0.00%,1.49%,8.96%,20.90%,5.97%,0.00%,13.43%
第12顺位,1.43%,1.43%,1.43%,5.71%,12.86%,2.86%,0.00%,17.14%
第13顺位,2.78%,1.39%,4.17%,6.94%,12.50%,6.94%,0.00%,15.28%
第14顺位,0.00%,0.00%,2.86%,5.71%,10.00%,5.71%,0.00%,17.14%
"

# 清洗数据
plot_data <- read_csv(csv_text, show_col_types = FALSE) %>%
  pivot_longer(cols = -Pick, names_to = "Achievement", values_to = "Label") %>%
  mutate(
    # 处理空值和百分号
    Label = ifelse(is.na(Label) | Label == "", "0.00%", Label),
    Value = as.numeric(str_remove(Label, "%")),
    Value = replace_na(Value, 0),
    
    # --- 【核心修复点】 ---
    # 这里的 levels 必须和 CSV 里的中文一模一样，电脑才能对号入座
    Pick = factor(Pick, levels = c(
      "状元", "榜眼", "探花", 
      "第4顺位", "第5顺位", "第6顺位", "第7顺位", "第8顺位", 
      "第9顺位", "第10顺位", "第11顺位", "第12顺位", "第13顺位", "第14顺位"
    )),
    
    # Y轴排序：从低到高写，画出来就是从下到上
    Achievement = factor(Achievement, levels = c(
      "全明星", "最佳防守阵容", "DPOY", 
      "最佳阵容", "一阵", "总冠军", "FMVP", "MVP"
    ))
  )

# --- 3. 绘图 ---
p <- ggplot(plot_data, aes(x = Pick, y = Achievement)) +
  # 1. 画方块 (2048红白配色)
  geom_tile(aes(fill = Value), color = 'floralwhite', linewidth = 1.5) +
  
  # 2. 写数字：【指定字体为 Consolas】
  # 增加了 fontface = 'bold' 参数
  geom_text(aes(label = Label), family = 'Consolas', fontface = 'bold', size = 3, color = '#2C2F2B') +
  
  # 3. 颜色映射
  scale_fill_gradient(low = '#F2F2F2', high = '#F7565A') +
  
  # 4. 强制正方形格子
  coord_fixed() +
  
  # 5. 主题设置
  theme_minimal() +
  theme(
    # --- 全图背景米白 ---
    plot.background = element_rect(fill = "floralwhite", color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA),
    
    # 全局字体
    text = element_text(family = "Microsoft YaHei"),
    
    # 隐藏图例
    legend.position = 'none',
    
    # 标题设置
    plot.title = element_text(face = 'bold', size = 18, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 9, color = 'grey40', hjust = 0.5, margin = margin(b = 20)),
    
    # 坐标轴调整
    axis.text.x = element_text(angle =45, hjust = 0.5, color = 'grey30'), 
    axis.text.y = element_text(color = 'grey30', face = "bold"),         
    axis.title = element_blank(), 
    
    # 去掉杂线
    panel.grid = element_blank(),
    
    # 【注意】这里不要用负数，负数会把字切掉，我们用 width 控制宽度即可
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # 6. 标签内容
  labs(
    title = "NBA 乐透秀 (1-14顺位) 各项荣誉成才率对比",
    subtitle = "数据源：hoopshype | 制作：俗人罢了 | 统计截止：2023-24赛季结束 | 制作时间：2026.1.1"
  )

# --- 4. 保存图片 ---
# width = 9.5 能完美裁掉左右空白
ggsave("NBA_Lottery_Fixed.png", plot = p, device = agg_png, 
       width = 9.5, height = 6.5, dpi = 300, bg = "floralwhite")

# 打开查看
browseURL("NBA_Lottery_Fixed.png")