library(shiny)
library(bslib)
library(tidyverse)
library(gt)
library(gtExtras)
library(glue)
library(readr)
library(scales)
library(ggimage)

addResourcePath(prefix = "headshots", directoryPath = "D:/nba/www")
addResourcePath(prefix = "team_logos", directoryPath = "D:/nba/网站自动工作抓取/nba-data-suren/team_logos")


nba_team_master <- tibble::tribble(
  ~team_abbreviation, ~team_name_cn, ~team_color,
  "ATL", "老鹰",   "#E03A3E", "BOS", "凯尔特人", "#008348", "BKN", "篮网",   "#666666",
  "CHA", "黄蜂",   "#00788C", "CHI", "公牛",   "#CD1F43", "CLE", "骑士",   "#860038",
  "DAL", "独行侠", "#0066B5", "DEN", "掘金",   "#FFD76A", "DET", "活塞",   "#C8102E",
  "GSW", "勇士",   "#FFD36F", "HOU", "火箭",   "#CE1141", "IND", "步行者", "#FEBC30",
  "LAC", "快船",   "#12173F", "LAL", "湖人",   "#360072", "MEM", "灰熊",   "#5D76A9",
  "MIA", "热火",   "#961A31", "MIL", "雄鹿",   "#004C26", "MIN", "森林狼", "#266092",
  "NOP", "鹈鹕",   "#B89A5B", "NYK", "尼克斯", "#F58426", "OKC", "雷霆",   "#007AC0",
  "ORL", "魔术",   "#0B77BD", "PHI", "76人",   "#1D428A", "PHX", "太阳",   "#EF9C6C",
  "POR", "开拓者", "#CF0A2C", "SAC", "国王",   "#592D81", "SAS", "马刺",   "#E1E7EB",
  "TOR", "猛龙",   "#F00940", "UTA", "爵士",   "#502C86", "WAS", "奇才",   "#00275D"
)

url_live <- "D:/nba/nba_daily_live.csv"
url_teams <- "D:/nba/nba_30teams_data_2025_26.csv"
url_duos <- "D:/nba/nba_Assist_Duos.csv"

live_raw <- tryCatch({
  read_csv(url_live, show_col_types = FALSE) %>%
    mutate(across(any_of(c("min", "pts", "fgm", "fga", "fg3m", "fg3a", "ftm", "fta", "oreb", "dreb", "reb", "ast", "tov", "stl", "blk", "pf", "plus_minus")), ~suppressWarnings(as.numeric(.))))
}, error = function(e) { tibble() })

df_teams_raw <- tryCatch({
  read_csv(url_teams, show_col_types = FALSE) %>%
    mutate(across(any_of(c("Points", "OffPoss", "OpponentPoints", "DefPoss", "Pace", "Assists", "AssistPoints", "DefFGReboundPct", "OffFGReboundPct", "EfgPct", "TsPct", "Avg2ptShotDistance", "Avg3ptShotDistance", "SecondsPerPossOff", "SecondsPerPossDef", "Travels", "Turnovers", "Steals", "Fg2Pct", "Fg3Pct", "AtRimFrequency", "AtRimAccuracy", "ShortMidRangeFrequency", "ShortMidRangeAccuracy", "LongMidRangeFrequency", "LongMidRangeAccuracy", "Corner3Frequency", "Corner3Accuracy", "Arc3Frequency", "Arc3Accuracy", "FTA", "FtPoints", "AtRimFG3AFrequency", "ShootingFoulsDrawnPct", "TwoPtShootingFoulsDrawnPct", "ThreePtShootingFoulsDrawnPct", "ShotQualityAvg", "BadPassSteals", "LostBallSteals", "LiveBallTurnovers", "LostBallTurnovers", "Charge Fouls", "FoulsDrawn", "Offensive Fouls Drawn")), ~suppressWarnings(as.numeric(.))))
}, error = function(e) { tibble() })

df_teams_processed <- df_teams_raw %>%
  rename_with(~ "team_abbreviation", any_of(c("TeamAbbreviation", "Abbreviation", "TEAM"))) %>%
  left_join(nba_team_master, by = "team_abbreviation") %>%
  mutate(
    `进攻效率` = Points / OffPoss * 100, `防守效率` = OpponentPoints / DefPoss * 100,
    `净效率（球队综合实力）` = (Points / OffPoss * 100) - (OpponentPoints / DefPoss * 100),
    `节奏` = Pace, `助攻` = Assists / OffPoss * 100, `助攻得分` = AssistPoints / OffPoss * 100, 
    `防守篮板率` = DefFGReboundPct, `进攻篮板率` = OffFGReboundPct, `有效命中率` = EfgPct, `真实命中率` = TsPct,
    `平均两分出手距离` = Avg2ptShotDistance, `平均三分出手距离` = Avg3ptShotDistance, `进攻节奏（秒）` = SecondsPerPossOff, `防守节奏（秒）` = SecondsPerPossDef,
    `走步次数` = Travels / OffPoss * 100, `失误` = Turnovers / OffPoss * 100, `抢断` = Steals / DefPoss * 100,
    `两分命中率` = Fg2Pct, `三分命中率` = Fg3Pct, `篮下出手频率` = AtRimFrequency, `篮下命中率` = AtRimAccuracy,
    `短中距离出手频率` = ShortMidRangeFrequency, `短中距离命中率` = ShortMidRangeAccuracy, `长中距离出手频率` = LongMidRangeFrequency, `长中距离命中率` = LongMidRangeAccuracy,
    `底角三分出手频率` = Corner3Frequency, `底角三分命中率` = Corner3Accuracy, `弧顶三分出手频率` = Arc3Frequency, `弧顶三分命中率` = Arc3Accuracy,
    `罚球出手` = FTA / OffPoss * 100, `罚球得分` = FtPoints / OffPoss * 100, `篮下&三分出手频率（魔球率）` = AtRimFG3AFrequency,
    `投篮造犯规率` = ShootingFoulsDrawnPct, `两分造犯规率` = TwoPtShootingFoulsDrawnPct, `三分造犯规率` = ThreePtShootingFoulsDrawnPct,
    `投篮质量` = ShotQualityAvg, 
    `抢断(传球)` = BadPassSteals / DefPoss * 100, 
    `抢断(丢球)` = LostBallSteals / DefPoss * 100, 
    `活球失误` = LiveBallTurnovers / OffPoss * 100, 
    `丢球失误` = LostBallTurnovers / OffPoss * 100, 
    `进攻犯规(撞人)` = `Charge Fouls` / OffPoss * 100, 
    `造犯规` = FoulsDrawn / OffPoss * 100, 
    `造进攻犯规` = `Offensive Fouls Drawn` / DefPoss * 100
  )
latest_game_date <- max(as.Date(live_raw$game_date), na.rm = TRUE)

df_daily_best <- live_raw %>%
  filter(as.Date(game_date) == latest_game_date) %>%
  mutate(minutes = as.numeric(min)) %>%
  filter(minutes >= 20) %>%
  left_join(nba_team_master, by = "team_abbreviation") %>%
  mutate(
    GmSc = pts + 0.4 * fgm - 0.7 * fga - 0.4 * (fta - ftm) + 0.7 * oreb + 0.3 * dreb + stl + 0.7 * ast + 0.7 * blk - 0.4 * pf - tov,
    gBPM = -0.55 * fga + 2.43 * fg3m + 1.39 * (fgm - fg3m) + 0.79 * ftm - 0.25 * fta + 0.17 * oreb + 0.27 * dreb + 0.43 * ast + 1.34 * stl + 0.64 * blk - 0.95 * tov - 0.34 * pf,
    team_color_hex = coalesce(team_color, "#f0f0f0"),
    head_html = glue("<img src='headshots/{player_id}.png' style='height:70px; width:70px; border-radius:50%; background-color:{team_color_hex}; object-fit:cover;'>"),
    logo_html = glue("<div style='display:flex; flex-direction:column; align-items:center;'><img src='team_logos/{team_abbreviation}.png' style='height:65px; width:65px; object-fit:contain;'><span style='font-size:14px; font-weight:bold; color:#333; margin-top:2px;'>{team_name_cn}</span></div>"),
    fgm_a = paste(fgm, fga, sep = "/"), fg3m_a = paste(fg3m, fg3a, sep = "/"), ftm_a = paste(ftm, fta, sep = "/")
  ) %>%
  arrange(desc(gBPM)) %>%
  select(logo_html, head_html, athlete_display_name = player_name, minutes, points = pts, offensive_rebounds = oreb, defensive_rebounds = dreb, rebounds = reb, assists = ast, turnovers = tov, steals = stl, blocks = blk, fgm_a, fg3m_a, ftm_a, plus_minus, GmSc, gBPM)

df_season_base <- live_raw %>%
  mutate(
    minutes = as.numeric(min),
    opponent_abbr = case_when(
      str_detect(matchup, " vs\\. ") ~ str_trim(str_extract(matchup, "(?<=vs\\. ).+$")),
      str_detect(matchup, " @ ") ~ str_trim(str_extract(matchup, "(?<=@ ).+$")),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(minutes >= 20) %>%
  mutate(
    GmSc = pts + 0.4 * fgm - 0.7 * fga - 0.4 * (fta - ftm) + 0.7 * oreb + 0.3 * dreb + stl + 0.7 * ast + 0.7 * blk - 0.4 * pf - tov,
    gBPM = -0.55 * fga + 2.43 * fg3m + 1.39 * (fgm - fg3m) + 0.79 * ftm - 0.25 * fta + 0.17 * oreb + 0.27 * dreb + 0.43 * ast + 1.34 * stl + 0.64 * blk - 0.95 * tov - 0.34 * pf
  )

df_season_best_view <- bind_rows(
  df_season_base %>% slice_max(order_by = gBPM, n = 100), 
  df_season_base %>% slice_min(order_by = gBPM, n = 100)  
) %>%
  left_join(nba_team_master, by = "team_abbreviation") %>%
  left_join(nba_team_master %>% select(team_abbreviation, opponent_name_cn = team_name_cn), by = c("opponent_abbr" = "team_abbreviation")) %>%
  mutate(
    opponent_name_cn = coalesce(opponent_name_cn, opponent_abbr),
    date_opponent = glue("{format(as.Date(game_date) + 1, '%Y/%m/%d')}<br>{opponent_name_cn}"),
    team_color_hex = coalesce(team_color, "#f0f0f0"),
    head_html = glue("<img src='headshots/{player_id}.png' style='height:70px; width:70px; border-radius:50%; background-color:{team_color_hex}; object-fit:cover;'>"),
    logo_html = glue("<div style='display:flex; flex-direction:column; align-items:center;'><img src='team_logos/{team_abbreviation}.png' style='height:65px; width:65px; object-fit:contain;'><span style='font-size:14px; font-weight:bold; color:#333; margin-top:2px;'>{team_name_cn}</span></div>"),
    fgm_a = paste(fgm, fga, sep = "/"), fg3m_a = paste(fg3m, fg3a, sep = "/"), ftm_a = paste(ftm, fta, sep = "/")
  ) %>%
  arrange(desc(gBPM)) %>%
  select(logo_html, head_html, athlete_display_name = player_name, date_opponent, minutes, points = pts, offensive_rebounds = oreb, defensive_rebounds = dreb, rebounds = reb, assists = ast, turnovers = tov, steals = stl, blocks = blk, fgm_a, fg3m_a, ftm_a, plus_minus, GmSc, gBPM)

# ==============================================================================
# 新增：球场拍档数据处理
# ==============================================================================
df_duos_raw <- tryCatch({
  read_csv(url_duos, show_col_types = FALSE) %>%
    mutate(across(any_of(c("Ast_2PT", "Ast_3PT", "Ast_Pts_Created", "Games_Together", "Ast_Per_Game", "Total_Ast")), as.numeric))
}, error = function(e) { tibble() })

df_duos_processed <- df_duos_raw %>%
  left_join(nba_team_master, by = c("Team" = "team_abbreviation")) %>%
  mutate(
    team_color_hex = coalesce(team_color, "#f0f0f0"),
    # 修改这里：调整球队LOGO大小（默认80px，比每日最佳稍大）
    logo_html = glue("<div style='display:flex; align-items:center; justify-content:center;'><img src='team_logos/{Team}.png' style='height:80px; width:80px; object-fit:contain;'><span style='display:none;'>{team_name_cn} {Team}</span></div>"),
    # 修改这里：调整传球人/终结者靓照大小（默认75px）
    passer_html = glue("<img src='headshots/{PasserID}.png' style='height:75px; width:75px; border-radius:50%; background-color:{team_color_hex}; object-fit:cover;'>"),
    scorer_html = glue("<img src='headshots/{ScorerID}.png' style='height:75px; width:75px; border-radius:50%; background-color:{team_color_hex}; object-fit:cover;'>")
  ) %>%
  arrange(desc(Total_Ast)) %>% # 默认按总助攻降序
  select(logo_html, passer_html, scorer_html, Ast_2PT, Ast_3PT, Ast_Pts_Created, Ast_Per_Game, Total_Ast)

ui <- page_navbar(
  title = "俗人的篮球场",
  navbar_options = navbar_options(collapsible = FALSE),
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  
  # 1. 每日最佳
  nav_panel(
    title = "🔥 每日最佳", 
    card(
      full_screen = TRUE, 
      card_header(tags$div(style = "display:flex; align-items:center; justify-content:flex-start; width:100%;", 
                           radioButtons("players_view", NULL, 
                                        choices = setNames(c("daily", "season"), 
                                                           c(glue("{format(as.Date(latest_game_date) + 1, '%m月%d日')} 参赛球员"), "本赛季参赛球员")), 
                                        selected = "daily", inline = TRUE))), 
      uiOutput("players_table_ui")
    )
  ),
  
  # 2. 球队对比
  nav_panel(
    title = "🏀 球队对比",
    layout_sidebar(
      sidebar = sidebar(
        width = 400, title = tags$h4("图表控制面板", class = "mb-4 fw-bold"),
        tags$div(
          class = "d-flex flex-column gap-4",
          tags$div(
            selectInput("x_var", "选择 X 轴数据 (横轴)", choices = c("进攻效率", "防守效率", "净效率（球队综合实力）", "节奏", "助攻", "助攻得分", "防守篮板率", "进攻篮板率", "有效命中率", "真实命中率", "平均两分出手距离", "平均三分出手距离", "进攻节奏（秒）", "防守节奏（秒）", "走步次数", "失误", "抢断", "两分命中率", "三分命中率", "篮下出手频率", "篮下命中率", "短中距离出手频率", "短中距离命中率", "长中距离出手频率", "长中距离命中率", "底角三分出手频率", "底角三分命中率", "弧顶三分出手频率", "弧顶三分命中率", "罚球出手", "罚球得分", "篮下&三分出手频率（魔球率）", "投篮造犯规率", "两分造犯规率", "三分造犯规率", "投篮质量", "抢断(传球)", "抢断(丢球)", "活球失误", "丢球失误", "进攻犯规(撞人)", "造犯规", "造进攻犯规"), selected = "进攻效率"),
            selectInput("y_var", "选择 Y 轴数据 (纵轴)", choices = c("进攻效率", "防守效率", "净效率（球队综合实力）", "节奏", "助攻", "助攻得分", "防守篮板率", "进攻篮板率", "有效命中率", "真实命中率", "平均两分出手距离", "平均三分出手距离", "进攻节奏（秒）", "防守节奏（秒）", "走步次数", "失误", "抢断", "两分命中率", "三分命中率", "篮下出手频率", "篮下命中率", "短中距离出手频率", "短中距离命中率", "长中距离出手频率", "长中距离命中率", "底角三分出手频率", "底角三分命中率", "弧顶三分出手频率", "弧顶三分命中率", "罚球出手", "罚球得分", "篮下&三分出手频率（魔球率）", "投篮造犯规率", "两分造犯规率", "三分造犯规率", "投篮质量", "抢断(传球)", "抢断(丢球)", "活球失误", "丢球失误", "进攻犯规(撞人)", "造犯规", "造进攻犯规"), selected = "防守效率")
          ),
          actionButton("swap_xy", "一键互换 X/Y 轴数据", class = "btn-dark w-100 py-2"),
          tags$div(checkboxInput("flip_x", "翻转 X 轴 (数值从大到小排列)", value = FALSE), checkboxInput("flip_y", "翻转 Y 轴 (数值从大到小排列)", value = FALSE)),
          sliderInput("logo_size", "调整 Logo 大小", min = 5, max = 20, value = 10),
          downloadButton("download_team_plot", "下载球队对比图", class = "btn-success w-100 py-2 mb-2"),
          helpText(HTML("提示：数据源 pbpstats.com<br>所有数据默认从小到大排序<br>因此记得留意甄别，如有必要请翻转所属X/Y轴"))
        )
      ),
      card(full_screen = TRUE, card_header("球队数据对比图"), plotOutput("team_scatter_plot", width = "100%", height = "800px"))
    )
  ),
  
  # 3. 球员榜单
  nav_panel(
    title = "📊 球员榜单", 
    card(card_header("全赛季球员高阶数据总榜"), p("🔨 正在施工中... 敬请期待！"))
  ), 
  
  # 4. 球场拍档 (现在它能进来了)
  nav_panel(
    title = "🤝 球场拍档",
    card(
      full_screen = TRUE,
      card_header(tags$b("双人组助攻数据细分 (按总助攻排序，只显示联盟前100位组合)")),
      gt_output("duos_table")
    )
  )
) # <--- 整个 page_navbar 在最后才闭合

server <- function(input, output, session) {
  season_gt_cache <- reactiveVal(NULL)
  output$players_table_ui <- renderUI({ if (isTruthy(input$players_view) && input$players_view == "season") { gt_output("season_gbpm_table") } else { gt_output("daily_gbpm_table") } })
  
  observeEvent(input$swap_xy, {
    current_x <- input$x_var
    current_y <- input$y_var
    updateSelectInput(session, "x_var", selected = current_y)
    updateSelectInput(session, "y_var", selected = current_x)
  })
  
  team_plot_data <- reactive({
    req(df_teams_processed, input$x_var, input$y_var)
    plot_data <- df_teams_processed %>%
      mutate(
        x_val = as.numeric(.data[[input$x_var]]),
        y_val = as.numeric(.data[[input$y_var]]),
        logo_local_path = file.path("D:/nba/网站自动工作抓取/nba-data-suren/team_logos", 
                                    paste0(team_abbreviation, ".png"))
      )
    x_mean <- mean(plot_data$x_val, na.rm = TRUE)
    y_mean <- mean(plot_data$y_val, na.rm = TRUE)
    
    my_layers <- list(
      geom_vline(xintercept = x_mean, linetype = "dashed", color = "gray50", linewidth = 0.6),
      geom_hline(yintercept = y_mean, linetype = "dashed", color = "gray50", linewidth = 0.6),
      geom_image(aes(image = logo_local_path), size = input$logo_size / 100),
      labs(
        title = NULL, 
        subtitle = glue("X轴：{input$x_var} | Y轴：{input$y_var}"),
        x = input$x_var, y = input$y_var, caption = "www.surennba.com"
      ),
      theme_minimal(base_size = 14),
      theme(
        aspect.ratio = 1, 
        plot.subtitle = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
        plot.caption = element_text(size = 16, color = "gray40", hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold", size = 14),
        axis.line = element_line(color = "black", linewidth = 0.8), 
        axis.ticks = element_line(color = "black", linewidth = 0.8),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#F0F0F0"), 
        plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA)
      )
    )
    if (isTruthy(input$flip_x)) my_layers <- append(my_layers, list(scale_x_reverse()))
    if (isTruthy(input$flip_y)) my_layers <- append(my_layers, list(scale_y_reverse()))
    
    ggplot(plot_data, aes(x = x_val, y = y_val)) + my_layers
  })
  
  output$team_scatter_plot <- renderPlot({
    team_plot_data()
  }, res = 96)
  
  output$download_team_plot <- downloadHandler(
    filename = function() { paste0("SurenNBA_Team_Plot_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = team_plot_data(), width = 10, height = 10, dpi = 300, bg = "white") }
  )
  
  
  output$daily_gbpm_table <- render_gt({
    df_daily_best %>% gt(id = "gbpm_table") %>% fmt_markdown(columns = c(logo_html, head_html)) %>% fmt_number(columns = plus_minus, force_sign = TRUE, decimals = 0) %>% fmt_number(columns = c(GmSc, gBPM), decimals = 1) %>% cols_label(logo_html = "球队", head_html = "靓照", athlete_display_name = "球员", minutes = "时间", points = "得分", offensive_rebounds = "进攻篮板", defensive_rebounds = "防守篮板", rebounds = "总篮板", assists = "助攻", turnovers = "失误", steals = "抢断", blocks = "盖帽", fgm_a = "投篮", fg3m_a = "三分", ftm_a = "罚球", plus_minus = "+/-", GmSc = html("<span title='GmSc 公式...' style='cursor: help; border-bottom: 1px dashed #666;'>GmSc</span>"), gBPM = html("<span title='gBPM 公式...' style='cursor: help; border-bottom: 1px dashed #666;'>gBPM</span>")) %>% tab_header(title = md(glue("今日最佳")), subtitle = md(glue("基于单场一体化数据gBPM排序|只统计出场时间不小于20分钟的球员<br><span style='color:#e74c3c; font-weight:bold;'>💡 提示：右上角 Search 框支持跨维度搜索（搜球队用中文，搜球员用英文，比如“湖人”或“james”）</span>"))) %>% data_color(columns = gBPM, palette = c("#FEE2E2", "#D1FAE5"), domain = range(df_daily_best$gBPM, na.rm = TRUE)) %>% data_color(columns = GmSc, palette = c("#FEE2E2", "#D1FAE5"), domain = range(df_daily_best$GmSc, na.rm = TRUE)) %>% opt_row_striping() %>% cols_align(align = "center", columns = everything()) %>% tab_style(style = cell_text(font = "Consolas", size = px(25)), locations = cells_body(columns = everything())) %>% tab_style(style = cell_text(size = px(15)), locations = cells_body(columns = athlete_display_name)) %>% opt_interactive(use_sorting = TRUE, use_search = TRUE, use_pagination = TRUE, page_size_default = 10) %>% tab_options(table.width = pct(100), data_row.padding = px(2), column_labels.font.weight = "bold") %>% cols_width(logo_html ~ px(90), head_html ~ px(90), athlete_display_name ~ px(180), c(GmSc, gBPM) ~ px(120)) %>% opt_css(css = ".navbar-nav .nav-link { font-family: 'Microsoft YaHei', sans-serif !important; font-size: 18px !important; font-weight: 500 !important; margin-left: 15px; } .navbar-brand { font-family: 'SimHei', serif !important; font-size: 24px !important; font-weight: bold !important; } #gbpm_table, #gbpm_table .rt-table, #gbpm_table .gt_table { width: 100% !important; max-width: 100% !important; } #gbpm_table td, #gbpm_table th, .gt_table td, .gt_table th { vertical-align: middle !important; } .gt_row, .gt_center, .gt_left { vertical-align: middle !important; } #gbpm_table .rt-td, #gbpm_table .rt-td-inner { display: flex !important; align-items: center !important; justify-content: center !important; font-size: 25px !important; } #gbpm_table .rt-td:nth-child(3) { justify-content: flex-start !important; font-size: 15px !important; }")
  })
  
  output$season_gbpm_table <- render_gt({
    if (is.null(season_gt_cache())) {
      season_gt_cache(
        df_season_best_view %>% gt(id = "gbpm_season_table") %>% fmt_markdown(columns = c(logo_html, head_html, date_opponent)) %>% fmt_number(columns = plus_minus, force_sign = TRUE, decimals = 0) %>% fmt_number(columns = c(GmSc, gBPM), decimals = 1) %>% cols_label(logo_html = "球队", head_html = "靓照", athlete_display_name = "球员", date_opponent = "日期/对手", minutes = "时间", points = "得分", offensive_rebounds = "进攻篮板", defensive_rebounds = "防守篮板", rebounds = "总篮板", assists = "助攻", turnovers = "失误", steals = "抢断", blocks = "盖帽", fgm_a = "投篮", fg3m_a = "三分", ftm_a = "罚球", plus_minus = "+/-", GmSc = html("<span title='GmSc' style='cursor: help; border-bottom: 1px dashed #666;'>GmSc</span>"), gBPM = html("<span title='gBPM' style='cursor: help; border-bottom: 1px dashed #666;'>gBPM</span>")) %>% tab_header(title = md(glue("本赛季最强单场表演")), subtitle = md(glue("基于单场一体化数据gBPM从高到低展示|只展示gbpm前100与倒数前100的单场数据<br><span style='color:#e74c3c; font-weight:bold;'>💡 提示：右上角 Search 框支持跨维度搜索</span>"))) %>% data_color(columns = gBPM, palette = c("#FEE2E2", "#D1FAE5"), domain = range(df_season_best_view$gBPM, na.rm = TRUE)) %>% data_color(columns = GmSc, palette = c("#FEE2E2", "#D1FAE5"), domain = range(df_season_best_view$GmSc, na.rm = TRUE)) %>% opt_row_striping() %>% cols_align(align = "center", columns = everything()) %>% tab_style(style = cell_text(font = "Consolas", size = px(25)), locations = cells_body(columns = everything())) %>% tab_style(style = cell_text(size = px(15)), locations = cells_body(columns = athlete_display_name)) %>% tab_style(style = cell_text(size = px(13)), locations = cells_body(columns = date_opponent)) %>% opt_interactive(use_sorting = TRUE, use_search = TRUE, use_pagination = TRUE, page_size_default = 10) %>% tab_options(table.width = pct(100), data_row.padding = px(2), column_labels.font.weight = "bold") %>% cols_width(logo_html ~ px(90), head_html ~ px(90), athlete_display_name ~ px(180), date_opponent ~ px(130), c(GmSc, gBPM) ~ px(120)) %>% opt_css(css = "#gbpm_season_table, #gbpm_season_table .rt-table, #gbpm_season_table .gt_table { width: 100% !important; max-width: 100% !important; } #gbpm_season_table td, #gbpm_season_table th, .gt_table td, .gt_table th { vertical-align: middle !important; } #gbpm_season_table .rt-td, #gbpm_season_table .rt-td-inner { display: flex !important; align-items: center !important; justify-content: center !important; font-size: 25px !important; } #gbpm_season_table .rt-td:nth-child(3) { justify-content: flex-start !important; font-size: 15px !important; } #gbpm_season_table .rt-td:nth-child(4) { font-size: 13px !important; line-height: 1.05 !important; padding-top: 0px !important; padding-bottom: 0px !important; }")
      )
    }
    season_gt_cache()
  })
  
  # ==============================================================================
  # 修复后：球场拍档表格渲染 (终极版：左右锚定，中间等距拉伸)
  # ==============================================================================
  output$duos_table <- render_gt({
    req(nrow(df_duos_processed) > 0)
    
    df_duos_processed %>%
      gt(id = "duos_gt_table") %>%
      fmt_markdown(columns = c(logo_html, passer_html, scorer_html)) %>%
      cols_label(
        logo_html = "球队",
        passer_html = "传球人",
        scorer_html = "终结者",
        Ast_2PT = "助攻两分",
        Ast_3PT = "助攻三分",
        Ast_Pts_Created = "助攻总得分",
        Ast_Per_Game = "场均助攻",
        Total_Ast = "助攻总数"
      ) %>%
      tab_header(
        title = md("👊 NBA 本赛季单向助攻次数最多的双人组"),
        subtitle = md("致敬永远的球场搭子<br><span style='color:#e74c3c; font-weight:bold;'> 温馨提示：支持通过右上角 Search 搜索球队中文或英文简写！例如“掘金”“DEN”，但不支持球员搜索</span>")
      ) %>%
      data_color(
        columns = Total_Ast,
        palette = c("#FEE2E2", "#D1FAE5"),
        domain = range(df_duos_processed$Total_Ast, na.rm = TRUE)
      ) %>%
      opt_row_striping() %>%
      # 🚨 修改这里：单独设定左右列的对齐方式，把它们“定死”在边缘
      cols_align(align = "center", columns = everything()) %>%
      cols_align(align = "center", columns = logo_html) %>%
      cols_align(align = "right", columns = Total_Ast) %>%
      tab_style(
        style = cell_text(font = "Consolas", size = px(25)), 
        locations = cells_body(columns = everything())       
      ) %>%
      opt_interactive(
        use_sorting = TRUE,
        use_search = TRUE,
        use_pagination = TRUE,
        page_size_default = 15
      ) %>%
      tab_options(
        table.width = pct(100),
        data_row.padding = px(2), 
        column_labels.font.weight = "bold"
      ) %>%
      cols_width(
        logo_html ~ pct(12),
        passer_html ~ pct(11),
        scorer_html ~ pct(11),
        Ast_2PT ~ pct(13),
        Ast_3PT ~ pct(13),
        Ast_Pts_Created ~ pct(13),
        Ast_Per_Game ~ pct(13),
        Total_Ast ~ pct(14)
      ) %>%
      opt_css(css = "
        #duos_gt_table, #duos_gt_table .rt-table, #duos_gt_table .gt_table { width: 100% !important; max-width: 100% !important; table-layout: fixed !important; } 
        #duos_gt_table td, #duos_gt_table th, .gt_table td, .gt_table th { vertical-align: middle !important; } 
        #duos_gt_table .rt-td, #duos_gt_table .rt-td-inner { display: flex !important; align-items: center !important; font-size: 25px !important; }
        #duos_gt_table .rt-align-right .rt-td-inner { justify-content: flex-end !important; padding-right: 15px !important; }
        #duos_gt_table .rt-align-center .rt-td-inner { justify-content: center !important; }
        #duos_gt_table .rt-align-right .rt-th-inner { justify-content: flex-end !important; padding-right: 15px !important; }
        #duos_gt_table .rt-align-center .rt-th-inner { justify-content: center !important; }
      ")
  })
}
shinyApp(ui, server)