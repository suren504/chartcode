suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

parquet_file <- "/Users/suren/Downloads/play_by_play_2027.parquet"
teamcolor_file <- "/Users/suren/Desktop/nba_site/src/lib/data/teamcolor.json"
logo_dir <- "/Users/suren/Desktop/nba_site/static/NBA/logo"
output_file <- "凯尔特人和马刺场均净胜分排名变化.png"
featured_teams <- c("BOS", "SAS")

teams <- jsonlite::fromJSON(teamcolor_file)
primary_colors <- setNames(teams$team_color, teams$team_abbreviation)
logo_paths <- setNames(file.path(logo_dir, paste0(teams$team_id, ".webp")),
                       teams$team_abbreviation)

font_zh <- "Hiragino Sans GB"
for (font_path in Sys.glob(
  "/System/Library/AssetsV2/com_apple_MobileAsset_Font*/*.asset/AssetData/PingFang.ttc"
)) {
  faces <- tryCatch(
    systemfonts::font_info(path = rep(font_path, 2L), index = c(3L, 11L)),
    error = function(e) NULL
  )
  if (identical(faces$family, rep("PingFang SC", 2L)) &&
      identical(faces$style, c("Regular", "Semibold"))) {
    font_zh <- "NBA Chart PingFang"
    systemfonts::register_font(
      font_zh, plain = list(font_path, 3L), bold = list(font_path, 11L)
    )
    break
  }
}

# 只用 Parquet 逐场计算净胜分，并按赛季累计场均净胜分排出联盟名次。
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
games <- tryCatch({
  parquet_path <- DBI::dbQuoteString(con, parquet_file)
  DBI::dbGetQuery(con, paste0(
    "SELECT MAX(game_date) AS game_date, ",
    "MAX(home_team_abbrev) AS home_team, MAX(away_team_abbrev) AS away_team, ",
    "MAX(home_score) AS home_score, MAX(away_score) AS away_score ",
    "FROM read_parquet(", parquet_path, ") GROUP BY game_id"
  ))
}, finally = DBI::dbDisconnect(con))

daily_games <- bind_rows(
  transmute(games, game_date, team = home_team, margin = home_score - away_score),
  transmute(games, game_date, team = away_team, margin = away_score - home_score)
) %>%
  mutate(
    game_date = as.Date(game_date) + 1L,
    team = recode(team, GS = "GSW", NO = "NOP", NY = "NYK",
                  SA = "SAS", UTAH = "UTA", WSH = "WAS")
  ) %>%
  filter(team %in% names(primary_colors), !is.na(game_date), !is.na(margin)) %>%
  summarise(points = sum(margin), games = n(), .by = c(team, game_date))

season_dates <- seq(min(daily_games$game_date), max(daily_games$game_date), by = "day")
trends <- daily_games %>%
  complete(team, game_date = season_dates, fill = list(points = 0, games = 0L)) %>%
  arrange(team, game_date) %>%
  group_by(team) %>%
  mutate(points = cumsum(points), games = cumsum(games)) %>%
  ungroup() %>%
  filter(games > 0L) %>%
  mutate(rank = min_rank(desc(points / games)), .by = game_date) %>%
  mutate(day = match(game_date, season_dates) - 1L)

last_day <- length(season_dates) - 1L
bottom_teams <- trends %>%
  filter(day == last_day) %>%
  arrange(desc(rank), team) %>%
  slice_head(n = 2L) %>%
  pull(team)
highlight_teams <- union(featured_teams, bottom_teams)

# 曲线和色带共用 LOESS 拟合值，最后两天渐近实际终点，保留真实并列。
smooth_trends <- trends %>%
  group_by(team) %>%
  group_modify(~ {
    sample_days <- seq(min(.x$day), max(.x$day), by = 1 / 8)
    fit <- ggplot(.x, aes(day, rank)) +
      geom_smooth(method = "loess", formula = y ~ x, span = 0.40,
                  se = FALSE, xseq = sample_days)
    fitted <- layer_data(fit)
    final_rank <- .x$rank[which.max(.x$day)]
    end_weight <- pmax(0, 1 - (max(sample_days) - fitted$x) / 2)^2
    adjusted <- fitted$y + end_weight * (final_rank - last(fitted$y))
    tibble(day = fitted$x, rank = pmin(length(primary_colors), pmax(1, adjusted)))
  }) %>%
  ungroup()

highlight_curves <- filter(smooth_trends, team %in% highlight_teams)
background_curves <- filter(smooth_trends, !team %in% highlight_teams)

curve_ranks <- highlight_curves %>%
  filter(team %in% featured_teams) %>%
  select(day, team, rank) %>%
  pivot_wider(names_from = team, values_from = rank) %>%
  arrange(day)
rank_matrix <- as.matrix(select(curve_ranks, all_of(featured_teams)))

# 两条高亮曲线交叉时切换色带颜色。
interval_band <- function(i) {
  from <- rank_matrix[i, ]
  to <- rank_matrix[i + 1L, ]
  if (anyNA(from) || anyNA(to)) return(NULL)

  start_gap <- from[1L] - from[2L]
  end_gap <- to[1L] - to[2L]
  cuts <- c(0, 1)
  if (start_gap * end_gap < 0) {
    cuts <- c(0, start_gap / (start_gap - end_gap), 1)
  }

  start_day <- curve_ranks$day[i]
  day_span <- curve_ranks$day[i + 1L] - start_day
  shift <- to - from

  bind_rows(lapply(seq_len(length(cuts) - 1L), function(j) {
    left <- cuts[j]
    right <- cuts[j + 1L]
    midpoint <- from + shift * (left + right) / 2
    upper <- which.min(midpoint)
    lower <- 3L - upper

    tibble(
      upper = colnames(rank_matrix)[upper],
      lower = colnames(rank_matrix)[lower],
      left_day = start_day + day_span * left,
      right_day = start_day + day_span * right,
      upper_left = from[upper] + shift[upper] * left,
      upper_right = from[upper] + shift[upper] * right,
      lower_left = from[lower] + shift[lower] * left,
      lower_right = from[lower] + shift[lower] * right
    )
  }))
}

bands <- bind_rows(lapply(seq_len(nrow(curve_ranks) - 1L), interval_band)) %>%
  arrange(upper, lower, left_day) %>%
  group_by(upper, lower) %>%
  mutate(piece = cumsum(left_day - lag(right_day, default = -Inf) > 1e-8)) %>%
  group_by(upper, lower, piece) %>%
  reframe(
    day = c(left_day, last(right_day), rev(c(left_day, last(right_day)))),
    rank = c(upper_left, last(upper_right), rev(c(lower_left, last(lower_right))))
  ) %>%
  mutate(band = interaction(upper, lower, piece, drop = TRUE))

endpoints <- trends %>%
  filter(day == last_day, team %in% highlight_teams) %>%
  arrange(rank, team) %>%
  mutate(
    logo_offset = (row_number() - 1L) * 2.40,
    logo_rank = cummax(rank - logo_offset) + logo_offset,
    logo_path = unname(logo_paths[team])
  )

date_ticks <- seq(0, last_day, by = 4)
date_ticks <- unique(c(0L, date_ticks[date_ticks <= last_day - 3L], last_day))
tick_dates <- season_dates[date_ticks + 1L]
date_labels <- sprintf(
  "<span style='font-family: Menlo;'>%d</span>月<span style='font-family: Menlo;'>%d</span>日",
  as.integer(format(tick_dates, "%m")), as.integer(format(tick_dates, "%d"))
)
bottom_rank <- max(length(primary_colors) + 0.75, endpoints$logo_rank + 1.35)
rank_ticks <- c(1L, seq_len(floor(length(primary_colors) / 5)) * 5L)
rank_labels <- sprintf("第<span style='font-family: Menlo;'>%d</span>名", rank_ticks)
background <- "floralwhite"

season_label <- paste0(
  "<span style='font-family: Menlo;'>[2026-27</span>常规赛",
  "<span style='font-family: Menlo;'>]</span>球队场均净效率排名"
)
headline <- sprintf(
  paste0(
    "<span style='color: %s;'>凯尔特人</span>",
    "<span style='font-family: Menlo;'>&amp;</span>",
    "<span style='color: %s;'>马刺</span>并列第一"
  ),
  primary_colors[["BOS"]],
  teams$team_alternate_color[match("SAS", teams$team_abbreviation)]
)

plot <- ggplot(trends, aes(day, rank)) +
  geom_hline(yintercept = rank_ticks, color = "#343942", alpha = 0.11, linewidth = 0.25) +
  geom_vline(xintercept = date_ticks, color = "#343942", alpha = 0.17,
             linetype = "dotted", linewidth = 0.32) +
  geom_smooth(
    data = background_curves, aes(group = team), stat = "identity",
    se = FALSE, color = "#8B929B43", linewidth = 0.40, lineend = "round"
  ) +
  geom_polygon(
    data = bands, aes(day, rank, group = band, fill = upper),
    inherit.aes = FALSE, alpha = 0.09
  ) +
  geom_line(
    data = highlight_curves, aes(color = team, group = team), alpha = 0.07,
    linewidth = 4.5, lineend = "round", linejoin = "round"
  ) +
  geom_line(
    data = highlight_curves, aes(color = team, group = team), alpha = 0.13,
    linewidth = 3.1, lineend = "round", linejoin = "round"
  ) +
  geom_line(
    data = highlight_curves, aes(color = team, group = team), alpha = 0.20,
    linewidth = 2.1, lineend = "round", linejoin = "round"
  ) +
  geom_smooth(
    data = highlight_curves, aes(color = team, group = team), stat = "identity",
    se = FALSE, linewidth = 1.12, lineend = "round", linejoin = "round"
  ) +
  geom_segment(
    data = endpoints, aes(xend = day + 0.70, yend = logo_rank, color = team),
    linewidth = 0.52, lineend = "round"
  ) +
  ggpath::geom_from_path(
    data = endpoints, aes(x = day + 1.16, y = logo_rank, path = logo_path),
    inherit.aes = FALSE, width = 0.046, height = 0.071
  ) +
  scale_color_manual(values = primary_colors, guide = "none") +
  scale_fill_manual(values = primary_colors, guide = "none") +
  scale_x_continuous(
    breaks = date_ticks, labels = date_labels,
    limits = c(-0.22, last_day + 2.0), expand = expansion()
  ) +
  scale_y_reverse(
    breaks = rank_ticks, labels = rank_labels,
    limits = c(bottom_rank, -0.4), expand = expansion()
  ) +
  labs(title = season_label, subtitle = headline, x = NULL, y = NULL) +
  theme_minimal(base_family = "Menlo", base_size = 11) +
  theme(
    plot.background = element_rect(fill = background, color = NA),
    panel.background = element_rect(fill = background, color = NA),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = ggtext::element_markdown(
      family = font_zh, size = 13, hjust = 0.5,
      color = "#515864", margin = margin(b = 5)
    ),
    plot.subtitle = ggtext::element_markdown(
      family = font_zh, face = "bold", size = 30, hjust = 0.5,
      color = "#24282F", margin = margin(b = 17)
    ),
    axis.text = element_text(color = "#515864"),
    axis.text.x.bottom = ggtext::element_markdown(
      family = font_zh, size = 10, hjust = 0.5, margin = margin(t = 10)
    ),
    axis.text.y.left = ggtext::element_markdown(
      family = font_zh, size = 10, hjust = 1, margin = margin(r = 9)
    ),
    plot.margin = margin(22, 18, 22, 24)
  )

ggsave(output_file, plot, width = 1920, height = 1080, units = "px", dpi = 160,
       device = ragg::agg_png, bg = background)
