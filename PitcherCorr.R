## ------------------------------------------------------------------------------------
## -------------------------------------- LIBRARIES & DATA -----------------------------------
## ------------------------------------------------------------------------------------

library(baseballr)
library(readr)
library(dplyr)
library(tidyverse)
library(spdep)
library(DescTools)
library(ggplot2)
library(tidyr)
library(ggtext)
library(knitr)


seasons = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)

fetch_season_data = function(season) {
  url = paste0(
    'https://baseballsavant.mlb.com/statcast_search/csv?all=true',
    '&hfGT=R%7C',
    '&hfSea=', season, '%7C',
    '&player_type=batter',
    '&batters_lookup%5B%5D=592450',
    '&min_pitches=0&min_results=0',
    '&group_by=name',
    '&sort_col=pitches',
    '&player_event_sort=h_launch_speed',
    '&sort_order=desc',
    '&min_abs=0',
    '&type=details'
  )
  
  message("Fetching season: ", season)
  
  tryCatch({
    df = read.csv(url)
    df$season = season
    return(df)
  }, error = function(e) {
    warning("Failed to fetch season ", season, ": ", e$message)
    return(NULL)
  })
}

# Fetch all seasons (this will take several minutes)
all_seasons_list = lapply(seasons, fetch_season_data)

# Remove any failed seasons and bind
data_raw = bind_rows(Filter(Negate(is.null), all_seasons_list))

variable.names(data_raw)

## ------------------------------------------------------------------------------------
## -------------------------------------- FEATURES -----------------------------------
## ------------------------------------------------------------------------------------

pitch_data = data_raw %>%
  select(
    player_name,
    batter,
    pitcher,
    p_throws,
    stand,
    events,
    plate_x,
    plate_z,
    sz_top,
    sz_bot,
    balls,
    strikes,
    outs_when_up,
    pitch_type,
    game_date,
    game_year,
    if_fielding_alignment,
    of_fielding_alignment
  )

# Aaron Judge vs Kevin Gausman & José Berrios
Matchup = pitch_data %>% 
  filter(
    batter == 592450,
    pitcher %in% c(592332, 621244),
    !is.na(plate_x),
    !is.na(plate_z),
    !is.na(sz_top),
    !is.na(sz_bot)
  )

med_top = median(Matchup$sz_top)
med_bot = median(Matchup$sz_bot)

Matchup %>% 
  filter(pitcher %in% c(592332, 621244)) %>%
  mutate(
    pitcher_name = case_when(
      pitcher == 592332 ~ "Kevin Gausman",
      pitcher == 621244 ~ "José Berrios"
    )
  ) %>% 
  ggplot() +
  geom_point(aes(x = plate_x, y = plate_z, color = pitcher_name), alpha = 0.5) +
  geom_rect(aes(xmin = -0.7083, xmax = 0.7083, ymin = med_bot, ymax = med_top),
            alpha = 0, fill = NA, color = "gray60", linewidth = 1) +
  labs(title = "Aaron Judge vs Kevin Gausman & José Berrios ", 
      subtitle = "Pitches drown by Kevin Gausman and José Berrios to Aaron Judge",
      caption = "The box is plotted according to Judge's rule book strike zone") +
  scale_color_manual(values = c("Kevin Gausman" = "orange", "José Berrios" = "blue"),
                    name = "Pitcher") +
  coord_fixed(ratio = 1) +
  theme_bw() +
  theme(
    text = element_text(family = "roboto"),
    legend.position = "none") +
  facet_wrap(~pitcher_name)


## ------------------------------------------------------------------------------------
## -------------------------------------- GRID CREATION -----------------------------------
## ------------------------------------------------------------------------------------

summary(Matchup$plate_x)
summary(Matchup$plate_z)

x_breaks = seq(-3, 2.5, by = 0.5)
z_breaks = seq(-1, 5, by = 0.5)

Matchup = Matchup %>% 
  mutate(
    x_bin = cut(plate_x, breaks = x_breaks, include.lowest = TRUE),
    z_bin = cut(plate_z, breaks = z_breaks, include.lowest = TRUE)
  )

grid_counts = Matchup %>%
  group_by(pitcher, x_bin, z_bin) %>%
  summarise(count = n(), .groups = 'drop')

grid_counts = pivot_wider(
  grid_counts, 
  names_from = pitcher, 
  values_from = count, 
  values_fill = list(count = 0)
)

x_mids = (x_breaks[-length(x_breaks)] + x_breaks[-1]) / 2
z_mids = (z_breaks[-length(z_breaks)] + z_breaks[-1]) / 2


full_grid = expand.grid(x_bin = levels(Matchup$x_bin), 
                        z_bin = levels(Matchup$z_bin))

grid_counts_full = full_grid %>%
  left_join(grid_counts, by = c("x_bin", "z_bin")) %>%
  mutate(
    `592332` = replace_na(`592332`, 0),
    `621244` = replace_na(`621244`, 0),
    x_mid = x_mids[as.numeric(x_bin)],
    z_mid = z_mids[as.numeric(z_bin)],
    prop_gausman = `592332` / sum(`592332`),
    prop_berrios = `621244` / sum(`621244`)
  ) %>%
  arrange(z_bin, x_bin) 


## ------------------------------------------------------------------------------------
## -------------------------------------- LEE TEST -----------------------------------
## ------------------------------------------------------------------------------------

neighbors_list = cell2nb(nrow = length(z_mids), 
                         ncol = length(x_mids), 
                         type = "queen")
neighbors_list = nb2listw(neighbors_list, style = "W", zero.policy = TRUE)

lee.mc(grid_counts_full$prop_gausman,
       grid_counts_full$prop_berrios,
       neighbors_list,
       nsim = 9999,
       alternative = "greater")


results_lee = data.frame(
  Parameter = c("Statistic (L)", "Observed rank", "p-value", "Alternative", "Simulations"),
  Value = c(0.52458, "10000 / 10000", "0.0001", "greater", "9999 + 1")
)

kable(results_lee, align = c("l", "r"))
## ------------------------------------------------------------------------------------
## -------------------------------------- HEATMAP -----------------------------------
## ------------------------------------------------------------------------------------

# Data for heatmap
heatmap_data = grid_counts_full %>%
  pivot_longer(
    cols      = c(prop_gausman, prop_berrios),
    names_to  = "pitcher",
    values_to = "prop"
  ) %>%
  mutate(
    pitcher_name = case_when(
      pitcher == "prop_gausman" ~ "Kevin Gausman",
      pitcher == "prop_berrios" ~ "José Berríos"
    ),
    pitcher_name = factor(pitcher_name, levels = c("José Berríos", "Kevin Gausman"))
  )

# Palette 
bg_col       = "#0d0d0f"
panel_col    = "#111114"
strip_col    = "#1a1a1f"  
grid_col     = "#1f1f25"
text_col     = "#e8e8e8"
subtext_col  = "#888899"
gausman_col  = "#f97316"   
berrios_col  = "#3b82f6"   

# Theme 
heatmap_theme = theme(
  # backgrounds
  plot.background   = element_rect(fill = bg_col,    color = NA),
  panel.background  = element_rect(fill = panel_col, color = NA),
  panel.grid        = element_blank(),
  panel.border      = element_rect(color = grid_col, fill = NA, linewidth = 0.5),

  # strips
  strip.background  = element_rect(fill = strip_col, color = NA),
  strip.text        = element_text(
                        color  = text_col,
                        family = "roboto",
                        face   = "bold",
                        size   = 13,
                        margin = margin(8, 0, 8, 0)
                      ),

  # axes
  axis.text         = element_text(color = subtext_col, family = "roboto", size = 9),
  axis.title        = element_text(color = text_col,    family = "roboto", size = 10, margin = margin(t = 6)),
  axis.ticks        = element_line(color = grid_col),

  # legend
  legend.background = element_rect(fill = bg_col, color = NA),
  legend.key        = element_rect(fill = bg_col, color = NA),
  legend.key.width  = unit(1.8, "cm"),
  legend.key.height = unit(0.35, "cm"),
  legend.text       = element_text(color = subtext_col, family = "roboto", size = 8),
  legend.title      = element_text(color = text_col,    family = "roboto", size = 9,
                                   margin = margin(b = 4)),
  legend.position   = "bottom",
  legend.direction  = "horizontal",

  # titles
  plot.title        = element_text(color = text_col,    family = "roboto",
                                   face  = "bold", size = 17,
                                   margin = margin(b = 6)),
  plot.subtitle     = element_text(color = subtext_col, family = "roboto",
                                   size  = 10,
                                   lineheight = 1.4,
                                   margin = margin(b = 14)),
  plot.caption      = element_text(color = subtext_col, family = "roboto",
                                   size  = 7,
                                   margin = margin(t = 8)),
  plot.margin       = margin(16, 16, 12, 16),

  # facet spacing
  panel.spacing     = unit(1.2, "lines")
)

# Color scale 
heat_scale = scale_fill_gradientn(
  colors = c("#0a0520", "#3b0764", "#7e22ce", "#c2410c", "#ea580c", "#f97316", "#fbbf24"),
  values = scales::rescale(c(0, 0.005, 0.015, 0.03, 0.045, 0.06, 0.084)),
  name   = "Pitch proportion",
  labels = scales::percent_format(accuracy = 0.1),
  guide  = guide_colorbar(
    title.position = "top",
    title.hjust    = 0.5,
    barwidth       = 12,
    barheight      = 0.5,
    ticks          = FALSE,
    frame.colour   = grid_col
  )
)
# Strike zone 
sz_box = list(
  annotate("rect",
           xmin = -0.7083, xmax = 0.7083,
           ymin = med_bot,  ymax = med_top,
           fill = NA, color = "white", linewidth = 0.9, alpha = 0.9),
  annotate("text",
           x = 0.7083, y = med_top + 0.25,
           label = "Strike zone", hjust = -0.1, vjust = 1,
           color = "white", family = "roboto", size = 2.8)
)

# p1: Side-by-side 
p1 = ggplot(heatmap_data, aes(x = x_mid, y = z_mid, fill = prop)) +
  geom_tile(color = grid_col, linewidth = 0.25) +
  heat_scale +
  sz_box +
  facet_wrap(~pitcher_name) +
  coord_fixed() +
  labs(
    title    = "Pitch Location vs Aaron Judge",
    subtitle = "Proportion of pitches thrown to each zone | Shared color scale (max Gausman 6.4%, Berríos 8.4%)",
    x        = "Horizontal position (ft)  —  catcher's perspective",
    y        = "Vertical position (ft)",
    caption  = "Data: Baseball Savant · Regular season 2016–2025"
  ) +
  heatmap_theme

print(p1)

# p2: Combined 
combined_data = grid_counts_full %>%
  mutate(
    total      = `592332` + `621244`,
    total_prop = total / sum(total)
  )

p2 = ggplot(combined_data, aes(x = x_mid, y = z_mid, fill = total_prop)) +
  geom_tile(color = grid_col, linewidth = 0.25) +
  heat_scale +
  sz_box +
  coord_fixed() +
  labs(
    title    = "Combined Pitch Location vs Aaron Judge",
    subtitle = "Gausman + Berríos — proportion of total pitches per zone",
    x        = "Horizontal position (ft)  —  catcher's perspective",
    y        = "Vertical position (ft)",
    caption  = "Data: Baseball Savant · Regular season 2016–2025"
  ) +
  heatmap_theme

print(p2)
