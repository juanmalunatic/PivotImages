# 03_make_category_mix_figures_lean.R
#
# Lean version of the G1 + G2 category-mix figures.
#
# Creates only three files:
#   output/figures/G1_market_transition.png
#   output/figures/G2_alternative_mix.png
#   output/data/category_mix_plot_data.csv
#
# G1:
#   A. Full five-category composition for the covered-market composite.
#   B. Share of new alternatives for the composite, USA, Europe and LatAm.
#
# G2:
#   A. Composition within alternatives for the covered-market composite.
#   B. Composition within alternatives by geography, 2018 vs 2023.
#
# Usage:
#   source("03_make_category_mix_figures_lean.R")
#
# Or:
#   Rscript 03_make_category_mix_figures_lean.R "D:/path/volumes_long.csv.gz"

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(here)
})

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

analysis_years <- 2018:2023
comparison_years <- c(2018L, 2023L)

category_levels <- c(
  "Cigarettes",
  "Smokeless Tobacco",
  "E-Vapour Products",
  "Heated Tobacco Products",
  "Tobacco Free Oral Nicotine"
)

category_labels <- c(
  "Cigarettes" = "Cigarettes",
  "Smokeless Tobacco" = "Traditional smokeless",
  "E-Vapour Products" = "E-vapour",
  "Heated Tobacco Products" = "HTP",
  "Tobacco Free Oral Nicotine" = "Nicotine pouches"
)

alternative_categories <- c(
  "E-Vapour Products",
  "Heated Tobacco Products",
  "Tobacco Free Oral Nicotine"
)

geography_levels <- c(
  "Covered-market composite",
  "USA",
  "Europe",
  "Latin America"
)

geography_short <- c(
  "Covered-market composite" = "Composite",
  "USA" = "USA",
  "Europe" = "Europe",
  "Latin America" = "LatAm"
)

category_palette <- setNames(
  grDevices::hcl.colors(length(category_levels), palette = "Dynamic"),
  category_levels
)

geography_palette <- setNames(
  grDevices::hcl.colors(length(geography_levels), palette = "Dark 3"),
  geography_levels
)

output_root <- here("output")
figure_dir <- file.path(output_root, "figures")
data_dir <- file.path(output_root, "data")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  input_path <- args[[1]]
} else {
  input_candidates <- c(
    here("data", "processed", "volumes_long.csv.gz"),
    here("data", "processed", "volumes_long.csv(1).gz"),
    here("volumes_long.csv.gz"),
    here("volumes_long.csv(1).gz")
  )

  existing_inputs <- input_candidates[file.exists(input_candidates)]

  if (length(existing_inputs) == 0) {
    stop(
      "No encontre volumes_long.csv.gz. ",
      "Pasalo como primer argumento o guardalo en data/processed/."
    )
  }

  input_path <- existing_inputs[[1]]
}

if (!file.exists(input_path)) {
  stop("El archivo de entrada no existe: ", input_path)
}

# -----------------------------------------------------------------------------
# 2. Load data and define the fixed sample
# -----------------------------------------------------------------------------

volumes <- read_csv(
  input_path,
  show_col_types = FALSE,
  progress = FALSE
)

required_columns <- c(
  "region",
  "country",
  "subcategory",
  "data_type",
  "global_brand_owner",
  "unit",
  "year",
  "volume"
)

missing_columns <- setdiff(required_columns, names(volumes))

if (length(missing_columns) > 0) {
  stop(
    "Faltan columnas requeridas: ",
    paste(missing_columns, collapse = ", ")
  )
}

base_data <- volumes |>
  filter(
    data_type == "Retail Volume (sticks)",
    unit == "million sticks",
    global_brand_owner == "Total",
    year %in% analysis_years,
    subcategory %in% category_levels
  ) |>
  transmute(
    region,
    country,
    year = as.integer(year),
    subcategory,
    volume = as.numeric(volume)
  )

if (nrow(base_data) == 0) {
  stop("No encontre filas estandarizadas en Retail Volume (sticks).")
}

if (
  base_data |>
    count(region, country, year, subcategory) |>
    filter(n > 1) |>
    nrow() > 0
) {
  stop("Hay duplicados country-year-subcategory en las filas Total.")
}

coverage <- base_data |>
  distinct(region, country, subcategory, year) |>
  count(region, country, subcategory, name = "n_years") |>
  pivot_wider(
    names_from = subcategory,
    values_from = n_years,
    values_fill = 0
  )

covered_markets <- coverage |>
  filter(
    Cigarettes == length(analysis_years),
    `E-Vapour Products` == length(analysis_years)
  ) |>
  select(region, country)

if (nrow(covered_markets) == 0) {
  stop("No pude construir la muestra fija de Cigarettes y E-Vapour.")
}

country_sets <- list(
  "Covered-market composite" = covered_markets$country,
  "USA" = covered_markets |>
    filter(country == "USA") |>
    pull(country),
  "Europe" = covered_markets |>
    filter(region == "European Union") |>
    pull(country),
  "Latin America" = covered_markets |>
    filter(region == "Latin America") |>
    pull(country)
)

if (any(lengths(country_sets) == 0)) {
  stop("Falta cobertura para alguna geografia: USA, Europe o Latin America.")
}

# -----------------------------------------------------------------------------
# 3. Reusable data functions
# -----------------------------------------------------------------------------

build_category_mix <- function(data, countries, geography_label) {
  result <- data |>
    filter(country %in% countries) |>
    group_by(year, subcategory) |>
    summarise(
      volume_million_stick_equivalents = sum(volume, na.rm = TRUE),
      .groups = "drop"
    ) |>
    complete(
      year = analysis_years,
      subcategory = category_levels,
      fill = list(volume_million_stick_equivalents = 0)
    ) |>
    group_by(year) |>
    mutate(
      total_volume = sum(volume_million_stick_equivalents),
      share = volume_million_stick_equivalents / total_volume
    ) |>
    ungroup() |>
    mutate(
      geography = geography_label,
      category = factor(subcategory, levels = category_levels)
    ) |>
    arrange(year, category)

  share_sums <- result |>
    group_by(year) |>
    summarise(total = sum(share), .groups = "drop")

  if (any(abs(share_sums$total - 1) > 1e-10)) {
    stop("Las participaciones no suman 100% para: ", geography_label)
  }

  result
}

all_detailed <- bind_rows(
  Map(
    f = function(countries, label) {
      build_category_mix(base_data, countries, label)
    },
    countries = country_sets,
    label = names(country_sets)
  )
) |>
  mutate(
    geography = factor(geography, levels = geography_levels)
  )

new_share <- all_detailed |>
  filter(subcategory %in% alternative_categories) |>
  group_by(geography, year) |>
  summarise(
    volume_million_stick_equivalents =
      sum(volume_million_stick_equivalents),
    share = sum(share),
    .groups = "drop"
  )

alternative_mix <- all_detailed |>
  filter(subcategory %in% alternative_categories) |>
  group_by(geography, year) |>
  mutate(
    alternative_total_volume =
      sum(volume_million_stick_equivalents),
    alternative_share =
      volume_million_stick_equivalents / alternative_total_volume,
    category = factor(subcategory, levels = alternative_categories)
  ) |>
  ungroup()

# Small overlap between adjacent polygons prevents raster seams.
add_stack_bounds <- function(data, share_column) {
  data |>
    group_by(across(all_of(intersect(c("geography", "year"), names(data))))) |>
    arrange(category, .by_group = TRUE) |>
    mutate(
      stack_share = .data[[share_column]],
      ymin = cumsum(stack_share) - stack_share,
      ymax = cumsum(stack_share),
      ymin_plot = pmax(0, ymin - 1e-4),
      ymax_plot = pmin(1, ymax + 1e-4)
    ) |>
    ungroup()
}

# -----------------------------------------------------------------------------
# 4. Plot helpers
# -----------------------------------------------------------------------------

base_theme <- function() {
  theme_minimal(base_size = 10.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      plot.margin = margin(5.5, 7, 5.5, 5.5),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8.5)
    )
}

save_two_panel_png <- function(
    left_plot,
    right_plot,
    filename,
    width = 11.5,
    height = 4.8,
    relative_widths = c(1.08, 1)
) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(
      filename = filename,
      width = width,
      height = height,
      units = "in",
      res = 320,
      background = "white"
    )
  } else {
    grDevices::png(
      filename = filename,
      width = width,
      height = height,
      units = "in",
      res = 320,
      bg = "white"
    )
  }

  grid::grid.newpage()
  layout <- grid::grid.layout(
    nrow = 1,
    ncol = 2,
    widths = grid::unit(relative_widths, "null")
  )

  grid::pushViewport(grid::viewport(layout = layout))

  print(
    left_plot,
    newpage = FALSE,
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)
  )

  print(
    right_plot,
    newpage = FALSE,
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2)
  )

  grid::popViewport()
  grDevices::dev.off()
}

# -----------------------------------------------------------------------------
# 5. G1: market transition
# -----------------------------------------------------------------------------

g1a_data <- all_detailed |>
  filter(geography == "Covered-market composite") |>
  add_stack_bounds("share")

g1a <- ggplot(
  g1a_data,
  aes(
    x = year,
    ymin = ymin_plot,
    ymax = ymax_plot,
    fill = category,
    group = category
  )
) +
  geom_ribbon(color = NA, linewidth = 0) +
  scale_x_continuous(breaks = analysis_years) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = category_palette,
    breaks = category_levels,
    labels = category_labels,
    drop = FALSE
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "A. Composition of covered markets",
    subtitle = paste0(
      length(country_sets[["Covered-market composite"]]),
      " markets; retail volume in stick equivalents"
    ),
    x = NULL,
    y = "Share of total volume",
    fill = NULL
  ) +
  base_theme() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

g1_line_top <- min(
  1,
  max(
    0.25,
    ceiling((max(new_share$share, na.rm = TRUE) + 0.02) * 20) / 20
  )
)

g1b <- ggplot(
  new_share,
  aes(
    x = year,
    y = share,
    color = geography,
    group = geography
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = analysis_years) +
  scale_y_continuous(
    limits = c(0, g1_line_top),
    breaks = pretty_breaks(n = 5),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_color_manual(
    values = geography_palette,
    breaks = geography_levels,
    labels = geography_short
  ) +
  labs(
    title = "B. Share of new alternatives",
    subtitle = "E-vapour + HTP + nicotine pouches",
    x = NULL,
    y = "Share of total volume",
    color = NULL
  ) +
  base_theme() +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

save_two_panel_png(
  left_plot = g1a,
  right_plot = g1b,
  filename = file.path(figure_dir, "G1_market_transition.png")
)

# -----------------------------------------------------------------------------
# 6. G2: composition within alternatives
# -----------------------------------------------------------------------------

g2a_data <- alternative_mix |>
  filter(geography == "Covered-market composite") |>
  add_stack_bounds("alternative_share")

g2a <- ggplot(
  g2a_data,
  aes(
    x = year,
    ymin = ymin_plot,
    ymax = ymax_plot,
    fill = category,
    group = category
  )
) +
  geom_ribbon(color = NA, linewidth = 0) +
  scale_x_continuous(breaks = analysis_years) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = category_palette,
    breaks = alternative_categories,
    labels = category_labels[alternative_categories],
    drop = FALSE
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "A. Alternative mix over time",
    subtitle = "Covered-market composite",
    x = NULL,
    y = "Share within alternatives",
    fill = NULL
  ) +
  base_theme() +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

g2b_data <- alternative_mix |>
  filter(year %in% comparison_years) |>
  mutate(
    geography_text = geography_short[as.character(geography)],
    bar_label = paste0(geography_text, "\n", year),
    bar_label = factor(
      bar_label,
      levels = unlist(
        lapply(
          geography_levels,
          function(g) paste0(geography_short[[g]], "\n", comparison_years)
        )
      )
    )
  ) |>
  group_by(geography, year) |>
  arrange(category, .by_group = TRUE) |>
  mutate(
    ymin = cumsum(alternative_share) - alternative_share,
    ymax = cumsum(alternative_share),
    ymin_plot = pmax(0, ymin - 1e-4),
    ymax_plot = pmin(1, ymax + 1e-4),
    x = as.numeric(bar_label)
  ) |>
  ungroup()

g2b <- ggplot(
  g2b_data,
  aes(
    xmin = x - 0.34,
    xmax = x + 0.34,
    ymin = ymin_plot,
    ymax = ymax_plot,
    fill = category
  )
) +
  geom_rect(color = NA, linewidth = 0) +
  scale_x_continuous(
    breaks = seq_along(levels(g2b_data$bar_label)),
    labels = levels(g2b_data$bar_label),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = category_palette,
    breaks = alternative_categories,
    labels = category_labels[alternative_categories],
    drop = FALSE
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "B. Alternative mix by geography",
    subtitle = "2018 versus 2023",
    x = NULL,
    y = "Share within alternatives",
    fill = NULL
  ) +
  base_theme() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 7.5)
  )

save_two_panel_png(
  left_plot = g2a,
  right_plot = g2b,
  filename = file.path(figure_dir, "G2_alternative_mix.png"),
  relative_widths = c(1, 1.12)
)

# -----------------------------------------------------------------------------
# 7. One consolidated data file
# -----------------------------------------------------------------------------

plot_data <- bind_rows(
  all_detailed |>
    filter(geography == "Covered-market composite") |>
    transmute(
      figure = "G1",
      panel = "A. Full composition",
      geography = as.character(geography),
      year,
      category = as.character(category),
      volume_million_stick_equivalents,
      share
    ),
  new_share |>
    transmute(
      figure = "G1",
      panel = "B. New alternatives share",
      geography = as.character(geography),
      year,
      category = "New alternatives",
      volume_million_stick_equivalents,
      share
    ),
  alternative_mix |>
    filter(geography == "Covered-market composite") |>
    transmute(
      figure = "G2",
      panel = "A. Alternative mix over time",
      geography = as.character(geography),
      year,
      category = as.character(category),
      volume_million_stick_equivalents,
      share = alternative_share
    ),
  alternative_mix |>
    filter(year %in% comparison_years) |>
    transmute(
      figure = "G2",
      panel = "B. Alternative mix by geography",
      geography = as.character(geography),
      year,
      category = as.character(category),
      volume_million_stick_equivalents,
      share = alternative_share
    )
)

write_csv(
  plot_data,
  file.path(data_dir, "category_mix_plot_data.csv")
)

# -----------------------------------------------------------------------------
# 8. Minimal execution summary
# -----------------------------------------------------------------------------

cat(
  "\nDone. Created only:\n",
  "  output/figures/G1_market_transition.png\n",
  "  output/figures/G2_alternative_mix.png\n",
  "  output/data/category_mix_plot_data.csv\n\n",
  "Sample sizes: composite=", length(country_sets[[1]]),
  ", Europe=", length(country_sets[["Europe"]]),
  ", Latin America=", length(country_sets[["Latin America"]]),
  ", USA=", length(country_sets[["USA"]]),
  "\n",
  sep = ""
)
