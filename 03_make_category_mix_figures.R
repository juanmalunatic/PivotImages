# 03_make_category_mix_figures_v3.R
#
# Produce la familia revisada de figuras G1 + G2 a partir de
# volumes_long.csv.gz.
#
# G1: mercado total, panel 4 x 2
#   Filas: World composite, USA, Europe, Latin America
#   Columna 1: cinco categorias
#   Columna 2: Traditional vs New alternatives
#
# G2: solo alternativas, panel 4 x 1
#   Filas: World composite, USA, Europe, Latin America
#   Categorias: E-vapour, HTP, Nicotine pouches
#
# Outputs principales:
#   output/figures/G1_total_market_composition_4x2.pdf|png
#   output/figures/G2_alternative_market_composition_4x1.pdf|png
#
# Tambien guarda los datos usados y auditorias simples de cobertura.
#
# Uso normal:
#   source("03_make_category_mix_figures_v3.R")
#
# Uso con un CSV en otra ubicacion:
#   Rscript 03_make_category_mix_figures_v3.R "D:/ruta/volumes_long.csv.gz"

SCRIPT_VERSION <- "v3-panel-architecture-2026-07-31"

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(here)
})

# -------------------------------------------------------------------------
# 1. Configuracion
# -------------------------------------------------------------------------

analysis_years <- 2018:2023

category_levels <- c(
  "Cigarettes",
  "Smokeless Tobacco",
  "E-Vapour Products",
  "Heated Tobacco Products",
  "Tobacco Free Oral Nicotine"
)

category_labels <- c(
  "Cigarettes" = "Cigarettes",
  "Smokeless Tobacco" = "Traditional smokeless tobacco",
  "E-Vapour Products" = "E-vapour",
  "Heated Tobacco Products" = "HTP",
  "Tobacco Free Oral Nicotine" = "Nicotine pouches"
)

alternative_categories <- c(
  "E-Vapour Products",
  "Heated Tobacco Products",
  "Tobacco Free Oral Nicotine"
)

traditional_categories <- c(
  "Cigarettes",
  "Smokeless Tobacco"
)

grouped_category_levels <- c(
  "Traditional",
  "New alternatives"
)

geography_levels <- c(
  "World composite",
  "USA",
  "Europe",
  "Latin America"
)

# Paleta estable para toda la familia de figuras.
detail_palette <- setNames(
  grDevices::hcl.colors(length(category_levels), palette = "Dynamic"),
  category_levels
)

grouped_palette <- c(
  "Traditional" = "#4D4D4D",
  "New alternatives" = "#E3A008"
)

all_palette <- c(detail_palette, grouped_palette)

# Estas dos exclusiones solo se usan para un chequeo de sensibilidad.
# No se eliminan de las figuras principales.
sensitivity_exclusions <- c("Peru", "Slovenia")

output_root <- here("output")
figure_dir <- file.path(output_root, "figures")
data_dir <- file.path(output_root, "data")
audit_dir <- file.path(output_root, "audits")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

# Evita confundir outputs viejos con la arquitectura nueva.
# Los dos nombres principales antiguos se sobrescriben mas abajo con las
# figuras nuevas; el antiguo panel USA separado se elimina.
legacy_usa_files <- file.path(
  figure_dir,
  c("G2_category_mix_usa.pdf", "G2_category_mix_usa.png")
)
unlink(legacy_usa_files[file.exists(legacy_usa_files)])

# Permite pasar el archivo como primer argumento. Si no, busca ubicaciones
# habituales dentro del proyecto.
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
      paste0(
        "No encontre volumes_long.csv.gz. ",
        "Pasalo como primer argumento o guardalo en data/processed/."
      )
    )
  }

  input_path <- existing_inputs[[1]]
}

if (!file.exists(input_path)) {
  stop("El archivo de entrada no existe: ", input_path)
}

# -------------------------------------------------------------------------
# 2. Cargar y verificar estructura
# -------------------------------------------------------------------------

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

# Debe existir una sola fila Total por pais, anio y categoria.
duplicates <- base_data |>
  count(region, country, year, subcategory, name = "n") |>
  filter(n > 1)

if (nrow(duplicates) > 0) {
  write_csv(
    duplicates,
    file.path(audit_dir, "category_mix_duplicates.csv")
  )

  stop(
    "Hay duplicados country-year-subcategory. ",
    "Revisa output/audits/category_mix_duplicates.csv."
  )
}

# -------------------------------------------------------------------------
# 3. Auditoria de cobertura y muestra fija
# -------------------------------------------------------------------------

coverage_long <- base_data |>
  distinct(region, country, subcategory, year) |>
  count(region, country, subcategory, name = "n_years")

coverage_wide <- coverage_long |>
  pivot_wider(
    names_from = subcategory,
    values_from = n_years,
    values_fill = 0
  ) |>
  arrange(region, country)

write_csv(
  coverage_wide,
  file.path(audit_dir, "category_mix_coverage_by_country.csv")
)

# Muestra principal: paises que tienen Cigarettes y E-Vapour Products
# en todos los anios 2018-2023. Las categorias restantes se completan con
# cero cuando no aparecen. Esta es una decision exploratoria.
covered_markets <- coverage_wide |>
  filter(
    Cigarettes == length(analysis_years),
    `E-Vapour Products` == length(analysis_years)
  ) |>
  select(region, country) |>
  arrange(region, country)

if (nrow(covered_markets) == 0) {
  stop("No pude construir una muestra comun de Cigarettes y E-Vapour.")
}

write_csv(
  covered_markets,
  file.path(audit_dir, "category_mix_country_sample.csv")
)

covered_countries <- covered_markets$country

europe_countries <- covered_markets |>
  filter(region == "European Union") |>
  pull(country)

latam_countries <- covered_markets |>
  filter(region == "Latin America") |>
  pull(country)

usa_countries <- covered_markets |>
  filter(country == "USA") |>
  pull(country)

if (
  length(europe_countries) == 0 ||
    length(latam_countries) == 0 ||
    length(usa_countries) == 0
) {
  stop("Falta cobertura para Europa, America Latina o USA en la muestra.")
}

country_sets <- list(
  "World composite" = covered_countries,
  "USA" = usa_countries,
  "Europe" = europe_countries,
  "Latin America" = latam_countries
)

sample_sizes <- tibble(
  geography = names(country_sets),
  n_markets = vapply(country_sets, length, integer(1))
)

write_csv(
  sample_sizes,
  file.path(audit_dir, "category_mix_sample_sizes.csv")
)

# Las filas World se guardan como auditoria. No se usan en las figuras porque
# las cinco categorias no estan disponibles alli en una unidad comun.
world_subcategories <- c(
  "Cigarettes",
  "Smokeless Tobacco",
  "E-Vapour Products",
  "Heated Tobacco",
  "Heated Tobacco Products",
  "Tobacco Free Oral Nicotine",
  "Nicotine Pouches"
)

world_units_audit <- volumes |>
  filter(
    region == "World",
    country == "World",
    global_brand_owner == "Total",
    year %in% analysis_years,
    subcategory %in% world_subcategories
  ) |>
  count(subcategory, data_type, unit, name = "n_rows") |>
  arrange(subcategory, data_type, unit)

write_csv(
  world_units_audit,
  file.path(audit_dir, "category_mix_world_available_units.csv")
)

# -------------------------------------------------------------------------
# 4. Funciones reutilizables
# -------------------------------------------------------------------------

build_category_mix <- function(data, countries, geography_label) {
  if (length(countries) == 0) {
    stop("La lista de paises esta vacia para: ", geography_label)
  }

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
      share = if_else(
        total_volume > 0,
        volume_million_stick_equivalents / total_volume,
        NA_real_
      )
    ) |>
    ungroup() |>
    mutate(
      geography = geography_label,
      category = factor(subcategory, levels = category_levels)
    ) |>
    arrange(year, category)

  share_check <- result |>
    group_by(year) |>
    summarise(share_sum = sum(share), .groups = "drop")

  if (any(abs(share_check$share_sum - 1) > 1e-10)) {
    stop("Las participaciones no suman 100% para: ", geography_label)
  }

  result
}

build_all_geographies <- function(data, sets) {
  bind_rows(
    Map(
      f = function(countries, label) {
        build_category_mix(
          data = data,
          countries = countries,
          geography_label = label
        )
      },
      countries = sets,
      label = names(sets)
    )
  ) |>
    mutate(
      geography = factor(geography, levels = geography_levels)
    )
}

build_grouped_mix <- function(detailed_data) {
  detailed_data |>
    mutate(
      grouped_category = if_else(
        subcategory %in% traditional_categories,
        "Traditional",
        "New alternatives"
      )
    ) |>
    group_by(geography, year, grouped_category) |>
    summarise(
      volume_million_stick_equivalents =
        sum(volume_million_stick_equivalents),
      total_volume = first(total_volume),
      share = sum(share),
      .groups = "drop"
    ) |>
    mutate(
      grouped_category = factor(
        grouped_category,
        levels = grouped_category_levels
      )
    ) |>
    arrange(geography, year, grouped_category)
}

build_alternative_mix <- function(detailed_data) {
  detailed_data |>
    filter(subcategory %in% alternative_categories) |>
    group_by(geography, year) |>
    mutate(
      alternative_total_volume =
        sum(volume_million_stick_equivalents),
      alternative_share = if_else(
        alternative_total_volume > 0,
        volume_million_stick_equivalents / alternative_total_volume,
        NA_real_
      )
    ) |>
    ungroup() |>
    mutate(
      category = factor(subcategory, levels = alternative_categories)
    ) |>
    arrange(geography, year, category)
}

base_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing.x = grid::unit(1.2, "lines"),
      panel.spacing.y = grid::unit(0.85, "lines"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, size = 8),
      strip.text = element_text(face = "bold"),
      strip.background = element_blank()
    )
}

save_plot_pair <- function(plot, filename_stem, width, height) {
  pdf_device <- if (capabilities("cairo")) {
    grDevices::cairo_pdf
  } else {
    "pdf"
  }

  png_device <- if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png
  } else {
    "png"
  }

  ggsave(
    filename = file.path(figure_dir, paste0(filename_stem, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = pdf_device,
    bg = "white"
  )

  ggsave(
    filename = file.path(figure_dir, paste0(filename_stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 320,
    device = png_device,
    bg = "white"
  )
}


# Guarda tambien una copia con el nombre antiguo para que al abrir el mismo
# archivo de output no aparezca por accidente la figura de la version previa.
save_plot_alias <- function(plot, filename_stem, width, height) {
  save_plot_pair(
    plot = plot,
    filename_stem = filename_stem,
    width = width,
    height = height
  )
}

# -------------------------------------------------------------------------
# 5. Datos comunes para G1 y G2
# -------------------------------------------------------------------------

all_detailed_data <- build_all_geographies(
  data = base_data,
  sets = country_sets
)

all_grouped_data <- build_grouped_mix(all_detailed_data)
all_alternative_data <- build_alternative_mix(all_detailed_data)

write_csv(
  all_detailed_data |>
    mutate(
      geography = as.character(geography),
      category = as.character(category)
    ),
  file.path(data_dir, "G1_total_market_detailed.csv")
)

write_csv(
  all_grouped_data |>
    mutate(
      geography = as.character(geography),
      grouped_category = as.character(grouped_category)
    ),
  file.path(data_dir, "G1_total_market_traditional_vs_new.csv")
)

write_csv(
  all_alternative_data |>
    mutate(
      geography = as.character(geography),
      category = as.character(category)
    ),
  file.path(data_dir, "G2_alternative_market_composition.csv")
)

# -------------------------------------------------------------------------
# 6. G1: mercado total, panel 4 x 2
# -------------------------------------------------------------------------

# Se usan dos capas geom_area separadas para que la leyenda muestre solo las
# cinco categorias detalladas. Importante: color = NA y linewidth = 0 eliminan
# los bordes blancos que producian las lineas punteadas entre areas.
g1_detailed_plot_data <- all_detailed_data |>
  transmute(
    geography,
    year,
    panel_column = "Five categories",
    series = factor(as.character(category), levels = names(all_palette)),
    share
  )

g1_grouped_plot_data <- all_grouped_data |>
  transmute(
    geography,
    year,
    panel_column = "Traditional vs new alternatives",
    series = factor(
      as.character(grouped_category),
      levels = names(all_palette)
    ),
    share
  )

g1_binary_labels <- all_grouped_data |>
  filter(year == max(analysis_years)) |>
  group_by(geography) |>
  arrange(grouped_category, .by_group = TRUE) |>
  mutate(
    label_y = cumsum(share) - share / 2,
    label = paste0(
      if_else(
        grouped_category == "Traditional",
        "Traditional",
        "New"
      ),
      "\n",
      label_percent(accuracy = 1)(share)
    )
  ) |>
  ungroup() |>
  mutate(
    panel_column = "Traditional vs new alternatives"
  )

g1_caption <- paste0(
  "Source: Pivot/Euromonitor dataset. World composite is the fixed sample of ",
  length(covered_countries),
  " covered markets, not the provider's World aggregate. Regional panels are ",
  "fixed subsets of that sample: ",
  length(europe_countries),
  " Europe, ",
  length(latam_countries),
  " Latin America and USA. Provider-reported Retail Volume (sticks); missing ",
  "category observations are treated as zero for this exploratory figure."
)

g1 <- ggplot() +
  geom_area(
    data = g1_detailed_plot_data,
    aes(x = year, y = share, fill = series),
    position = position_stack(reverse = TRUE),
    color = NA,
    linewidth = 0,
    alpha = 1,
    show.legend = TRUE
  ) +
  geom_area(
    data = g1_grouped_plot_data,
    aes(x = year, y = share, fill = series),
    position = position_stack(reverse = TRUE),
    color = NA,
    linewidth = 0,
    alpha = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(g1_binary_labels, grouped_category == "Traditional"),
    aes(x = year - 0.2, y = label_y, label = label),
    inherit.aes = FALSE,
    color = "white",
    size = 3.2,
    lineheight = 0.95,
    fontface = "bold"
  ) +
  geom_text(
    data = filter(g1_binary_labels, grouped_category == "New alternatives"),
    aes(x = year - 0.2, y = label_y, label = label),
    inherit.aes = FALSE,
    color = "black",
    size = 3.2,
    lineheight = 0.95,
    fontface = "bold"
  ) +
  facet_grid(
    rows = vars(geography),
    cols = vars(panel_column),
    scales = "fixed"
  ) +
  scale_x_continuous(
    breaks = analysis_years,
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = all_palette,
    breaks = category_levels,
    labels = category_labels,
    drop = FALSE
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "G1. Composition of nicotine-product retail volume",
    subtitle = paste0(
      "Detailed categories and traditional vs new alternatives, ",
      min(analysis_years), "-", max(analysis_years)
    ),
    x = NULL,
    y = "Share of total standardized retail volume",
    fill = NULL,
    caption = g1_caption
  ) +
  base_theme() +
  theme(
    legend.key.width = grid::unit(1.4, "lines"),
    legend.text = element_text(size = 9)
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  )

save_plot_pair(
  plot = g1,
  filename_stem = "G1_total_market_composition_4x2",
  width = 13,
  height = 12
)

# Sobrescribe el nombre usado por la version anterior.
save_plot_alias(
  plot = g1,
  filename_stem = "G1_category_mix_covered_markets",
  width = 13,
  height = 12
)

# -------------------------------------------------------------------------
# 7. G2: solo alternativas, panel 4 x 1
# -------------------------------------------------------------------------

g2_caption <- paste0(
  "Source: Pivot/Euromonitor dataset. Shares are normalized within the three ",
  "alternative categories only: E-vapour, HTP and nicotine pouches. World ",
  "composite and regional samples are the same fixed samples used in G1."
)

g2 <- ggplot(
  all_alternative_data,
  aes(
    x = year,
    y = alternative_share,
    fill = category
  )
) +
  geom_area(
    position = position_stack(reverse = TRUE),
    color = NA,
    linewidth = 0,
    alpha = 1
  ) +
  facet_grid(
    rows = vars(geography),
    scales = "fixed"
  ) +
  scale_x_continuous(
    breaks = analysis_years,
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = detail_palette,
    breaks = alternative_categories,
    labels = category_labels[alternative_categories],
    drop = FALSE
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "G2. Composition of alternative nicotine-product retail volume",
    subtitle = paste0(
      "E-vapour, HTP and nicotine pouches, ",
      min(analysis_years), "-", max(analysis_years)
    ),
    x = NULL,
    y = "Share within alternative-product volume",
    fill = NULL,
    caption = g2_caption
  ) +
  base_theme() +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE)
  )

save_plot_pair(
  plot = g2,
  filename_stem = "G2_alternative_market_composition_4x1",
  width = 9.5,
  height = 11
)

# Sobrescribe el nombre regional usado por la version anterior.
save_plot_alias(
  plot = g2,
  filename_stem = "G2_category_mix_europe_latam",
  width = 9.5,
  height = 11
)

# -------------------------------------------------------------------------
# 8. Chequeos rapidos de robustez
# -------------------------------------------------------------------------

# Detecta cambios anuales muy grandes en e-vapour. Es diagnostico: no excluye
# automaticamente ningun pais, porque un salto puede ser crecimiento real.
evapour_outliers <- base_data |>
  filter(
    country %in% covered_countries,
    subcategory == "E-Vapour Products"
  ) |>
  arrange(country, year) |>
  group_by(country) |>
  mutate(
    previous_volume = lag(volume),
    year_ratio = volume / previous_volume
  ) |>
  ungroup() |>
  filter(
    is.finite(year_ratio),
    year_ratio < 0.2 | year_ratio > 5
  ) |>
  arrange(country, year)

write_csv(
  evapour_outliers,
  file.path(audit_dir, "category_mix_e_vapour_large_changes.csv")
)

# Sensibilidad: excluir Peru y Slovenia. La figura principal conserva ambos;
# este archivo cuantifica cuanto cambian las participaciones si se retiran.
robustness_countries <- setdiff(
  covered_countries,
  sensitivity_exclusions
)

robustness_data <- build_category_mix(
  data = base_data,
  countries = robustness_countries,
  geography_label = "World composite excluding Peru and Slovenia"
)

main_world_data <- all_detailed_data |>
  filter(geography == "World composite")

robustness_comparison <- main_world_data |>
  select(year, subcategory, share_main = share) |>
  left_join(
    robustness_data |>
      select(year, subcategory, share_sensitivity = share),
    by = c("year", "subcategory")
  ) |>
  mutate(
    difference_percentage_points =
      100 * (share_sensitivity - share_main)
  ) |>
  arrange(year, match(subcategory, category_levels))

write_csv(
  robustness_comparison,
  file.path(audit_dir, "category_mix_robustness_excluding_peru_slovenia.csv")
)

max_robustness_difference <- max(
  abs(robustness_comparison$difference_percentage_points),
  na.rm = TRUE
)

# -------------------------------------------------------------------------
# 9. Resumen de ejecucion
# -------------------------------------------------------------------------

cat(
  "\nCategory-mix figures created successfully\n",
  "Script version: ", SCRIPT_VERSION, "\n",
  "Input: ", input_path, "\n",
  "Years: ", min(analysis_years), "-", max(analysis_years), "\n",
  "World composite markets: ", length(covered_countries), "\n",
  "Europe markets: ", length(europe_countries), "\n",
  "Latin America markets: ", length(latam_countries), "\n",
  "USA included: ", length(usa_countries) == 1, "\n",
  "Large e-vapour changes flagged: ", nrow(evapour_outliers), "\n",
  "Maximum share change after excluding Peru and Slovenia: ",
  round(max_robustness_difference, 3), " percentage points\n",
  "PNG renderer: ",
  ifelse(requireNamespace("ragg", quietly = TRUE), "ragg::agg_png", "base png"),
  "\n",
  "New G1: G1_total_market_composition_4x2.png/pdf\n",
  "Legacy G1 overwritten: G1_category_mix_covered_markets.png/pdf\n",
  "New G2: G2_alternative_market_composition_4x1.png/pdf\n",
  "Legacy G2 overwritten: G2_category_mix_europe_latam.png/pdf\n",
  "Figures: ", figure_dir, "\n",
  "Data: ", data_dir, "\n",
  "Audits: ", audit_dir, "\n\n",
  sep = ""
)
