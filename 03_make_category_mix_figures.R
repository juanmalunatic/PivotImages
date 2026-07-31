# 03_make_category_mix_figures.R
#
# Produce la familia de figuras G1 + G2 a partir de volumes_long.csv.gz.
#
# Outputs principales:
#   output/figures/G1_category_mix_covered_markets.pdf|png
#   output/figures/G2_category_mix_europe_latam.pdf|png
#   output/figures/G2_category_mix_usa.pdf|png
#
# Tambien guarda los datos usados y auditorias simples de cobertura.
#
# Uso normal:
#   source("03_make_category_mix_figures.R")
#
# Uso con un CSV en otra ubicacion:
#   Rscript 03_make_category_mix_figures.R "D:/ruta/volumes_long.csv.gz"

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
# cero cuando no aparecen. Esta es una decision exploratoria y queda
# documentada en los captions y auditorias.
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

# Las filas World se guardan como auditoria. No se usan en las figuras porque
# las cinco categorias no estan disponibles alli en una unidad comun:
# e-vapour y nicotine pouches aparecen en units, y smokeless tobacco en tonnes.
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

plot_category_mix <- function(data, title, subtitle, caption) {
  ggplot(
    data,
    aes(
      x = year,
      y = share,
      fill = category
    )
  ) +
    geom_area(
      position = position_stack(reverse = TRUE),
      color = "white",
      linewidth = 0.15
    ) +
    scale_x_continuous(
      breaks = analysis_years
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.2),
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_fill_discrete(
      breaks = category_levels,
      labels = category_labels,
      drop = FALSE
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Share of standardized retail volume",
      fill = NULL,
      caption = caption
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, size = 8),
      strip.text = element_text(face = "bold")
    )
}

save_plot_pair <- function(plot, filename_stem, width, height) {
  ggsave(
    filename = file.path(figure_dir, paste0(filename_stem, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    units = "in"
  )

  ggsave(
    filename = file.path(figure_dir, paste0(filename_stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300
  )
}

# -------------------------------------------------------------------------
# 5. G1: composicion en la muestra fija de mercados cubiertos
# -------------------------------------------------------------------------

g1_data <- build_category_mix(
  data = base_data,
  countries = covered_countries,
  geography_label = "Covered markets"
)

write_csv(
  g1_data |>
    mutate(category = as.character(category)),
  file.path(data_dir, "G1_category_mix_covered_markets.csv")
)

g1_caption <- paste0(
  "Source: Pivot/Euromonitor dataset. Sample: ",
  length(covered_countries),
  " markets with complete Cigarettes and E-Vapour coverage in 2018-2023. ",
  "Provider-reported Retail Volume (sticks); missing HTP, nicotine-pouch and ",
  "traditional-smokeless observations are treated as zero for this exploratory figure."
)

g1 <- plot_category_mix(
  data = g1_data,
  title = "Composition of nicotine-product retail volume",
  subtitle = paste0(
    length(covered_countries),
    " covered markets, 2018-2023"
  ),
  caption = g1_caption
)

save_plot_pair(
  plot = g1,
  filename_stem = "G1_category_mix_covered_markets",
  width = 9.5,
  height = 6
)

# -------------------------------------------------------------------------
# 6. G2: Europa frente a America Latina
# -------------------------------------------------------------------------

g2_data <- bind_rows(
  build_category_mix(
    data = base_data,
    countries = europe_countries,
    geography_label = "Europe"
  ),
  build_category_mix(
    data = base_data,
    countries = latam_countries,
    geography_label = "Latin America"
  )
) |>
  mutate(
    geography = factor(
      geography,
      levels = c("Europe", "Latin America")
    )
  )

write_csv(
  g2_data |>
    mutate(category = as.character(category)),
  file.path(data_dir, "G2_category_mix_europe_latam.csv")
)

g2_caption <- paste0(
  "Source: Pivot/Euromonitor dataset. Fixed subsets of the G1 sample: ",
  length(europe_countries),
  " European markets and ",
  length(latam_countries),
  " Latin American markets. Provider-reported Retail Volume (sticks)."
)

g2 <- plot_category_mix(
  data = g2_data,
  title = "Composition of nicotine-product retail volume by region",
  subtitle = "Covered European and Latin American markets, 2018-2023",
  caption = g2_caption
) +
  facet_wrap(
    vars(geography),
    ncol = 2
  )

save_plot_pair(
  plot = g2,
  filename_stem = "G2_category_mix_europe_latam",
  width = 11,
  height = 6
)

# -------------------------------------------------------------------------
# 7. USA: mismo metodo, salida separada para comparacion o apendice
# -------------------------------------------------------------------------

usa_data <- build_category_mix(
  data = base_data,
  countries = usa_countries,
  geography_label = "USA"
)

write_csv(
  usa_data |>
    mutate(category = as.character(category)),
  file.path(data_dir, "G2_category_mix_usa.csv")
)

usa_plot <- plot_category_mix(
  data = usa_data,
  title = "Composition of nicotine-product retail volume in the USA",
  subtitle = "2018-2023",
  caption = paste0(
    "Source: Pivot/Euromonitor dataset. Provider-reported Retail Volume (sticks). ",
    "Missing category observations are treated as zero for this exploratory figure."
  )
)

save_plot_pair(
  plot = usa_plot,
  filename_stem = "G2_category_mix_usa",
  width = 9.5,
  height = 6
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

# Sensibilidad acordada durante la exploracion: excluir Peru y Slovenia.
# La figura principal conserva ambos mercados; este archivo cuantifica cuanto
# cambian las participaciones si se retiran.
robustness_countries <- setdiff(
  covered_countries,
  sensitivity_exclusions
)

robustness_data <- build_category_mix(
  data = base_data,
  countries = robustness_countries,
  geography_label = "Covered markets excluding Peru and Slovenia"
)

robustness_comparison <- g1_data |>
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
  "Input: ", input_path, "\n",
  "Years: ", min(analysis_years), "-", max(analysis_years), "\n",
  "G1 markets: ", length(covered_countries), "\n",
  "Europe markets: ", length(europe_countries), "\n",
  "Latin America markets: ", length(latam_countries), "\n",
  "USA included: ", length(usa_countries) == 1, "\n",
  "Large e-vapour changes flagged: ", nrow(evapour_outliers), "\n",
  "Maximum share change after excluding Peru and Slovenia: ",
  round(max_robustness_difference, 3), " percentage points\n",
  "Figures: ", figure_dir, "\n",
  "Data: ", data_dir, "\n",
  "Audits: ", audit_dir, "\n\n",
  sep = ""
)
