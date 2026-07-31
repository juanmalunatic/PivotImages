library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)

input_path <- here(
  "data",
  "processed",
  "volumes_raw.parquet"
)

output_path <- here(
  "data",
  "processed",
  "volumes_long.parquet"
)

volumes_raw <- read_parquet(input_path)

# -------------------------------------------------------------------------
# 1. Verificaciones de estructura
# -------------------------------------------------------------------------

share_columns  <- paste0("x", 2014:2023, "_percent")
volume_columns <- paste0("unit_", 2014:2023)
rank_columns   <- paste0("rank_", 2014:2023)

required_columns <- c(
  "region",
  "country",
  "category",
  "subcategory",
  "lowest_level",
  "hierarchy_level",
  "data_type",
  "global_brand_owner",
  share_columns,
  volume_columns,
  rank_columns,
  "unit"
)

stopifnot(
  nrow(volumes_raw) == 15627,
  all(required_columns %in% names(volumes_raw))
)

# Las dos parejas de columnas duplicadas del Excel deben contener
# exactamente la misma informacion.
same_with_na <- function(x, y) {
  all(
    (is.na(x) & is.na(y)) |
      (!is.na(x) & !is.na(y) & x == y)
  )
}

stopifnot(
  same_with_na(
    volumes_raw$currency_conversion,
    volumes_raw$currency_conversion_2
  ),
  same_with_na(
    volumes_raw$unit,
    volumes_raw$unit_2
  )
)

# -------------------------------------------------------------------------
# 2. Convertir los tres bloques anuales a formato largo
# -------------------------------------------------------------------------

volumes_long <- volumes_raw |>
  mutate(
    source_row_id = row_number()
  ) |>
  rename_with(
    \(x) str_replace(
      x,
      "^x(\\d{4})_percent$",
      "share_pct_\\1"
    ),
    all_of(share_columns)
  ) |>
  rename_with(
    \(x) str_replace(
      x,
      "^unit_(\\d{4})$",
      "volume_\\1"
    ),
    all_of(volume_columns)
  ) |>
  select(
    -current_constant,
    -currency_conversion_2,
    -unit_2,
    -starts_with("growth_percent_"),
    -historic_period_growth_years,
    -historic_p_growth_percent,
    -historic_cagr_percent
  ) |>
  pivot_longer(
    cols = matches(
      "^(share_pct|volume|rank)_\\d{4}$"
    ),
    names_to = c(".value", "year"),
    names_pattern = "^(share_pct|volume|rank)_(\\d{4})$"
  ) |>
  mutate(
    year = as.integer(year),
    is_total = global_brand_owner == "Total",
    is_others = global_brand_owner == "Others",
    is_lowest_level = str_to_lower(lowest_level) == "yes"
  ) |>
  filter(
    !if_all(
      c(share_pct, volume, rank),
      is.na
    )
  ) |>
  arrange(
    region,
    country,
    category,
    hierarchy_level,
    subcategory,
    data_type,
    unit,
    global_brand_owner,
    year
  )

# -------------------------------------------------------------------------
# 3. Verificaciones del resultado
# -------------------------------------------------------------------------

stopifnot(
  all(volumes_long$year %in% 2014:2023),
  !anyDuplicated(
    volumes_long[c("source_row_id", "year")]
  ),
  is.numeric(volumes_long$share_pct),
  is.numeric(volumes_long$volume),
  is.numeric(volumes_long$rank)
)

# Este es el numero esperado para el archivo recibido.
stopifnot(
  nrow(volumes_long) == 107480
)

# -------------------------------------------------------------------------
# 4. Guardar y comprobar
# -------------------------------------------------------------------------

write_parquet(
  volumes_long,
  output_path
)

csv_output_path <- here(
  "data",
  "processed",
  "volumes_long.csv.gz"
)

write_csv(
  volumes_long,
  csv_output_path
)

message(
  "CSV largo: ",
  csv_output_path
)

volumes_check <- read_parquet(output_path)

stopifnot(
  nrow(volumes_check) == nrow(volumes_long),
  ncol(volumes_check) == ncol(volumes_long)
)

cat(
  "\nDataset largo creado correctamente\n",
  "Filas:", nrow(volumes_long), "\n",
  "Columnas:", ncol(volumes_long), "\n",
  "Años:", min(volumes_long$year), "-", max(volumes_long$year), "\n\n"
)

print(
  volumes_long |>
    count(data_type, unit, sort = TRUE)
)

print(
  volumes_long |>
    select(
      region,
      country,
      category,
      subcategory,
      global_brand_owner,
      year,
      share_pct,
      volume,
      unit,
      rank
    ) |>
    slice_head(n = 5)
)

message(
  "Parquet largo: ",
  output_path
)
