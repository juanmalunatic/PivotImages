library(readxl)
library(arrow)
library(dplyr)
library(janitor)
library(here)

excel_path <- here(
  "data",
  "raw",
  "260727_Innovation Dataset_selected_countries.xlsx"
)

sheet_name <- "Volumes & firms (all products)"

# La hoja tiene:
# fila 1: descripcion
# fila 2: fuente
# fila 3: vacia
# fila 4: encabezados reales
volumes_raw <- read_excel(
  path = excel_path,
  sheet = sheet_name,
  skip = 3,
  .name_repair = "minimal"
) |>
  clean_names() |>
  mutate(
    across(
      where(is.character),
      \(x) trimws(x)
    )
  )

# Comprobaciones basicas.
expected_identifiers <- c(
  "region",
  "country",
  "category",
  "subcategory",
  "lowest_level",
  "hierarchy_level",
  "data_type",
  "global_brand_owner"
)

stopifnot(
  identical(
    names(volumes_raw)[1:8],
    expected_identifiers
  )
)

stopifnot(
  nrow(volumes_raw) == 15627,
  ncol(volumes_raw) == 55
)

output_path <- here(
  "data",
  "processed",
  "volumes_raw.parquet"
)

write_parquet(
  volumes_raw,
  output_path
)

# Verificar que el archivo pueda recuperarse.
volumes_check <- read_parquet(output_path)

stopifnot(
  nrow(volumes_check) == nrow(volumes_raw),
  ncol(volumes_check) == ncol(volumes_raw)
)

print(dim(volumes_raw))
print(names(volumes_raw))
print(volumes_raw[1:3, 1:12])

message("Parquet corregido: ", output_path)