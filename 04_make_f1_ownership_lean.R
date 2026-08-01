# 04_make_f1_ownership_lean.R
#
# F1: identified cigarette-incumbent ownership of e-vapour and HTP
# in Latin America, 2023.
#
# Creates only:
#   output/figures/F1_ownership_evapour_htp_latam.png
#   output/data/F1_ownership_plot_data.csv
#
# Interpretation:
#   dark blue  = identified cigarette incumbents
#   medium gray = unattributed volume ("Others")
#   light gray  = other identified owners
#
# The blue share is therefore the observed lower bound on incumbent ownership.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(here)
})

analysis_year <- 2023L
analysis_region <- "Latin America"

aligned_country_order <- c(
  "Colombia",
  "Costa Rica",
  "Dominican Republic",
  "Guatemala",
  "Chile",
  "Peru"
)

country_labels <- c(
  "Colombia" = "Colombia",
  "Costa Rica" = "Costa Rica",
  "Dominican Republic" = "Dominican Rep.",
  "Guatemala" = "Guatemala",
  "Chile" = "Chile",
  "Peru" = "Peru"
)

ownership_levels <- c(
  "Identified cigarette incumbents",
  "Other identified owners",
  "Others"
)

segment_palette <- c(
  "Identified cigarette incumbents" = "#2C7FB8",
  "Others" = "#AFAFAF",
  "Other identified owners" = "#E8E8E8"
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

volumes <- read_csv(
  input_path,
  show_col_types = FALSE,
  progress = FALSE
) |>
  mutate(
    year = as.integer(year),
    share_pct = as.numeric(share_pct)
  )

required_columns <- c(
  "region",
  "country",
  "subcategory",
  "data_type",
  "global_brand_owner",
  "unit",
  "year",
  "share_pct"
)

missing_columns <- setdiff(required_columns, names(volumes))

if (length(missing_columns) > 0) {
  stop(
    "Faltan columnas requeridas: ",
    paste(missing_columns, collapse = ", ")
  )
}

htp_candidates <- c("Heated Tobacco Products", "Heated Tobacco")
htp_subcategory <- htp_candidates[
  htp_candidates %in% unique(volumes$subcategory)
][1]

if (is.na(htp_subcategory)) {
  stop("No encontre la subcategoria HTP.")
}

category_specs <- tibble(
  category = c("E-vapour", "HTP"),
  subcategory = c("E-Vapour Products", htp_subcategory),
  data_type = c("Retail Volume (litres)", "Retail Volume (sticks)"),
  unit = c("litre", "million sticks")
)

# Identified owner with positive cigarette share in the same country and year.
local_incumbents <- volumes |>
  filter(
    region == analysis_region,
    year == analysis_year,
    subcategory == "Cigarettes",
    data_type == "Retail Volume (sticks)",
    unit == "million sticks",
    !global_brand_owner %in% c("Total", "Others"),
    !is.na(share_pct),
    share_pct > 0
  ) |>
  distinct(country, global_brand_owner) |>
  mutate(is_incumbent = TRUE)

owner_rows <- bind_rows(
  lapply(seq_len(nrow(category_specs)), function(i) {
    spec <- category_specs[i, ]
    
    volumes |>
      filter(
        region == analysis_region,
        year == analysis_year,
        subcategory == spec$subcategory,
        data_type == spec$data_type,
        unit == spec$unit,
        global_brand_owner != "Total",
        !is.na(share_pct),
        share_pct > 0
      ) |>
      transmute(
        category = spec$category,
        country,
        global_brand_owner,
        raw_share_pct = share_pct
      )
  })
)

if (nrow(owner_rows) == 0) {
  stop("No encontre filas de propietarios para e-vapour o HTP.")
}

duplicates <- owner_rows |>
  count(category, country, global_brand_owner, name = "n") |>
  filter(n > 1)

if (nrow(duplicates) > 0) {
  stop("Hay duplicados category-country-owner en F1.")
}

market_presence <- owner_rows |>
  distinct(category, country) |>
  mutate(has_data = TRUE)

classified <- owner_rows |>
  left_join(
    local_incumbents,
    by = c("country", "global_brand_owner")
  ) |>
  mutate(
    owner_group = case_when(
      global_brand_owner == "Others" ~ "Others",
      is_incumbent %in% TRUE ~ "Identified cigarette incumbents",
      TRUE ~ "Other identified owners"
    )
  )

plot_data <- classified |>
  group_by(category, country, owner_group) |>
  summarise(
    raw_share_pct = sum(raw_share_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  complete(
    nesting(category, country),
    owner_group = ownership_levels,
    fill = list(raw_share_pct = 0)
  ) |>
  group_by(category, country) |>
  mutate(
    reported_total_pct = sum(raw_share_pct),
    normalized_share = raw_share_pct / reported_total_pct
  ) |>
  ungroup()

share_check <- plot_data |>
  distinct(category, country, reported_total_pct)

if (any(
  share_check$reported_total_pct < 98.5 |
  share_check$reported_total_pct > 101.5
)) {
  stop("Los shares no cierran cerca de 100% para alguna categoria-pais.")
}

write_csv(
  plot_data |>
    transmute(
      figure = "F1",
      year = analysis_year,
      region = analysis_region,
      category,
      country,
      owner_group,
      raw_share_pct,
      normalized_share
    ),
  file.path(data_dir, "F1_ownership_plot_data.csv")
)

panel_data <- crossing(
  category = c("E-vapour", "HTP"),
  country = aligned_country_order
) |>
  left_join(
    plot_data |>
      select(category, country, owner_group, normalized_share) |>
      pivot_wider(
        names_from = owner_group,
        values_from = normalized_share,
        values_fill = 0
      ),
    by = c("category", "country")
  ) |>
  left_join(
    market_presence,
    by = c("category", "country")
  ) |>
  mutate(
    has_data = coalesce(has_data, FALSE),
    incumbent_share = if_else(
      has_data,
      `Identified cigarette incumbents`,
      NA_real_
    ),
    others_share = if_else(has_data, Others, NA_real_),
    other_identified_share = if_else(
      has_data,
      `Other identified owners`,
      NA_real_
    ),
    incumbent_end = incumbent_share,
    unattributed_end = incumbent_share + others_share,
    country_label = factor(
      unname(country_labels[country]),
      levels = rev(unname(country_labels[aligned_country_order]))
    )
  )

make_panel <- function(data, category_name, title, subtitle) {
  panel <- data |>
    filter(category == category_name)
  
  incumbent_labels_inside <- panel |>
    filter(has_data, incumbent_share >= 0.14) |>
    mutate(
      x_label = incumbent_share / 2,
      label = label_percent(accuracy = 1)(incumbent_share)
    )
  
  incumbent_labels_outside <- panel |>
    filter(has_data, incumbent_share < 0.14) |>
    mutate(
      x_label = pmax(incumbent_share + 0.018, 0.018),
      label = label_percent(accuracy = 1)(incumbent_share)
    )
  
  ggplot(panel, aes(y = country_label)) +
    geom_segment(
      data = filter(panel, has_data),
      aes(x = 0, xend = 1, yend = country_label),
      linewidth = 12,
      color = segment_palette[["Other identified owners"]],
      lineend = "butt"
    ) +
    geom_segment(
      data = filter(panel, has_data, others_share > 0),
      aes(
        x = incumbent_end,
        xend = unattributed_end,
        yend = country_label
      ),
      linewidth = 12,
      color = segment_palette[["Others"]],
      lineend = "butt"
    ) +
    geom_segment(
      data = filter(panel, has_data, incumbent_share > 0),
      aes(
        x = 0,
        xend = incumbent_end,
        yend = country_label
      ),
      linewidth = 12,
      color = segment_palette[["Identified cigarette incumbents"]],
      lineend = "butt"
    ) +
    geom_text(
      data = incumbent_labels_inside,
      aes(x = x_label, label = label),
      color = "white",
      fontface = "bold",
      size = 3.5
    ) +
    geom_text(
      data = incumbent_labels_outside,
      aes(x = x_label, label = label),
      color = segment_palette[["Identified cigarette incumbents"]],
      fontface = "bold",
      hjust = 0,
      size = 3.5
    ) +
    geom_text(
      data = filter(panel, !has_data),
      aes(x = 0.5, label = "No data"),
      color = "#8A8A8A",
      fontface = "italic",
      size = 3.4
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_discrete(drop = FALSE) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Share of category volume",
      y = NULL
    ) +
    theme_minimal(base_size = 10.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E4E4E4"),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      axis.text = element_text(size = 8.5),
      plot.margin = margin(5.5, 10, 5.5, 5.5)
    )
}

f1a <- make_panel(
  panel_data,
  category_name = "E-vapour",
  title = "A. E-vapour",
  subtitle = "Retail volume (litres), 2023"
)

f1b <- make_panel(
  panel_data,
  category_name = "HTP",
  title = "B. Heated tobacco products",
  subtitle = "Retail volume (sticks), 2023"
)

save_f1_png <- function(left_plot, right_plot, filename) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(
      filename = filename,
      width = 12,
      height = 6.3,
      units = "in",
      res = 320,
      background = "white"
    )
  } else {
    grDevices::png(
      filename = filename,
      width = 12,
      height = 6.3,
      units = "in",
      res = 320,
      bg = "white"
    )
  }
  
  grid::grid.newpage()
  
  layout <- grid::grid.layout(
    nrow = 3,
    ncol = 2,
    heights = grid::unit(c(1, 0.10, 0.11), "null"),
    widths = grid::unit(c(1, 1), "null")
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
  
  grid::pushViewport(
    grid::viewport(
      layout.pos.row = 2,
      layout.pos.col = 1:2
    )
  )
  
  legend_x <- c(0.22, 0.49, 0.73)
  legend_labels <- c(
    "Identified cigarette incumbents",
    "Unattributed volume",
    "Other identified owners"
  )
  legend_colors <- c(
    segment_palette[["Identified cigarette incumbents"]],
    segment_palette[["Others"]],
    segment_palette[["Other identified owners"]]
  )
  
  for (i in seq_along(legend_x)) {
    grid::grid.rect(
      x = grid::unit(legend_x[i], "npc"),
      y = grid::unit(0.5, "npc"),
      width = grid::unit(0.018, "npc"),
      height = grid::unit(0.27, "npc"),
      gp = grid::gpar(fill = legend_colors[i], col = NA)
    )
    grid::grid.text(
      legend_labels[i],
      x = grid::unit(legend_x[i] + 0.016, "npc"),
      y = grid::unit(0.5, "npc"),
      just = "left",
      gp = grid::gpar(fontsize = 9)
    )
  }
  
  grid::popViewport()
  
  grid::pushViewport(
    grid::viewport(
      layout.pos.row = 3,
      layout.pos.col = 1:2
    )
  )
  
  grid::grid.text(
    paste0(
      "Dark blue is the identified incumbent share. Unattributed volume ",
      "may include additional incumbents, so the blue share is a lower bound. ",
      "\"No data\" does not mean zero market volume."
    ),
    x = grid::unit(0.01, "npc"),
    y = grid::unit(0.5, "npc"),
    just = "left",
    gp = grid::gpar(fontsize = 8.5, col = "#4D4D4D")
  )
  
  grid::popViewport()
  grid::popViewport()
  
  grDevices::dev.off()
}

save_f1_png(
  left_plot = f1a,
  right_plot = f1b,
  filename = file.path(
    figure_dir,
    "F1_ownership_evapour_htp_latam.png"
  )
)

cat(
  "\nDone. Created only:\n",
  "  output/figures/F1_ownership_evapour_htp_latam.png\n",
  "  output/data/F1_ownership_plot_data.csv\n\n",
  sep = ""
)
