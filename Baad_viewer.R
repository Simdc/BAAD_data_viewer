# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(baad.data)
library(lubridate)

# Load BAAD dataset
baad <- baad.data::baad_data()
df <- baad$data

# Extract variable metadata with units and description
var_meta <- baad$dictionary %>%
  select(variable, label, units, description)

# Save variable mapping to CSV for reference
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
mapping_filename <- paste0("Baad_varname_mapping_", timestamp, ".csv")
write.csv(var_meta, mapping_filename, row.names = FALSE)

# Add year column using best available info
if ("year" %in% colnames(df)) {
  df$year_actual <- df$year
} else if ("date" %in% colnames(df)) {
  df$year_actual <- as.numeric(format(as.Date(df$date), "%Y"))
} else {
  df$year_actual <- floor(df$age)
}

# Filter and assign grid cells
df <- df %>%
  filter(!is.na(year_actual), !is.na(pft), !is.na(latitude), !is.na(longitude), !is.na(age)) %>%
  mutate(
    lat_bin = floor(latitude * 2) / 2 + 0.25,
    lon_bin = floor(longitude * 2) / 2 + 0.25
  )

# Only keep grid cells with >1 unique age and at least one age < 50
cell_time_series <- df %>%
  group_by(lat_bin, lon_bin) %>%
  filter(n_distinct(age) > 1, any(age < 50, na.rm = TRUE)) %>%
  ungroup()

# Group into list by grid cell
age_gradient_list <- cell_time_series %>%
  arrange(lat_bin, lon_bin, age) %>%
  group_by(lat_bin, lon_bin) %>%
  group_split()

# Output file names
pdf_filename <- paste0("Baad_output_", timestamp, ".pdf")
csv_filename <- paste0("Baad_output_", timestamp, ".csv")

age_gradient_master <- data.frame()
pdf(pdf_filename, width = 10, height = 6)

for (cell in age_gradient_list) {
  lat <- unique(cell$lat_bin)
  lon <- unique(cell$lon_bin)
  cell_id <- paste0("lat_", lat, "_lon_", lon)
  cat("\n--- Grid Cell:", cell_id, "---\n")
  cell$cell_id <- cell_id
  age_gradient_master <- rbind(age_gradient_master, cell)

  numeric_vars <- cell %>%
    select(where(is.numeric)) %>%
    select(-latitude, -longitude, -lat_bin, -lon_bin) %>%
    colnames()

  for (var in numeric_vars) {
    if (all(is.na(cell[[var]]))) next
    meta_row <- var_meta %>% filter(variable == var)
    long_name <- if (nrow(meta_row) > 0 && !is.na(meta_row$label) && meta_row$label != "") meta_row$label else var
    unit <- if (nrow(meta_row) > 0 && !is.na(meta_row$units) && meta_row$units != "") meta_row$units else ""
    description <- if (nrow(meta_row) > 0 && !is.na(meta_row$description) && meta_row$description != "") meta_row$description else ""
    y_label <- if (unit != "") paste0(long_name, " (", unit, ")") else long_name
    subtitle_text <- if (description != "") paste("Description:", description) else ""

    group_var <- "pft"

    p <- ggplot(cell, aes(x = age, y = .data[[var]], color = .data[[group_var]])) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 1) +
      labs(title = paste("Grid Cell:", cell_id), subtitle = subtitle_text, x = "Age", y = y_label, color = toupper(group_var)) +
      theme_minimal(base_size = 9) +
      theme(
        axis.title.y = element_text(size = 5, margin = margin(r = 6)),
        axis.text.y = element_text(size = 5),
        axis.title.x = element_text(size = 5),
        axis.text.x = element_text(size = 5),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 5),
        plot.title = element_text(size = 5),
        plot.subtitle = element_text(size = 5),
        legend.position = "bottom"
      )
    print(p)
  }
}
dev.off()

write.csv(age_gradient_master, csv_filename, row.names = FALSE)

cat("\n? All done.\nSaved outputs:\n - PDF plots: ", pdf_filename,
    "\n - Age-gradient data: ", csv_filename,
    "\n - Variable name mapping: ", mapping_filename, "\n")

# === Aggregated PDF with symbol table and better legends ===
library(patchwork)
library(gridExtra)
library(grid)

agg_pdf_filename <- paste0("Baad_aggregated_enhanced_", timestamp, ".pdf")
pdf(agg_pdf_filename, width = 8.5, height = 11)  # Portrait layout

filtered_df <- age_gradient_master %>%
  filter(age <= 50) %>%
  group_by(cell_id) %>%
  filter(n_distinct(age) >= 2) %>%
  ungroup()

site_levels <- unique(filtered_df$cell_id)
shape_vals <- rep(0:25, length.out = length(site_levels))
names(shape_vals) <- site_levels

# Shape label mapping
shape_names <- c("circle", "triangle", "plus", "cross", "square", "diamond", "triangle_down", "triangle_up", "octagon", "star", "asterisk")
shape_labels <- rep(shape_names, length.out = length(shape_vals))

symbol_table <- data.frame(
  Site = names(shape_vals),
  Shape = shape_vals,
  Symbol = shape_labels,
  Latitude = as.numeric(sub("lat_([0-9.-]+)_.*", "\\1", names(shape_vals))),
  Longitude = as.numeric(sub(".*_lon_([0-9.-]+)", "\\1", names(shape_vals)))
)
symbol_table$Shape <- factor(symbol_table$Shape)

# Paginate symbol table
symbol_chunks <- split(symbol_table, ceiling(seq_len(nrow(symbol_table)) / 20))
for (chunk in symbol_chunks) {
  table_grob <- tableGrob(chunk, rows = NULL, theme = ttheme_default(
    core = list(fg_params = list(cex = 0.8)),
    colhead = list(fg_params = list(cex = 0.9, fontface = "bold"))
  ))
  grid.newpage()
  grid.draw(table_grob)
}

numeric_vars <- filtered_df %>%
  select(where(is.numeric)) %>%
  select(-latitude, -longitude, -lat_bin, -lon_bin, -year_actual, -age) %>%
  colnames()

for (var in numeric_vars) {
  if (all(is.na(filtered_df[[var]]))) next

  meta_row <- var_meta %>% filter(variable == var)
  long_name <- if (nrow(meta_row) > 0 && !is.na(meta_row$label) && meta_row$label != "") meta_row$label else var
  unit <- if (nrow(meta_row) > 0 && !is.na(meta_row$units) && meta_row$units != "") meta_row$units else ""
  description <- if (nrow(meta_row) > 0 && !is.na(meta_row$description) && meta_row$description != "") meta_row$description else ""
  y_label <- if (unit != "") paste0(long_name, " (", unit, ")") else long_name
  subtitle_text <- if (description != "") paste("Description:", description) else ""

  p1 <- ggplot(filtered_df, aes(x = age, y = .data[[var]])) +
    geom_point(aes(color = pft, shape = cell_id), size = 1.6, alpha = 0.6) +
    geom_smooth(aes(group = cell_id, color = pft), method = "loess", se = FALSE, linewidth = 0.4) +
    scale_shape_manual(values = shape_vals) +
    labs(title = "1. Site-wise Fit", x = "Age (years)", y = y_label) +
    theme_minimal(base_size = 9) +
    theme(
      axis.title.y = element_text(size = 5, margin = margin(r = 6)),
      axis.text.y = element_text(size = 5),
      axis.title.x = element_text(size = 5),
      axis.text.x = element_text(size = 5),
      legend.position = "none",
      plot.title = element_text(size = 5),
      plot.subtitle = element_text(size = 5)
    )

  p2 <- ggplot(filtered_df, aes(x = age, y = .data[[var]])) +
    geom_point(aes(color = pft, shape = cell_id), size = 1.6, alpha = 0.6) +
    geom_smooth(aes(group = interaction(cell_id, pft), color = pft), method = "loess", se = FALSE, linewidth = 0.4) +
    scale_shape_manual(values = shape_vals) +
    labs(title = "2. Per Site & PFT Fit", x = "Age (years)", y = y_label) +
    theme_minimal(base_size = 9) +
    theme(
      axis.title.y = element_text(size = 5, margin = margin(r = 6)),
      axis.text.y = element_text(size = 5),
      axis.title.x = element_text(size = 5),
      axis.text.x = element_text(size = 5),
      legend.position = "none",
      plot.title = element_text(size = 5),
      plot.subtitle = element_text(size = 5)
    )

  p3 <- ggplot(filtered_df, aes(x = age, y = .data[[var]])) +
    geom_point(aes(color = pft), size = 1.6, alpha = 0.6) +
    geom_smooth(aes(color = pft), method = "loess", se = FALSE, linewidth = 0.4) +
    labs(title = "3. Global PFT Fit", x = "Age (years)", y = y_label, color = "PFT") +
    theme_minimal(base_size = 9) +
    theme(
      axis.title.y = element_text(size = 5, margin = margin(r = 6)),
      axis.text.y = element_text(size = 5),
      axis.title.x = element_text(size = 5),
      axis.text.x = element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 5),
      legend.position = "bottom",
      plot.title = element_text(size = 5),
      plot.subtitle = element_text(size = 5)
    )

  full_page <- (p1 / p2 / p3) +
    plot_annotation(
      title = paste("Age Gradient Summary for:", long_name),
      subtitle = subtitle_text
    )

  print(full_page)
}

dev.off()
cat("\n? PDF with symbol table and plots saved to:", agg_pdf_filename, "\n")
