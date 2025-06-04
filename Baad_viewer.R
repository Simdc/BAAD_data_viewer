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

# === Filter for useful grid cells ===
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

# Output file names with timestamp
pdf_filename <- paste0("Baad_output_", timestamp, ".pdf")
csv_filename <- paste0("Baad_output_", timestamp, ".csv")

# Create empty dataframe to save combined summary
age_gradient_master <- data.frame()

# Open a single PDF to store all plots
pdf(pdf_filename, width = 10, height = 6)

# Loop over each grid cell
for (cell in age_gradient_list) {
  lat <- unique(cell$lat_bin)
  lon <- unique(cell$lon_bin)
  cell_id <- paste0("lat_", lat, "_lon_", lon)

  cat("\n--- Grid Cell:", cell_id, "---\n")

  # Append data to master table
  cell$cell_id <- cell_id
  age_gradient_master <- rbind(age_gradient_master, cell)

  # Print table summary for reference
  print(cell[, c("age", "year_actual", "pft", "species")])

  # Identify numeric variables (excluding coords and bins)
  numeric_vars <- cell %>%
    select(where(is.numeric)) %>%
    select(-latitude, -longitude, -lat_bin, -lon_bin) %>%
    colnames()

  for (var in numeric_vars) {
    # Skip variable if all values are NA in this cell
    if (all(is.na(cell[[var]]))) {
      next
    }

    # Lookup metadata for variable
    meta_row <- var_meta %>% filter(variable == var)

    # Fallback to variable name if metadata missing
    long_name <- if (nrow(meta_row) > 0 && !is.na(meta_row$label) && meta_row$label != "") {
      meta_row$label
    } else {
      var
    }

    unit <- if (nrow(meta_row) > 0 && !is.na(meta_row$units) && meta_row$units != "") {
      meta_row$units
    } else {
      ""
    }

    description <- if (nrow(meta_row) > 0 && !is.na(meta_row$description) && meta_row$description != "") {
      meta_row$description
    } else {
      ""
    }

    # Compose y-axis label with units
    y_label <- if (unit != "") {
      paste0(long_name, " (", unit, ")")
    } else {
      long_name
    }

    # Compose subtitle with description if available
    subtitle_text <- if (description != "") {
      paste("Description:", description)
    } else {
      ""
    }

    # === Use 'pft' or 'species' as grouping variable ===
    group_var <- "pft"  # Change to "species" if preferred

    p <- ggplot(cell, aes(x = age, y = .data[[var]], color = .data[[group_var]])) +
      geom_point(alpha = 0.6) +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        se = FALSE,
        linewidth = 1
      ) +
      labs(
        title = paste("Grid Cell:", cell_id),
        subtitle = subtitle_text,
        x = "Age",
        y = y_label,
        color = toupper(group_var)
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    print(p)
  }
}

# Close the PDF device
dev.off()

# Save master CSV of all filtered data
write.csv(age_gradient_master, csv_filename, row.names = FALSE)

# Final message
cat("\n✅ All done.\nSaved outputs:\n - PDF plots: ", pdf_filename,
    "\n - Age-gradient data: ", csv_filename,
    "\n - Variable name mapping: ", mapping_filename, "\n")
