# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(baad.data)
library(lubridate)

# Load BAAD dataset
baad <- baad.data::baad_data()
df <- baad$data

# Extract variable metadata to create mapping between short and long names
var_meta <- baad$dictionary
print(str(var_meta))
varname_mapping <- var_meta %>% select(variable, label)

# Save variable mapping to CSV for reference
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
mapping_filename <- paste0("Baad_varname_mapping_", timestamp, ".csv")
write.csv(varname_mapping, mapping_filename, row.names = FALSE)

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

  # Plot all numeric variables vs age
  numeric_vars <- cell %>%
    select(where(is.numeric)) %>%
    select(-latitude, -longitude, -lat_bin, -lon_bin) %>%
    colnames()

  for (var in numeric_vars) {
    # Skip variable if all values are NA in this cell
    if (all(is.na(cell[[var]]))) {
      next
    }

    # Find long variable name (label), fallback to short name if none
    long_name <- varname_mapping$label[varname_mapping$variable == var]
    if (length(long_name) == 0 || is.na(long_name) || long_name == "") {
      long_name <- var
    }

    # Skip if long_name is NA or empty
    if (is.na(long_name) || long_name == "") next

    p <- ggplot(cell, aes_string(x = "age", y = var)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "steelblue") +
      labs(
        title = paste("Grid Cell:", cell_id),
        subtitle = paste("Variable:", long_name),
        x = "Age",
        y = long_name
      ) +
      theme_minimal()

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
