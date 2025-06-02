# BAAD Dataset Grid-Based Temporal Analysis

## Overview

This R script analyzes the BAAD (Biomass And Allometry Database) dataset to explore spatial and temporal patterns in plant traits. The focus is on grid cells where multiple age records exist, including "young" stages (age under 50 years), enabling analysis of ontogenetic or successional trends.

---

## How to Use

1. Install required packages:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "lubridate"))
install.packages("baad.data")
```
2. Run the script in R or RStudio.

3. Outputs generated:

A PDF file containing plots of numeric variables vs. age for each qualifying grid cell.

A CSV file with the filtered data used for analysis.

---
# Script with Detailed Explanations and Code Blocks
1. Load Required Libraries
Load the packages for data manipulation, plotting, and BAAD data access.

```r
baad <- baad.data::baad_data()
df <- baad$data

if ("year" %in% colnames(df)) {
  df$year_actual <- df$year
} else if ("date" %in% colnames(df)) {
  df$year_actual <- as.numeric(format(as.Date(df$date), "%Y"))
} else {
  df$year_actual <- floor(df$age)
}
```

3. Filter Records and Assign Grid Cells
Remove rows missing key info and bin locations into 0.5-degree grid cells for spatial grouping.

```r
df <- df %>%
  filter(
    !is.na(year_actual),
    !is.na(pft),
    !is.na(latitude),
    !is.na(longitude),
    !is.na(age)
  ) %>%
  mutate(
    lat_bin = floor(latitude * 2) / 2 + 0.25,
    lon_bin = floor(longitude * 2) / 2 + 0.25
  )
```

4. Select Grid Cells With Multiple Ages Including Young Stages
Keep only grid cells that have:

- More than one distinct age, and

- At least one record with age under 50 years ("young stage").

```r
cell_time_series <- df %>%
  group_by(lat_bin, lon_bin) %>%
  filter(
    n_distinct(age) > 1,
    any(age < 50, na.rm = TRUE)
  ) %>%
  ungroup()
```
5. Split Data by Grid Cell for Separate Analysis
Divide the filtered data into a list, each element representing one grid cell’s data.

```r
age_gradient_list <- cell_time_series %>%
  arrange(lat_bin, lon_bin, age) %>%
  group_by(lat_bin, lon_bin) %>%
  group_split()
```
6. Prepare Output File Names With Timestamps
Generate timestamped names to avoid overwriting existing files.

```r
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
pdf_filename <- paste0("Baad_output_", timestamp, ".pdf")
csv_filename <- paste0("Baad_output_", timestamp, ".csv")
```

7. Initialize Container for Combined Data
Create an empty dataframe to collect filtered data from all grid cells.

```r
age_gradient_master <- data.frame()
```

8. Plot Data and Collect Results Per Grid Cell
Open a PDF device to save plots. Loop through each grid cell and:

- Print summary info
- Add cell ID to data
- Append data to master dataframe
- Plot every numeric variable against age with points and smooth trend
- 
```r
pdf(pdf_filename, width = 10, height = 6)

for (cell in age_gradient_list) {
  lat <- unique(cell$lat_bin)
  lon <- unique(cell$lon_bin)
  cell_id <- paste0("lat_", lat, "_lon_", lon)

  cat("\n--- Grid Cell:", cell_id, "---\n")
  
  cell$cell_id <- cell_id
  age_gradient_master <- rbind(age_gradient_master, cell)
  
  print(cell[, c("age", "year_actual", "pft", "species")])
  
  numeric_vars <- cell %>%
    select(where(is.numeric)) %>%
    select(-latitude, -longitude, -lat_bin, -lon_bin) %>%
    colnames()
  
  for (var in numeric_vars) {
    p <- ggplot(cell, aes_string(x = "age", y = var)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "steelblue") +
      labs(
        title = paste("Grid Cell:", cell_id),
        subtitle = paste("Variable:", var),
        x = "Age",
        y = var
      ) +
      theme_minimal()
    
    print(p)
  }
}

dev.off()
```

9. Save Combined Filtered Data
Export all filtered and tagged data as a CSV for further analysis.

```r
write.csv(age_gradient_master, csv_filename, row.names = FALSE)
```

10. Final User Notification
Print message indicating successful completion and file locations.

```r
cat("\n✅ All done.\nSaved outputs:\n - PDF plots: ", pdf_filename, "\n - Age-gradient data: ", csv_filename, "\n")
```


