# FarmTech Solutions - Statistical Analysis
#
# This script performs statistical analysis on crop data exported from the
# farm management system. It calculates totals, averages, and standard deviations
# for various metrics and provides summaries of crop data and input usage.
#
# Author: Gabriel Mule <gabemule@gmail.com>
# Date: 2024

# Check if the CSV file exists
if (!file.exists("crop_data.csv")) {
  cat("Error: crop_data.csv file not found. Please make sure the file exists in the current directory.\n")
  quit(status = 1)
}

# Read data from CSV file
data <- read.csv("crop_data.csv", stringsAsFactors = FALSE)

#' Safely convert values to numeric
#'
#' @param x A vector to be converted to numeric
#' @return A numeric vector with NAs replaced by 0
safe_numeric <- function(x) {
  result <- suppressWarnings(as.numeric(as.character(x)))
  result[is.na(result)] <- 0
  return(result)
}

#' Safely calculate standard deviation
#'
#' @param x A numeric vector
#' @return Standard deviation or NA if it can't be calculated
safe_sd <- function(x) {
  if (length(x) <= 1 || all(is.na(x)) || length(unique(x)) == 1) {
    return(NA)
  }
  return(sd(x, na.rm = TRUE))
}

#' Calculate statistics (mean, standard deviation, total)
#'
#' @param x A numeric vector
#' @return A named vector with mean, standard deviation, and total
calc_stats <- function(x) {
  x <- safe_numeric(x)
  c(mean = mean(x, na.rm = TRUE),
    sd = safe_sd(x),
    total = sum(x, na.rm = TRUE))
}

# Print results header
cat("Statistical Analysis Results:\n")
cat("Total number of crops:", nrow(data), "\n\n")

# Area statistics
cat("Area (ha):\n")
area_stats <- calc_stats(data$area)
cat("  Total across all crops:", round(area_stats["total"], 2), "\n")
cat("  Average / crop:", round(area_stats["mean"], 2), "\n")
cat("  Standard Deviation:", ifelse(is.na(area_stats["sd"]), "N/A", round(area_stats["sd"], 2)), "\n")
cat("\n")

# Number of rows statistics
cat("Number of Rows:\n")
rows_stats <- calc_stats(data$num_rows)
cat("  Total across all crops:", as.integer(rows_stats["total"]), "\n")
cat("  Average / crop:", round(rows_stats["mean"], 2), "\n")
cat("  Standard Deviation:", ifelse(is.na(rows_stats["sd"]), "N/A", round(rows_stats["sd"], 2)), "\n")
cat("\n")

# Data Summary by Crop Type
cat("Data Summary by Crop Type:\n")
summary_data <- tapply(safe_numeric(data$area), data$type, function(x) {
  crop_data <- data[data$type == unique(data$type)[which(x == x[1])], ]
  rows <- safe_numeric(crop_data$num_rows)
  c(Count = length(x), 
    `Total Area (ha)` = sum(x, na.rm = TRUE), 
    `Avg Area / Crop (ha)` = mean(x, na.rm = TRUE),
    `SD Area (ha)` = safe_sd(x),
    `Total Rows` = sum(rows, na.rm = TRUE),
    `Avg Rows / Crop` = mean(rows, na.rm = TRUE),
    `SD Rows` = safe_sd(rows))
})
print(summary_data)
cat("\n")

# Full List of Material Usage
cat("Full List of Material Usage:\n")
input_columns <- grep("_amount_per_ha$", names(data), value = TRUE)

for (col in input_columns) {
  input_type <- sub("_amount_per_ha$", "", col)
  total_amount_col <- paste0(input_type, "_total_amount")
  
  if (total_amount_col %in% names(data)) {
    unit <- data[[paste0(input_type, "_unit")]][1]
    
    total_amount_stats <- calc_stats(data[[total_amount_col]])
    per_ha_stats <- calc_stats(data[[col]])
    
    # Count crops using this material
    crops_using <- sum(data[[col]] > 0, na.rm = TRUE)
    
    if (total_amount_stats["total"] > 0) {
      cat(input_type, ":\n")
      cat("  Total Amount:", round(total_amount_stats["total"], 2), unit, "\n")
      cat("  Avg Amount / ha:", round(per_ha_stats["mean"], 2), unit, "\n")
      
      if (is.na(per_ha_stats["sd"]) || crops_using <= 1) {
        cat("  SD / crops using material: N/A (used by single crop or not used)\n")
      } else {
        cat("  SD / crops using material:", round(per_ha_stats["sd"], 2), unit, "\n")
      }
      
      cat("  Number of crops using:", crops_using, "\n")
      cat("\n")
    }
  }
}

cat("Statistical analysis complete.\n")