if (!require("devtools", quietly = TRUE)) install.packages("devtools")
try(detach("package:goFlux", unload = TRUE), silent = TRUE)
devtools::install_github("Qepanna/goFlux")
library(goFlux)
# Install packages
# install.packages("dplyr")
# install.packages("purrr")
# install.packages("readxl")
# install.packages("openxlsx")
# Load packages
library(dplyr)
library(purrr)
library(readxl)
library(openxlsx)

# Check if the path exists and what's in it
list.files("/Users/jongewirtzman/My Drive/Research/Blueflux/blueflux-ground/analyzer_data/LGR_UGGA/LGR1", 
           recursive = TRUE)

# Check for any file extensions present
list.files("/Users/jongewirtzman/My Drive/Research/Blueflux/blueflux-ground/analyzer_data/LGR_UGGA/LGR1", 
           recursive = TRUE, pattern = "\\.")




# Complete workflow to import LGR GLA131 (MGGA) data using goFlux
# This handles the mixed file structure and filters out empty files

library(goFlux)

# Set your data path
data_path <- "/Users/jongewirtzman/My Drive/Research/Blueflux/blueflux-ground/analyzer_data/LGR_UGGA/LGR1"

# Step 1: Find all .txt files recursively
txt_files <- list.files(data_path, 
                        recursive = TRUE, 
                        pattern = "\\.txt$", 
                        full.names = TRUE)

# Step 2: Filter out files inside extracted directories and empty files
clean_txt_files <- txt_files[!grepl("/.*\\.txt/", txt_files)]
data_files <- clean_txt_files[file.size(clean_txt_files) > 0]

cat("Found", length(clean_txt_files), "total .txt files\n")
cat("Found", length(data_files), "non-empty data files\n")

# Step 3: Create temporary directory with only data files
temp_data_path <- tempfile(pattern = "lgr_data_")
dir.create(temp_data_path, recursive = TRUE)

# Copy only non-empty files to avoid import issues
file.copy(data_files, temp_data_path)

# Step 4: Import using goFlux
lgr_data <- import2RData(
  path = temp_data_path,
  instrument = "UGGA",
  date.format = "mdy",        # Corrected: LGR files use MM/DD/YYYY format
  timezone = "UTC",
  keep_all = FALSE,           # Set to TRUE if you want all columns
  prec = c(0.35, 0.9, 200),  # Precision for GLA131: CO2, CH4, H2O
  merge = TRUE               # Combines all files into one data frame
)

# Step 5: Clean up temporary directory
unlink(temp_data_path, recursive = TRUE)

# View the results
cat("Import complete. Data dimensions:", dim(lgr_data), "\n")
head(lgr_data)
