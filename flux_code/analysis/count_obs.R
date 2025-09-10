library(goFlux)

# Define data paths  
data_paths <- list(
  LGR1 = "analyzer_data/LGR_GLA131/LGR1",
  LGR2 = "analyzer_data/LGR_GLA131/LGR2", 
  LGR3 = "analyzer_data/LGR_GLA131/LGR3",
  Picarro = "analyzer_data/Picarro_G4301"
)

# Import all analyzer data using the same approach as your working code
total_obs <- 0

for (analyzer in names(data_paths)) {
  path <- data_paths[[analyzer]]
  cat("Importing", analyzer, "data...\n")
  
  if (dir.exists(path)) {
    if (grepl("Picarro", analyzer)) {
      # Picarro G4301 import (same as your working code)
      dat_files <- list.files(path, recursive = TRUE, pattern = "\\.dat$", full.names = TRUE)
      data_files <- dat_files[file.size(dat_files) > 0]
      
      if (length(data_files) > 0) {
        temp_dir <- tempfile(paste0("picarro_temp_"))
        dir.create(temp_dir, recursive = TRUE)
        file.copy(data_files, temp_dir)
        
        cat("Found", length(data_files), ".dat files for Picarro\n")
        
        picarro_data <- tryCatch({
          import2RData(
            path = temp_dir,
            instrument = "G4301",
            date.format = "ymd",
            timezone = "UTC",
            keep_all = FALSE,
            prec = c(0.025, 0.1, 10),  # CO2, CH4, H2O precision for G4301
            merge = TRUE
          )
        }, error = function(e) {
          cat("Error importing Picarro:", e$message, "\n")
          return(NULL)
        })
        
        unlink(temp_dir, recursive = TRUE)
        
        if (!is.null(picarro_data)) {
          obs <- nrow(picarro_data)
          cat("Picarro:", obs, "observations\n")
          total_obs <- total_obs + obs
        }
      }
      
    } else {
      # LGR import (same as your working code)
      # Extract zip files if needed
      zip_files <- list.files(path, recursive = TRUE, pattern = "\\.zip$", full.names = TRUE)
      if (length(zip_files) > 0) {
        cat("Extracting", length(zip_files), "zip files for", analyzer, "\n")
        for(zip_file in zip_files) {
          extract_dir <- dirname(zip_file)
          tryCatch({
            unzip(zip_file, exdir = extract_dir, overwrite = TRUE)
          }, error = function(e) {
            cat("Error extracting", basename(zip_file), "\n")
          })
        }
      }
      
      # Get all txt files
      txt_files <- list.files(path, recursive = TRUE, pattern = "\\.txt$", full.names = TRUE)
      clean_txt_files <- txt_files[!grepl("/.*\\.txt/", txt_files)]
      data_files <- clean_txt_files[file.size(clean_txt_files) > 0]
      
      if (length(data_files) > 0) {
        temp_dir <- tempfile(paste0(tolower(analyzer), "_temp_"))
        dir.create(temp_dir, recursive = TRUE)
        file.copy(data_files, temp_dir)
        
        cat("Found", length(data_files), ".txt files for", analyzer, "\n")
        
        lgr_data <- tryCatch({
          import2RData(
            path = temp_dir,
            instrument = "UGGA",
            date.format = "mdy",
            timezone = "UTC",
            keep_all = FALSE,
            prec = c(0.35, 0.9, 200),  # MGGA GLA131 precision
            merge = TRUE
          )
        }, error = function(e) {
          cat("Error importing", analyzer, ":", e$message, "\n")
          return(NULL)
        })
        
        unlink(temp_dir, recursive = TRUE)
        
        if (!is.null(lgr_data)) {
          obs <- nrow(lgr_data)
          cat(analyzer, ":", obs, "observations\n")
          total_obs <- total_obs + obs
        }
      }
    }
  } else {
    cat("Directory not found:", path, "\n")
  }
}

cat("Total observations across all analyzers:", total_obs, "\n")