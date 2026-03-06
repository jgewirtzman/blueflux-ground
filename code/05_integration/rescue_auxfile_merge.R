#!/usr/bin/env Rscript
# ===============================================================================
# FIX: Merge Rescued Flux Results with Auxfile Metadata
# This script reads the auxfiles and merges metadata back to rescued fluxes
# ===============================================================================

library(dplyr)
library(readr)
library(stringr)

cat("\n===============================================================================\n")
cat("MERGING RESCUED FLUX METADATA\n")
cat("===============================================================================\n\n")

# =============================================================================
# STEP 1: READ ALL AUXFILES
# =============================================================================

cat("Step 1: Reading auxfiles...\n")

# Define auxfile paths
auxfiles <- list(
  picarro = "intermediate/auxfiles/picarro_rescue_auxfile.csv",
  picarro_comp = "intermediate/auxfiles/picarro_comprehensive_rescue_auxfile.csv",
  lgr1 = "intermediate/auxfiles/lgr1_rescue_auxfile.csv",
  lgr2 = "intermediate/auxfiles/lgr2_rescue_auxfile.csv"
)

# Read and combine all auxfiles
all_metadata <- list()

for(name in names(auxfiles)) {
  file_path <- auxfiles[[name]]
  if(file.exists(file_path)) {
    cat("  Reading:", file_path, "\n")
    aux <- read_csv(file_path, show_col_types = FALSE)
    aux$auxfile_source <- name
    all_metadata[[name]] <- aux
    cat("    Loaded", nrow(aux), "measurements\n")
  } else {
    cat("  WARNING: File not found:", file_path, "\n")
  }
}

if(length(all_metadata) == 0) {
  stop("ERROR: No auxfiles found! Check file paths.")
}

# Combine all auxfile metadata
combined_metadata <- bind_rows(all_metadata)
cat("\nTotal metadata rows (before deduplication):", nrow(combined_metadata), "\n")

# Remove duplicates - keep first occurrence of each UniqueID
combined_metadata <- combined_metadata %>%
  distinct(UniqueID, .keep_all = TRUE)
cat("Total metadata rows (after deduplication):", nrow(combined_metadata), "\n")

# Extract plot and other info from UniqueID in metadata
combined_metadata <- combined_metadata %>%
  mutate(
    # Extract plot name (pattern: letters followed by numbers or just letters)
    plot_from_id = str_extract(UniqueID, "[A-Z]+[0-9]*"),
    
    # Extract component (Water, Soil, stem, root, etc)
    component_from_id = case_when(
      grepl("_Water_", UniqueID) ~ "water",
      grepl("_Soil_", UniqueID) ~ "soil",
      grepl("_stem", UniqueID) ~ "stem",
      grepl("_root", UniqueID) ~ "root",
      grepl("_CWD", UniqueID) ~ "CWD",
      TRUE ~ NA_character_
    ),
    
    # Parse DATE if it exists
    date_parsed = if("DATE" %in% names(.)) as.Date(DATE) else NA,
    
    # Extract year and month from DATE
    year_from_date = if("DATE" %in% names(.)) as.integer(format(as.Date(DATE), "%Y")) else NA,
    month_from_date = if("DATE" %in% names(.)) as.integer(format(as.Date(DATE), "%m")) else NA
  )

# Show what we extracted
cat("\nMetadata extraction summary:\n")
cat("  Plots found:", length(unique(combined_metadata$plot_from_id)), "\n")
cat("    ", paste(sort(unique(combined_metadata$plot_from_id)), collapse=", "), "\n")
cat("  Components found:", length(unique(combined_metadata$component_from_id)), "\n")
cat("    ", paste(sort(unique(combined_metadata$component_from_id)), collapse=", "), "\n")
cat("  Dates found:", sum(!is.na(combined_metadata$date_parsed)), "\n")

# =============================================================================
# STEP 2: READ RESCUED FLUX FILES
# =============================================================================

cat("\n\nStep 2: Reading rescued flux files...\n")

rescued_files <- c(
  "intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv",
  "intermediate/rescue/ALL_RESCUED_CO2_BEST_FLUX.csv"
)

rescued_data <- list()

for(file_path in rescued_files) {
  if(file.exists(file_path)) {
    cat("  Reading:", file_path, "\n")
    df <- read_csv(file_path, show_col_types = FALSE)
    rescued_data[[basename(file_path)]] <- df
    cat("    Loaded", nrow(df), "measurements\n")
  } else {
    cat("  WARNING: File not found:", file_path, "\n")
  }
}

if(length(rescued_data) == 0) {
  stop("ERROR: No rescued flux files found! Check file paths.")
}

# Combine rescued flux data
rescued_fluxes <- bind_rows(rescued_data)
cat("\nTotal rescued flux measurements:", nrow(rescued_fluxes), "\n")

# =============================================================================
# STEP 3: MERGE RESCUED FLUXES WITH METADATA
# =============================================================================

cat("\n\nStep 3: Merging rescued fluxes with metadata...\n")

# Columns we want from auxfile metadata
aux_cols <- c("UniqueID", "DATE", "TIME", "start.time",
              "plot_from_id", "component_from_id",
              "date_parsed", "year_from_date", "month_from_date",
              "auxfile_source")

# Drop any conflicting columns from rescued_fluxes before joining
# (they may already exist from a previous run of this script)
conflict_cols <- setdiff(aux_cols, "UniqueID")
rescued_clean <- rescued_fluxes %>%
  select(-any_of(conflict_cols))

# Left join to add metadata to rescued fluxes
rescued_with_metadata <- rescued_clean %>%
  left_join(
    combined_metadata %>%
      select(any_of(aux_cols)),
    by = "UniqueID"
  )

# Check merge success
cat("\nMerge results:\n")
merge_summary <- rescued_with_metadata %>%
  summarise(
    total = n(),
    with_metadata = sum(!is.na(plot_from_id)),
    missing_metadata = sum(is.na(plot_from_id)),
    pct_merged = 100 * sum(!is.na(plot_from_id)) / n()
  )
print(merge_summary)

# Show which UniqueIDs didn't get metadata
if(merge_summary$missing_metadata > 0) {
  cat("\nWARNING: Some rescued fluxes didn't match any auxfile:\n")
  unmatched <- rescued_with_metadata %>%
    filter(is.na(plot_from_id)) %>%
    select(UniqueID, rescued_gas, rescued_analyzer) %>%
    distinct()
  print(unmatched, n = 50)
}

# =============================================================================
# STEP 4: EXTRACT ADDITIONAL INFO FROM UNIQUEID FOR UNMATCHED ROWS
# =============================================================================

cat("\n\nStep 4: Extracting info from UniqueID for unmatched rows...\n")

# For rows that didn't match auxfile, try to extract from UniqueID
rescued_with_metadata <- rescued_with_metadata %>%
  mutate(
    # If no plot from auxfile, try to extract from UniqueID
    plot = if_else(
      is.na(plot_from_id),
      # Pattern like "Oct_22_1_FLM30_stem" -> extract "FLM30"
      str_extract(UniqueID, "(?:_|^)([A-Z]+[0-9]*)(?:_|$)") %>% str_remove_all("_"),
      plot_from_id
    ),
    
    # If no component from auxfile, try to extract from UniqueID
    component = if_else(
      is.na(component_from_id),
      case_when(
        grepl("_Water_|_water", UniqueID, ignore.case = TRUE) ~ "water",
        grepl("_Soil_|_soil", UniqueID, ignore.case = TRUE) ~ "soil",
        grepl("_stem", UniqueID, ignore.case = TRUE) ~ "stem",
        grepl("_root", UniqueID, ignore.case = TRUE) ~ "root",
        grepl("_CWD", UniqueID, ignore.case = TRUE) ~ "CWD",
        grepl("pneum", UniqueID, ignore.case = TRUE) ~ "pneumatophore",
        TRUE ~ NA_character_
      ),
      component_from_id
    ),
    
    # Extract date patterns from UniqueID if no DATE from auxfile
    # Pattern 1: "Oct_22" -> October 2022
    # Pattern 2: "Mar_23" -> March 2023
    month_from_id = if_else(
      is.na(month_from_date) & grepl("(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)_[0-9]{2}", UniqueID),
      case_when(
        grepl("Jan_", UniqueID) ~ 1L,
        grepl("Feb_", UniqueID) ~ 2L,
        grepl("Mar_", UniqueID) ~ 3L,
        grepl("Apr_", UniqueID) ~ 4L,
        grepl("May_", UniqueID) ~ 5L,
        grepl("Jun_", UniqueID) ~ 6L,
        grepl("Jul_", UniqueID) ~ 7L,
        grepl("Aug_", UniqueID) ~ 8L,
        grepl("Sep_", UniqueID) ~ 9L,
        grepl("Oct_", UniqueID) ~ 10L,
        grepl("Nov_", UniqueID) ~ 11L,
        grepl("Dec_", UniqueID) ~ 12L,
        TRUE ~ NA_integer_
      ),
      month_from_date
    ),
    
    year_from_id = if_else(
      is.na(year_from_date) & grepl("(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)_([0-9]{2})", UniqueID),
      # Extract 2-digit year and convert to full year (22 -> 2022, 23 -> 2023)
      as.integer(paste0("20", str_extract(UniqueID, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)_([0-9]{2})") %>% 
                          str_extract("[0-9]{2}$"))),
      year_from_date
    ),
    
    # Create year and month columns for date harmonization
    year = year_from_id,
    month = as.character(month_from_id),
    
    # Create date field if we have year and month
    # Convert DATE to character to match the extracted format
    date = if_else(
      !is.na(year_from_id) & !is.na(month_from_id),
      paste0(month_from_id, "/1/", str_sub(as.character(year_from_id), 3, 4)),  # Format: M/D/YY
      as.character(DATE)
    )
  )

# Summary after extraction
cat("\nAfter UniqueID extraction:\n")
extraction_summary <- rescued_with_metadata %>%
  summarise(
    total = n(),
    with_plot = sum(!is.na(plot)),
    with_component = sum(!is.na(component)),
    with_year = sum(!is.na(year)),
    with_month = sum(!is.na(month))
  )
print(extraction_summary)

# Show examples of extracted data
cat("\nExamples of data with extracted info:\n")
examples <- rescued_with_metadata %>%
  select(UniqueID, rescued_gas, plot, component, year, month, date) %>%
  head(20)
print(examples)

# =============================================================================
# STEP 5: SAVE UPDATED FILES
# =============================================================================

cat("\n\nStep 5: Saving updated rescued flux files...\n")

# Split back into CH4 and CO2
rescued_ch4 <- rescued_with_metadata %>%
  filter(rescued_gas == "CH4")

rescued_co2 <- rescued_with_metadata %>%
  filter(rescued_gas == "CO2")

# Save
write_csv(rescued_ch4, "intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv")
write_csv(rescued_co2, "intermediate/rescue/ALL_RESCUED_CO2_BEST_FLUX.csv")

cat("✓ Saved: intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv\n")
cat("✓ Saved: intermediate/rescue/ALL_RESCUED_CO2_BEST_FLUX.csv\n")

# =============================================================================
# STEP 6: SUMMARY REPORT
# =============================================================================

cat("\n\n===============================================================================\n")
cat("SUMMARY REPORT\n")
cat("===============================================================================\n\n")

cat("Total rescued measurements:", nrow(rescued_with_metadata), "\n")
cat("  CH4:", nrow(rescued_ch4), "\n")
cat("  CO2:", nrow(rescued_co2), "\n\n")

cat("Data completeness:\n")
completeness <- rescued_with_metadata %>%
  summarise(
    has_plot = sum(!is.na(plot)),
    has_component = sum(!is.na(component)),
    has_date = sum(!is.na(date)),
    has_year = sum(!is.na(year)),
    has_month = sum(!is.na(month)),
    total = n()
  ) %>%
  mutate(
    pct_plot = 100 * has_plot / total,
    pct_component = 100 * has_component / total,
    pct_date = 100 * has_date / total,
    pct_year = 100 * has_year / total,
    pct_month = 100 * has_month / total
  )

cat(sprintf("  Plot:      %d/%d (%.1f%%)\n", completeness$has_plot, completeness$total, completeness$pct_plot))
cat(sprintf("  Component: %d/%d (%.1f%%)\n", completeness$has_component, completeness$total, completeness$pct_component))
cat(sprintf("  Date:      %d/%d (%.1f%%)\n", completeness$has_date, completeness$total, completeness$pct_date))
cat(sprintf("  Year:      %d/%d (%.1f%%)\n", completeness$has_year, completeness$total, completeness$pct_year))
cat(sprintf("  Month:     %d/%d (%.1f%%)\n", completeness$has_month, completeness$total, completeness$pct_month))

cat("\nPlots in rescued data:\n")
print(table(rescued_with_metadata$plot, useNA = "always"))

cat("\nComponents in rescued data:\n")
print(table(rescued_with_metadata$component, useNA = "always"))

cat("\nMonths in rescued data:\n")
print(table(rescued_with_metadata$month, useNA = "always"))

cat("\n\n===============================================================================\n")
cat("NEXT STEPS\n")
cat("===============================================================================\n\n")

cat("1. Re-run stitch_all_files.R to combine with main dataset\n")
cat("2. Run date_harmonize.R to create month_year\n")
cat("3. Run summary_table.R for final analysis\n")

cat("\nDONE!\n")