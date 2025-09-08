# First, let's examine what date information is available across the dataset
df %>%
  summarise(
    has_date = sum(!is.na(date) & date != ""),
    has_datetime = sum(!is.na(datetime) & datetime != ""),
    has_date_parsed = sum(!is.na(date_parsed) & date_parsed != ""),
    has_year_month = sum(!is.na(year) & !is.na(month)),
    total_rows = n()
  )

# Check some examples of rows with different date column combinations
print("Rows with date but no date_parsed:")
df %>% 
  filter(!is.na(date) & date != "" & (is.na(date_parsed) | date_parsed == "")) %>%
  select(date, datetime, date_parsed, year, month) %>%
  head(5)

print("Rows with date_parsed but no date:")
df %>% 
  filter(!is.na(date_parsed) & date_parsed != "" & (is.na(date) | date == "")) %>%
  select(date, datetime, date_parsed, year, month) %>%
  head(5)

# Now create month_year using all available date sources with priority order
df <- df %>%
  mutate(month_year = case_when(
    # Priority 1: Use date_parsed if available (already in YYYY-MM-DD format)
    !is.na(date_parsed) & date_parsed != "" ~ format(as.Date(date_parsed), "%Y-%m"),
    
    # Priority 2: Use datetime if available
    !is.na(datetime) & datetime != "" ~ format(as.Date(datetime), "%Y-%m"),
    
    # Priority 3: Parse the date column (M/D/YY format)
    !is.na(date) & date != "" ~ format(as.Date(date, format = "%m/%d/%y"), "%Y-%m"),
    
    # Priority 4: Use year and month columns directly
    !is.na(year) & !is.na(month) ~ paste(year, sprintf("%02d", month), sep = "-"),
    
    # Default: NA
    TRUE ~ NA_character_
  ))

# Check the results
print("Summary of month_year creation:")
table(df$month_year, useNA = "ifany")

# Check for any remaining NAs
remaining_nas <- df %>% 
  filter(is.na(month_year)) %>%
  select(date, datetime, date_parsed, year, month, month_year)

print(paste("Remaining NAs:", nrow(remaining_nas)))
if(nrow(remaining_nas) > 0) {
  print("Examples of rows that couldn't be processed:")
  head(remaining_nas)
}

# Save the updated dataset
write.csv(df, "combined_gas_flux_dataset_with_month_year.csv", row.names = FALSE)