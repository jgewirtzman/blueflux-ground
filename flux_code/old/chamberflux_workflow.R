# Workflow for processing raw LGR/Picarro data and calculating
# chamber flux on different dates at different soil chambers

library(tidyverse)
library(lubridate)
library(data.table)

# Load local functions
#file_sources <- list.files("R/functions", pattern="*.R", full.names = TRUE)
#sapply(file_sources, source, .GlobalEnv)
source("/Users/jongewirtzman/Google Drive/Research/Blueflux/Blueflux Tree Methane/Blueflux - Yale Ground Team - Shared/flux_code/format_LGR_output.R")
source("/Users/jongewirtzman/Google Drive/Research/Blueflux/Blueflux Tree Methane/Blueflux - Yale Ground Team - Shared/flux_code/calculate_chamber_flux_lgr2.R")


date_time <- read_csv("/Users/jongewirtzman/Downloads/compiled - compiled.csv") %>%
  mutate(dates = lubridate::mdy(date),
         UniqueID = index)
date_time$flux_start<-paste(date_time$dates, date_time$start_time)
date_time$flux_end<-paste(date_time$dates, date_time$end_time)
date_time$start_time = lubridate::ymd_hms(date_time$flux_start)
date_time$end_time = lubridate::ymd_hms(date_time$flux_end)

#lgr3
data_path <-  "/Users/jongewirtzman/Google Drive/Blueflux Raw Data/LGR2"
raw_files <- list.files(data_path, full.names=TRUE)
date_time<-date_time[which(date_time$analyzer_id=="LGR2"),]


####NEED TO UPDATE VOLUME CALC#####
date_time$rep_vol_L<-

#%>%filter(!is.na(start_time))

# Flux processing settings - change these for your application
init <- list()
init$analyzer <- "lgr" # can be "picarro" or "lgr"
init$data_path <- data_path # path to analyzer files
init$startdelay <- 20 # 20s delay for Picarro
init$fluxend   <- 3 # minutes to include data after start (will ignore if end times are in date_time)
init$surfarea  <- pi*(0.23)^2  #m^2, buckets
init$vol_system <- 0.315 + 0.001 # interior volume of picarro = 0.0316 L 
init$plotslope <- 1 # make a plot with the slope: 0 = off
init$outputfile <- 1 # write an output file: 0 = off
init$outfilename <- "blueflux_lgr2_ch4.csv"


# Calculate soil CO2 & CH4 flux for each measurement date & replicate

flux_data <- calculate_chamber_flux(raw_files, date_time, init)          

# AFTER THIS YOU CAN EDIT TO MERGE WHATEVER OTHER DATA YOU WANT

merged_data<-left_join(date_time, flux_data)
write_csv(merged_data, "lgr2_data.csv")
