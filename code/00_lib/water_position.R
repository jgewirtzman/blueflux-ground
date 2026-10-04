# Water-chamber position from the field collar-location note (BlueFlux Dataset_soils_water.csv).
# Channel / open-water placements are named in the note; unlabelled chambers are the standard
# placement over the flooded forest floor (the ghost and regenerating sites have no channels).
WATER_OFFPLOT_PAT <- "river|open|off peir|off pier|pier|interface|edge"
water_position <- function(collar_location)
  ifelse(!is.na(collar_location) & grepl(WATER_OFFPLOT_PAT, collar_location, ignore.case = TRUE),
         "channel / open water", "above forest floor")
