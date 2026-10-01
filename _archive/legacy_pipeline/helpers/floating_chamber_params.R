# floating_chamber_params.R
# goFlux chamber parameters for the "Floating 8 in" water chamber, built from
# the dimension tables so they track additional_vol.csv / soil_water_dims.csv.
# All water traces are Oct 2022 / Mar 2023, i.e. the large Drierite column.
#
# Usage: params <- floating_chamber_params("LGR")   # or "Picarro"

floating_chamber_params <- function(analyzer) {
  chamber_dims <- readr::read_csv("data/field_notes/dimension_csvs/soil_water_dims.csv",
                                  show_col_types = FALSE)
  chamber_dims <- chamber_dims[chamber_dims$Chamber == "Floating 8 in", ]
  instrument_vol <- readr::read_csv("data/field_notes/dimension_csvs/additional_vol.csv",
                                    show_col_types = FALSE)
  instrument_name <- if (grepl("LGR", analyzer)) "lgr_mgga" else "picarro"
  iv <- instrument_vol[tolower(instrument_vol$instrument) == instrument_name, ]

  Vcham <- chamber_dims$`Chamber+Collar_Volume_L` * 1000   # cm3 (chamber + collar)
  Vinst <- iv$analyzer_cell + iv$drierite_large            # cm3
  list(
    Area   = chamber_dims$Ground_Surface_Area_cm2,         # cm2
    offset = 0,                                            # cm (floating chamber)
    Vcham  = Vcham,
    Vtube  = iv$tubing,                                    # cm3
    Vinst  = Vinst,
    Vtot   = (Vcham + iv$tubing + Vinst) / 1000,           # L
    Pcham  = 101.325                                       # kPa
  )
}
