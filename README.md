# BlueFlux Ground

A comprehensive R-based analysis pipeline for processing greenhouse gas flux measurements from mangrove and coastal wetland ecosystems. This project processes data from multiple gas analyzers (Los Gatos Research and Picarro instruments) to quantify CO₂ and CH₄ fluxes from tree stems and soil/water surfaces.

## 🔬 Project Overview

BlueFlux Ground is part of a broader research initiative studying greenhouse gas dynamics in blue carbon ecosystems. The project provides automated workflows for:

- **Multi-instrument data processing**: LGR UGGA (3 units) and Picarro G4301 analyzers
- **Dual measurement types**: Tree stem emissions and soil/water surface fluxes  
- **Comprehensive QA/QC**: Automated quality control with manual validation
- **Standardized outputs**: Harmonized datasets ready for analysis and publication

**Key Features:**
- Processes 867 files totaling ~1 GB of analyzer data
- Handles complex spatiotemporal data from 2022-2023 field campaigns
- Automated flux calculation using the goFlux R package
- Integrated environmental data (temperature, salinity, redox conditions)

## 📁 Project Structure

```
blueflux-ground/
├── analyzer_data/           # Raw instrument data (463 files, 🔒 Not for direct editing)
│   ├── LGR_GLA131/         # Los Gatos Research analyzer data
│   │   ├── LGR1/           # Analyzer unit 1 data
│   │   ├── LGR2/           # Analyzer unit 2 data
│   │   └── LGR3/           # Analyzer unit 3 data
│   └── Picarro_G4301/      # Picarro analyzer data
├── flux_code/              # Analysis scripts and workflows
│   ├── goflux/            # Main processing scripts (37 R files)
│   ├── preprocess/        # Data preparation utilities
│   ├── results/           # Processed flux calculations
│   ├── plots/             # Generated visualizations (40 PDFs)
│   └── analysis/          # Summary analysis scripts
├── field_notes/           # Field measurements and metadata
│   ├── entered-data/      # Manual data entry sheets
│   ├── dimension_calcs/   # Chamber volume calculations
│   └── Scanned_Datasheets/ # Original field datasheets
├── env-data/              # Environmental data
│   ├── salinity/          # Salinity measurements
│   └── porewater-gas/     # Porewater gas concentrations
└── RData/                 # Processed R data objects (165 files)
```

## 🚀 Quick Start

### Prerequisites

**Required R Packages:**
```r
# Core analysis packages
install.packages(c("dplyr", "readr", "lubridate", "ggplot2"))

# Flux analysis (install from GitHub)
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Qepanna/goFlux")

# Additional utilities
install.packages(c("readxl", "openxlsx", "shiny", "plotly"))
```

**System Requirements:**
- R ≥ 4.0.0
- RStudio (recommended)

### Basic Usage

1. **Clone and setup:**
```bash
git clone <repository-url>
cd blueflux-ground
```

2. **Run complete analysis:**
```r
# Open RStudio project
file.edit("blueflux-ground.Rproj")

# Process all analyzer data
source("flux_code/goflux/flexible_import.R")

# Generate summary plots
source("flux_code/analysis/initial_summary_plots.R")
```

3. **View results:**
- **Final dataset**: `combined_gas_flux_dataset_complete.csv`
- **Plots**: `flux_code/plots/` directory
- **QC reports**: `flux_code/diagnostics/` directory

## 📊 Data Processing Workflow

### 1. Data Import and Preparation
```r
# Import raw analyzer data
source("flux_code/goflux/lgr_1_import.R")  # LGR1 data
source("flux_code/goflux/lgr_2_import.R")  # LGR2 data  
source("flux_code/goflux/lgr_3_import.R")  # LGR3 data
```

### 2. Flux Calculations
```r
# Tree flux processing
source("flux_code/goflux/goflux_lgr1_trees.R")
source("flux_code/goflux/goflux_lgr2_trees.R") 
source("flux_code/goflux/goflux_lgr3_trees.R")

# Soil/water flux processing
source("flux_code/goflux/goflux_lgr1_soil.R")
source("flux_code/goflux/goflux_lgr2_soil.R")
source("flux_code/goflux/goflux_lgr3_soil.R")
```

### 3. Data Integration
```r
# Combine all datasets
source("flux_code/goflux/stitch_all_files.R")
```

## 🔬 Measurement Types

### Tree Stem Fluxes
- **Target**: CO₂ and CH₄ emissions from mangrove tree stems
- **Method**: Closed-chamber measurements with tree collars
- **Duration**: 2-5 minute measurement periods

### Soil/Water Surface Fluxes  
- **Target**: CO₂ and CH₄ exchange at soil-atmosphere interface
- **Method**: Floating chambers on water; cylinder chambers inserted in sediment surface
- **Duration**: 2-5 minute measurement periods  

## 📈 Key Outputs

### Primary Datasets
- **`combined_gas_flux_dataset_complete.csv`**: Master dataset (1.4 MB, all measurements)
- **`flux_code/results/trees/`**: Tree-specific flux calculations
- **`flux_code/results/surface/`**: Soil/water flux calculations

### Quality Control
- **Manual validation**: Interactive peak identification for questionable measurements
- **Automated QC**: R² thresholds, concentration range checks, temporal filters
- **Diagnostic plots**: Visual inspection of all flux calculations

## Field Site Context

**Study System**: Florida mangrove ecosystems  
**Measurement Period**: March 2022 - March 2023  

**Field Sites**: 
- **Flamingo and Christian Point**: Ghost forest sites
- **Bear Lake**: Regenerating mangrove forest 
- **SE-1 (US-EvM)**: Scrub mangrove at ecotone/sawgrass with saltwater intrusion and mangrove invasion
- **Marco Island**: Mangrove die-off ghost forest at edge of Ten Thousand Islands
- **RB10**: Rookery Bay living mangroves 
- **SRS5 (Gunboat Island)**: Intermediate stature mangrove forest, Shark River Slough
- **SRS6 (Lower Shark)**: Large stature mangrove forest, Shark River Slough

**Target Species**: *Rhizophora mangle*, *Avicennia germinans*, *Conocarpus erectus*

## Advanced Usage

**Interactive quality control:**
```r
# Launch Shiny app for manual flux validation
source("flux_code/preprocess/offset_finder_app.R")
```
**ZIP file extraction errors:**
- Scripts automatically handle compressed data files
- Manual extraction may be needed for corrupted archives

**Timezone mismatches:**
- All data standardized to UTC
- Local time conversions handled automatically

**Missing environmental data:**
- Scripts include gap-filling algorithms
- Manual data entry templates provided

## Dependencies

### Core R Packages
- **goFlux**: Flux calculation engine
- **dplyr**: Data manipulation  
- **ggplot2**: Visualization
- **lubridate**: Date/time handling
- **readr/readxl**: Data import

### Data Processing
- **shiny/plotly**: Interactive applications
- **openxlsx**: Excel file handling

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

For questions about data processing workflows, analysis methods, or collaborative opportunities, please contact Jon Gewirtzman (jonathan.gewirtzman@yale.edu).

## Related Publications

Poulter, B., Adams-Metayer, F. M., Amaral, C., Barenblitt, A., Campbell, A., Charles, S. P., Roman-Cuesta, R. M., D'Ascanio, R., Delaria, E. R., Doughty, C., Fatoyinbo, T., Gewirtzman, J., Hanisco, T. F., Hull, M., Kawa, S. R., Hannun, R., Lagomasino, D., Lait, L., Malone, S. L., Newman, P. A., Raymond, P., Rosentreter, J. A., Thomas, N., Vaughn, D., Wolfe, G. M., Xiong, L., Ying, Q., & Zhang, Z. (2023). Multi-scale observations of mangrove blue carbon ecosystem fluxes: The NASA Carbon Monitoring System BlueFlux field campaign. Environmental Research Letters, 18(7), 075009. https://doi.org/10.1088/1748-9326/acdae6Retry
