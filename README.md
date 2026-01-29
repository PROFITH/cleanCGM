# cleanCGM

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/PROFITH/cleanCGM/workflows/R-CMD-check/badge.svg)](https://github.com/PROFITH/cleanCGM/actions)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

`cleanCGM` is an R package developed by the [PROFITH Research Group](https://profith.ugr.es/) for the advanced preprocessing and cleaning of Continuous Glucose Monitoring (CGM) data.

While optimized for the **PROFITH group projects**, it is designed to handle international sensor exports (FreeStyle Libre, Dexcom) by resolving common data artifacts like column shifts, timezone distortions, and localized decimal separators.

### Why cleanCGM?

While excellent packages like `iglu`, `cgmanalysis`, or `CGMprocessing` exist for glucose analysis, they often assume "clean" input data in English formats. `cleanCGM` was developed specifically to bridge the gap between **raw sensor exports** and **analysis-ready data**, particularly for clinical research settings using FreeStyle Libre.

#### Key Differentiators:

-   **Localization Support**: Handles Spanish-language headers (e.g., *Histórico de glucosa*, *Tipo de registro*) that typically cause other packages to fail.

-   **Format Agnostic**: Seamlessly processes `.xlsx`, `.csv`, and `.txt` files in a single pipeline.

-   **Error Repair**: Specifically engineered to detect and fix inconsistencies introduced by manual file editing or software export glitches common in large-scale research projects like EXTREME.

-   **Automatic Unit Standardization**: Automatically detects if data is in `mmol/L` or `mg/dL` and standardizes the output.

-   **Focused Scope**: Unlike other packages that perform statistical analysis, `cleanCGM` focuses exclusively on the "Signal Cleaning" phase, providing high-fidelity raw data for downstream researchers.

#### Output Structure

The package produces a standardized directory structure to ensure reproducibility:

1.  **`scans/`**: Contains one `.RData` file per input. Each contains a `SCANS` data frame (Timestamp in ISO8601 and Glucose in mg/dL) representing manual "flash" readings.

2.  **`time series/`**: Contains one `.RData` file per input. Each contains a `TS` data frame (Timestamp in ISO8601 and Glucose in mg/dL) representing the continuous automated recordings.

3.  **Quality Control (Multi-file IDs)**: If a participant has multiple sensor sessions, the package generates:

    -   `quality_checks.csv`: A detailed report on gaps, duplicates, and overlaps.

    -   `quality_check_visualization.pdf`: A visual timeline of how the multiple files were merged.

> NOTE: the ISO8601 format was selected for the output as it makes it output compatible with almost any other analysis tool (Python, R, or Excel).

## Installation

``` r
# install.packages("remotes")
remotes::install_github("PROFITH/cleanCGM")
```

## Quick Start

### 1. Load Sample Data

The package includes sample datasets for testing. Access them using `system.file`.

``` r
library(cleanCGM)  
# Locate a sample file 
file_path <- system.file("extdata", "sample_cgm_txt_spanish.txt", package = "cleanCGM")
# Read
raw_data <- readCGM(file_path) 
```

### 2. Full Pipeline

The `cleanCGM()` function runs the entire cleaning logic, saves processed RData files, and generates a Quality Control report.

``` r
cleanCGM(
  datadir = "path/to/raw_data",
  outputdir = "path/to/output",
  suffix = "_StudyV1" ) 
```

## Advanced Configuration: Column Mapping

If your sensor uses non-standard column names, you can provide a custom map:

```         
custom_map <- list(
  id = "Subject_ID",
  time = "Timestamp_UTC",
  hist_gluc = "Glucose_Value" )

data <- readCGM(files, col_map = custom_map) 
```

## Quality Control Outputs

The package generates a `quality_checks.csv` file for every run, reporting:

-   **Gaps**: Recordings missing for \>30 minutes.

-   **Duplicates**: Multiple readings for the same timestamp.

-   **Sequentiality**: Timeline "jumps" or clock reset errors.

-   **Overlaps**: Analysis of how multiple files were merged.

## Contributing

The PROFITH Research Group welcomes contributions. If you encounter a sensor format that is not currently supported, please open an Issue with a sample header.

## License

APACHE © PROFITH Research Group.
