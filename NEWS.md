# cleanCGM 1.0.1

* **Flexible ID Extraction**: Refactored the filename parsing logic to support diverse study prefixes (e.g., "NP-001", "EWR-001"). The package now splits filenames by both underscores (`_`) and hyphens (`-`) and automatically identifies the first numeric component as the Subject ID.
* **Prefix parameter**: Included a prefix parameter that works similarly to suffix.
* **Flexible time parsing**: Fixed a pipeline crash and parsing error by implementing robust, multi-pass date-time detection to handle mid-file format shifts and trailing empty rows.

# cleanCGM 1.0.0

This is the first stable release of `cleanCGM`, representing a significant 
refactor from the internal 0.1.0 version.

### Major Changes & New Features
* **Internationalization Support**: 
    * Implemented a flexible column mapping system that identifies headers in both Spanish and English (e.g., "Histórico glucosa" vs "Historic Glucose").
    * Added localized string detection for sensor low-glucose alerts ("Nivel bajo", "Low", "Faible", "Niedrig").
* **Enhanced Data Cleaning**:
    * **Imputation Logic**: Refined the pipeline to distinguish between missing gaps (handled via LOCF) and physiological low values (handled via Lower Detection Limit imputation).
    * **Decimal Repair**: Added automated detection and correction of localized decimal separators (e.g., converting European commas to dots) to prevent data loss during numeric conversion.
    * **Header Detection**: Implemented a "soft" header removal logic that identifies metadata rows based on non-numeric content rather than hard-coded range values.
* **Timeline & Sorting Robustness**:
    * **Chronological Merging**: Completely refactored `readCGM` to ensure multiple files per subject are sorted by their actual start times rather than filename order.
    * **DST Protection**: Integrated `lubridate` to reconstruct timelines using UTC-based increments, protecting data integrity during Daylight Savings Time changes.
* **Standardized Output Structure**:
    * Output is now strictly organized into `/scans` and `/time series` directories.
    * Timestamps are exported in standardized ISO8601 format.
    * Added automated Quality Control reporting, providing both a `quality_checks.csv` summary and a `quality_check_visualization.pdf` for multi-file sessions.

### Bug Fixes
* Fixed a critical bug where `file_nr` was assigned based on the order of files in the directory rather than chronological sensor start times.
* Resolved an "unreachable code" issue in `formatGlucose` where decimal cleaning occurred after numeric coercion.
* Fixed `fileSep` detection logic that erroneously identified text strings as punctuation separators.

### Documentation & Infrastructure
* Added comprehensive Roxygen2 documentation for all core functions.
* Implemented a modern unit testing suite using `testthat` (3rd Edition).
* Added sample datasets (.csv, .xlsx, .txt) in `inst/extdata` for user training and testing.
* Transitioned to the **Apache License 2.0** with a dedicated `NOTICE` file for institutional IP protection.

---

# cleanCGM 0.1.0

* Initial internal version developed for the **EXTREME project**.
* Basic support for reading FreeStyle Libre `.txt` exports.
* Experimental logic for merging multiple recordings from the same participant.
* Initial Spanish-to-English header translation.
