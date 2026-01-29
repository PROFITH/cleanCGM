#' Extract and Clean Glucose Values from CGM Data
#'
#' @description
#' `formatGlucose` processes raw glucose data by automatically identifying
#' glucose columns, handling international decimal separators, converting units,
#' and imputing specific sensor-error strings.
#'
#' @details
#' The function employs an intelligent discovery logic for glucose data:
#' \itemize{
#'   \item **Column Discovery**: If `glucCol` is NULL, it searches for columns
#'   containing "glucos" (case-insensitive).
#'   \item **Ambiguity Resolution**: If multiple columns are found, it selects
#'   the one with the highest variability (more than 2 unique values) to avoid
#'   static metadata columns.
#'   \item **Localization**: Detects and replaces non-default decimal separators
#'   (e.g., converting European commas to dots).
#'   \item **Unit Conversion**: Automatically converts values from mmol/L to
#'   mg/dL if "mmol/l" is detected in the column header using the formula:
#'   \deqn{mg/dL = mmol/L \times 18.0182}
#' }
#'
#'
#'
#' @param data A data frame containing raw CGM data (typically the output of `readCGM`).
#' @param glucCol Character. Optional. The name of the column containing glucose data.
#' If NULL (default), the function performs automatic detection.
#' @param low_pattern Character. Regex pattern to identify values below sensor range.
#'
#' @section Data Cleaning & Imputation:
#' \itemize{
#'   \item **Header Removal**: Automatically detects and removes 8-row metadata
#'   headers common in specific sensor exports (e.g., rows containing range limits 250/70).
#'   \item **"Nivel bajo" Handling**: Replaces the Spanish string "Nivel bajo"
#'   (often indicating values below sensor range) with `NA`.
#'   \item **Missing Values**: Uses Last Observation Carried Forward (LOCF)
#'   via `data.table::nafill` to fill remaining `NA` values.
#' }
#'
#' @return A numeric vector of glucose values in mg/dL.
#'
#' @importFrom data.table nafill
#' @export
formatGlucose = function(data, glucCol = NULL,
                         low_pattern = "bajo|low|faible|niedrig") {

  # detect the glucose column if not provided
  if (is.null(glucCol)) {

    # columns including "glucos" (this works in Spanish and English)
    glucCol = grep("glucos", colnames(data),
                   ignore.case = TRUE, value = TRUE)

    # If more than 1 column detected, one of them only contains low and high normal range
    if (length(glucCol) > 1) {
      glucCol_lengths = sapply(glucCol, function(x) length(unique(data[[x]])))
      glucCol = glucCol[which(glucCol_lengths > 2)]
    }

    # extract glucose in vector
    if (length(glucCol) == 1) {
      glucose = data[, glucCol]
    } else if (length(glucCol) > 1) {
      # Handle cases where glucose is spread across multiple columns
      tmp = apply(data[, glucCol], 2, is.na)
      if (any(rowSums(tmp) == 0)) {
        glucCol = glucCol[which.max(sapply(glucCol, function(x) length(unique(data[[x]]))))]
        glucose = data[, glucCol]
      } else {
        colIndex = apply(tmp, 1, function(x) ifelse(any(!x), which(!x)[1], 0))
        glucose = rep(NA, nrow(data))
        for (ii in 1:length(glucCol)) {
          idx = which(colIndex == ii)
          glucose[idx] = data[idx, glucCol[ii]]
        }
        glucCol = glucCol[which.max(sapply(glucCol, function(x) length(unique(data[[x]]))))]
      }
    }
  } else {
    glucose = data[, glucCol]
  }



  # --- 1. REFACTOR: Language-agnostic "Low" Detection ---
  # Identifies strings like "Nivel bajo", "Low", "Glucose Low" etc.
  current_ldl = if(grepl("mmol/l", glucCol, ignore.case = TRUE)) 2.2 else 39
  is_low = grep(low_pattern, glucose, ignore.case = TRUE)
  if (length(is_low) > 0) {
    glucose[is_low] = as.character(current_ldl)
  }

  # --- 2. REFACTOR: Handle Decimals BEFORE numeric conversion ---
  # Check if character and contains common non-dot separators
  defaultSep = unlist(options("OutDec"))
  fileSep = gsub('[[:digit:]]+', '', glucose[which(!is.na(glucose))])
  fileSep = unique(fileSep[fileSep != ""])
  if (length(fileSep) > 0) {
    if (nchar(fileSep) > 0) {
      if (fileSep != defaultSep) {
        glucose = gsub(fileSep, defaultSep, glucose)
      }
    }
  }

  # --- 3. REFACTOR: Safer Header Removal ---
  # Instead of hardcoding 250/70, we look for typical Libre header patterns
  # only if the first few rows are clearly non-numeric text
  if (length(glucose) > 8) {
    head_check = glucose[1:8]
    # If more than 50% of the first 8 rows are non-numeric and not "Low"
    # it's likely a header
    if (sum(is.na(as.numeric(head_check))) > 4) {
      # Check for 250/70 as a specific sub-case for PROFITH
      if (any(grepl("250", head_check)) && any(grepl("70", head_check))) {
        glucose = glucose[-c(1:8)]
      }
    }
  }

  # --- 4. Conversion and Imputation ---
  glucose = as.numeric(glucose)

  # Impute missing values using LOCF
  glucose = data.table::nafill(glucose, "locf")

  # --- 5. Unit Conversion ---
  # 18.0182 is the high-precision factor for mmol/L to mg/dL
  if (grepl("mmol/l", glucCol, ignore.case = TRUE)) {
    glucose = glucose * 18.0182
  }

  # return glucose vector
  return(glucose)
}
