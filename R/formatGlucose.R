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
formatGlucose = function(data, glucCol = NULL) {
  if (is.null(glucCol)) {
    glucCol = grep("glucos", colnames(data),
                   ignore.case = TRUE, value = TRUE)
    if (length(glucCol) > 1) {
      glucCol_lengths = rep(NA, length(glucCol))
      for (glucCol_i in 1:length(glucCol)) {
        glucCol_lengths[glucCol_i] = length(table(data[, glucCol[glucCol_i]]))
      }
      glucCol = glucCol[which(glucCol_lengths > 2)]
    }
    # extract glucose in vector
    if (length(glucCol) == 1) {
      glucose = data[, glucCol]
    } else {
      tmp = apply(data[, glucCol], 2, is.na)
      if (any(rowSums(tmp) == 0)) {
        glucCol = glucCol[which.max(glucCol_lengths)]
        glucose = data[, glucCol]
      } else {
        colIndex = apply(tmp, 1, function(x) ifelse(any(x == F),
                                                    which(x == F), 0))
        glucose = rep(NA, nrow(data))
        for (ii in 1:length(glucCol)) {
          glucose[which(colIndex == ii)] = data[which(colIndex == ii), glucCol[ii]]
        }
        # keep only one glucCol for future calculations
        glucCol = glucCol[which.max(glucCol_lengths)]
      }
    }
    # check if decimal sep is other than default
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
    # remove header
    if (all(is.na(as.numeric(glucose[c(1, 8)]))) &
        all(!is.na(as.numeric(glucose[c(6, 7)])))) {
      if (as.numeric(glucose[6]) == 250 & as.numeric(glucose[7]) == 70) {
        glucose = glucose[-c(1:8)]
      }
    }
    # impute missing values if needed
    toImpute = grep("bajo|low|faible", glucose, ignore.case = TRUE)
    glucose[toImpute] = NA
    glucose = as.numeric(glucose)
    glucose = data.table::nafill(glucose, "locf")
    # check units
    if (is.character(glucose)) glucose = gsub(",", ".", glucose, fixed = TRUE)
    glucose = as.numeric(glucose)
    if (grepl("mmol/l", glucCol, ignore.case = TRUE)) glucose = glucose*18.0182
  }
  return(glucose)
}
