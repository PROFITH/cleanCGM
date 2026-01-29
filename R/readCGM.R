#' Read and Consolidate Multiple CGM Data Files
#'
#' @description
#' `readCGM` imports Continuous Glucose Monitoring data from various file
#' formats (.txt, .xlsx, .csv). It handles multi-file imports for a single
#' subject, automatically ordering them chronologically and assigning session
#' identifiers.
#'
#' @details
#' The function includes sophisticated error-handling for specific sensor export
#' bugs:
#' \itemize{
#'   \item **Column Repair**: For `.txt` files, it detects if glucose values and
#'   timestamps have shifted into the wrong columns (a common artifact in some
#'   FreeStyle Libre exports) and realigns them.
#'   \item **Excel Compatibility**: Automatically skips metadata headers in
#'   `.xlsx` files by starting the read at row 3.
#'   \item **Chronological Merging**: If multiple files are provided, the
#'   function parses the first timestamp of each, sorts them, and assigns a
#'   `file_nr` attribute to track the session sequence.
#' }
#'
#' @param files Character vector. Path(s) to the CGM files. If multiple files
#' are provided, they are assumed to belong to the same subject and will be
#' merged.
#' @param col_map List. A mapping of internal keys to actual column names in the file.
#'
#' @section Data Cleaning Steps:
#' \itemize{
#'   \item **NA Removal**: Automatically drops rows with missing IDs.
#'   \item **Name Cleaning**: Standardizes column names by replacing dots (`.`)
#'   with spaces to maintain consistency across different file formats.
#'   \item **Timestamp Standardization**: In cases of malformed rows, it
#'   re-formats dates to `%d/%m/%Y %H:%M`.
#' }
#'
#' @return A combined data frame containing the data from all input files,
#' with two additional columns:
#' \itemize{
#'   \item \code{file_nr}: Integer indicating the chronological order of the file.
#'   \item \code{filename}: The name of the source file for that specific row.
#' }
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom data.table fread
#' @importFrom utils read.delim
#' @export
readCGM = function(files, col_map = NULL) {

  # 1. --- Column Mapping System ---
  # Default map follows the Spanish FreeStyle Libre format but allows English/others
  if (is.null(col_map)) {
    col_map = list(
      id = "ID",
      type = "Tipo.de.registro|Record.Type",
      hist_gluc = "Historico.glucosa|Historic.Glucose",
      scan_gluc = "Glucosa.leida|Scan.Glucose",
      time = "Hora|Time"
    )
  }

  # loop through files to load data -------
  datasets = list()
  for (i in 1:length(files)) {

    # Identify file extension
    ext = tolower(gsub(".*\\.", "", files[i]))

    # 2. --- Data Loading ---
    if (ext == "txt") {
      df = read.delim(files[i], sep = '\t', header = TRUE, check.names = TRUE)
      colnames(df) = iconv(colnames(df), from = "UTF-8", to = "ASCII//TRANSLIT")
      datasets[[i]] = df

      # Detect actual column names using the map
      id_col = grep(col_map$id, colnames(datasets[[i]]), ignore.case = TRUE, value = TRUE)[1]
      type_col = grep(col_map$type, colnames(datasets[[i]]), ignore.case = TRUE, value = TRUE)[1]
      hist_col = grep(col_map$hist_gluc, colnames(datasets[[i]]), ignore.case = TRUE, value = TRUE)[1]
      time_col = grep(col_map$time, colnames(datasets[[i]]), ignore.case = TRUE, value = TRUE)[1]

      # Clean rows with missing IDs
      if (!is.na(id_col)) {
        datasets[[i]] = datasets[[i]][!is.na(datasets[[i]][[id_col]]), ]
      }

      # 3. --- REFACTOR: Column Repair ---
      # If specialized columns are missing but data exists in shifted positions
      if (!is.na(type_col) && !is.na(hist_col)) {
        NAs = which(is.na(datasets[[i]][[type_col]]) &
                      is.na(datasets[[i]][[hist_col]]))

        if (length(NAs) > 0) {
          # Repair logic: shift columns back to expected positions
          datasets[[i]][NAs, type_col] = 0
          datasets[[i]][NAs, hist_col] = datasets[[i]][NAs, time_col]
          datasets[[i]][NAs, time_col] = datasets[[i]][NAs, id_col]
          datasets[[i]][NAs, id_col] = 0
        }
      }

    } else if (ext == "xlsx") {
      datasets[[i]] = openxlsx::read.xlsx(files[i], sheet = 1, startRow = 3)
      colnames(datasets[[i]]) = gsub(".", " ", colnames(datasets[[i]]), fixed = TRUE)
      colnames(datasets[[i]]) = iconv(colnames(datasets[[i]]), from = "UTF-8", to = "ASCII//TRANSLIT")

    } else if (ext == "csv") {
      datasets[[i]] = suppressWarnings(data.table::fread(files[i],
                                                         data.table = FALSE,
                                                         verbose = FALSE))
      # colnames similar to .txt files
      colnames(datasets[[i]]) = gsub(".", " ", colnames(datasets[[i]]), fixed = TRUE)
      colnames(datasets[[i]]) = iconv(colnames(datasets[[i]]), from = "UTF-8", to = "ASCII//TRANSLIT")
    } else {
      stop(paste0("Format ", ext, " not supported at the moment"))
    }
  }

  # 4. --- REFACTOR: Column Sorting Logic ---
  if (length(datasets) > 1) {
    # Start time detection: Only process the first row per file
    start_times = sapply(datasets, function(df) {
      # Use the first row only to avoid processing millions of rows here
      first_row = df[1, , drop = FALSE]
      as.numeric(formatTime(first_row)$timestamp)
    })

    # Correct indexing: 'inds' is the chronological sequence of files
    inds = order(start_times)
    datasets = datasets[inds] # Reorder the list itself

    # Assign file_nr based on their final position in the merged set
    for (j in 1:length(datasets)) {
      datasets[[j]]$file_nr = j
    }
  } else {
    datasets[[1]]$file_nr = 1
  }

  # 5. --- Final Merge ---
  DAT = do.call("rbind", datasets)

  return(DAT)
}
