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
#'
#'
#' @param files Character vector. Path(s) to the CGM files. If multiple files
#' are provided, they are assumed to belong to the same subject and will be
#' merged.
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
#' @export
readCGM = function(files) {

  # loop through files to load data -------
  datasets = list()
  length(datasets) = length(files)
  for (i in 1:length(files)) {

    # format of files
    format = unique(tools::file_ext(files))
    if (format == "txt") {
      datasets[[i]] = read.delim(files[i], sep = '\t', header = TRUE)
      NAs = which(is.na(datasets[[i]]$ID))
      if (length(NAs) > 0) datasets[[i]] = datasets[[i]][-NAs,]

      # reformat dataset?
      NAs = which(is.na(datasets[[i]]$Tipo.de.registro) & is.na(datasets[[i]]$Histórico.glucosa..mg.dL.) &
                    is.na(datasets[[i]]$Glucosa.leída..mg.dL.))
      if (length(NAs) > 0) {
        datasets[[i]]$Tipo.de.registro[NAs] = 0
        datasets[[i]]$Histórico.glucosa..mg.dL.[NAs] = datasets[[i]]$Hora[NAs]
        datasets[[i]]$Hora[NAs] = datasets[[i]]$ID[NAs]
        datasets[[i]]$ID[NAs] = 0
        # fix timestamp format in scan rows
        tsRevise = 1:(NAs[1] - 1)
        newTS = strptime(datasets[[i]]$Hora[tsRevise], format = "%Y/%m/%d %H:%M")
        newTS = format(newTS, format = "%d/%m/%Y %H:%M")
        datasets[[i]]$Hora[tsRevise] = newTS
        # remove extra rows if any
        extra_rows = which(datasets[[i]]$Hora == "" & datasets[[i]]$Histórico.glucosa..mg.dL. == "")
        if (length(extra_rows) > 0) datasets[[i]] = datasets[[i]][-extra_rows,]
      }


    } else if (format == "xlsx") {
      datasets[[i]] = openxlsx::read.xlsx(files[i], sheet = 1, startRow = 3)
      # colnames similar to .txt files
      colnames(datasets[[i]]) = gsub(".", " ", colnames(datasets[[i]]), fixed = TRUE)
    } else if (format == "csv") {
      datasets[[i]] = suppressWarnings(data.table::fread(files[i],
                                                         data.table = FALSE,
                                                         verbose = FALSE))
      # colnames similar to .txt files
      colnames(datasets[[i]]) = gsub(".", " ", colnames(datasets[[i]]), fixed = TRUE)
    } else {
      stop(paste0("Format ", format, " not supported at the moment"))
    }
  }
  # indices for rbinding
  inds = 1
  if (length(datasets) > 1) {
    t0 = c()
    for (i in 1:length(datasets)) {
      t0 = c(t0,as.POSIXct(formatTime(head(datasets[[i]], 1))$timestamp))
    }
    inds = order(t0)
    for (i in 1:length(datasets)) {
      datasets[[i]]$file_nr = inds[i]
      datasets[[i]]$filename = basename(files[i])
    }
  } else {
    datasets[[1]]$file_nr = 1
    datasets[[1]]$filename = basename(files[i])
  }
  # rbind datasets
  DAT = do.call("rbind", datasets[inds])

  return(DAT)
}
