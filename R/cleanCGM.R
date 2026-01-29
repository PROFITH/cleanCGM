#' Full Pipeline for CGM Data Cleaning and Preprocessing
#'
#' @description
#' `cleanCGM` is the core function of the package. It automates the process of
#' reading raw Continuous Glucose Monitoring (CGM) files, identifying subjects
#' via filenames, resolving overlaps between multiple sensor sessions, and
#' performing quality control checks.
#'
#' @details
#' The function expects filenames to follow a specific pattern where the ID is
#' the second element when split by underscores (e.g., `STUDY_ID123_EXTREME.xlsx`).
#' If the second element is not numeric or doesn't match specific project codes
#' (ASP, MG, NF, NVS), it defaults to the first element.
#'
#'
#'
#' @param datadir Character. Path to the directory containing raw CGM files
#' (supports `.txt` and `.xlsx`).
#' @param outputdir Character. Path where processed data and reports will be stored.
#' @param timeCol Character. Name of the column containing timestamps.
#' If NULL, the function attempts to find it automatically.
#' @param glucCol Character. Name of the column containing glucose values.
#' @param typeCol Character. Name of the column distinguishing between
#' "scans" (manual checks) and "historic/time series" (automatic measures).
#' @param suffix Character. Optional string to append to output filenames
#' (e.g., "_v1").
#' @param verbose Logical. If `TRUE`, prints processing progress and
#' identified IDs to the console.
#'
#' @section Process Workflow:
#' 1. **ID Extraction**: Parsed from filenames.
#' 2. **Reading**: Imports data using `readCGM`.
#' 3. **Validation**: Skips subjects with fewer than 10 recordings.
#' 4. **Overlap Resolution**: If a participant has multiple files, the function
#' identifies time overlaps and prioritizes newer data or sequential merges.
#' 5. **Quality Control**: Generates a CSV summarizing gaps, duplicates, and overlaps.
#' 6. **Visualization**: If multiple files per ID exist, a PDF is generated to
#' visualize the merging logic.
#'
#' @return No object is returned to the R environment. The function writes:
#' \itemize{
#'   \item \code{/scans/}: RData files containing manual scan data.
#'   \item \code{/time series/}: RData files containing automated sensor readings.
#'   \item \code{quality_checks.csv}: A summary of data integrity per subject.
#'   \item \code{quality_check_visualization.pdf}: Visual plots for overlapping sensor sessions.
#' }
#'
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics legend lines par
#' @importFrom utils write.csv
#' @export
cleanCGM = function(datadir = NULL, outputdir = NULL,
                    timeCol = NULL, glucCol = NULL, typeCol = NULL,
                    prefix = "", suffix = "",
                    verbose = TRUE) {
  # do directories exist?
  if (is.null(datadir)) stop("Please, specify the datadir.")
  if (is.null(outputdir)) stop("Please, specify the outputdir.")

  # print in console...
  if (verbose) {
    cat('\n')
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat("Processing data...\n")
    cat("ID: ")
  }

  # list of files to be read
  FILES = dir(datadir, full.names = TRUE)

  # extract IDs from filenames
  IDs = data.frame(filename = FILES, id = NA)

  for (i in 1:length(FILES)) {
    # Split by underscore (_) OR hyphen (-)
    # [_-] is a regex character class for both symbols
    id_parts = unlist(strsplit(basename(FILES[i]), split = "[_-]"))

    # Find the first part that contains at least one number
    # This handles "ID", "NP", or "EWR" prefixes automatically
    numeric_index = which(grepl("[0-9]", id_parts))[1]

    # Default to part 1 if no number is found, otherwise take the identified ID
    if (!is.na(numeric_index)) {
      id = id_parts[numeric_index]
    } else {
      id = id_parts[1]
    }

    IDs[i, 2] = id
  }
  ids = unique(IDs$id)

  # back up of typeCol
  typeCol_bu = typeCol

  # loop through ids
  doublefile = 0
  QC = data.frame(ID = ids,
                  n_files = 1,
                  files_overlap_hours_1_2 = NA,
                  files_overlap_hours_2_3 = NA,
                  files_overlap_hours_3_4 = NA,
                  less_than_10recordings = FALSE,
                  n_gaps_over_30min = 0,
                  duplicated_timestamps = FALSE,
                  no_sequential_timestamps = FALSE)
  for (i in 1:length(ids)) {
    id = ids[i]
    if (verbose) cat(id, " ")
    # files to read -----
    files2read = IDs$filename[which(IDs$id == id)]
    nfiles = length(files2read)
    QC$n_files[i] = nfiles
    # 1 - read CGM file/s
    data = as.data.frame(readCGM(files = files2read))
    if (nrow(data) < 10) {
      cat("skipped because insufficient data collected")
      QC$less_than_10recordings[i] = TRUE
      next
    }

    # 2 - mark scan and time series
    typeCol = typeCol_bu
    if (is.null(typeCol)) {
      typeCol = grep("type|tipo", colnames(data),
                     ignore.case = TRUE, value = TRUE)
      if (length(typeCol) > 1) {
        if (any(grepl("evento", typeCol))) {
          typeCol = typeCol[-grep("evento", typeCol)]
        }
      }
    }
    # categories
    if (length(typeCol) > 0) {
      ts = which(data[, typeCol] == 0)
      scans = which(data[, typeCol] == 1)
    } else {
      scans = c()
      ts = 1:nrow(data)
    }

    # 2 - format timestamp
    timestamp = formatTime(data, timeCol = timeCol, ts_indices = ts, scan_indices = scans)
    QC$n_gaps_over_30min[i] = timestamp$n_gaps_over_30min
    QC$duplicated_timestamps[i] = timestamp$duplicated_timestamps
    QC$no_sequential_timestamps[i] = timestamp$no_sequential_timestamps
    ts_indices = timestamp$ts_indices # removed timestamps that overlap
    timestamp = timestamp$timestamp

    # 3 - format glucose
    glucose = formatGlucose(data, glucCol = glucCol)
    # adjustment for PREFIT UP data (first rows )
    if (length(glucose) > length(timestamp)) {
      diff = length(glucose) - length(timestamp)
      glucose = glucose[(diff + 1):length(glucose)]
    }

    # 4 - if more than 1 file, visualize
    if (nfiles > 1) {
      colors = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")
      doublefile = doublefile + 1
      if (doublefile == 1) {
        pdffilepath = file.path(outputdir, paste0("quality_check_visualization_",prefix,"_",suffix,".pdf"))
        pdf(pdffilepath, width = 10, height = 7)
      }
      xlim = range(timestamp[ts])
      ylim = range(glucose[ts])
      t0 = t1 = c()
      for (fi in 1:nfiles) {
        select = ts[which(data$file_nr[ts] == fi)]
        if (fi == 1) {
          par(las = 1)
          plot(x = timestamp[select], y = glucose[select],
               type = "l", col = colors[fi],
               xlim = as.numeric(xlim), ylim = ylim,
               xlab = "", ylab = "Glucose (mg/dL)",
               main = id)
        } else {
          lines(x = timestamp[select], y = glucose[select],
               type = "l", col = colors[fi])
        }
        # check overlapping in files
        t0 = c(t0, as.POSIXct(min(timestamp[select])))
        t1 = c(t1, as.POSIXct(max(timestamp[select])))
      }
      legend("top", lty = 1, col = colors[1:nfiles], bty = "n",
             legend = c(paste("recording", 1:nfiles)), horiz = T)
      # handle overlapping files
      overlaps = as.numeric(difftime(t0[2:nfiles], t1[1:(nfiles - 1)], units = "hours"))
      if (any(overlaps < 0)) {
        if (t1[2] < t1[1]) {
          # then second device starts after first, and ends before first
          # decision: keep only first
          ts = ts[which(data$file_nr[ts] == 1)]
        } else {
          # then, second device starts before first finishes, but ends after
          # decision: keep first and then second (new prevails)
          ts1 = which(data$file_nr[ts] == 1)
          ts2 = which(data$file_nr[ts] == 2)
          del = which(timestamp[ts][ts1] > t0[2])
          ts = ts[-del]
        }
      }
      # store in QC
      if (length(overlaps) < 3) overlaps = c(overlaps, NA, NA)[1:3]
      QC[i, c("files_overlap_hours_1_2",
              "files_overlap_hours_2_3",
              "files_overlap_hours_3_4")] = overlaps
    }

    # 5 - handle duplicate timestamps (keep first)
    if (QC$duplicated_timestamps[i]) {
      ts = ts[-which(duplicated(timestamp[ts]))]
    }

    # 6 - merge data
    SCANS = data.frame(timestamp = format(timestamp[scans], format = "%Y-%m-%dT%H:%M:%S%z"),
                       GLUC = glucose[scans])
    TS = data.frame(timestamp = format(timestamp[ts], format = "%Y-%m-%dT%H:%M:%S%z"),
                    GLUC = glucose[ts])

    # 7 - save timeseries
    scansDir = file.path(outputdir, "scans")
    tsDir = file.path(outputdir, "time series")
    if (!dir.exists(scansDir)) dir.create(scansDir)
    if (!dir.exists(tsDir)) dir.create(tsDir)
    if (!is.null(prefix)) id = paste0(prefix, id)
    if (!is.null(suffix)) id = paste0(id, suffix)
    save(SCANS, file = file.path(scansDir, paste0(id, ".RData")))
    save(TS, file = file.path(tsDir, paste0(id, ".RData")))
  }
  qcfilepath = file.path(outputdir, paste0("quality_checks_",prefix,"_",suffix,".csv"))
  write.csv(QC, file = qcfilepath, row.names = F, na = "")
  if (doublefile > 0) dev.off()
}
