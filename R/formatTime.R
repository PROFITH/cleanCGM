#' Normalize and Repair CGM Timestamps
#'
#' @description
#' `formatTime` converts raw date/time columns into standardized `POSIXct`
#' objects. It is specifically designed to handle common CGM sensor issues such
#' as inconsistent date formats, numeric Excel-style timestamps, and
#' Daylight Savings Time (DST) distortions.
#'
#' @details
#' The function follows a hierarchical logic to identify and clean time data:
#' \itemize{
#'   \item **Column Discovery**: Searches for columns matching common time keywords
#'   (e.g., "fecha", "hora", "timestamp"). If multiple exist, it prioritizes
#'   `POSIXct` classes or columns with the most unique values.
#'   \item **Numeric Parsing**: Converts Excel-origin numeric dates (origin "1899-12-30")
#'   by converting them to UTC and then reapplying the local timezone to avoid
#'   DST "spring forward" or "fall back" gaps.
#'   \item **String Parsing**: Attempts multiple ISO and European date-time
#'   formats (`DD/MM/YYYY`, `YYYY/MM/DD`, `DD-MM-YYYY`).
#'   \item **Rounding**: All timestamps are rounded to the nearest minute to
#'   align sensor readings that may be seconds apart.
#' }
#'
#'
#'
#' @param data A data frame containing raw CGM data.
#' @param timeCol Character. Optional. The name of the timestamp column.
#' If NULL, the function performs automatic detection.
#' @param ts_indices Integer vector. Row indices corresponding to continuous
#' time series (historical) data.
#' @param scan_indices Integer vector. Row indices corresponding to manual
#' scan events.
#'
#' @section Quality Control Metrics:
#' The function doesn't just return time; it evaluates data integrity:
#' \itemize{
#'   \item **Gaps**: Identifies periods where the recording interval exceeds 30 minutes.
#'   \item **Duplicates**: Checks for multiple readings at the exact same minute.
#'   \item **Sequentiality**: Detects "time travel" errors where a timestamp is
#'   earlier than the preceding one (often caused by manual clock resets).
#' }
#'
#' @return A named list containing:
#' \item{timestamp}{A `POSIXct` vector of normalized date-times.}
#' \item{n_gaps_over_30min}{Integer count of data gaps > 30 mins.}
#' \item{duplicated_timestamps}{Logical indicating if duplicates were found.}
#' \item{no_sequential_timestamps}{Logical indicating if time-order violations occurred.}
#' \item{ts_indices}{The original or updated time series indices.}
#'
#' @importFrom lubridate force_tz
#' @export
formatTime = function(data, timeCol = NULL, ts_indices = NULL, scan_indices = NULL) {
  # initialize out objects
  duplicated_timestamps = no_sequential_timestamps = FALSE
  n_gaps_over_30min = 0
  ts_indices_to_remove = NULL
  if (is.null(timeCol)) {
    # identify time column
    timeCol = grep("time|tiempo|date|fecha|hora|temporal", colnames(data),
                   ignore.case = TRUE, value = TRUE)
    if (length(timeCol) > 1) {
      # keep the one formatted as POSIX
      timeCol_class = rep(NA, length(timeCol))
      for (timeCol_i in 1:length(timeCol)) {
        timeCol_class[timeCol_i] = class(data[, timeCol[timeCol_i]])[1]
      }
      if (any(timeCol_class == "POSIXct")) {
        timeCol = timeCol[which(timeCol_class == "POSIXct")]
      }
    }
    if (length(timeCol) > 1) {
      # keep the one with more distinct values
      timeCol_lengths = rep(NA, length(timeCol))
      for (timeCol_i in 1:length(timeCol)) {
        timeCol_lengths[timeCol_i] = length(table(data[, timeCol[timeCol_i]]))
      }
      timeCol = timeCol[which.max(timeCol_lengths)]
    }
  }
  if (is.numeric(data[, timeCol])) {
    # read as UTC as device does not detect DSTs (UTC does not include DSTs)
    ts_utc = as.POSIXct(data[, timeCol]*86400, origin = "1899-12-30", tz = "UTC")
    # calculate time increment along recordings to derive new timestamp
    time_increment = as.numeric(diff(ts_utc, units = "secs"))
    # new timestamp = t0 (in local timezone) + time increments
    t0 = lubridate::force_tz(ts_utc[1], tzone = Sys.timezone())
    timestamp = c(t0, t0 + cumsum(time_increment))
  } else if (is.character(data[, timeCol])) {
    ts_utc = strptime(data[, timeCol], format = "%d/%m/%Y %H:%M", tz = "UTC")
    if (any(is.na(ts_utc))) {
      ts_utc = strptime(data[, timeCol], format = "%Y/%m/%d %H:%M", tz = "UTC")
    }
    if (any(is.na(ts_utc))) {
      ts_utc = strptime(data[, timeCol], format = "%d-%m-%Y %H:%M", tz = "UTC")
    }
    # calculate time increment along recordings to derive new timestamp
    time_increment = as.numeric(diff(ts_utc, units = "secs"))
    # new timestamp = t0 (in local timezone) + time increments
    t0 = lubridate::force_tz(ts_utc[1], tzone = Sys.timezone())
    timestamp = c(t0, t0 + cumsum(time_increment))
  } else if ("POSIXct" %in% class(data[, timeCol])) {
    timestamp = data[, timeCol]
  }

  # round to minutes
  timestamp = round(as.POSIXct(timestamp), "mins")

  # quality checks (only in time series indices)
  time_increments = as.numeric(diff(timestamp[ts_indices], units = "mins"))
  if (any(time_increments > 30)) n_gaps_over_30min = sum(time_increments > 30)
  if (any(duplicated(timestamp[ts_indices]))) duplicated_timestamps = TRUE
  if (any(time_increments < 0)) no_sequential_timestamps = TRUE

  # return
  return(list(timestamp = timestamp,
              n_gaps_over_30min = n_gaps_over_30min,
              duplicated_timestamps = duplicated_timestamps,
              no_sequential_timestamps = no_sequential_timestamps,
              ts_indices = ts_indices))
}
