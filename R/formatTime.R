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
#' @param tz Character. The timezone of the data (defaults to system timezone).
#' #'
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
#' @importFrom lubridate force_tz parse_date_time
#' @export
formatTime = function(data, timeCol = NULL, ts_indices = NULL, scan_indices = NULL,
                      tz = Sys.timezone()) {

  # initialize output objects
  duplicated_timestamps = no_sequential_timestamps = FALSE
  n_gaps_over_30min = 0
  ts_indices_to_remove = NULL

  # 1. --- Column Discovery ---
  if (is.null(timeCol)) {
    time_patterns = "time|tiempo|date|fecha|hora|temporal"
    timeCol = grep(time_patterns, colnames(data),
                   ignore.case = TRUE, value = TRUE)

    if (length(timeCol) > 1) {
      # Use vectorized sapply to check classes
      classes = sapply(data[, timeCol, drop = FALSE], function(x) class(x)[1])
      if (any(classes == "POSIXct")) {
        timeCol = timeCol[which(classes == "POSIXct")[1]]
      } else {
        # Fallback to the column with most unique entries
        unique_counts = sapply(data[, timeCol, drop = FALSE],
                               function(x) length(unique(x)))
        timeCol = timeCol[which.max(unique_counts)]
      }
    }
  }

  raw_time = data[[timeCol]]

  # 2. --- Parsing ---
  if (is.numeric(raw_time)) {
    # Excel origin (1899-12-30). Convert to UTC first to keep increments stable.
    ts_utc = as.POSIXct(raw_time * 86400, origin = "1899-12-30", tz = "UTC")

  } else if (is.character(raw_time)) {
    # Initialize an empty POSIXct vector
    ts_utc = as.POSIXct(rep(NA, length(raw_time)), tz = "UTC")

    # Define the potential formats found in CGM exports
    # We parse them separately to prevent 'bleeding' between formats
    possible_orders = c("ymd HM", "dmy HM", "ymd HMS", "dmy HMS")

    # Iterate through orders and fill ONLY the NAs
    # This ensures that once a row is correctly parsed, it isn't overwritten
    for (ord in possible_orders) {
      nas = is.na(ts_utc)
      if (!any(nas)) break

      # Attempt to parse only the remaining NA slots
      attempt = lubridate::parse_date_time(raw_time[nas], orders = ord, tz = "UTC", quiet = TRUE)
      ts_utc[nas] = attempt
    }

    if (all(is.na(ts_utc))) stop("Could not parse date format. Check timeCol.")

  } else if (inherits(raw_time, "POSIXct")) {
    ts_utc = lubridate::force_tz(raw_time, tzone = "UTC")
  }

  # 3. --- The DST-Proof Timeline Reconstruction ---
  # We calculate increments in UTC (no DST jumps)
  # and then apply those increments to a local-timezone start point.
  time_increment = as.numeric(diff(ts_utc, units = "secs"))
  t0 = lubridate::force_tz(ts_utc[1], tzone = tz)
  timestamp = t0 + c(0, cumsum(time_increment))

  # 4. --- Rounding & Quality Checks ---
  # Rounding to "mins" can create duplicates if records are seconds apart.
  # We round specifically for comparison, but keep high precision if needed.
  rounded_ts = round(timestamp, "mins")

  if (!is.null(ts_indices)) {
    # Calculate increments between indices specifically
    ts_only = rounded_ts[ts_indices]
    time_increments_min = as.numeric(diff(ts_only, units = "mins"))

    n_gaps_over_30min = sum(time_increments_min > 30, na.rm = TRUE)
    duplicated_timestamps = any(duplicated(ts_only))
    no_sequential_timestamps = any(time_increments_min < 0, na.rm = TRUE)
  }

  # round to minutes
  timestamp = round(as.POSIXct(timestamp), "mins")

  # quality checks (only in time series indices)
  time_increments = as.numeric(diff(timestamp[ts_indices], units = "mins"))
  if (any(time_increments > 30, na.rm = TRUE)) n_gaps_over_30min = sum(time_increments > 30, na.rm = TRUE)
  if (any(duplicated(timestamp[ts_indices]))) duplicated_timestamps = TRUE
  if (any(time_increments < 0, na.rm = TRUE)) no_sequential_timestamps = TRUE

  # return
  return(list(
    timestamp = as.POSIXct(timestamp),
    n_gaps_over_30min = n_gaps_over_30min,
    duplicated_timestamps = duplicated_timestamps,
    no_sequential_timestamps = no_sequential_timestamps,
    ts_indices = ts_indices
  ))
}
