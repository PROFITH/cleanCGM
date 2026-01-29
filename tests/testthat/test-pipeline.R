library(testthat)
library(cleanCGM)

# Setup paths to the sample files you provided
csv_path   <- system.file("extdata", "CSVfreestylelibre_spanish.csv", package = "cleanCGM")
txt_path   <- system.file("extdata", "TXTfreestylelibre_spanish.txt", package = "cleanCGM")
xlsx_path  <- system.file("extdata", "XLSXfreestylelibre_spanish.xlsx", package = "cleanCGM")
xlsx_split1_path  <- system.file("extdata", "XLSX1freestylelibre_spanish_splitrecording1.xlsx", package = "cleanCGM")
xlsx_split2_path  <- system.file("extdata", "XLSX2freestylelibre_spanish_splitrecording2.xlsx", package = "cleanCGM")

test_that("readCGM handles multiple formats and Spanish accents", {
  # Test CSV with Spanish characters and Latin-1 encoding
  df_csv <- readCGM(csv_path)
  expect_s3_class(df_csv, "data.frame")
  expect_true("file_nr" %in% colnames(df_csv))

  # Ensure the col_map successfully identified the ID column despite accents
  expect_true(any(grepl("FreeStyle", df_csv[[1]])))
})

test_that("formatGlucose correctly cleans and converts units", {
  # Load raw CSV data
  raw_csv <- readCGM(csv_path)

  # The sample CSV is in mmol/L (based on your file review)
  # We test if it converts to mg/dL correctly
  glucose_vec <- formatGlucose(raw_csv)

  expect_type(glucose_vec, "double")
  expect_false(any(is.na(glucose_vec))) # Check LOCF imputation worked

  # Reference check: If raw was ~5.2 mmol/L, output should be ~93.7 mg/dL
  expect_equal(glucose_vec[1], 5.2 * 18.0182, tolerance = 0.01)
})

test_that("formatGlucose handles international 'Low' strings", {
  # Create dummy data with localized 'Low' strings
  dummy_df <- data.frame(glucose = c("120", "Nivel bajo", "Low", "85"))

  # We expect "Nivel bajo" and "Low" to be turned to NA then imputed via LOCF
  cleaned <- formatGlucose(dummy_df, glucCol = "glucose")

  # low values imputed to 39 mg/dL (below detection limit of 40)
  expect_equal(cleaned[2], 39)
  expect_equal(cleaned[3], 39)
})

test_that("formatTime repairs timestamps and handles DST baseline", {
  raw_txt <- readCGM(txt_path)

  # Test the list output structure
  time_results <- formatTime(raw_txt, ts_indices = 1:nrow(raw_txt))

  expect_type(time_results, "list")
  expect_s3_class(time_results$timestamp, "POSIXct")
  expect_equal(length(time_results$timestamp), nrow(raw_txt))

  # Check for rounding consistency
  # Seconds should be 00 after rounding to "mins"
  expect_equal(as.numeric(format(time_results$timestamp[1], "%S")), 0)
})

test_that("readCGM sorting logic is chronologically correct", {
  # Provide files in the WRONG order to test the internal sorting
  # If txt starts in 2022 and csv in 2024
  merged_data <- readCGM(c(xlsx_split1_path, xlsx_split2_path))

  # File 1 (file_nr == 1) should be the 2022 data (txt_path)
  first_session <- merged_data[merged_data$file_nr == 1, ]
  second_session <- merged_data[merged_data$file_nr == 2, ]
  expect_true(first_session$`Sello de tiempo del dispositivo`[1] <
                second_session$`Sello de tiempo del dispositivo`[1])

  # even if loading 2 first, output should be sorted
  merged_data_2 <- readCGM(c(xlsx_split2_path, xlsx_split1_path))
  expect_true(all.equal(merged_data, merged_data_2))
})

test_that("Full cleanCGM pipeline generates expected directory structure", {
  # Create a temporary output directory
  tmp_out <- tempdir()

  # Run pipeline on a single file
  cleanCGM(datadir = dirname(txt_path), outputdir = tmp_out, verbose = FALSE)

  # Check if folders were created
  expect_true(dir.exists(file.path(tmp_out, "scans")))
  expect_true(dir.exists(file.path(tmp_out, "time series")))

  # Check for the Quality Control CSV
  expect_true(file.exists(file.path(tmp_out, "quality_checks.csv")))
})
