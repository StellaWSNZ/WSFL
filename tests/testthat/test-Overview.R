
test_that("GetFolderPath returns correct paths for different terms and years", {
  skip_on_cran()  # Skip on CRAN because it depends on OneDrive being installed

  # Detect OneDrive path dynamically
  onedrive_path <- Sys.getenv("OneDriveCommercial")

  # Skip test if OneDrive path is missing
  if (onedrive_path == "") {
    skip("OneDrive is not configured on this system.")
  }

  # Expected paths (adjust if necessary)
  expected_path_2023_3 <- file.path(
    onedrive_path,
    "WSNZ",
    "Interventions",
    "Funding",
    "WSFL Databases June 23",
    "WSFL 2023 Providers School Class Lists etc",
    "Term 3 Schools 2023"
  )

  expected_path_2024_1 <- file.path(
    onedrive_path,
    "WSNZ",
    "Interventions",
    "Funding",
    "WSFL Databases June 23",
    "WSFL 2023 Providers School Class Lists etc",
    "Term 1 Schools 2024"
  )

  expected_path_2024_3 <- file.path(
    onedrive_path,
    "WSNZ",
    "Interventions",
    "Funding",
    "WSFL Databases June 23",
    "WSFL 2024_25 Providers School Class Lists etc",
    "Term 3 Schools 2024"
  )

  # Run tests
  expect_equal(GetFolderPath(2023, 3), expected_path_2023_3)
  expect_equal(GetFolderPath(2024, 1), expected_path_2024_1)
  expect_equal(GetFolderPath(2024, 3), expected_path_2024_3)
})

test_that("GetFolderPath throws an error for invalid inputs", {
  expect_error(GetFolderPath(2025, 1), "No path specified for selected Term")
  expect_error(GetFolderPath(2024, 5), "No path specified for selected Term")
})







test_that("Valid CurrentDate returns expected competencies", {
  # Connect to database (using a mock or actual connection)
  con <- GetWSFLAzureConnection()

  # Use a test date that is expected to return some competencies
  test_date <- as.Date("2024-03-01")

  # Fetch competencies
  results <- GetRelevantCompetencies(test_date, con)

  # Check that the result is a dataframe
  expect_s3_class(results, "data.frame")

  # Ensure expected columns exist
  expect_true(all(c("CompetencyID", "StartDate", "EndDate") %in% colnames(results)))

  # Ensure StartDate is not NULL
  expect_false(any(is.na(results$StartDate)))

  # Disconnect from database
  dbDisconnect(con)
})

test_that("Handles non-Date input correctly", {
  con <- GetWSFLAzureConnection()

  # Expect error when passing incorrect types
  expect_error(GetRelevantCompetencies("2024-03-01", con), "must be a Date object")
  expect_error(GetRelevantCompetencies(20240301, con), "must be a Date object")
  expect_error(GetRelevantCompetencies(Sys.time(), con), "must be a Date object")

  dbDisconnect(con)
})



test_that("Handles invalid database connection correctly", {
  con <- GetWSFLAzureConnection()
  dbDisconnect(con)

  expect_error(GetRelevantCompetencies(as.Date("2024-03-01"), con), "Invalid database connection")
})



test_that("Handles invalid years correctly", {
  con <- GetWSFLAzureConnection()

  expect_error(
    CheckFiles(
      GetWSFLAzureConnection(),
      2024,
      c(1, 5),
      "One or more of your values for Term are invalid."
    )
  )
  expect_error(
    CheckFiles(GetWSFLAzureConnection(), 2026, c(1), regexp = "Error: CalendarYear is out of range. Valid range is")
  )


  expect_error(
    CheckFiles(GetWSFLAzureConnection(), 2022, c(1), regexp = "Error: CalendarYear is out of range. Valid range is")
  )

  expect_error(
    CheckFiles(GetWSFLAzureConnection(), 2023, c(1), regexp = "is not valid for CalendarYear")
  )
})
