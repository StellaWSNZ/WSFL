
test_that("Database contains tables", {

  con <- GetWSFLAzureConnection()

  # Get all tables
  all_tables <- dbListTables(con)

  # Check if at least some expected tables exist
  expect_gt(length(all_tables), 45, "Database should have more than 10 tables")  # Modify as needed

  # Disconnect
  dbDisconnect(con)
})

#
# test_that("Handles missing credentials correctly", {
#   # Temporarily remove environment variables
#
#   # Expect an error due to missing credentials
#   expect_error(GetWSFLAzureConnection(), "Database username or password is missing")
#
#   # Restore environment variables
# })

test_that("Handles incorrect credentials correctly", {
  # Use incorrect credentials
  expect_error(GetWSFLAzureConnection(Username = "wrong_user", Password = "wrong_pass"),
               "Database connection failed")
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
  dbDisconnect(con)  # Close the connection to make it invalid

  expect_error(GetRelevantCompetencies(as.Date("2024-03-01"), con), "Invalid database connection")
})

