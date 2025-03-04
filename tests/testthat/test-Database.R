
test_that("Database contains tables", {

  con <- GetWSFLAzureConnection()

  # Get all tables
  all_tables <- dbListTables(con)

  # Check if at least some expected tables exist
  expect_gt(length(all_tables), 45, "Database should have more than 10 tables")  # Modify as needed

  # Disconnect
  dbDisconnect(con)
})



test_that("Handles incorrect credentials correctly", {
  # Use incorrect credentials
  expect_error(GetWSFLAzureConnection(Username = "wrong_user", Password = "wrong_pass"),
               "Database connection failed")
})




