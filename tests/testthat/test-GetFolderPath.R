
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
