#' Get the OneDrive Path for WSFL Data
#'
#' Constructs the file path for WSFL school class lists based on the selected calendar year and term.
#' The function dynamically detects the OneDrive path and builds the directory structure accordingly.
#'
#' @param CALENDARYEAR An integer specifying the calendar year (e.g., 2023, 2024).
#' @param TERM An integer specifying the school term (e.g., 1, 2, 3, or 4).
#'
#' @return A string containing the full file path to the relevant WSFL data directory.
#' @export
#'
#' @examples
#' \dontrun{
#' folder_path <- GetFolderPath(2024, 3)
#' print(folder_path)
#' }
#'
GetFolderPath <- function(CALENDARYEAR, TERM) {
  # Detect OneDrive path dynamically
  onedrive_path <- Sys.getenv("OneDriveCommercial")

  # Ensure OneDrive path is found
  if (onedrive_path == "") {
    stop("OneDrive path not found. Check if OneDrive is installed and running.")
  }

  # Define base directory structure
  base_dir <- file.path(
    onedrive_path,
    "WSNZ",
    "Interventions",
    "Funding",
    "WSFL Databases June 23"
  )

  # Determine the subfolder based on year and term
  if (CALENDARYEAR == 2023 & TERM %in% c(3, 4)) {
    year_folder <- "WSFL 2023 Providers School Class Lists etc"
  } else if (CALENDARYEAR == 2024 & TERM %in% c(1, 2)) {
    year_folder <- "WSFL 2023 Providers School Class Lists etc"
  } else if (CALENDARYEAR == 2024 & TERM %in% c(3, 4)) {
    year_folder <- "WSFL 2024_25 Providers School Class Lists etc"
  } else {
    stop(paste0("No path specified for selected Term (", TERM, ") and Year (", CALENDARYEAR, ")"))
  }

  # Construct the final path
  term_folder <- paste0("Term ", TERM, " Schools ", CALENDARYEAR)
  final_path <- file.path(base_dir, year_folder, term_folder)

  return(final_path)
}

#' Retrieve Relevant Competencies Based on a Given Date
#'
#' This function retrieves active competencies from the `Competency` table where the given date (`CurrentDate`)
#' falls within the start and end date range. If an end date is `NULL`, the competency is considered active
#' if the `StartDate` is before or on `CurrentDate`. If any competency has a `NULL` start date, an error is thrown.
#'
#' @param CurrentDate A `Date` object specifying the date for filtering competencies.
#' @param con A `DBI` database connection object.
#'
#' @return A data frame containing relevant competencies.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- GetWSFLAzureConnection()
#' relevant_competencies <- GetRelevantCompetencies(Sys.Date(), con)
#' head(relevant_competencies)
#' }
#'
#' @import DBI
GetRelevantCompetencies <- function(CurrentDate, con) {
  # Validate input
  if (!inherits(CurrentDate, "Date")) {
    stop("Error: 'CurrentDate' must be a Date object.")
  }
  if (!DBI::dbIsValid(con)) {
    stop("Error: Invalid database connection.")
  }

  # SQL query to fetch relevant competencies
  query <- "SELECT *
            FROM Competency
            WHERE StartDate IS NOT NULL
              AND (
                (EndDate IS NOT NULL AND ? BETWEEN StartDate AND EndDate)
                OR (EndDate IS NULL AND StartDate <= ?)
              )"

  # Execute the query and fetch the data
  df <- DBI::dbGetQuery(con, query, params = list(CurrentDate, CurrentDate))

  # Check for missing start dates (unexpected case)
  if (any(is.na(df$StartDate))) {
    stop("Error: Some rows have NULL StartDate. Please verify the data integrity.")
  }

  return(df)
}


#' Check and Summarize File Presence in Directories
#'
#' This function scans directories for files based on a given `CALENDARYEAR` and `TERM`,
#' compares them against database records (`ClassFilePath` and `Error` tables),
#' and returns summaries of present and missing files.
#'
#' @param con A database connection object (`DBI::dbConnect`).
#' @param df A data frame containing `CALENDARYEAR` and `TERM` columns, specifying the years
#' and terms for which to check file presence.
#'
#' @return A list with two elements:
#'   - `missing_files`: A data frame with columns `FullPath`, `CALENDARYEAR`, and `TERM` for files
#'      that are missing from both `ClassFilePath` and `Error` tables.
#'   - `file_summary`: A data frame summarizing the number of total files, files present,
#'      and files not present for each `CALENDARYEAR` and `TERM`.
#'
#' @details
#' The function does not change the working directory and instead works with absolute file paths.
#' It filters out files with names containing `"Provider Template"`, `"Missing"`, `"Incomplete"`,
#' `"Waiting on"`, and `"desktop.ini"`.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(odbc::odbc(), "YourDatabase")
#' df <- data.frame(CALENDARYEAR = c(2023, 2024), TERM = c(1, 2))
#' result <- CheckFiles(con, df)
#' missing_files <- result$missing_files
#' file_summary <- result$file_summary
#' }
#'
#' @seealso [GetFolderPath] for retrieving the folder path based on `CALENDARYEAR` and `TERM`.
#'
#' @export
CheckFiles <- function(con, df) {
  # Fetch data from the database
  Class <- dbGetQuery(con, "SELECT FilePath FROM ClassFilePath")
  Error <- dbGetQuery(con, "SELECT FilePath FROM Error")

  # Initialize result data frames
  missing_files <- data.frame(FulllFilePath = character(),
                              CALENDARYEAR = integer(),
                              TERM = integer(),
                              stringsAsFactors = FALSE)

  file_summary <- data.frame(CALENDARYEAR = integer(),
                             TERM = integer(),
                             TotalFiles = integer(),
                             FilesNotPresent = integer(),
                             FilesPresent = integer(),
                             stringsAsFactors = FALSE)

  # Loop through each row in df
  for (i in seq_len(nrow(df))) {
    # Extract CALENDARYEAR and TERM
    CALENDARYEAR <- df$CALENDARYEAR[i]
    TERM <- df$TERM[i]

    # Get the folder path for the given CALENDARYEAR and TERM
    PATH <- GetFolderPath(CALENDARYEAR, TERM)

    # Get file paths from the directory
    file_list <- list.files(PATH, recursive = TRUE, full.names = TRUE)

    # Filter out unwanted files
    valid_files <- file_list[!grepl("Provider Template|Missing|Incomplete|Waiting on|desktop.ini", file_list, ignore.case = TRUE)]

    # Create a data frame of valid files
    Files <- data.frame(FullPath = valid_files, stringsAsFactors = FALSE)

    # Initialize columns for Class and Error presence
    Files$ClassFile <- ifelse(Files$FullPath %in% Class$FilePath, "Present", "Not Present")
    Files$ErrorFile <- ifelse(Files$FullPath %in% Error$FilePath, "Present", "Not Present")

    # Identify missing files (not present in both Class and Error tables)
    missing <- Files[Files$ClassFile == "Not Present" & Files$ErrorFile == "Not Present", ]

    if (nrow(missing) > 0) {
      missing$CALENDARYEAR <- CALENDARYEAR
      missing$TERM <- TERM
      missing_files <- rbind(missing_files, missing[, c("FullPath", "CALENDARYEAR", "TERM")])
    }

    # Summarize file counts
    total_files <- nrow(Files)
    files_not_present <- nrow(missing)
    files_present <- total_files - files_not_present

    file_summary <- rbind(file_summary, data.frame(
      CALENDARYEAR = CALENDARYEAR,
      TERM = TERM,
      TotalFiles = total_files,
      FilesNotPresent = files_not_present,
      FilesPresent = files_present
    ))
  }

  return(list(missing_files = missing_files, file_summary = file_summary))
}
