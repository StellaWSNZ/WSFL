#' Connect to the WSFL Azure SQL Database
#'
#' Establishes an ODBC connection to the WSFL Azure SQL Server database.
#'
#' @param Username A string specifying the database username. Defaults to `Sys.getenv("WSNZDBUSER")`.
#' @param Password A string specifying the database password. Defaults to `Sys.getenv("WSNZDBPASS")`.
#'
#' @return A DBI connection object (`DBI::dbConnect`) if successful.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- GetWSFLAzureConnection()
#' DBI::dbListTables(con)
#' DBI::dbDisconnect(con)
#' }
#'
#' @import DBI
#' @import odbc
#'
GetWSFLAzureConnection <- function(
    Username = Sys.getenv("WSNZDBUSER"),
    Password = Sys.getenv("WSNZDBPASS")
) {
  # Ensure required packages are available
  if (!requireNamespace("odbc", quietly = TRUE)) {
    stop("The 'odbc' package is not installed. Install it with install.packages('odbc').")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("The 'DBI' package is not installed. Install it with install.packages('DBI').")
  }

  # Check if credentials are available
  if (is.null(Username) || Username == "" || is.null(Password) || Password == "") {
    stop("Error: Database username or password is missing. Ensure environment variables WSNZDBUSER and WSNZDBPASS are set.")
  }

  # Attempt connection
  con <- tryCatch(
    {
      DBI::dbConnect(
        odbc::odbc(),
        Driver = "SQL Server",
        Server = "heimatau.database.windows.net",
        Database = "WSFL",
        Port = 1433,
        Uid = Username,
        Pwd = Password
      )
    },
    error = function(e) {
      stop("Database connection failed: ", e$message)
    }
  )

  return(con)
}

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
