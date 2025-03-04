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


