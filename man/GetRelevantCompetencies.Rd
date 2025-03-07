% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Overview.R
\name{GetRelevantCompetencies}
\alias{GetRelevantCompetencies}
\title{Retrieve Relevant Competencies Based on a Given Date}
\usage{
GetRelevantCompetencies(CurrentDate, con)
}
\arguments{
\item{CurrentDate}{A `Date` object specifying the date for filtering competencies.}

\item{con}{A `DBI` database connection object.}
}
\value{
A data frame containing relevant competencies.
}
\description{
This function retrieves active competencies from the `Competency` table where the given date (`CurrentDate`)
falls within the start and end date range. If an end date is `NULL`, the competency is considered active
if the `StartDate` is before or on `CurrentDate`. If any competency has a `NULL` start date, an error is thrown.
}
\examples{
\dontrun{
con <- GetWSFLAzureConnection()
relevant_competencies <- GetRelevantCompetencies(Sys.Date(), con)
head(relevant_competencies)
}

}
