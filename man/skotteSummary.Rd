% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PhysAccelMain.R
\name{skotteSummary}
\alias{skotteSummary}
\title{Summarizing the physical activity type for one file day by day}
\usage{
skotteSummary(filename, id = "NA")
}
\arguments{
\item{filename}{Filename of the csv file}

\item{id}{The id of the subject}
}
\value{
summary dataframe with time spent Sitting,Move,Stand,Bike,Stairs,Run,Walk and Lying
}
\description{
\code{skotteSummary} Estimating the activity type summary measure day by day from physical activity csv files.
}
\seealso{
\code{\link{intensitySummary}}
}
