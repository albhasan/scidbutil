% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scidbutil.R
\name{grid2date}
\alias{grid2date}
\title{time_id to year-day-of-the-year}
\usage{
grid2date(time_id, period, startyear)
}
\arguments{
\item{time_id}{Input time index}

\item{period}{Number of days between observations (e.g 8)}

\item{startyear}{Initial year of the index (e.g 2000)}
}
\value{
A number vector  representing a date in the format year and day-of-the-year format (e.g 2001032 is Febraury the 2nd of 2001)
}
\description{
DEPRECATED. Return a year and day-of-the-year from the given time_id.
}
\author{
Alber Sanchez, \email{alber.ipia@inpe.br}
}
