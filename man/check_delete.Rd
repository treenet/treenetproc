% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_input_variables.R
\name{check_delete}
\alias{check_delete}
\title{Check the Date Sequence in Delete}
\usage{
check_delete(delete)
}
\arguments{
\item{delete}{character vector, specify pairs of dates between which
all dendrometer data will be deleted (i.e. 4 dates will result in two
periods: 1-2 and 3-4 in which data is deleted). Dates need to be in the
same standard date or datetime format
(e.g. \code{\%Y-\%m-\%d \%H:\%M:\%S}).}
}
\description{
\code{check_delete} checks whether the first value in a pair of dates
  occurs before the second value.
}
\keyword{internal}
