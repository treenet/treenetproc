% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_input_data.R
\name{reso_check_L0}
\alias{reso_check_L0}
\title{Check Time Resolution of L0 Input Data}
\usage{
reso_check_L0(df, reso, tz)
}
\arguments{
\item{df}{input \code{data.frame}.}

\item{reso}{numeric, desired output time resolution (in minutes). See
\code{Details} for more information on data aggregation.}

\item{tz}{specify the desired time zone. Default is \code{"UTC"}.}
}
\description{
\code{reso_check} extracts the median time resolution of \code{L0} data
  and compares it to the user-specified \code{reso}. If \code{2.1 * reso}
  is smaller than the median time resolution, a warning message is printed.
}
\keyword{internal}
