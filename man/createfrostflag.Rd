% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_L2.R
\name{createfrostflag}
\alias{createfrostflag}
\title{Creates Flag for Potential Frost}
\usage{
createfrostflag(df, tem, lowtemp = 5, sample_temp)
}
\arguments{
\item{df}{input \code{data.frame}.}

\item{lowtemp}{numeric, specifies the temperature in °C below which frost
shrinkage or expansion is expected. Default value is set to
\code{5°C} due to hysteresis shortly before or after frost events.}

\item{sample_temp}{logical, specifying whether sample temperature dataset
is used.}
}
\description{
\code{createfrostflag} adds a flag for potential frost. Gaps of temperature
  data are filled with previous non-NA value.
}
\keyword{internal}
