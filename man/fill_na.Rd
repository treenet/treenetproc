% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_L2.R
\name{fill_na}
\alias{fill_na}
\title{Fills NA's With Last Non-NA Value}
\usage{
fill_na(x, from_last = FALSE)
}
\arguments{
\item{x}{input \code{vector}.}
}
\description{
\code{fillna} fills NA's with previous non-NA value (function
  adapted from \code{\link[zoo]{na.locf}} of the \code{zoo} package).
}
\keyword{internal}
