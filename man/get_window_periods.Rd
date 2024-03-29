% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{get_window_periods}
\alias{get_window_periods}
\title{Get whole windows period index}
\usage{
get_window_periods(st_period, window_length)
}
\arguments{
\item{st_period}{Start date}

\item{window_length}{window length}
}
\value{
A period index sequence corresponding to each date in the window
}
\description{
Calculate a sequence of period indexes for given window
}
\keyword{internal}
