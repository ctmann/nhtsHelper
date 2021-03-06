% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/time_functions.R
\name{get_mins_from_4am}
\alias{get_mins_from_4am}
\title{Turn military time into numerical units}
\usage{
get_mins_from_4am(x)
}
\arguments{
\item{x}{a character string in military time, e.g. `1523` is 3:23 pm.}
}
\value{
the number of minutes after 4 am.
}
\description{
The NHTS codes its data in character strings representing military time. For
instance, \code{723} is 7:23 AM. This is useful, except that the day in the
NHTS begins at \code{0400} hours. This function converts these times into
a number of discrete minutes after the beginning point. This allows
arithmetic operations on the time.
}

