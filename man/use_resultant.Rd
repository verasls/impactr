% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{use_resultant}
\alias{use_resultant}
\title{Use resultant vector}
\usage{
use_resultant(data)
}
\arguments{
\item{data}{An \code{impactr_data} object, as obtained with}
}
\value{
An object of class \code{impactr_data} with the \code{acc_R} column
containing the acceleration resultant vector.
}
\description{
Computes the acceleration resultant vector.
}
\examples{
data <- read_acc(impactr_example("hip-raw.csv"))
use_resultant(data)
}
