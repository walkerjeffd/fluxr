% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/flux_interp.R
\name{flux_interp}
\alias{flux_interp}
\title{Estimate daily loads using linear interpolation of observed concentration}
\usage{
flux_interp(df, max_interval = 90)
}
\arguments{
\item{df}{data frame containing observed concentrations, flows, and loads}

\item{max_interval}{maximum interval for using interpolation}
}
\value{
input data frame with estimated concentration and loads
}
\description{
Estimate daily loads using linear interpolation of observed concentration
}

