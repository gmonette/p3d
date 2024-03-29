% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misscode.R
\name{misscode}
\alias{misscode}
\alias{misscode.default}
\alias{misscode.factor}
\alias{misscode.data.frame}
\title{Recode missing data for p3d}
\usage{
misscode(x, ...)

\method{misscode}{default}(x, ..., offset = 0.1)

\method{misscode}{factor}(x, ...)

\method{misscode}{data.frame}(x, ...)
}
\arguments{
\item{x}{vector with possible missing values}

\item{offset}{(default .1) how far below the minimum value as a proportion of the range}
}
\description{
Recodes missing data to a value less than the minimum value -- or to a non-missing
code to indicate missingness for a factor -- to facilitate plotting in 3d
}
