% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi_interpolate.R
\name{epi_interpolate}
\alias{epi_interpolate}
\title{A wrapper for interpolation methods.}
\usage{
epi_interpolate(epi, pts, method = "krig")
}
\arguments{
\item{epi}{a proper EpiVis data frame.}

\item{pts}{an integer denoting the number of points to on the heatmap. Note that points = -1 if and only if kernal density is used.}

\item{method}{a string denoting the function of interpolation.}
}
\value{
a function call to the method of choice.
}
\description{
This wrappter is used so that further methods are more easily added.
}
\details{
It should be noted that this function is a relatively high-level function;
users would not be able to make changes to the function's default parameters.
To allow such change, users should call those functions directly. Also, this method could be significantly slow if too many points are to be calculated,
as calculating Covariance matrix is O(n^2)
}
