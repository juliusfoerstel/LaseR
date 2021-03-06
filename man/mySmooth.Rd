% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mysmooth.R
\name{mySmooth}
\alias{mySmooth}
\title{my own smooth function}
\usage{
mySmooth(data, l = 6, clear.outliers = FALSE, gaussian = TRUE, pad = FALSE)
}
\arguments{
\item{data}{vector of data}

\item{l}{number of points to be averaged}

\item{clear.outliers}{Boolean}

\item{gaussian}{Boolean}

\item{pad}{boolean whether NA should be added in front and in the back to maintain the same length}
}
\description{
This function smooths a signal with a floating mean or a gaussian convolution.
}
\keyword{smooth}
