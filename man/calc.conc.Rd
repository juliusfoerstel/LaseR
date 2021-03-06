% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc.conc.R
\name{calc.conc}
\alias{calc.conc}
\title{Calculate the "true" concentration}
\usage{
calc.conc(d, info)
}
\arguments{
\item{d}{Vector of data.}

\item{info}{data.frame with the columns ID with autamic column names and 
Rel.abund with the relative abundance of the isotope}
}
\description{
This function calculates the "true" concentration of the element, based on the natural relative abundance.
}
\examples{
calc.conc(d = data, info = isotopeInfo)
}
\keyword{abundance}
\keyword{concentration,}
\keyword{relative}
