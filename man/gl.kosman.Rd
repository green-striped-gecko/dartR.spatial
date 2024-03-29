% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gl.kosman.r
\name{gl.kosman}
\alias{gl.kosman}
\title{Calculates a Kosman distance matrix for each pair of individuals}
\usage{
gl.kosman(x, verbose = NULL)
}
\arguments{
\item{x}{genlight/dartR object with a unique ploidy}

\item{verbose}{verbosity of the function.}
}
\value{
returns a matrix of [dimensions nInd(x) x nInd(x)] of kosman distances between individuals,
}
\description{
This script calculates an individual based distance matrix.
}
\examples{
#use only five individuals and seven loci
gg <- gl.kosman(possums.gl[1:5,14:21])
gg$kosman
gg$nloci
}
