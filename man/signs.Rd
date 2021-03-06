% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signs.r
\name{signs}
\alias{signs}
\title{Signs and summary of input matrix}
\usage{
signs(x = "matrix")
}
\arguments{
\item{x}{a square matrix of real numbers.  While this function is more
general, the initail intention was for this to be a utility matrix or the
mixed trophic impacts matrix.}
}
\value{
\item{sign}{A sign matrix for matrix x.} \item{relations}{A matrix
of the pairwise signed relationships in x.} \item{rs.tab}{Table summarizing
the pairwise relationships and identifying their ecological label.}
\item{relationship.counts}{A count of the different kinds of pairwise
relationships found in matrix x.}
}
\description{
Identifies the signs and pairwise relationsips of a given matrix.  This
includes also returns a summary table that provides the ecological name of
each pairwise realtionship, and a summary of the counts.
}
\note{
This function is called by relationalChange, and was created to
generate more informative output from enaUtility and enaMTI.
}
\seealso{
\code{\link{relationalChange}}
}
\author{
Stuart R. Borrett
}
