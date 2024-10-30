\name{SparseArraySeed-utils}

\alias{SparseArraySeed-utils}

\alias{rbind,SparseArraySeed-method}
\alias{cbind,SparseArraySeed-method}

\alias{is.na,SparseArraySeed-method}
\alias{is.infinite,SparseArraySeed-method}
\alias{is.nan,SparseArraySeed-method}
\alias{tolower,SparseArraySeed-method}
\alias{toupper,SparseArraySeed-method}
\alias{nchar,SparseArraySeed-method}

\alias{anyNA,SparseArraySeed-method}
\alias{which,SparseArraySeed-method}

\alias{max,SparseArraySeed-method}
\alias{min,SparseArraySeed-method}
\alias{range.SparseArraySeed}
\alias{range,SparseArraySeed-method}
\alias{sum,SparseArraySeed-method}
\alias{prod,SparseArraySeed-method}
\alias{any,SparseArraySeed-method}
\alias{all,SparseArraySeed-method}

\alias{mean.SparseArraySeed}
\alias{mean,SparseArraySeed-method}

\title{Operate natively on SparseArraySeed objects}

\description{
   WARNING: SparseArraySeed objects are defunct in BioC >= 3.21!

   Some utilities to operate natively on \link{SparseArraySeed} objects.
   Mostly for internal use by the \pkg{DelayedArray} package e.g. they
   support block processed methods for sparse \link{DelayedArray} objects
   like \code{sum()}, \code{mean()}, \code{which()}, etc...
}

\examples{
## WARNING: SparseArraySeed objects are defunct in BioC >= 3.21!
}
\keyword{methods}
