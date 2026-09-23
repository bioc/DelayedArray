\name{DelayedArray-utils}

\alias{DelayedArray-utils}

\alias{+,DelayedArray,missing-method}
\alias{-,DelayedArray,missing-method}

\alias{pmax2}
\alias{pmin2}
\alias{pmax2,ANY,ANY-method}
\alias{pmin2,ANY,ANY-method}
\alias{pmax2,DelayedArray,vector-method}
\alias{pmax2,DelayedArray,array-method}
\alias{pmin2,DelayedArray,vector-method}
\alias{pmin2,DelayedArray,array-method}
\alias{pmax2,vector,DelayedArray-method}
\alias{pmax2,array,DelayedArray-method}
\alias{pmin2,vector,DelayedArray-method}
\alias{pmin2,array,DelayedArray-method}
\alias{pmax2,DelayedArray,DelayedArray-method}
\alias{pmin2,DelayedArray,DelayedArray-method}

\alias{is.na,DelayedArray-method}
\alias{is.finite,DelayedArray-method}
\alias{is.infinite,DelayedArray-method}
\alias{is.nan,DelayedArray-method}
\alias{!,DelayedArray-method}

\alias{type<-,DelayedArray-method}

\alias{lengths,DelayedArray-method}

\alias{nchar,DelayedArray-method}
\alias{tolower,DelayedArray-method}
\alias{toupper,DelayedArray-method}

\alias{log,DelayedArray-method}
\alias{round,DelayedArray-method}
\alias{signif,DelayedArray-method}

\alias{grepl,ANY,DelayedArray-method}
\alias{sub,ANY,ANY,DelayedArray-method}
\alias{gsub,ANY,ANY,DelayedArray-method}

\alias{anyNA,DelayedArray-method}
\alias{which,DelayedArray-method}
\alias{nzwhich,DelayedArray-method}

\alias{unique}
\alias{unique.DelayedArray}
\alias{unique,DelayedArray-method}
\alias{table}
\alias{table,DelayedArray-method}

\alias{range}
\alias{range.DelayedArray}
\alias{range,DelayedArray-method}

\alias{mean}
\alias{mean.DelayedArray}
\alias{mean,DelayedArray-method}

\alias{apply}
\alias{apply,DelayedArray-method}

\alias{sweep}
\alias{sweep,DelayedArray-method}

\alias{scale}
\alias{scale.DelayedMatrix}
\alias{scale,DelayedMatrix-method}

\alias{paste2}
\alias{paste2,DelayedArray,vector-method}
\alias{paste2,DelayedArray,array-method}
\alias{paste2,vector,DelayedArray-method}
\alias{paste2,array,DelayedArray-method}
\alias{paste2,DelayedArray,DelayedArray-method}

\title{Common operations on DelayedArray objects}

\description{
  Common operations on \link{DelayedArray} objects.
}

\details{
  The operations currently supported on \link{DelayedArray} objects are:

  Delayed operations:
  \itemize{
    \item all the members of the \code{\link[methods]{Ops}},
          \code{\link[methods]{Math}}, and \code{\link[methods]{Math2}} groups
    \item \code{!}
    \item \code{is.na}, \code{is.finite}, \code{is.infinite}, \code{is.nan}
    \item \code{type<-}
    \item \code{lengths}
    \item \code{nchar}, \code{tolower}, \code{toupper},
          \code{grepl}, \code{sub}, \code{gsub}
    \item \code{pmax2} and \code{pmin2}
    \item \code{\link[base]{sweep}}
    \item \code{\link[base]{scale}} (when the supplied \code{center} and
          \code{scale} are not TRUE)
    \item \code{\link[BiocGenerics]{paste2}},
          \code{\link[BiocGenerics]{add_prefix}},
          \code{\link[BiocGenerics]{add_suffix}}
    \item statistical functions like \code{dnorm}, \code{dbinom}, \code{dpois},
          and \code{dlogis} (for the Normal, Binomial, Poisson, and Logistic
          distribution, respectively) and related functions (documented in
          \link{DelayedArray-stats})
  }

  Block-processed operations:
  \itemize{
    \item \code{anyNA}, \code{which}, \code{nzwhich}
    \item \code{unique}, \code{table}
    \item all the members of the \code{\link[methods]{Summary}} group
    \item \code{mean}
    \item \code{apply}
  }

  Mix delayed and block-processed operations:
  \itemize{
    \item \code{\link[base]{scale}} (when the supplied \code{center} and/or
          \code{scale} are TRUE)
  }
}

\seealso{
  \itemize{
    \item \code{\link[base]{is.na}}, \code{\link[base]{!}},
          \code{\link[base]{table}}, \code{\link[base]{mean}},
          \code{\link[base]{apply}}, and \code{\link[base]{\%*\%}} in the
          \pkg{base} package for the corresponding operations on ordinary
          arrays or matrices.

    \item \link{DelayedArray-stats} for statistical functions on
          \link{DelayedArray} objects.

    \item \link{matrixStats-methods} for \link{DelayedMatrix} row/col
          summarization.

    \item \link{DelayedArray} objects.

    \item \link[HDF5Array]{HDF5Array} objects in the \pkg{HDF5Array} package.

    \item \code{\link[methods]{S4groupGeneric}} in the \pkg{methods} package
          for the members of the \code{\link[methods]{Ops}},
          \code{\link[methods]{Math}}, and \code{\link[methods]{Math2}} groups.

    \item \code{\link[base]{sweep}} and \code{\link[base]{scale}} in the
          \pkg{base} package.

    \item \code{\link[BiocGenerics]{paste2}} in the \pkg{BiocGenerics} package.
  }
}

\examples{
library(HDF5Array)
toy_h5 <- system.file("extdata", "toy.h5", package="HDF5Array")
h5ls(toy_h5)
M1 <- HDF5Array(toy_h5, "M1")
M2 <- HDF5Array(toy_h5, "M2")

M1 >= 0.5 & M1 < 0.75              # delayed
log(M1)                            # delayed
pmax2(M2, 0)                       # delayed

type(M2) <- "integer"              # delayed
M2

## table() is block-processed:
a1 <- array(sample(50L, 2000000L, replace=TRUE), c(200, 4, 2500))
A1 <- as(a1, "HDF5Array")
table(A1)
a2 <- array(sample(20L, 2000000L, replace=TRUE), c(200, 4, 2500))
A2 <- as(a2, "HDF5Array")
table(A2)

A1 - 2 * A2                        # delayed
table(A1 - 2 * A2)                 # block-processed

## range() is block-processed:
range(A1 - 2 * A2)
range(M1)

cmeans <- colMeans(M2)             # block-processed
sweep(M2, 2, cmeans)               # delayed
scale(M2)                          # delayed & block-processed
scale(M2, center=FALSE, scale=10)  # delayed

M3 <- DelayedArray(matrix(1:12, nrow=4))
paste2(letters[1:4], M3)           # delayed
M4 <- add_prefix(M3, "ID_")        # delayed
M5 <- add_suffix(M4, "_0")         # delayed
M5

showtree(M5)
}
\keyword{methods}
