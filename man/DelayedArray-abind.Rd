\name{DelayedArray-abind}

\alias{DelayedArray-abind}
\alias{DelayedArray_abind}
\alias{DelayedArray-combine}
\alias{DelayedArray_combine}
\alias{abind,DelayedArray-method}

\title{Bind DelayedArray objects with abind()}

\description{
  Like ordinary matrices and arrays in base R, \link{DelayedMatrix} objects
  can be combined by rows or columns, with \code{rbind()} or \code{cbind()},
  and multidimensional \link{DelayedArray} objects can be bound along
  any dimension with \code{\link[S4Arrays]{abind}()}.

  Note that \code{\link[S4Arrays]{arbind}()} can also be used to combine
  multidimensional array-like objects along their first dimension, and
  \code{\link[S4Arrays]{acbind}()} can be used to combine them along their
  second dimension.

  All these binding operations are implemented as \emph{delayed operations}.
  More precisely, they all return a \link{DelayedMatrix} or
  \link{DelayedArray} object that carries a description of the
  binding operations to be performed at realization time.
}

\seealso{
  \itemize{
    \item \code{\link[base]{cbind}} in base R.

    \item \code{\link[S4Arrays]{abind}} in the \pkg{S4Arrays} package.

    \item \link{DelayedArray} objects.

    \item \link[HDF5Array]{HDF5Array} objects in the \pkg{HDF5Array} package.

    \item \link[SparseArray]{SparseArray} objects in the \pkg{SparseArray}
          package.

    \item Ordinary \link[base]{array} objects in base R.
  }
}

\examples{
## ---------------------------------------------------------------------
## Binding DelayedMatrix objects with rbind() or cbind()
## ---------------------------------------------------------------------

m1 <- matrix(1:8, ncol=4,
             dimnames=list(NULL, paste0("M1y", 1:4)))
m2 <- matrix(101:120, ncol=4,
             dimnames=list(paste0("M2x", 1:5), NULL))
M1 <- DelayedArray(m1)
M2 <- DelayedArray(m2)

rbind(M1, M2)  # delayed

m3 <- matrix(0.5 - 1:28, ncol=4,
             dimnames=list(paste0("M3x", 1:7), paste0("M3y", 1:4)))
M3 <- DelayedArray(m3)

rbind(M2, M3, M1)  # delayed

M4 <- cbind(M3, rbind(M2, M1))  # all the operations are delayed
M4

showtree(M4)

## --- With HDF5Array objects ---

library(HDF5Array)
toy_h5 <- system.file("extdata", "toy.h5", package="HDF5Array")
h5ls(toy_h5)
TOY1 <- HDF5Array(toy_h5, "M1")
TOY2 <- HDF5Array(toy_h5, "M2")

M <- rbind(TOY1, t(TOY2))  # all the operations are delayed
M

colMeans(M)  # block-processed

## ---------------------------------------------------------------------
## Binding 3D DelayedArray objects with abind()
## ---------------------------------------------------------------------

a1 <- array(1:105, dim=c(5, 7, 3),
            dimnames=list(NULL, paste0("A1y", 1:7), NULL))
a2 <- array(1001:1105, dim=c(5, 7, 3),
            dimnames=list(paste0("A2x", 1:5), paste0("A2y", 1:7), NULL))
A1 <- DelayedArray(a1)
A2 <- DelayedArray(a2)

abind(A1, A2)               # same as 'abind(A1, A2, along=3)'
abind(A1, A2, rev.along=0)  # same as 'abind(A1, A2, along=4)'

## Note that, like rbind() and cbind(), abind() on DelayedArray objects
## is a delayed operation.

a3 <- array(1:60, dim=c(3, 5, 4),
            dimnames=list(NULL, paste0("A3y", 1:5), NULL))
a4 <- array(101:240, dim=c(7, 5, 4),
            dimnames=list(paste0("A4x", 1:7), paste0("A4y", 1:5), NULL))
A3 <- DelayedArray(a3)
A4 <- DelayedArray(a4)

arbind(A3, A4)  # same as 'abind(A3, A4, along=1)'

## ---------------------------------------------------------------------
## Sanity checks
## ---------------------------------------------------------------------

stopifnot(
    identical(as.matrix(rbind(M1, M2)),
              rbind(m1, m2)),
    identical(as.matrix(rbind(M2, M3, M1)),
              rbind(m2, m3, m1)),
    identical(as.matrix(cbind(M3, rbind(M2, M1))),
              cbind(m3, rbind(m2, m1))),
    identical(as.array(abind(A1, A2)),
              abind(a1, a2)),
    identical(as.array(abind(A1, A2, rev.along=0)),
              abind(a1, a2, rev.along=0)),
    identical(as.array(arbind(A3, A4)),
              arbind(a3, a4))
)
}
\keyword{methods}
