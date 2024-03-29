\name{chunkGrid}

\alias{class:integer_OR_NULL}
\alias{integer_OR_NULL-class}
\alias{integer_OR_NULL}

\alias{chunkdim}
\alias{chunkdim,ANY-method}
\alias{chunkdim,DelayedUnaryOp-method}
\alias{chunkdim,DelayedSubset-method}
\alias{chunkdim,DelayedAperm-method}

\alias{chunkGrid}
\alias{chunkGrid,ANY-method}

\title{chunkGrid}

\description{
  \code{chunkGrid} and \code{chunkdim} are internal generic functions not
  aimed to be used directly by the user.
}

\usage{
chunkGrid(x)
chunkdim(x)
}

\arguments{
  \item{x}{
    An array-like object.
  }
}

\details{
  Coming soon...
}

\value{
  \code{chunkGrid} returns NULL or an \link[S4Arrays]{ArrayGrid} object
  defining a grid on reference array \code{x}.

  \code{chunkdim} returns NULL or the chunk dimensions in an integer vector
  parallel to \code{dim(x)}.
}

\seealso{
  \itemize{
    \item \code{\link{defaultAutoGrid}} and family to create automatic
          grids to use for block processing of array-like objects.

    \item \link{DelayedArray} objects.

    \item \link[S4Arrays]{ArrayGrid} in the \pkg{S4Arrays} package for
          the formal representation of grids and viewports.
  }
}

\examples{
## Coming soon...
}
\keyword{internal}
