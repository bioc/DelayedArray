\name{SparseArraySeed-class}
\docType{class}

\alias{class:SparseArraySeed}
\alias{SparseArraySeed-class}
\alias{SparseArraySeed}

\alias{nzindex}
\alias{nzindex,SparseArraySeed-method}
\alias{nzdata}
\alias{nzdata,SparseArraySeed-method}
\alias{dimnames,SparseArraySeed-method}
\alias{dimnames<-,SparseArraySeed,ANY-method}

\alias{dense2sparse}
\alias{sparse2dense}

\alias{OLD_extract_sparse_array}

\alias{is_sparse,SparseArraySeed-method}
\alias{OLD_extract_sparse_array,SparseArraySeed-method}
\alias{extract_array,SparseArraySeed-method}

\alias{as.array.SparseArraySeed}
\alias{as.array,SparseArraySeed-method}
\alias{as.matrix.SparseArraySeed}
\alias{as.matrix,SparseArraySeed-method}

\alias{coerce,ANY,SparseArraySeed-method}

\alias{coerce,SparseArraySeed,CsparseMatrix-method}
\alias{coerce,SparseArraySeed,RsparseMatrix-method}
\alias{coerce,SparseArraySeed,sparseMatrix-method}
\alias{coerce,SparseArraySeed,dgCMatrix-method}
\alias{coerce,SparseArraySeed,dgRMatrix-method}
\alias{coerce,SparseArraySeed,lgCMatrix-method}
\alias{coerce,SparseArraySeed,lgRMatrix-method}
\alias{coerce,SparseArraySeed,COO_SparseArray-method}

\alias{coerce,dgCMatrix,SparseArraySeed-method}
\alias{coerce,dgRMatrix,SparseArraySeed-method}
\alias{coerce,lgCMatrix,SparseArraySeed-method}
\alias{coerce,lgRMatrix,SparseArraySeed-method}

\alias{OLD_extract_sparse_array,dgCMatrix-method}
\alias{OLD_extract_sparse_array,dgRMatrix-method}
\alias{OLD_extract_sparse_array,lgCMatrix-method}
\alias{OLD_extract_sparse_array,lgRMatrix-method}

\alias{aperm.SparseArraySeed}
\alias{aperm,SparseArraySeed-method}

\title{SparseArraySeed objects}

\description{
  WARNING: SparseArraySeed objects are deprecated in BioC >= 3.20!

  SparseArraySeed objects were used internally to support block processing
  of sparse array-like objects in Bioconductor < 3.20.
  In Bioconductor 3.20, they were replaced with
  \link[SparseArray]{SparseArray} objects from the \pkg{SparseArray}
  package.
}

\usage{
## Constructor function:
SparseArraySeed(dim, nzindex=NULL, nzdata=NULL, dimnames=NULL, check=TRUE)

## Getters (in addition to dim(), length(), and dimnames()):
nzindex(x)
nzdata(x)

## Two low-level utilities:
dense2sparse(x)
sparse2dense(sas)
}

\arguments{
  \item{dim}{
    The dimensions (specified as an integer vector) of the
    SparseArraySeed object to create.
  }
  \item{nzindex}{
    A matrix containing the array indices of the nonzero data.

    This must be an integer matrix like one returned by
    \code{base::\link[base]{arrayInd}}, that is, with \code{length(dim)}
    columns and where each row is an n-uplet representing an \emph{array index}.
  }
  \item{nzdata}{
    A vector (atomic or list) of length \code{nrow(nzindex)} containing
    the nonzero data.
  }
  \item{dimnames}{
    The \emph{dimnames} of the object to be created. Must be \code{NULL} or
    a list of length the number of dimensions. Each list element must be
    either \code{NULL} or a character vector along the corresponding dimension.
  }
  \item{check}{
    Should the object be validated upon construction?
  }
  \item{x}{
    A SparseArraySeed object for the \code{nzindex} and \code{nzdata}
    getters.

    An array-like object for \code{dense2sparse}.
  }
  \item{sas}{
    A SparseArraySeed object.
  }
}

\value{
  \itemize{
    \item For \code{SparseArraySeed()}: A SparseArraySeed instance.

    \item For \code{nzindex()}: The matrix containing the array indices of the
          nonzero data.

    \item For \code{nzdata()}: The vector of nonzero data.

    \item For \code{dense2sparse()}: A SparseArraySeed instance.

    \item For \code{sparse2dense()}: An ordinary array.
  }
}

\seealso{
  \itemize{
    \item \link{SparseArraySeed-utils} for native operations on
          SparseArraySeed objects.

    \item S4 classes \linkS4class{dgCMatrix}, \linkS4class{dgRMatrix}, and
          \linkS4class{lsparseMatrix}, defined in the \pkg{Matrix} package,
          for the de facto standard of sparse matrix representations in R.

    \item The \code{\link[S4Arrays]{read_block}} function in the
          \pkg{S4Arrays} package.

    \item \code{\link{blockApply}} and family for convenient block
          processing of an array-like object.

    \item \code{\link[S4Arrays]{extract_array}} in the \pkg{S4Arrays} package.

    \item \link{DelayedArray} objects.

    \item \code{\link[base]{arrayInd}} in the \pkg{base} package.

    \item \link[base]{array} objects in base R.
  }
}

\examples{
## WARNING: SparseArraySeed objects are deprecated in BioC >= 3.20!
}
\keyword{classes}
\keyword{methods}
