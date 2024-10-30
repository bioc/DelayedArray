\name{SparseArraySeed-class}
\docType{class}

\alias{class:SparseArraySeed}
\alias{SparseArraySeed-class}
\alias{SparseArraySeed}

\alias{nzindex}
\alias{nzdata,SparseArraySeed-method}
\alias{dim,SparseArraySeed-method}
\alias{dimnames,SparseArraySeed-method}
\alias{dimnames<-,SparseArraySeed,ANY-method}

\alias{dense2sparse}
\alias{sparse2dense}

\alias{OLD_extract_sparse_array}

\alias{is_sparse,SparseArraySeed-method}
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

\alias{coerce,sparseMatrix,SparseArraySeed-method}

\alias{aperm.SparseArraySeed}
\alias{aperm,SparseArraySeed-method}

\title{SparseArraySeed objects}

\description{
  WARNING: SparseArraySeed objects are defunct in BioC >= 3.21!

  SparseArraySeed objects were used internally to support block processing
  of sparse array-like objects in Bioconductor < 3.20.
  In Bioconductor 3.20, they were replaced with
  \link[SparseArray]{SparseArray} objects from the \pkg{SparseArray}
  package.
}

\examples{
## WARNING: SparseArraySeed objects are defunct in BioC >= 3.21!
}
\keyword{classes}
\keyword{methods}
