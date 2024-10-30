### =========================================================================
### SparseArraySeed objects
### -------------------------------------------------------------------------

### Everything in this file is defunct in BioC >= 3.21!

setClass("SparseArraySeed",
    contains="Array",
    representation(
        dim="integer",     # This gives us dim() for free!
        nzindex="matrix",  # M-index of the nonzero data.
        nzdata="vector",   # A vector (atomic or list) of length
                           # 'nrow(nzindex)' containing the nonzero data.
        dimnames="list"    # List with one list element per dimension. Each
                           # list element must be NULL or a character vector.
    ),
    prototype(
        dim=0L,
        nzindex=matrix(integer(0), ncol=1L),
        dimnames=list(NULL)
    )
)

setGeneric("nzindex",
    function(x)
    {
        msg <- "nzindex() is defunct in BioC >= 3.21"
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("nzdata", "SparseArraySeed",
    function(x)
    {
        msg <- c("nzdata() getter for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("dim", "SparseArraySeed",
    function(x)
    {
        msg <- c("dim() getter for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("dimnames", "SparseArraySeed",
    function(x)
    {
        msg <- c("dimnames() getter for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setReplaceMethod("dimnames", "SparseArraySeed",
    function(x, value)
    {
        msg <- c("dimnames() setter for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

SparseArraySeed <- function(dim, nzindex=NULL, nzdata=NULL, dimnames=NULL,
                            check=TRUE)
{
    msg <- "SparseArraySeed objects are defunct in BioC >= 3.21"
    .Defunct(msg=wmsg(msg))
}

dense2sparse <- function(x)
{
    msg <- "dense2sparse() is defunct in BioC >= 3.21"
    .Defunct(msg=wmsg(msg))
}

### 'sas' must be a SparseArraySeed object.
### Return an ordinary array.
sparse2dense <- function(sas)
{
    if (!is(sas, "SparseArraySeed"))
        stop(wmsg("'sas' must be a SparseArraySeed object"))
    msg <- "SparseArraySeed objects are defunct in BioC >= 3.21"
    .Defunct(msg=wmsg(msg))
}

setGeneric("OLD_extract_sparse_array",
    function(x, index)
    {
        msg <- c("OLD_extract_sparse_array() is defunct in BioC >= 3.21. ",
                 "Please use SparseArray::extract_sparse_array() instead.")
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("is_sparse", "SparseArraySeed",
    function(x)
    {
        msg <- c("is_sparse() method for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("extract_array", "SparseArraySeed",
    function(x, index)
    {
        msg <- c("extract_array() method for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

### S3/S4 combo for as.array.SparseArraySeed
as.array.SparseArraySeed <- function(x, ...)
{
    msg <- c("as.array() method for SparseArraySeed objects ",
             "is defunct in BioC >= 3.21")
    .Defunct(msg=wmsg(msg))
}
setMethod("as.array", "SparseArraySeed", as.array.SparseArraySeed)

### S3/S4 combo for as.matrix.SparseArraySeed
.from_SparseArraySeed_to_matrix <- function(x)
{
    msg <- c("as.matrix() method for SparseArraySeed objects ",
             "is defunct in BioC >= 3.21")
    .Defunct(msg=wmsg(msg))
}
as.matrix.SparseArraySeed <-
    function(x, ...) .from_SparseArraySeed_to_matrix(x, ...)
setMethod("as.matrix", "SparseArraySeed", .from_SparseArraySeed_to_matrix)

setAs("ANY", "SparseArraySeed",
    function(from)
    {
        msg <- "coercion to SparseArraySeed is defunct in BioC >= 3.21"
        .Defunct(msg=wmsg(msg))
    }
)

.from_SparseArraySeed_to_CsparseMatrix <- function(from)
{
    msg <- c("coercion from SparseArraySeed to sparseMatrix or CsparseMatrix ",
             "is defunct in BioC >= 3.21")
    .Defunct(msg=wmsg(msg))
}

.from_SparseArraySeed_to_RsparseMatrix <- function(from)
{
    msg <- c("coercion from SparseArraySeed to RsparseMatrix ",
             "is defunct in BioC >= 3.21")
    .Defunct(msg=wmsg(msg))
}

setAs("SparseArraySeed", "CsparseMatrix",
    .from_SparseArraySeed_to_CsparseMatrix
)
setAs("SparseArraySeed", "RsparseMatrix",
    .from_SparseArraySeed_to_RsparseMatrix
)
setAs("SparseArraySeed", "sparseMatrix",
    .from_SparseArraySeed_to_CsparseMatrix
)
setAs("SparseArraySeed", "dgCMatrix",
    function(from) as(as(from, "CsparseMatrix"), "dgCMatrix")
)
setAs("SparseArraySeed", "dgRMatrix",
    function(from) as(as(from, "RsparseMatrix"), "dgRMatrix")
)
setAs("SparseArraySeed", "lgCMatrix",
    function(from) as(as(from, "CsparseMatrix"), "lgCMatrix")
)
setAs("SparseArraySeed", "lgRMatrix",
    function(from) as(as(from, "RsparseMatrix"), "lgRMatrix")
)

setAs("SparseArraySeed", "COO_SparseArray",
    function(from)
    {
        msg <- c("coercion from SparseArraySeed to COO_SparseArray ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setAs("sparseMatrix", "SparseArraySeed",
    function(from)
    {
        msg <- c("coercion from ", class(from)[[1L]], " to SparseArraySeed ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))

    }
)

.aperm.SparseArraySeed <- function(a, perm)
{
    msg <- c("aperm() method for SparseArraySeed objects ",
             "is defunct in BioC >= 3.21")
    .Defunct(msg=wmsg(msg))
}

### S3/S4 combo for aperm.SparseArraySeed
aperm.SparseArraySeed <-
    function(a, perm, ...) .aperm.SparseArraySeed(a, perm, ...)
setMethod("aperm", "SparseArraySeed", aperm.SparseArraySeed)

