### =========================================================================
### Operate natively on SparseArraySeed objects
### -------------------------------------------------------------------------

### Everything in this file is defunct in BioC >= 3.21!

setMethod("rbind", "SparseArraySeed",
    function(...)
    {
        msg <- c("rbind() method for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("cbind", "SparseArraySeed",
    function(...)
    {
        msg <- c("cbind() method for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

.UNARY_ISO_OPS <- c("is.na", "is.infinite", "is.nan", "tolower", "toupper")

for (.Generic in .UNARY_ISO_OPS) {
    setMethod(.Generic, "SparseArraySeed",
        function(x)
        {
            msg <- c(.Generic, "() method for SparseArraySeed objects ",
                     "is defunct in BioC >= 3.21")
            .Defunct(msg=wmsg(msg))
        }
    )
}

setMethod("nchar", "SparseArraySeed",
    function(x, type="chars", allowNA=FALSE, keepNA=NA)
    {
        msg <- c("nchar() method for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("anyNA", "SparseArraySeed",
    function(x, recursive=FALSE)
    {
        msg <- c("anyNA() method for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("which", "SparseArraySeed",
    function(x, arr.ind=FALSE, useNames=TRUE)
    {
        msg <- c("which() method for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

setMethod("Summary", "SparseArraySeed",
    function(x, ..., na.rm=FALSE)
    {
        msg <- c(.Generic, "() method for SparseArraySeed objects ",
                 "is defunct in BioC >= 3.21")
        .Defunct(msg=wmsg(msg))
    }
)

### S3/S4 combo for range.SparseArraySeed
range.SparseArraySeed <- function(..., na.rm=FALSE, finite=FALSE)
{
    msg <- c("range() method for SparseArraySeed objects ",
             "is defunct in BioC >= 3.21")
    .Defunct(msg=wmsg(msg))
}
setMethod("range", "SparseArraySeed",
    function(x, ..., finite=FALSE, na.rm=FALSE)
        range.SparseArraySeed(x, ..., na.rm=na.rm, finite=finite)

)

.mean_SparseArraySeed <- function(x, na.rm=FALSE)
{
    msg <- c("mean() method for SparseArraySeed objects ",
             "is defunct in BioC >= 3.21")
    .Defunct(msg=wmsg(msg))
}

### S3/S4 combo for mean.SparseArraySeed
mean.SparseArraySeed <- function(x, na.rm=FALSE, ...)
    .mean_SparseArraySeed(x, na.rm=na.rm, ...)
setMethod("mean", "SparseArraySeed", .mean_SparseArraySeed)

