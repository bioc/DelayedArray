### =========================================================================
### DelayedOp objects
### -------------------------------------------------------------------------
###
### In a DelayedArray object the delayed operations are stored as a tree of
### DelayedOp objects. Each node in the tree is represented by a DelayedOp
### object. 8 types of nodes are currently supported. Each type is a concrete
### DelayedOp subclass:
###
###   Node type                        Represented operation
###   -------------------------------------------------------------------
###   DelayedOp (VIRTUAL)
###   -------------------------------------------------------------------
###   * DelayedUnaryOp (VIRTUAL)
###     o DelayedSubset                Multi-dimensional single bracket
###                                    subsetting.
###     o DelayedAperm                 Extended aperm() (can drop and/or
###                                    add ineffective dimensions).
###     o DelayedUnaryIsoOp (VIRTUAL)  Unary op that preserves the
###                                    geometry.
###       - DelayedUnaryIsoOpStack     Simple ops stacked together.
###       - DelayedUnaryIsoOpWithArgs  One op with vector-like arguments
###                                    along the dimensions of the input.
###       - DelayedSubassign           Multi-dimensional single bracket
###                                    subassignment.
###       - DelayedSetDimnames         Set/replace the dimnames.
###   -------------------------------------------------------------------
###   * DelayedNaryOp (VIRTUAL)
###     o DelayedNaryIsoOp             N-ary op that preserves the
###                                    geometry.
###     o DelayedAbind                 abind()
###   -------------------------------------------------------------------
###
### All DelayedOp objects must comply with the "seed contract" i.e. they must
### support dim(), dimnames(), and extract_array(). This makes them de facto
### array-like objects. However, end users will never interact with them
### directly, except for the root of the tree which is the DelayedArray
### object itself and the only node in the tree that they are able to see
### and touch.
###

### This virtual class and its 8 concrete subclasses are for internal use
### only and never exposed to the end user.
setClass("DelayedOp", contains="Array", representation("VIRTUAL"))

### NOT exported for now.
setGeneric("is_noop", function(x) standardGeneric("is_noop"))

### S3/S4 combo for summary.DelayedOp

.DelayedOp_summary <- function(object) sprintf("%s object", class(object))
summary.DelayedOp <- function(object, ...) .DelayedOp_summary(object, ...)
setMethod("summary", "DelayedOp", summary.DelayedOp)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DelayedUnaryOp objects
###

setClass("DelayedUnaryOp",
    contains="DelayedOp",
    representation(
        "VIRTUAL",
        seed="ANY"    # The input array-like object. Expected to comply
                      # with the "seed contract".
    ),
    prototype(
        seed=new("array")
    )
)

.validate_DelayedUnaryOp <- function(x)
{
    if (length(dim(x@seed)) == 0L)
        return("the supplied seed must have dimensions")
    res <- try(S4Arrays:::extract_empty_array(x@seed), silent=TRUE)
    if (inherits(res, "try-error"))
        return("the supplied seed must support extract_array()")
    TRUE
}

setValidity2("DelayedUnaryOp", .validate_DelayedUnaryOp)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DelayedUnaryIsoOp objects
###
### Representation of a delayed unary isometric operation.
###

setClass("DelayedUnaryIsoOp",
    contains="DelayedUnaryOp",
    representation("VIRTUAL")
)

### Seed contract.
### The 3 default methods below give DelayedUnaryIsoOp derivatives a no-op
### semantic by default.
### DelayedUnaryIsoOpStack and DelayedUnaryIsoOpWithArgs objects overwrite
### this default "extract_array" method.
### DelayedSetDimnames objects overwrite this default "dimnames" method.
### Note that a DelayedArray object is also a DelayedUnaryIsoOp derivative
### and is considered to be the root node of the tree of DelayedOp objects
### contained in it. From a DelayedOp point of view, this root node must
### represent a no-op so DelayedArray objects inherit the 3 default methods
### below.

setMethod("dim", "DelayedUnaryIsoOp", function(x) dim(x@seed))

setMethod("dimnames", "DelayedUnaryIsoOp", function(x) dimnames(x@seed))

setMethod("extract_array", "DelayedUnaryIsoOp",
    function(x, index) extract_array(x@seed, index)
)

### is_sparse() and extract_sparse_array().
### Like the 3 default methods above (seed contract), the 2 default methods
### below also implement a no-op semantic and are also inherited by
### DelayedArray objects.

setMethod("is_sparse", "DelayedUnaryIsoOp", function(x) is_sparse(x@seed))

### 'is_sparse(x)' is assumed to be TRUE and 'index' is assumed to
### not contain duplicates. See "extract_sparse_array() contract"
### in SparseArray/R/extract_sparse_array.R (SparseArray package).
setMethod("extract_sparse_array", "DelayedUnaryIsoOp",
    function(x, index) extract_sparse_array(x@seed, index)
)

### 'is_sparse(x)' is assumed to be TRUE and 'index' is assumed to
### not contain duplicates. See "OLD_extract_sparse_array() Terms of Use"
### in SparseArraySeed-class.R
setMethod("OLD_extract_sparse_array", "DelayedUnaryIsoOp",
    function(x, index) OLD_extract_sparse_array(x@seed, index)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DelayedNaryOp objects
###

setClass("DelayedNaryOp",
    contains="DelayedOp",
    representation(
        "VIRTUAL",
        seeds="list"  # The input array-like objects. Each object is
                      # expected to comply with the "seed contract".
    ),
    prototype(
        seeds=list(new("array"))
    )
)

.validate_DelayedNaryOp <- function(x)
{
    if (!is.list(x@seeds))
        return("'x@seeds' must be a list")
    if (length(x@seeds) == 0L)
        return("'x@seeds' cannot be empty")
    for (i in seq_along(x@seeds)) {
        seed <- x@seeds[[i]]
        if (length(dim(seed)) == 0L)
            return(paste0("x@seeds[[", i, "]] has no dimensions ",
                          "(all the supplied seeds must have dimensions)"))
        res <- try(S4Arrays:::extract_empty_array(seed), silent=TRUE)
        if (inherits(res, "try-error"))
            return(paste0("x@seeds[[", i, "]] does not support ",
                          "extract_array() (all the supplied seeds ",
                          "must support extract_array())"))
    }
    TRUE
}

setValidity2("DelayedNaryOp", .validate_DelayedNaryOp)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### updateObject()
###

setMethod("updateObject", "DelayedOp",
    function(object, ..., verbose=FALSE)
    {
        if (.hasSlot(object, "seed")) {
            object@seed <- updateObject(object@seed, ..., verbose=verbose)
        }
        if (.hasSlot(object, "seeds")) {
            object@seeds <- lapply(object@seeds,
                function(seed) updateObject(seed, ..., verbose=verbose))
        }
        object
    }
)

