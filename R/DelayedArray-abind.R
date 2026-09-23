### =========================================================================
### Bind DelayedArray objects with abind()
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### abind()
###
### Note that making abind() work on DelayedArray objects also makes arbind(),
### acbind(), rbind(), cbind(), and bindROWS() work on these objects.
###

.abind_DelayedArray_objects <- function(..., along=NULL, rev.along=NULL)
{
    objects <- S4Vectors:::delete_NULLs(list(...))
    if (length(objects) == 0L)
        return(NULL)

    ndims <- vapply(objects, function(object) length(dim(object)), integer(1))
    N <- max(ndims)
    along <- S4Arrays:::get_along(N, along=along, rev.along=rev.along)
    ans_ndim <- max(N, along)
    objects <- S4Arrays:::add_missing_dims(objects, ans_ndim)

    ## Check dim compatibility.
    dims <- S4Arrays:::get_dims_to_bind(objects, along)
    if (is.character(dims))
        stop(wmsg(dims))
    x <- objects[[1L]]
    if (length(objects) == 1L)
        return(x)

    stash_DelayedAbind(x, objects[-1L], along=along)
}

setMethod("abind", "DelayedArray", .abind_DelayedArray_objects)

