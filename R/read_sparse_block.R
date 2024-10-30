### =========================================================================
### read_sparse_block()
### -------------------------------------------------------------------------
###

setGeneric("read_sparse_block", signature="x",
    function(x, viewport)
    {
        msg <- c("read_sparse_block() is defunct in BioC >= 3.21. ",
                 "Please use SparseArray::read_block_as_sparse() instead.")
        .Defunct(msg=wmsg(msg))
    }
)

