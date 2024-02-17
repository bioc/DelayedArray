### =========================================================================
### rowsum() and colsum() methods for DelayedMatrix objects
### -------------------------------------------------------------------------
###
### These methods are block processed.
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Helpers for BLOCK_rowsum() and BLOCK_colsum()
###

### Whether 'BACKEND' is compatible with the "shared sink" route (see below
### in this file for what the "shared sink" route is).
.compatible_BACKEND <- function(BACKEND)
{
    if (is.null(BACKEND))
        return(FALSE)
    ## Same check as in load_BACKEND_package().
    if (!isSingleString(BACKEND))
        stop(wmsg("'BACKEND' must be a single string or NULL"))
    ## write_block() method for RleRealizationSink objects is broken (it
    ## ignores the 'viewport' argument!) so, until this is fixed, the
    ## RleArray realization backend is not compatible.
    BACKEND != "RleArray"
}

### 'input_grid' must be a 2D grid.
### Returns a 2D grid 'sink_grid' that verifies:
###   (a) refdim(sink_grid)[[1]] == sink_nrow;
###   (b) refdim(sink_grid)[[2]] == refdim(input_grid)[[2]];
###   (c) the blocks on 'sink_grid' are made of full columns and they align
###       with the vertical strips on 'input_grid'.
### The consequences of (c) are that nrow(sink_grid) == 1 and
### ncol(sink_grid) == length(sink_grid) == ncol(input_grid).
.make_sink_grid_of_vstrips <- function(input_grid, sink_nrow)
{
    stopifnot(is(input_grid, "ArrayGrid"),
              length(dim(input_grid)) == 2L,
              isSingleInteger(sink_nrow))
    if (is(input_grid, "ArbitraryArrayGrid")) {
        tickmarks <- list(sink_nrow, input_grid@tickmarks[[2L]])
        ArbitraryArrayGrid(tickmarks)
    } else {
        refdim <- c(sink_nrow, refdim(input_grid)[[2L]])
        spacings <- c(sink_nrow, ncol(input_grid[[1L]]))
        RegularArrayGrid(refdim, spacings=spacings)
    }
}

.make_sink_grid_of_hstrips <- function(input_grid, sink_ncol)
{
    stopifnot(is(input_grid, "ArrayGrid"),
              length(dim(input_grid)) == 2L,
              isSingleInteger(sink_ncol))
    if (is(input_grid, "ArbitraryArrayGrid")) {
        tickmarks <- list(input_grid@tickmarks[[1L]], sink_ncol)
        ArbitraryArrayGrid(tickmarks)
    } else {
        refdim <- c(refdim(input_grid)[[1L]], sink_ncol)
        spacings <- c(nrow(input_grid[[1L]]), sink_ncol)
        RegularArrayGrid(refdim, spacings=spacings)
    }
}

### The blocks on 'sink_grid' are made of full columns so each block is
### a vertical strip.
.chunking_is_compatible_with_vstrips <- function(sink_chunkdim, sink_grid)
{
    stopifnot(is(sink_grid, "ArrayGrid"),
              length(dim(sink_grid)) == 2L,
              nrow(sink_grid) == 1L)
    if (is.null(sink_chunkdim))  # no-chunking
        return(TRUE)
    stopifnot(is.integer(sink_chunkdim), length(sink_chunkdim) == 2L)
    ## We treat the "single big chunk" case like the no-chunking case.
    ## Note that the "single big chunk" situation only happens for very
    ## small sinks in which case the chunking does not significantly impact
    ## the writing performance. However, treating this situation as compatible
    ## with the sink vertical strips is convenient when testing things like
    ## BLOCK_rowsum(..., BACKEND="HDF5Array") on a small toy dataset.
    if (all(sink_chunkdim == refdim(sink_grid)))
        return(TRUE)
    ## Dumb heuristic: We consider incompatible chunks that are wider than
    ## the first block in 'sink_grid'.
    ## FIXME: This could certainly be improved/refined.
    ## Anyway, the most important thing for now is that it covers the
    ## worst-case scenario, which is when the sink uses a storage layout
    ## that is row-oriented (i.e. is the transposed of what is used by a
    ## TENxRealizationSink object), and 'sink_grid' has more than one
    ## vertical strip. Whatever heuristic we use, we want to make sure that
    ## it returns FALSE in this case.
    if (sink_chunkdim[[2L]] <= ncol(sink_grid[[1L]]))
        return(TRUE)
    FALSE
}

### The blocks on 'sink_grid' are made of full rows so each block is
### a horizontal strip.
.chunking_is_compatible_with_hstrips <- function(sink_chunkdim, sink_grid)
{
    stopifnot(is(sink_grid, "ArrayGrid"),
              length(dim(sink_grid)) == 2L,
              ncol(sink_grid) == 1L)
    if (is.null(sink_chunkdim))  # no-chunking
        return(TRUE)
    stopifnot(is.integer(sink_chunkdim), length(sink_chunkdim) == 2L)
    if (all(sink_chunkdim == refdim(sink_grid)))
        return(TRUE)
    ## Dumb heuristic: We consider incompatible chunks that are taller than
    ## the first block in 'sink_grid'.
    ## FIXME: This could certainly be improved/refined.
    ## Anyway, the most important thing for now is that it covers the
    ## worst-case scenario, which is when the sink uses a storage layout
    ## that is column-oriented (e.g. TENxRealizationSink object),
    ## and 'sink_grid' has more than one horizontal strip.
    ## So whatever heuristic we use, we want to make sure that it returns
    ## FALSE in this case.
    if (sink_chunkdim[[1L]] <= nrow(sink_grid[[1L]]))
        return(TRUE)
    FALSE
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### BLOCK_rowsum() and BLOCK_colsum()
###

### x: a matrix-like object (typically a DelayedMatrix).
### Walks on the grid defined on matrix-like object 'x'.
### If 'BACKEND' is NULL, returns an ordinary matrix. Otherwise, returns
### a DelayedMatrix object that is either pristine or the result of cbind'ing
### several pristine DelayedMatrix objects together (delayed cbind()).
BLOCK_rowsum <- function(x, group, reorder=TRUE, na.rm=FALSE,
                         grid=NULL, as.sparse=NA,
                         BACKEND=getAutoRealizationBackend(),
                         BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    stopifnot(length(dim(x)) == 2L)  # matrix-like object
    ugroup <- as.character(S4Arrays:::compute_ugroup(group, nrow(x), reorder))
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))

    ## INIT() must return a matrix of type "double" rather than "integer".
    ## This is to avoid integer overflows during the within-strip walks.
    INIT <- function(j, grid, ugroup, x_colnames) {
        vp <- grid[[1L, j]]
        dn <- list(ugroup, extractROWS(x_colnames, ranges(vp)[2L]))
        matrix(0.0, nrow=length(ugroup), ncol=ncol(vp), dimnames=dn)
    }
    INIT_MoreArgs <- list(ugroup=ugroup, x_colnames=colnames(x))

    FUN <- function(init, block, group, ugroup, na.rm=FALSE) {
        if (is(block, "SparseArraySeed"))
            block <- as(block, "CsparseMatrix")  # to dgCMatrix or lgCMatrix
        vp <- currentViewport()
        group2 <- extractROWS(group, ranges(vp)[1L])
        block_ans <- rowsum(block, group2, reorder=FALSE, na.rm=na.rm)
        if (!is.matrix(block_ans))
            block_ans <- as.matrix(block_ans)
        m <- match(rownames(block_ans), ugroup)
        init[m, ] <- init[m, ] + block_ans
        init
    }
    FUN_MoreArgs <- list(group=group, ugroup=ugroup, na.rm=na.rm)

    use_shared_sink <- FALSE
    grid <- best_grid_for_vstrip_apply(x, grid)
    if (ncol(grid) >= 2L && .compatible_BACKEND(BACKEND) && is.null(BPPARAM)) {
        ## Try the "shared sink" route which consists in using a single
        ## realization sink shared across all strips. Note that we don't try
        ## it in the context of parallel processing at the moment because
        ## there's no guarantee that the sink will support concurrent
        ## writes (e.g. HDF5 does not).
        sink <- RealizationSink(BACKEND, c(length(ugroup), ncol(x)),
                                         dimnames=list(ugroup, colnames(x)),
                                         type="double")
        sink_grid <- .make_sink_grid_of_vstrips(grid, nrow(sink))
        ## We take the "shared sink" route only if the chunks are "compatible"
        ## with the writing of full sink columns by callback function FINAL()
        ## below (this callback function will get called at the end of
        ## processing each vertical strip).
        use_shared_sink <- .chunking_is_compatible_with_vstrips(chunkdim(sink),
                                                                sink_grid)
    }
    if (use_shared_sink) {
        FINAL <- function(init, j, grid, sink, sink_grid) {
            write_block(sink, sink_grid[[1L, j]], init)  # write full sink cols
        }
        FINAL_MoreArgs <- list(sink=sink, sink_grid=sink_grid)
    } else {
        ## No-op if 'BACKEND' is NULL.
        FINAL <- function(init, j, grid, BACKEND) realize(init, BACKEND=BACKEND)
        FINAL_MoreArgs <- list(BACKEND=BACKEND)
    }

    strip_results <- vstrip_apply(x, INIT, INIT_MoreArgs,
                                     FUN, FUN_MoreArgs,
                                     FINAL, FINAL_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    if (use_shared_sink) {
        close(sink)
        as(sink, "DelayedArray")
    } else {
        do.call(cbind, strip_results)
    }
}

### x: a matrix-like object (typically a DelayedMatrix).
### Walks on the grid defined on matrix-like object 'x'.
### If 'BACKEND' is NULL, returns an ordinary matrix. Otherwise, returns
### a DelayedMatrix object that is either pristine or the result of rbind'ing
### several pristine DelayedMatrix objects together (delayed rbind()).
BLOCK_colsum <- function(x, group, reorder=TRUE, na.rm=FALSE,
                         grid=NULL, as.sparse=NA,
                         BACKEND=getAutoRealizationBackend(),
                         BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    stopifnot(length(dim(x)) == 2L)  # matrix-like object
    ugroup <- as.character(S4Arrays:::compute_ugroup(group, ncol(x), reorder))
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))

    ## INIT() must return a matrix of type "double" rather than "integer".
    ## This is to avoid integer overflows during the within-strip walks.
    INIT <- function(i, grid, ugroup, x_rownames) {
        vp <- grid[[i, 1L]]
        dn <- list(extractROWS(x_rownames, ranges(vp)[1L]), ugroup)
        matrix(0.0, nrow=nrow(vp), ncol=length(ugroup), dimnames=dn)
    }
    INIT_MoreArgs <- list(ugroup=ugroup, x_rownames=rownames(x))

    FUN <- function(init, block, group, ugroup, na.rm=FALSE) {
        if (is(block, "SparseArraySeed"))
            block <- as(block, "CsparseMatrix")  # to dgCMatrix or lgCMatrix
        vp <- currentViewport()
        group2 <- extractROWS(group, ranges(vp)[2L])
        block_ans <- colsum(block, group2, reorder=FALSE, na.rm=na.rm)
        if (!is.matrix(block_ans))
            block_ans <- as.matrix(block_ans)
        m <- match(colnames(block_ans), ugroup)
        init[ , m] <- init[ , m] + block_ans
        init
    }
    FUN_MoreArgs <- list(group=group, ugroup=ugroup, na.rm=na.rm)

    use_shared_sink <- FALSE
    grid <- best_grid_for_hstrip_apply(x, grid)
    if (nrow(grid) >= 2L && .compatible_BACKEND(BACKEND) && is.null(BPPARAM)) {
        ## Try the "shared sink" route which consists in using a single
        ## realization sink shared across all strips. Note that we don't try
        ## it in the context of parallel processing at the moment because
        ## there's no guarantee that the sink will support concurrent
        ## writes (e.g. HDF5 does not).
        sink <- RealizationSink(BACKEND, c(nrow(x), length(ugroup)),
                                         dimnames=list(rownames(x), ugroup),
                                         type="double")
        sink_grid <- .make_sink_grid_of_hstrips(grid, ncol(sink))
        ## We take the "shared sink" route only if the chunks are "compatible"
        ## with the writing of full sink rows by callback function FINAL()
        ## below (this callback function will get called at the end of
        ## processing each horizontal strip).
        use_shared_sink <- .chunking_is_compatible_with_hstrips(chunkdim(sink),
                                                                sink_grid)
    }
    if (use_shared_sink) {
        FINAL <- function(init, i, grid, sink, sink_grid) {
            write_block(sink, sink_grid[[i, 1L]], init)  # write full sink rows
        }
        FINAL_MoreArgs <- list(sink=sink, sink_grid=sink_grid)
    } else {
        ## No-op if 'BACKEND' is NULL.
        FINAL <- function(init, i, grid, BACKEND) realize(init, BACKEND=BACKEND)
        FINAL_MoreArgs <- list(BACKEND=BACKEND)
    }

    strip_results <- hstrip_apply(x, INIT, INIT_MoreArgs,
                                     FUN, FUN_MoreArgs,
                                     FINAL, FINAL_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    if (use_shared_sink) {
        close(sink)
        as(sink, "DelayedArray")
    } else {
        do.call(rbind, strip_results)
    }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### rowsum() and colsum() methods
###

### S3/S4 combo for rowsum.DelayedMatrix
rowsum.DelayedMatrix <- function(x, group, reorder=TRUE, ...)
    BLOCK_rowsum(x, group, reorder=reorder, ...)
setMethod("rowsum", "DelayedMatrix", BLOCK_rowsum)

setMethod("colsum", "DelayedMatrix", BLOCK_colsum)
