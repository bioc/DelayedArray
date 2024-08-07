### =========================================================================
### Row and column summarization methods for DelayedMatrix objects
### -------------------------------------------------------------------------
###


### Raise an error if invalid input type. Otherwise return "integer",
### "numeric", "double", or "complex".
### NOTE: No longer used in this file but used in DelayedMatrixStats.
.get_ans_type <- function(x, must.be.numeric=FALSE)
{
    x_type <- type(x)
    ans_type <- switch(x_type,
        logical="integer",
        integer=, numeric=, double=, complex=x_type,
        stop(wmsg("operation not supported on matrices of type ", x_type)))
    if (must.be.numeric && !is.numeric(get(ans_type)(0)))
        stop(wmsg("operation not supported on matrices of type ", x_type))
    ans_type
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### row/colSums()
###
### The methods for ordinary matrices are defined in the base package.
###

BLOCK_rowSums <- function(x, na.rm=FALSE, useNames=TRUE,
                          grid=NULL, as.sparse=NA,
                          BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    INIT <- function(i, grid) numeric(nrow(grid[[i, 1L]]))
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        init + MatrixGenerics::rowSums(block, na.rm=na.rm)
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    strip_results <- hstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- rownames(x)
    ans
}

BLOCK_colSums <- function(x, na.rm=FALSE, useNames=TRUE,
                          grid=NULL, as.sparse=NA,
                          BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    INIT <- function(j, grid) numeric(ncol(grid[[1L, j]]))
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        init + MatrixGenerics::colSums(block, na.rm=na.rm)
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    strip_results <- vstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- colnames(x)
    ans
}

.check_dims <- function(dims, method)
{
    if (!identical(dims, 1))
        stop(wmsg("\"", method, "\" method for DelayedMatrix objects ",
                  "does not support the 'dims' argument"))
}

### base::rowSums() has a 'dims' argument. We do NOT support it.
.rowSums_DelayedMatrix <- function(x, na.rm=FALSE, dims=1)
{
    .check_dims(dims, "rowSums")
    BLOCK_rowSums(x, na.rm=na.rm)
}
setMethod("rowSums", "DelayedMatrix", .rowSums_DelayedMatrix)

### base::colSums() has a 'dims' argument. We do NOT support it.
.colSums_DelayedMatrix <- function(x, na.rm=FALSE, dims=1)
{
    .check_dims(dims, "colSums")
    BLOCK_colSums(x, na.rm=na.rm)
}
setMethod("colSums", "DelayedMatrix", .colSums_DelayedMatrix)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### row/colMeans()
###
### The methods for ordinary matrices are defined in the base package.
###

.row_sums_and_nvals <- function(x, na.rm=FALSE)
{
    ## 'x' is either an ordinary matrix or SparseMatrix derivative
    ## (SVT_SparseMatrix or COO_SparseMatrix object).
    row_sums <- MatrixGenerics::rowSums(x, na.rm=na.rm)
    row_nvals <- rep.int(ncol(x), nrow(x))
    if (na.rm)
        row_nvals <- row_nvals - MatrixGenerics::rowSums(is.na(x))
    data.frame(sum=row_sums, nval=row_nvals)
}

.col_sums_and_nvals <- function(x, na.rm=FALSE)
{
    ## 'x' is either an ordinary matrix or SparseMatrix derivative
    ## (SVT_SparseMatrix or COO_SparseMatrix object).
    col_sums <- MatrixGenerics::colSums(x, na.rm=na.rm)
    col_nvals <- rep.int(nrow(x), ncol(x))
    if (na.rm)
        col_nvals <- col_nvals - MatrixGenerics::colSums(is.na(x))
    data.frame(sum=col_sums, nval=col_nvals)
}

BLOCK_rowMeans <- function(x, na.rm=FALSE, useNames=TRUE,
                           grid=NULL, as.sparse=NA,
                           BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    INIT <- function(i, grid) {
        n <- nrow(grid[[i, 1L]])
        .row_sums_and_nvals(matrix(nrow=n, ncol=0L))
    }
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        init + .row_sums_and_nvals(block, na.rm=na.rm)
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    FINAL <- function(init, i, grid) { init$sum / init$nval }

    strip_results <- hstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     FINAL=FINAL,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- rownames(x)
    ans
}

BLOCK_colMeans <- function(x, na.rm=FALSE, useNames=TRUE,
                           grid=NULL, as.sparse=NA,
                           BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    INIT <- function(j, grid) {
        n <- ncol(grid[[1L, j]])
        .col_sums_and_nvals(matrix(nrow=0L, ncol=n))
    }
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        init + .col_sums_and_nvals(block, na.rm=na.rm)
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    FINAL <- function(init, j, grid) { init$sum / init$nval }

    strip_results <- vstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     FINAL=FINAL,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- colnames(x)
    ans
}

### base::rowMeans() has a 'dims' argument. We do NOT support it.
.rowMeans_DelayedMatrix <- function(x, na.rm=FALSE, dims=1)
{
    .check_dims(dims, "rowMeans")
    BLOCK_rowMeans(x, na.rm=na.rm)
}
setMethod("rowMeans", "DelayedMatrix", .rowMeans_DelayedMatrix)

### base::colMeans() has a 'dims' argument. We do NOT support it.
.colMeans_DelayedMatrix <- function(x, na.rm=FALSE, dims=1)
{
    .check_dims(dims, "colMeans")
    BLOCK_colMeans(x, na.rm=na.rm)
}
setMethod("colMeans", "DelayedMatrix", .colMeans_DelayedMatrix)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### row/colMins()
###
### The methods for ordinary matrices are defined in the matrixStats package.
###

BLOCK_rowMins <- function(x, na.rm=FALSE, useNames=TRUE,
                          grid=NULL, as.sparse=NA,
                          BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    if (ncol(x) == 0L) {
        ans <- rep.int(Inf, nrow(x))
        if (useNames)
            names(ans) <- rownames(x)
        return(ans)
    }

    INIT <- function(i, grid) NULL
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        block_rowmins <- MatrixGenerics::rowMins(block, na.rm=na.rm,
                                                 useNames=FALSE)
        if (is.null(init))
            return(block_rowmins)
        pmin(init, block_rowmins)
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    strip_results <- hstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- rownames(x)
    ans
}

BLOCK_colMins <- function(x, na.rm=FALSE, useNames=TRUE,
                          grid=NULL, as.sparse=NA,
                          BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    if (nrow(x) == 0L) {
        ans <- rep.int(Inf, ncol(x))
        if (useNames)
            names(ans) <- colnames(x)
        return(ans)
    }

    INIT <- function(j, grid) NULL
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        block_colmins <- MatrixGenerics::colMins(block, na.rm=na.rm,
                                                 useNames=FALSE)
        if (is.null(init))
            return(block_colmins)
        pmin(init, block_colmins)
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    strip_results <- vstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- colnames(x)
    ans
}

.check_rows_cols <- function(rows, cols, method)
{
    if (!(is.null(rows) && is.null(cols)))
        stop(wmsg("\"", method, "\" method for DelayedMatrix objects ",
                  "does not support arguments 'rows' and 'cols'"))
}

### MatrixGenerics::rowMins() has the 'rows' and 'cols' arguments.
### We do NOT support them.
.rowMins_DelayedMatrix <- function(x, rows=NULL, cols=NULL, na.rm=FALSE,
                                   useNames=TRUE)
{
    .check_rows_cols(rows, cols, "rowMins")
    BLOCK_rowMins(x, na.rm=na.rm, useNames=useNames)
}
setMethod("rowMins", "DelayedMatrix", .rowMins_DelayedMatrix)

### MatrixGenerics::colMins() has the 'rows' and 'cols' arguments.
### We do NOT support them.
.colMins_DelayedMatrix <- function(x, rows=NULL, cols=NULL, na.rm=FALSE,
                                   useNames=TRUE)
{
    .check_rows_cols(rows, cols, "colMins")
    BLOCK_colMins(x, na.rm=na.rm, useNames=useNames)
}
setMethod("colMins", "DelayedMatrix", .colMins_DelayedMatrix)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### row/colMaxs()
###
### The methods for ordinary matrices are defined in the matrixStats package.
###

BLOCK_rowMaxs <- function(x, na.rm=FALSE, useNames=TRUE,
                          grid=NULL, as.sparse=NA,
                          BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    if (ncol(x) == 0L) {
        ans <- rep.int(-Inf, nrow(x))
        if (useNames)
            names(ans) <- rownames(x)
        return(ans)
    }

    INIT <- function(i, grid) NULL
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        block_rowmaxs <- MatrixGenerics::rowMaxs(block, na.rm=na.rm,
                                                 useNames=FALSE)
        if (is.null(init))
            return(block_rowmaxs)
        pmax(init, block_rowmaxs)
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    strip_results <- hstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- rownames(x)
    ans
}

BLOCK_colMaxs <- function(x, na.rm=FALSE, useNames=TRUE,
                          grid=NULL, as.sparse=NA,
                          BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    if (nrow(x) == 0L) {
        ans <- rep.int(-Inf, ncol(x))
        if (useNames)
            names(ans) <- colnames(x)
        return(ans)
    }

    INIT <- function(j, grid) NULL
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        block_colmaxs <- MatrixGenerics::colMaxs(block, na.rm=na.rm,
                                                 useNames=FALSE)
        if (is.null(init))
            return(block_colmaxs)
        pmax(init, block_colmaxs)
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    strip_results <- vstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- colnames(x)
    ans
}

### MatrixGenerics::rowMaxs() has the 'rows' and 'cols' arguments.
### We do NOT support them.
.rowMaxs_DelayedMatrix <- function(x, rows=NULL, cols=NULL, na.rm=FALSE,
                                   useNames=TRUE)
{
    .check_rows_cols(rows, cols, "rowMaxs")
    BLOCK_rowMaxs(x, na.rm=na.rm, useNames=useNames)
}
setMethod("rowMaxs", "DelayedMatrix", .rowMaxs_DelayedMatrix)

### MatrixGenerics::colMaxs() has the 'rows' and 'cols' arguments.
### We do NOT support them.
.colMaxs_DelayedMatrix <- function(x, rows=NULL, cols=NULL, na.rm=FALSE,
                                   useNames=TRUE)
{
    .check_rows_cols(rows, cols, "colMaxs")
    BLOCK_colMaxs(x, na.rm=na.rm, useNames=useNames)
}
setMethod("colMaxs", "DelayedMatrix", .colMaxs_DelayedMatrix)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### row/colRanges()
###
### The methods for ordinary matrices are defined in the matrixStats package.
###

BLOCK_rowRanges <- function(x, na.rm=FALSE, useNames=TRUE,
                            grid=NULL, as.sparse=NA,
                            BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop("'na.rm' must be TRUE or FALSE")
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    if (ncol(x) == 0L) {
        ans <- cbind(rep.int(Inf, nrow(x)), rep.int(-Inf, nrow(x)))
        if (useNames)
            rownames(ans) <- rownames(x)
        return(ans)
    }

    INIT <- function(i, grid) NULL
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        block_rowranges <- MatrixGenerics::rowRanges(block, na.rm=na.rm,
                                                     useNames=FALSE)
        if (is.null(init))
            return(block_rowranges)
        cbind(pmin(init[ , 1L], block_rowranges[ , 1L]),
              pmax(init[ , 2L], block_rowranges[ , 2L]))
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    strip_results <- hstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- do.call(rbind, strip_results)
    if (useNames)
        rownames(ans) <- rownames(x)
    ans
}

BLOCK_colRanges <- function(x, na.rm=FALSE, useNames=TRUE,
                            grid=NULL, as.sparse=NA,
                            BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop("'na.rm' must be TRUE or FALSE")
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    if (nrow(x) == 0L) {
        ans <- cbind(rep.int(Inf, ncol(x)), rep.int(-Inf, ncol(x)))
        if (useNames)
            rownames(ans) <- colnames(x)
        return(ans)
    }

    INIT <- function(i, grid) NULL
    INIT_MoreArgs <- list()

    FUN <- function(init, block, na.rm=FALSE) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        block_colranges <- MatrixGenerics::colRanges(block, na.rm=na.rm,
                                                     useNames=FALSE)
        if (is.null(init))
            return(block_colranges)
        cbind(pmin(init[ , 1L], block_colranges[ , 1L]),
              pmax(init[ , 2L], block_colranges[ , 2L]))
    }
    FUN_MoreArgs <- list(na.rm=na.rm)

    strip_results <- vstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                                     grid=grid, as.sparse=as.sparse,
                                     BPPARAM=BPPARAM, verbose=verbose)
    ans <- do.call(rbind, strip_results)
    if (useNames)
        rownames(ans) <- colnames(x)
    ans
}

### MatrixGenerics::rowRanges() has the 'rows' and 'cols' arguments.
### We do NOT support them.
.rowRanges_DelayedMatrix <- function(x, rows=NULL, cols=NULL, na.rm=FALSE,
                                     useNames=TRUE)
{
    .check_rows_cols(rows, cols, "rowRanges")
    BLOCK_rowRanges(x, na.rm=na.rm, useNames=useNames)
}
setMethod("rowRanges", "DelayedMatrix", .rowRanges_DelayedMatrix)

### MatrixGenerics::colRanges() has the 'rows' and 'cols' arguments.
### We do NOT support them.
.colRanges_DelayedMatrix <- function(x, rows=NULL, cols=NULL, na.rm=FALSE,
                                     useNames=TRUE)
{
    .check_rows_cols(rows, cols, "colRanges")
    BLOCK_colRanges(x, na.rm=na.rm, useNames=useNames)
}
setMethod("colRanges", "DelayedMatrix", .colRanges_DelayedMatrix)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### row/colVars()
###
### The methods for ordinary matrices are defined in the matrixStats package.
###

.compute_rowVars_for_full_width_blocks <-
    function(grid, x, na.rm=FALSE, center=NULL,
             as.sparse=NA, BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    stopifnot(ncol(grid) == 1L)
    stopifnot(is.null(center) ||
              (is.numeric(center) && length(center) == nrow(x)))

    blockApply(x,
        function(block, na.rm, center) {
            if (is.null(center)) {
                block_center <- NULL
            } else {
                viewport_range1 <- ranges(currentViewport())[1L]
                block_center <- extractROWS(center, viewport_range1)
            }
            ## 'block' is either an ordinary matrix or SparseMatrix derivative
            ## (SVT_SparseMatrix or COO_SparseMatrix object).
            MatrixGenerics::rowVars(block, na.rm=na.rm, center=block_center,
                                    useNames=FALSE)
        },
        na.rm, center,
        grid=grid, as.sparse=as.sparse,
        BPPARAM=BPPARAM, verbose=verbose
    )
}

.compute_colVars_for_full_height_blocks <-
    function(grid, x, na.rm=FALSE, center=NULL,
             as.sparse=NA, BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    stopifnot(nrow(grid) == 1L)
    stopifnot(is.null(center) ||
              (is.numeric(center) && length(center) == ncol(x)))

    blockApply(x,
        function(block, na.rm, center) {
            if (is.null(center)) {
                block_center <- NULL
            } else {
                viewport_range2 <- ranges(currentViewport())[2L]
                block_center <- extractROWS(center, viewport_range2)
            }
            ## 'block' is either an ordinary matrix or SparseMatrix derivative
            ## (SVT_SparseMatrix or COO_SparseMatrix object).
            MatrixGenerics::colVars(block, na.rm=na.rm, center=block_center,
                                    useNames=FALSE)
        },
        na.rm, center,
        grid=grid, as.sparse=as.sparse,
        BPPARAM=BPPARAM, verbose=verbose
    )
}

.compute_rowVars_for_horizontal_strips <-
    function(grid, x, na.rm=FALSE, center=NULL,
             as.sparse=NA, BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    stopifnot(is.null(center) ||
              (is.numeric(center) && length(center) == nrow(x)))

    INIT <- function(i, grid, x, na.rm=FALSE, center=NULL,
                              as.sparse=NA, verbose=NA)
    {
        n <- nrow(grid[[i, 1L]])
        if (!is.null(center))
            return(data.frame(sum2=numeric(n), nval=integer(n)))
        reduce_grid_hstrip(i, grid, x,
            INIT=function(i, grid, n) {
                    .row_sums_and_nvals(matrix(nrow=n, ncol=0L))
            },
            INIT_MoreArgs=list(n=n),
            FUN=function(init, block, na.rm) {
                    init + .row_sums_and_nvals(block, na.rm=na.rm)
            },
            FUN_MoreArgs=list(na.rm=na.rm),
            FINAL=function(init, i, grid, n) {
                    center <- init$sum / init$nval
                    data.frame(sum2=numeric(n), nval=init$nval, center=center)
            },
            FINAL_MoreArgs=list(n=n),
            as.sparse,
            verbose
        )
    }
    INIT_MoreArgs <- list(x=x, na.rm=na.rm, center=center,
                          as.sparse=as.sparse, verbose=verbose)

    FINAL <- function(init, i, grid) { init$sum2 / (init$nval - 1L) }

    FUN <- function(init, block, na.rm, center) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        if (is.null(center)) {
            block_center <- init$center
        } else {
            viewport_range1 <- ranges(currentViewport())[1L]
            block_center <- extractROWS(center, viewport_range1)
            block_nvals <- rep.int(ncol(block), nrow(block))
            if (na.rm)
                block_nvals <- block_nvals -
                               MatrixGenerics::rowSums(is.na(block))
        }
        delta <- block
        ## 'delta' could be a SparseMatrix derivative. However
        ## adding/subtracting an ordinary vector to/from it is not
        ## supported so we first turn it into an ordinary matrix.
        if (length(nzwhich(block_center)) != 0L)
            delta <- as.matrix(delta) - block_center
        ## 'delta' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        block_sums2 <- MatrixGenerics::rowSums(delta * delta, na.rm=na.rm)
        if (is.null(center)) {
            init$sum2 <- init$sum2 + block_sums2
            init
        } else {
            init + data.frame(sum2=block_sums2, nval=block_nvals)
        }
    }
    FUN_MoreArgs <- list(na.rm, center)

    hstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                    FINAL=FINAL,
                    grid=grid, as.sparse=as.sparse,
                    BPPARAM=BPPARAM, verbose=verbose)
}

.compute_colVars_for_vertical_strips <-
    function(grid, x, na.rm=FALSE, center=NULL,
             as.sparse=NA, BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    stopifnot(is.null(center) ||
              (is.numeric(center) && length(center) == ncol(x)))

    INIT <- function(j, grid, x, na.rm=FALSE, center=NULL,
                              as.sparse=NA, verbose=NA)
    {
        n <- ncol(grid[[1L, j]])
        if (!is.null(center))
            return(data.frame(sum2=numeric(n), nval=integer(n)))
        reduce_grid_vstrip(j, grid, x,
            INIT=function(j, grid, n) {
                    .col_sums_and_nvals(matrix(nrow=0L, ncol=n))
            },
            INIT_MoreArgs=list(n=n),
            FUN=function(init, block, na.rm) {
                    init + .col_sums_and_nvals(block, na.rm=na.rm)
            },
            FUN_MoreArgs=list(na.rm=na.rm),
            FINAL=function(init, j, grid, n) {
                    center <- init$sum / init$nval
                    data.frame(sum2=numeric(n), nval=init$nval, center=center)
            },
            FINAL_MoreArgs=list(n=n),
            as.sparse,
            verbose
        )
    }
    INIT_MoreArgs <- list(x=x, na.rm=na.rm, center=center,
                          as.sparse=as.sparse, verbose=verbose)

    FINAL <- function(init, j, grid) { init$sum2 / (init$nval - 1L) }

    FUN <- function(init, block, na.rm, center) {
        ## 'block' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        if (is.null(center)) {
            block_center <- init$center
        } else {
            viewport_range2 <- ranges(currentViewport())[2L]
            block_center <- extractROWS(center, viewport_range2)
            block_nvals <- rep.int(nrow(block), ncol(block))
            if (na.rm)
                block_nvals <- block_nvals -
                               MatrixGenerics::colSums(is.na(block))
        }
        delta <- block
        ## 'delta' could be a SparseMatrix derivative. However
        ## adding/subtracting an ordinary vector to/from it is not
        ## supported so we first turn it into an ordinary matrix.
        if (length(nzwhich(block_center)) != 0L)
            delta <- as.matrix(delta) - rep(block_center, each=nrow(block))
        ## 'delta' is either an ordinary matrix or SparseMatrix derivative
        ## (SVT_SparseMatrix or COO_SparseMatrix object).
        block_sums2 <- MatrixGenerics::colSums(delta * delta, na.rm=na.rm)
        if (is.null(center)) {
            init$sum2 <- init$sum2 + block_sums2
            init
        } else {
            init + data.frame(sum2=block_sums2, nval=block_nvals)
        }
    }
    FUN_MoreArgs <- list(na.rm, center)

    vstrip_apply(x, INIT, INIT_MoreArgs, FUN, FUN_MoreArgs,
                    FINAL=FINAL,
                    grid=grid, as.sparse=as.sparse,
                    BPPARAM=BPPARAM, verbose=verbose)
}

BLOCK_rowVars <- function(x, na.rm=FALSE, center=NULL, useNames=TRUE,
                          grid=NULL, as.sparse=NA,
                          BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    if (!is.null(center)) {
        if (!is.numeric(center))
            stop(wmsg("'center' must be NULL or a numeric vector"))
        x_nrow <- nrow(x)
        if (length(center) != x_nrow) {
            if (length(center) != 1L)
                stop(wmsg("'center' must have length 1 or nrow(x)"))
            center <- rep.int(center, x_nrow)
        }
    }

    grid <- best_grid_for_hstrip_apply(x, grid=grid)
    if (ncol(grid) == 1L) {
        fun <- .compute_rowVars_for_full_width_blocks
    } else {
        fun <- .compute_rowVars_for_horizontal_strips
    }
    strip_results <- fun(grid, x, na.rm=na.rm, center=center,
                               as.sparse=as.sparse,
                               BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- rownames(x)
    ans
}

BLOCK_colVars <- function(x, na.rm=FALSE, center=NULL, useNames=TRUE,
                          grid=NULL, as.sparse=NA,
                          BPPARAM=getAutoBPPARAM(), verbose=NA)
{
    if (!isTRUEorFALSE(na.rm))
        stop(wmsg("'na.rm' must be TRUE or FALSE"))
    if (!isTRUEorFALSE(useNames))
        stop(wmsg("'useNames' must be TRUE or FALSE"))

    if (!is.null(center)) {
        if (!is.numeric(center))
            stop(wmsg("'center' must be NULL or a numeric vector"))
        x_ncol <- ncol(x)
        if (length(center) != x_ncol) {
            if (length(center) != 1L)
                stop(wmsg("'center' must have length 1 or ncol(x)"))
            center <- rep.int(center, x_ncol)
        }
    }

    grid <- best_grid_for_vstrip_apply(x, grid=grid)
    if (nrow(grid) == 1L) {
        fun <- .compute_colVars_for_full_height_blocks
    } else {
        fun <- .compute_colVars_for_vertical_strips
    }
    strip_results <- fun(grid, x, na.rm=na.rm, center=center,
                               as.sparse=as.sparse,
                               BPPARAM=BPPARAM, verbose=verbose)
    ans <- unlist(strip_results, recursive=FALSE, use.names=FALSE)
    if (useNames)
        names(ans) <- colnames(x)
    ans
}

setMethod("rowVars", "DelayedMatrix",
    function(x, rows=NULL, cols=NULL, na.rm=FALSE, center=NULL, useNames=TRUE)
    {
        .check_rows_cols(rows, cols, "rowVars")
        BLOCK_rowVars(x, na.rm=na.rm, center=center, useNames=useNames)
    }
)

setMethod("colVars", "DelayedMatrix",
    function(x, rows=NULL, cols=NULL, na.rm=FALSE, center=NULL, useNames=TRUE)
    {
        .check_rows_cols(rows, cols, "colVars")
        BLOCK_colVars(x, na.rm=na.rm, center=center, useNames=useNames)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### TODO: Add more row/column summarization generics/methods
###

