#setAutoRealizationBackend("RleArray")
#setAutoRealizationBackend("HDF5Array")

ARITH_OPS   <- c("+", "-", "*", "/", "^", "%%", "%/%")
COMPARE_OPS <- c("==", "!=", "<=", ">=", "<", ">")
LOGIC_OPS   <- c("&", "|")  # currently untested

### Toy integer 3D SVT_SparseArray.
.make_toy_svt1 <- function()
{
    dim1 <- c(5L, 10L, 3L)
    svt1 <- SVT_SparseArray(dim=dim1)
    nzvals1 <- 24:-9
    nzvals1[2:3] <- NA
    nzvals1[4:5] <- 0L
    set.seed(123)
    svt1[sample(length(svt1), length(nzvals1))] <- nzvals1
    svt1
}

### Toy integer 3D array with no zeros or NAs.
.make_toy_a1b <- function()
{
    dim1b <- c(5L, 10L, 3L)
    set.seed(123)
    array(sample(10L, prod(dim1b), replace=TRUE), dim1b)
}

### Toy numeric 3D array with one NA and plenty of zeros, Inf's, -Inf's,
### and NaN's.
.make_toy_a2 <- function()
{
    a1b <- .make_toy_a1b()
    a2 <- 2:-2 / (a1b - 5)
    a2[2, 9, 2] <- NA  # same as a2[[92]] <- NA
    a2
}

### Toy character 3D SVT_SparseArray.
.make_toy_svt3 <- function()
{
    svt3 <- .make_toy_svt1()
    nzvals3 <- nzvals(svt3)
    nzvals3 <- paste0(nzvals3, "aXb")
    nzvals3[2:3] <- NA
    nzvals3[4:5] <- ""
    nzvals(svt3) <- nzvals3
    svt3
}

.BLOCK_SIZES1 <- c(12L, 20L, 50L, 15000L)
.BLOCK_SIZES2 <- 2L * .BLOCK_SIZES1

test_DelayedArray_unary_iso_ops <- function()
{
    do_tests <- function(.Generic, a, A) {
        GENERIC <- match.fun(.Generic)
        current <- GENERIC(A)
        checkTrue(is(current, "DelayedArray"))
        checkIdentical(dim(a), dim(current))
        checkIdentical(GENERIC(a), as.array(current))
    }

    a1 <- as.array(.make_toy_svt1())  # integer 3D array
    A1 <- DelayedArray(realize(a1))
    a2 <- .make_toy_a2()  # numeric 3D array
    A2 <- DelayedArray(realize(a2))
    for (.Generic in c("is.na", "is.finite", "is.infinite", "is.nan")) {
        do_tests(.Generic, a1, A1)
        do_tests(.Generic, a2, A2)
    }

    a3 <- as.array(.make_toy_svt3())  # character 3D array
    A3 <- DelayedArray(realize(a3))
    for (.Generic in c("nchar", "tolower", "toupper")) {
        do_tests(.Generic, a3, A3)
    }
}

test_DelayedArray_Math_ans_Arith <- function()
{
    toto1 <- function(a) { 100 / floor(abs((5 * log(a + 0.2) - 1)^3)) }
    toto2 <- function(a) { 100L + (5L * (a - 2L)) %% 7L }
    do_tests <- function(a, A) {
        current1 <- toto1(A)
        checkTrue(is(current1, "DelayedArray"))
        checkIdentical(dim(a), dim(current1))
        ## Supress "NaNs produced" warnings.
        target1 <- suppressWarnings(toto1(a))
        checkIdentical(target1, suppressWarnings(as.array(current1)))

        current2 <- toto2(A)
        checkTrue(is(current2, "DelayedArray"))
        checkIdentical(dim(a), dim(current2))
        target2 <- toto2(a)
        checkIdentical(target2, as.array(current2))

        current <- toto2(current1)
        checkTrue(is(current, "DelayedArray"))
        checkIdentical(dim(a), dim(current))
        ## Supress "NaNs produced" warnings.
        checkIdentical(toto2(target1), suppressWarnings(as.array(current)))

        current <- toto1(current2)
        checkTrue(is(current, "DelayedArray"))
        checkIdentical(dim(a), dim(current))
        checkIdentical(toto1(target2), as.array(current))
    }

    a1 <- as.array(.make_toy_svt1())  # integer 3D array
    A1 <- DelayedArray(realize(a1))
    a2 <- .make_toy_a2()  # numeric 3D array
    A2 <- DelayedArray(realize(a2))
    do_tests(a1, A1)
    do_tests(a2, A2)

    a <- a1[ , 10:4, -2]
    A <- A1[ , 10:4, -2]
    do_tests(a, A)
    do_tests(a, DelayedArray(realize(A)))
    a <- a2[ , 10:4, -2]
    A <- A2[ , 10:4, -2]
    do_tests(a, A)
    do_tests(a, DelayedArray(realize(A)))

    ## with a numeric matrix
    m <- a2[ , , 2]
    M <- A2[ , , 2]
    do_tests(m, M)
    do_tests(m, DelayedArray(realize(M)))
    do_tests(t(m), t(M))
    do_tests(t(m), DelayedArray(realize(t(M))))
    checkIdentical(t(toto1(m)), as.matrix(t(toto1(M))))
}

test_DelayedArray_Ops_with_left_or_right_vector <- function()
{
    do_tests <- function(.Generic, a, A, m, M) {
        on.exit(suppressMessages(setAutoBlockSize()))
        GENERIC <- match.fun(.Generic)

        target_current <- list(
            list(GENERIC(a, m[ , 1]), GENERIC(A, M[ , 1])),
            list(GENERIC(m[ , 1], a), GENERIC(M[ , 1], A)),

            list(GENERIC(a, a[ , 1, 1]), GENERIC(A, A[ , 1, 1])),
            list(GENERIC(a[ , 1, 1], a), GENERIC(A[ , 1, 1], A)),

            list(GENERIC(a, a[[1]]), GENERIC(A, A[[1]])),
            list(GENERIC(a[[1]], a), GENERIC(A[[1]], A))
        )
        for (i in seq_along(target_current)) {
            target <- target_current[[i]][[1L]]
            current <- target_current[[i]][[2L]]
            checkIdentical(target, as.array(current))
            checkIdentical(target[5:3, , -2], as.array(current[5:3, , -2]))
            checkIdentical(target[0, 0, 0], as.array(current[0, 0, 0]))
            checkIdentical(target[0, 0, -2], as.array(current[0, 0, -2]))
            checkIdentical(target[ , 0, ], as.array(current[ , 0, ]))
            for (block_size in .BLOCK_SIZES2) {
                suppressMessages(setAutoBlockSize(block_size))
                checkEquals(sum(target, na.rm=TRUE), sum(current, na.rm=TRUE))
            }
        }
    }

    a2 <- .make_toy_a2()  # numeric 3D array
    A2 <- DelayedArray(realize(a2))
    m <- a2[ , , 2]
    M <- DelayedArray(realize(m))
    for (.Generic in c(ARITH_OPS, COMPARE_OPS))
        do_tests(.Generic, a2, A2, m, M)

    a1 <- as.array(.make_toy_svt1())  # integer 3D array
    a <- a1 >= 1L  # logical 3D array
    A <- DelayedArray(realize(a))
    m <- a[ , , 2]
    M <- DelayedArray(realize(m))
    for (.Generic in LOGIC_OPS)
        do_tests(.Generic, a2, A2, m, M)
}

test_DelayedArray_Ops_with_conformable_args <- function()
{
    a1 <- as.array(.make_toy_svt1())  # integer 3D array
    A1 <- DelayedArray(realize(a1))
    a2 <- .make_toy_a2()  # numeric 3D array
    A2 <- DelayedArray(realize(a2))
    a3 <- array(sample(5L, 150, replace=TRUE), c(5, 10, 3))
    a3[2, 9, 2] <- NA  # same as a3[[92]] <- NA
    A3 <- DelayedArray(realize(a3))
    for (.Generic in c(ARITH_OPS, COMPARE_OPS)) {
        GENERIC <- match.fun(.Generic)
        target1 <- GENERIC(a1, a2)
        target2 <- GENERIC(a2, a1)
        target3 <- GENERIC(a1, a3)
        checkIdentical(target1, as.array(GENERIC(A1, A2)))
        checkIdentical(target2, as.array(GENERIC(A2, A1)))
        checkIdentical(target3, as.array(GENERIC(A1, A3)))
    }

    x <- a1 >= 6L  # logical 3D array
    X <- DelayedArray(realize(x))
    y <- a1 >= 1L & a1 <= 10L  # logical 3D array
    Y <- DelayedArray(realize(y))
    for (.Generic in LOGIC_OPS) {
        GENERIC <- match.fun(.Generic)
        target <- GENERIC(x, y)
        checkIdentical(target, as.array(GENERIC(X, Y)))
        checkIdentical(target, as.array(GENERIC(Y, X)))
    }
}

test_DelayedMatrix_Ops <- function()
{
    test_delayed_Ops_on_matrix <- function(.Generic, m, M) {
        GENERIC <- match.fun(.Generic)

        target_current <- list(
            list(GENERIC(m, m[ , 1]), GENERIC(M, M[ , 1])),
            list(GENERIC(m[ , 2], m), GENERIC(M[ , 2], M))
        )
        for (i in seq_along(target_current)) {
            target <- target_current[[i]][[1L]]
            current <- target_current[[i]][[2L]]
            checkIdentical(target, as.matrix(current))
            checkIdentical(t(target), as.matrix(t(current)))
            checkIdentical(target[-2, 8:5], as.matrix(current[-2, 8:5]))
            checkIdentical(t(target[-2, 8:5]), as.matrix(t(current[-2, 8:5])))
            checkIdentical(target[-2, 0], as.matrix(current[-2, 0]))
            checkIdentical(t(target[-2, 0]), as.matrix(t(current[-2, 0])))
            checkIdentical(target[0, ], as.matrix(current[0, ]))
            checkIdentical(t(target[0, ]), as.matrix(t(current[0, ])))
        }

        target_current <- list(
            list(GENERIC(t(m), 8:-1), GENERIC(t(M), 8:-1)),
            list(GENERIC(8:-1, t(m)), GENERIC(8:-1, t(M))),

            list(GENERIC(t(m), m[1 , ]), GENERIC(t(M), M[1 , ])),
            list(GENERIC(m[2 , ], t(m)), GENERIC(M[2 , ], t(M))),

            list(GENERIC(t(m), m[1 , 6:10]), GENERIC(t(M), M[1 , 6:10])),
            list(GENERIC(m[2 , 8:7], t(m)), GENERIC(M[2 , 8:7], t(M)))
        )
        for (i in seq_along(target_current)) {
            target <- target_current[[i]][[1L]]
            current <- target_current[[i]][[2L]]
            checkIdentical(target, as.matrix(current))
            checkIdentical(target[1:3 , ], as.matrix(current[1:3 , ]))
            checkIdentical(target[ , 1:3], as.matrix(current[ , 1:3]))
            checkIdentical(t(target), as.matrix(t(current)))
            checkIdentical(t(target)[1:3 , ], as.matrix(t(current)[1:3 , ]))
            checkIdentical(t(target)[ , 1:3], as.matrix(t(current)[ , 1:3]))
            checkIdentical(target[8:5, -2], as.matrix(current[8:5, -2]))
            checkIdentical(t(target[8:5, -2]), as.matrix(t(current[8:5, -2])))
            checkIdentical(target[0, -2], as.matrix(current[0, -2]))
            checkIdentical(t(target[0, -2]), as.matrix(t(current[0, -2])))
            checkIdentical(target[ , 0], as.matrix(current[ , 0]))
            checkIdentical(t(target[ , 0]), as.matrix(t(current[ , 0])))
        }
    }

    a <- array(sample(5L, 150, replace=TRUE), c(5, 10, 3))  # integer array
    a <- a + runif(150) - 0.5                               # numeric array
    a[2, 9, 2] <- NA  # same as a[[92]] <- NA

    toto <- function(x) t((5 * x[ , 1:2] ^ 3 + 1L) * log(x)[, 10:9])[ , -1]

    m <- a[ , , 2]
    M <- DelayedArray(realize(m))
    checkIdentical(toto(m), as.array(toto(M)))
    ## Logic ops currently untested.
    for (.Generic in c(ARITH_OPS, COMPARE_OPS))
        test_delayed_Ops_on_matrix(.Generic, m, M)

    M <- DelayedArray(realize(a))[ , , 2]
    checkIdentical(toto(m), as.array(toto(M)))
    ## Logic ops currently untested.
    for (.Generic in c(ARITH_OPS, COMPARE_OPS))
        test_delayed_Ops_on_matrix(.Generic, m, M)
}

test_DelayedArray_anyNA <- function()
{
    on.exit(suppressMessages(setAutoBlockSize()))
    BLOCK_anyNA <- DelayedArray:::.BLOCK_anyNA

    a1 <- as.array(.make_toy_svt1())   # integer 3D array
    A1 <- DelayedArray(realize(a1))
    a1b <- .make_toy_a1b()  # integer 3D array with no zeros or NAs
    A1b <- DelayedArray(realize(a1b))

    for (block_size in .BLOCK_SIZES1) {
        suppressMessages(setAutoBlockSize(block_size))
        checkIdentical(TRUE, anyNA(A1))
        checkIdentical(TRUE, BLOCK_anyNA(a1))
        checkIdentical(FALSE, anyNA(A1b))
        checkIdentical(FALSE, BLOCK_anyNA(a1b))
    }
}

test_DelayedArray_which <- function()
{
    do_which_tests <- function(svt, block_sizes) {
        on.exit(suppressMessages(setAutoBlockSize()))
        BLOCK_which <- DelayedArray:::BLOCK_which
        a <- as.array(svt)
        A <- DelayedArray(realize(a))
        B <- DelayedArray(realize(svt))
        target1 <- which(a)
        target2 <- which(a, arr.ind=TRUE, useNames=FALSE)
        ## TODO: Uncomment 4 tests below when which(<SVT_SparseArray>)
        ## is ready.
        for (block_size in block_sizes) {
            suppressMessages(setAutoBlockSize(block_size))
            checkIdentical(target1, which(A))
            #checkIdentical(target1, which(B))
            checkIdentical(target1, BLOCK_which(a))
            #checkIdentical(target1, BLOCK_which(svt))
            checkIdentical(target2, which(A, arr.ind=TRUE))
            #checkIdentical(target2, which(B, arr.ind=TRUE))
            checkIdentical(target2, BLOCK_which(a, arr.ind=TRUE))
            #checkIdentical(target2, BLOCK_which(svt, arr.ind=TRUE))
        }
    }

    # logical 3D array
    svt <- .make_toy_svt1() >= 1L
    do_which_tests(svt, .BLOCK_SIZES1)

    # logical 3D array with only FALSE/NA values
    svt <- .make_toy_svt1() == -100L
    do_which_tests(svt, .BLOCK_SIZES1)
}

test_DelayedArray_nzwhich <- function()
{
    do_nzwhich_tests <- function(svt, block_sizes) {
        on.exit(suppressMessages(setAutoBlockSize()))
        BLOCK_nzwhich <- DelayedArray:::BLOCK_nzwhich
        a <- as.array(svt)
        A <- DelayedArray(realize(a))
        B <- DelayedArray(realize(svt))
        target1 <- nzwhich(svt)
        target2 <- nzwhich(svt, arr.ind=TRUE)
        for (block_size in block_sizes) {
            suppressMessages(setAutoBlockSize(block_size))
            checkIdentical(target1, nzwhich(A))
            checkIdentical(target1, nzwhich(B))
            checkIdentical(target1, BLOCK_nzwhich(a))
            checkIdentical(target1, BLOCK_nzwhich(svt))
            checkIdentical(target2, nzwhich(A, arr.ind=TRUE))
            checkIdentical(target2, nzwhich(B, arr.ind=TRUE))
            checkIdentical(target2, BLOCK_nzwhich(a, arr.ind=TRUE))
            checkIdentical(target2, BLOCK_nzwhich(svt, arr.ind=TRUE))
        }
    }

    # integer 3D array
    svt1 <- .make_toy_svt1()
    do_nzwhich_tests(svt1, .BLOCK_SIZES1)

    # logical 3D array
    do_nzwhich_tests(svt1 >= 1L, .BLOCK_SIZES1)
}

test_DelayedArray_Summary <- function()
{
    do_tests <- function(.Generic, a, block_sizes, checkFun) {
        on.exit(suppressMessages(setAutoBlockSize()))
        BLOCK_Summary <- DelayedArray:::.BLOCK_Summary

        GENERIC <- match.fun(.Generic)
        target1 <- GENERIC(a)
        target2 <- GENERIC(a, na.rm=TRUE)
        A <- DelayedArray(realize(a))
        for (block_size in block_sizes) {
            suppressMessages(setAutoBlockSize(block_size))
            checkFun(target1, GENERIC(A))
            checkFun(target1, BLOCK_Summary(.Generic, a))
            checkFun(target1, GENERIC(aperm(A)))
            checkFun(target2, GENERIC(A, na.rm=TRUE))
            checkFun(target2, BLOCK_Summary(.Generic, a, na.rm=TRUE))
            checkFun(target2, GENERIC(aperm(A), na.rm=TRUE))
        }
    }

    a1 <- as.array(.make_toy_svt1())   # integer 3D array
    a1b <- .make_toy_a1b()  # integer 3D array with no zeros or NAs
    a2 <- .make_toy_a2()  # numeric 3D array
    for (.Generic in c("max", "min", "range")) {
        do_tests(.Generic, a1, .BLOCK_SIZES1, checkIdentical)
        do_tests(.Generic, a1b, .BLOCK_SIZES1, checkIdentical)
        do_tests(.Generic, a1b - 0.5, .BLOCK_SIZES2, checkIdentical)
        do_tests(.Generic, a2, .BLOCK_SIZES2, checkIdentical)
    }
    for (.Generic in c("sum", "prod")) {
        do_tests(.Generic, a1, .BLOCK_SIZES1, checkIdentical)
        do_tests(.Generic, a1b, .BLOCK_SIZES1, checkEquals)
        do_tests(.Generic, a1b - 0.5, .BLOCK_SIZES2, checkEquals)
        do_tests(.Generic, a2, .BLOCK_SIZES2, checkEquals)
    }

    ## on a logical array
    a <- array(c(rep(NA, 62), rep(TRUE, 87), FALSE), c(5, 10, 3))
    for (.Generic in c("any", "all"))
        do_tests(.Generic, a, .BLOCK_SIZES1, checkIdentical)
}

test_DelayedArray_mean <- function()
{
    do_tests <- function(a, block_sizes) {
        on.exit(suppressMessages(setAutoBlockSize()))
        BLOCK_mean <- DelayedArray:::.BLOCK_mean

        target1 <- mean(a)
        target2 <- mean(a, na.rm=TRUE)
        target3 <- mean(a[ , 10:4, -2])
        A <- DelayedArray(realize(a))
        for (block_size in block_sizes) {
            suppressMessages(setAutoBlockSize(block_size))
            checkEquals(target1, mean(A))
            checkEquals(target1, BLOCK_mean(a))
            checkEquals(target1, mean(aperm(A)))
            checkEquals(target2, mean(A, na.rm=TRUE))
            checkEquals(target2, BLOCK_mean(a, na.rm=TRUE))
            checkEquals(target2, mean(aperm(A), na.rm=TRUE))
            checkEquals(target3, mean(A[ , 10:4, -2]))
            checkEquals(target3, BLOCK_mean(a[ , 10:4, -2]))
            checkEquals(target3, mean(aperm(A[ , 10:4, -2])))
        }
    }

    a1 <- as.array(.make_toy_svt1())   # integer 3D array
    a1b <- .make_toy_a1b()  # integer 3D array with no zeros or NAs
    a2 <- .make_toy_a2()  # numeric 3D array
    do_tests(a1, .BLOCK_SIZES1)
    do_tests(a1b, .BLOCK_SIZES1)
    do_tests(a1b - 0.5, .BLOCK_SIZES2)
    do_tests(a2, .BLOCK_SIZES2)
}

test_DelayedArray_apply <- function()
{
    do_tests <- function(a) {
        A <- DelayedArray(realize(a))
        for (MARGIN in seq_along(dim(a))) {
            checkIdentical(apply(a, MARGIN, dim),
                           apply(A, MARGIN, dim))
            checkIdentical(apply(a, MARGIN, sum),
                           apply(A, MARGIN, sum))
            checkIdentical(apply(a, MARGIN, sum, na.rm=TRUE),
                           apply(A, MARGIN, sum, na.rm=TRUE))
            ## row/colSums and row/colMeans don't work yet in that case.
            if (dim(A)[[MARGIN]] == 0L && length(dim(A)) >= 3L)
                next
            checkIdentical(apply(a, MARGIN, rowSums),
                           apply(A, MARGIN, rowSums))
            checkIdentical(apply(a, MARGIN, rowSums, na.rm=TRUE),
                           apply(A, MARGIN, rowSums, na.rm=TRUE))
            checkIdentical(apply(a, MARGIN, colMeans),
                           apply(A, MARGIN, colMeans))
            checkIdentical(apply(a, MARGIN, colMeans, na.rm=TRUE),
                           apply(A, MARGIN, colMeans, na.rm=TRUE))
        }
    }

    a1b <- .make_toy_a1b()  # integer 3D array with no zeros or NAs
    do_tests(a1b)
    do_tests(a1b[ , , 0])

    dimnames(a1b) <- list(NULL, NULL, LETTERS[1:3])
    do_tests(a1b)
    do_tests(a1b[ , , 0])

    dimnames(a1b) <- list(NULL, letters[1:10], LETTERS[1:3])
    do_tests(a1b)
    do_tests(a1b[ , , 0])
}

test_DelayedArray_scale <- function()
{
    scaled_DelayedMatrix_as_matrix <- function(x) {
        ans <- as.matrix(x)
        attr(ans, "scaled:center") <- attr(x, "scaled:center")
        attr(ans, "scaled:scale") <- attr(x, "scaled:scale")
        ans
    }
    check_scaled_DelayedMatrix <- function(target, current) {
        checkTrue(is(current, "DelayedMatrix"))
        checkIdentical(dim(target), dim(current))
        #checkTrue("scaled:center" %in% names(attributes(current)))
        #checkTrue("scaled:scale" %in% names(attributes(current)))
        checkEquals(attr(target, "scaled:center"),
                    attr(current, "scaled:center"))
        checkEquals(attr(target, "scaled:scale"),
                    attr(current, "scaled:scale"))
        checkEquals(target, scaled_DelayedMatrix_as_matrix(current))
    }

    m <- matrix(-9:70, ncol=8, dimnames=list(LETTERS[1:10], letters[1:8]))
    m[1, 1] <- NA
    m[2, 2] <- NaN
    m[3, 3] <- Inf
    m[4, 4] <- -Inf
    m[5:6, 5] <- c(NaN, Inf)
    m[6:8, 6] <- c(NaN, NA, -Inf)
    M <- DelayedArray(realize(m))

    ## 'center' is TRUE:
    target <- scale(m)
    current <- scale(M)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, scale=FALSE)
    current <- scale(M, scale=FALSE)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, scale=-4:3)
    current <- scale(M, scale=-4:3)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, scale=c(NaN, -Inf, -2:1, Inf, NA))
    current <- scale(M, scale=c(NaN, -Inf, -2:1, Inf, NA))
    check_scaled_DelayedMatrix(target, current)

    ## 'center' is FALSE:
    target <- scale(m, center=FALSE)
    current <- scale(M, center=FALSE)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, center=FALSE, scale=FALSE)
    current <- scale(M, center=FALSE, scale=FALSE)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, center=FALSE, scale=-4:3)
    current <- scale(M, center=FALSE, scale=-4:3)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, center=FALSE, scale=c(NaN, -Inf, -2:1, Inf, NA))
    current <- scale(M, center=FALSE, scale=c(NaN, -Inf, -2:1, Inf, NA))
    check_scaled_DelayedMatrix(target, current)

    ## 'center' is numeric:
    target <- scale(m, center=5:-2)
    current <- scale(M, center=5:-2)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, center=5:-2, scale=FALSE)
    current <- scale(M, center=5:-2, scale=FALSE)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, center=5:-2, scale=-4:3)
    current <- scale(M, center=5:-2, scale=-4:3)
    check_scaled_DelayedMatrix(target, current)

    target <- scale(m, center=5:-2, scale=c(NaN, -Inf, -2:1, Inf, NA))
    current <- scale(M, center=5:-2, scale=c(NaN, -Inf, -2:1, Inf, NA))
    check_scaled_DelayedMatrix(target, current)
}

