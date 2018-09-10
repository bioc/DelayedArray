.onLoad <- function(libname, pkgname)
{
    options(DelayedArray.simplify=TRUE)
    options(DelayedArray.block.size=DEFAULT_BLOCK_SIZE)
    setDefaultBPPARAM()
}

.test <- function()
{
    setRealizationBackend("RleArray")
    BiocGenerics:::testPackage("DelayedArray")
}

