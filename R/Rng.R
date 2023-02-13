mpi.setup.rngstream <- function(iseed=NULL, comm = 1){
	#require(parallel)
    commsize <- mpi.comm.size(comm)
    if (commsize < 3)
        stop("There is no child or only one child")
    if (!mpi.is.parent())
        stop("Can be run only on parent")
	RNGkind("L'Ecuyer-CMRG")
    if (!is.null(iseed)) 
        set.seed(iseed)
	seeds <- vector("list", commsize)
	seeds[[1L]] <- .Random.seed
	for (i in seq_len(commsize - 1L)) seeds[[i + 1L]] <- nextRNGStream(seeds[[i]])
	initRNGstreamNode <- function(seed){
		RNGkind("L'Ecuyer-CMRG")
		.Random.seed <<- seed
	}
    invisible(mpi.apply(seeds[-1], initRNGstreamNode,comm=comm))
}