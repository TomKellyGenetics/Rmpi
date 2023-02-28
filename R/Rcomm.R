### Copyright (C) 2002 Hao Yu 
mpi.barrier <- function(comm=1){
    .Call("mpi_barrier", as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.set.errhandler <- function(comm=1){
    .Call("mpi_comm_set_errhandler", as.integer(comm),
    PACKAGE = "Rmpi")
}

mpi.comm.test.inter <- function(comm=2){
    if(mpi.comm.is.null(comm))
        stop("NULL communicator")
    .Call("mpi_comm_test_inter",as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.rank <- function(comm=1){
    .Call("mpi_comm_rank", as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.size <- function(comm=1){
    if (.Call("mpi_comm_is_null", as.integer(comm),
        PACKAGE = "Rmpi")==1)
        0
    else 
        .Call("mpi_comm_size",as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.dup <- function(comm, newcomm){
        .Call("mpi_comm_dup", as.integer(comm), as.integer(newcomm),
    PACKAGE = "Rmpi")
}

mpi.comm.remote.size <- function(comm=2){
    .Call("mpi_comm_remote_size", as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.free <- function(comm=1){
    if (mpi.comm.size(comm)==0){
    tmp<-paste("It seems no members(children) associated with comm", comm)
    stop(tmp)
     }
     .Call("mpi_comm_free",as.integer(comm),PACKAGE = "Rmpi")
}

mpi.abort <- function(comm=1){
    if (mpi.comm.size(comm)==0){
    tmp<-paste("It seems no members(children) associated with comm", comm)
    stop(tmp)
     }
     .Call("mpi_abort",as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.disconnect <- function(comm=1){
    if (mpi.comm.size(comm)==0){
    tmp<-paste("It seems no members(children) associated with comm", comm)
    stop(tmp)
     }
     if (!is.loaded("mpi_comm_disconnect"))
        stop("MPI_Comm_disconnect is not supported.")
     .Call("mpi_comm_disconnect",as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.spawn <- function(child, 
            childarg=character(0), 
            nchildren=mpi.universe.size(),
            info=0,
            root=0, 
            intercomm=2,
			quiet=FALSE){
        if (!is.loaded("mpi_comm_spawn"))
            stop("MPI_Comm_spawn is not supported.")

    if (!is.character(child))
        stop("character argument (child) expected")
    #if (nchildren > mpi.universe.size()){
    #            tmp <- paste("Number of R children is over",
    #                    mpi.universe.size(),": maximum CPUs.")
    #            warning(tmp)
    #    }
    else if (nchildren <= 0)
        stop("Choose a positive number of children.")
    outs <- .Call("mpi_comm_spawn",
                as.character(child),
                as.character(childarg),
                as.integer(nchildren),
        as.integer(info),
        as.integer(root),
        as.integer(intercomm),
		as.integer(quiet),
		PACKAGE = "Rmpi")
    print("mpi_comm_spawn done")
    return(outs)
}

mpi.comm.get.parent <- function(comm=2){
    if (!is.loaded("mpi_comm_get_parent"))
        stop("MPI_Comm_get_parent is not supported.")
    .Call("mpi_comm_get_parent", as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.is.null <- function(comm){
    as.logical(.Call("mpi_comm_is_null", as.integer(comm),
    PACKAGE = "Rmpi"))
}

mpi.intercomm.merge <- function(intercomm=2,high=0,comm=1){
    .Call("mpi_intercomm_merge", as.integer(intercomm),
                     as.integer(high),
                     as.integer(comm),PACKAGE = "Rmpi")
}

mpi.comm.c2f <- function(comm=1){
    .Call("mpi_comm_c2f", as.integer(comm),PACKAGE = "Rmpi")
}
