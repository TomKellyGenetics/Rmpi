cworkerPI <- function (n)
{
	if (mpi.comm.size(1) > 1)
		stop ("It seems some workers running on comm 1.")
        mpi.comm.spawn("cworkerPI")
	mpi.intercomm.merge(2,0,1)
        mpi.bcast(as.integer(n),1)
        out <-mpi.reduce(0)
        mpi.comm.free()
        out
}

