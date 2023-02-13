simple.pi <- function (n, comm=1) 
{
	#let children ready receive n
	mpi.bcast.cmd(n <- mpi.bcast(integer(1), type=1, comm=.comm), comm=comm)  
	
	#send n to all children
	mpi.bcast(as.integer(n), type=1, comm=comm) 
	
	#let each child find its own id(rank) and total number of children
	mpi.bcast.cmd(id <- mpi.comm.rank(.comm), comm=comm)
	mpi.bcast.cmd(nchildren <- mpi.comm.size(.comm)-1, comm=comm)

	#let each child compute corresponding PI value
	mpi.bcast.cmd(mypi <- 
		4*sum(1/(1+((seq(id,n,nchildren)-.5)/n)^2))/n, comm=comm)
	#send computed values to parent and sum together
	mpi.bcast.cmd(mpi.reduce(mypi,comm=.comm), comm=comm)

	#parent must sum together as well so it adds 0 to it
	mpi.reduce(0,comm=comm)
}
