child2 <- function (){
    n <- mpi.bcast(integer(1),type=1,rank=0,comm=.comm)
    request <-1
    job <-2
    anytag <- mpi.any.tag()
    mypi <- 0
    while (1) {
    	#send parent a request
    	mpi.send(integer(1),type=1,dest=0,tag=request,comm=.comm)
    	jobrange<-mpi.recv(integer(2),type=1,source=0,tag=anytag,comm=.comm)
	tag <- mpi.get.sourcetag()[2]
	if (tag==job)
	    mypi <- 4*sum(1/(1+((seq(jobrange[1],jobrange[2])-.5)/n)^2))/n +mypi
	else 
	    break #tag=0 means stop
    }
    mpi.reduce(mypi,comm=.comm)
}

parent2PI <- function (n,maxjoblen,comm=1) 
{
    tsize <- mpi.comm.size(comm)
    if (tsize < 2)
	stop("Need at least 1 child")
    #send the function child2 to all children
    mpi.bcast.Robj2child(child2, comm=comm)

    #let child run the function child2
    mpi.bcast.cmd(child2(), comm=comm)

    #send n to all children	
    mpi.bcast(as.integer(n),type=1,comm=comm)
    
    count <- 0
    request <- 1
    job <- 2
    anysrc <- mpi.any.source()
    while (count < n){
	mpi.recv(integer(1), type=1, source=anysrc, tag=request, comm=comm)
    	src <- mpi.get.sourcetag()[1]
	jobrange <- c(count+1, min(count+maxjoblen, n))
	mpi.send(as.integer(jobrange),type=1,dest=src,tag=job,comm=comm)
	count <- count+maxjoblen
    }
    #tell workers to stop with tag=0
    for (i in 1:(tsize-1)){
	mpi.recv(integer(1),type=1,source=anysrc,tag=request,comm=comm)
    	src <- mpi.get.sourcetag()[1]
	mpi.send(integer(1),type=1,dest=src,tag=0,comm=comm)
    }
    mpi.reduce(0, comm=comm)
}
