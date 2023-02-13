library(Rmpi)
mpi.comm.get.parent(2)
if (!mpi.comm.is.null(2))
     mpi.intercomm.merge(2,1,1)

#parent job
parent <- function(){
	if (mpi.comm.size(1) > 0)
	    stop("There are some children running on comm 1")
	child<-system.file("Rchildren.sh",package="Rmpi")
	Rscript <- system.file("demo","parentchildPI.R",package="Rmpi")
 	tmp <- paste(Sys.getpid(), "+", 1, sep="")   
	arg <- c(Rscript, tmp, "nolog", R.home())
	mpi.comm.spawn(child=child,childarg=arg )
	mpi.intercomm.merge(2,0,1)
	out <- mpi.reduce(0)
        mpi.comm.free()
	out
}

#child jobs
child <- function(n){
	totalcpu <- mpi.comm.size(0)
	id <- mpi.comm.rank(0)+1
	mypi <- 4*sum(1/(1+((seq(id,n,totalcpu)-.5)/n)^2))/n
	mpi.reduce(mypi)
        mpi.comm.free()
	mpi.exit()
}

#real code
n<- 10000

if (mpi.is.parent()){
	parent()
} else {
	child(n)
}
