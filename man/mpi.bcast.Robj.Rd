\name{mpi.bcast.Robj}
\title{Extensions of MPI\_Bcast API}
\alias{mpi.bcast.Robj}
\alias{mpi.bcast.Robj2worker}
\alias{mpi.bcast.Rfun2worker}
\alias{mpi.bcast.data2worker}
\description{
  \code{mpi.bcast.Robj} and \code{mpi.bcast.Robj2worker} are used to move 
a general R object around among manager and all workers. 
}
\usage{
mpi.bcast.Robj(obj = NULL, rank = 0, comm = 1)
mpi.bcast.Robj2worker(obj, comm = 1, all = FALSE)
mpi.bcast.Rfun2worker(comm = 1)
mpi.bcast.data2worker(obj, comm = 1, buffunit = 100)
}
\arguments{
  \item{obj}{an R object to be transmitted from the sender}
  \item{rank}{the sender.}
  \item{comm}{a communicator number.}
  \item{all}{a logical. If TRUE, all R objects on manager are transmitted to workers.}
  \item{buffunit}{a buffer unit number.}
}
\value{
  \code{mpi.bcast.Robj} returns no value for the sender and the 
transmitted one for others. \code{mpi.bcast.Robj2worker} returns no value for the manager 
and the transmitted R object along its name on workers. \code{mpi.bcast.Rfun2worker} 
transmits all manager's functions to workers and returns no value. \code{mpi.bcast.data2worker}
transmits a double vector or a matrix to workers and returns no value.
} 
\details{
  \code{mpi.bcast.Robj} is an extension of \code{\link{mpi.bcast}} for 
  moving a general R object around from a sender to everyone. 
  \code{mpi.bcast.Robj2worker} does an R object transmission from 
  manager to all workers unless all=TRUE in which case, all manager's objects with 
  the global enviroment are transmitted to all workerrs.  
  
  \code{mpi.bcast.data2worker} transfers data (a double vector or a matrix) 
  natively without (un)serilization. It should be used with a huge vector or matrix.
  It results less memory usage and faster transmission. Notice that data with 
  missing values (NA) are allowed.
}
\author{
Hao Yu
}
\seealso{
  \code{\link{mpi.send.Robj}},
  \code{\link{mpi.recv.Robj}},
}
%\examples{
%}
\keyword{utilities}
