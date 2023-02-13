\name{mpi.bcast.Robj}
\title{Extensions of MPI\_Bcast API}
\alias{mpi.bcast.Robj}
\alias{mpi.bcast.Robj2child}
\alias{mpi.bcast.Rfun2child}
\alias{mpi.bcast.data2child}
\description{
  \code{mpi.bcast.Robj} and \code{mpi.bcast.Robj2child} are used to move 
a general R object around among parent and all children. 
}
\usage{
mpi.bcast.Robj(obj = NULL, rank = 0, comm = 1)
mpi.bcast.Robj2child(obj, comm = 1, all = FALSE)
mpi.bcast.Rfun2child(comm = 1)
mpi.bcast.data2child(obj, comm = 1, buffunit = 100)
}
\arguments{
  \item{obj}{an R object to be transmitted from the sender}
  \item{rank}{the sender.}
  \item{comm}{a communicator number.}
  \item{all}{a logical. If TRUE, all R objects on parent are transmitted to children.}
  \item{buffunit}{a buffer unit number.}
}
\value{
  \code{mpi.bcast.Robj} returns no value for the sender and the 
transmitted one for others. \code{mpi.bcast.Robj2child} returns no value for the parent 
and the transmitted R object along its name on children. \code{mpi.bcast.Rfun2child} 
transmits all parent's functions to children and returns no value. \code{mpi.bcast.data2child}
transmits a double vector or a matrix to children and returns no value.
} 
\details{
  \code{mpi.bcast.Robj} is an extension of \code{\link{mpi.bcast}} for 
  moving a general R object around from a sender to everyone. 
  \code{mpi.bcast.Robj2child} does an R object transmission from 
  parent to all children unless all=TRUE in which case, all parent's objects with 
  the global enviroment are transmitted to all childrs.  
  
  \code{mpi.bcast.data2child} transfers data (a double vector or a matrix) 
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
