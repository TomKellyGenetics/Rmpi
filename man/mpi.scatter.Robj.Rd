\name{mpi.scatter.Robj}
\title{Extensions of MPI\_ SCATTER and MPI\_SCATTERV }
\alias{mpi.scatter.Robj}
\alias{mpi.scatter.Robj2child}
\description{
  \code{mpi.scatter.Robj} and \code{mpi.scatter.Robj2child} are used to scatter a list 
   to all members. They are more efficient than using any parallel apply functions.
}
\usage{
mpi.scatter.Robj(obj = NULL, root = 0, comm = 1)
mpi.scatter.Robj2child(obj, comm = 1)
}
\arguments{
  \item{obj}{a list object to be scattered from the root or parent}
  \item{root}{rank of the scatter.}
  \item{comm}{a communicator number.}
}
\value{
  \code{mpi.scatter.Robj} for non-root members,  returns the
     scattered R object. For the root  member, it returns the
     portion belonging to itself. \code{mpi.scatter.Robj2child} returns no value
     for the parent and all children get their corresponding components in the list,
     i.e., the first child gets the first component in the list. 
}
\details{
  \code{mpi.scatter.Robj} is an extension of \code{\link{mpi.scatter}} for
  scattering a list object from a sender (root) to everyone. \code{mpi.scatter.Robj2child}
  scatters a list to all children.
}
\author{
Hao Yu  and   Wei Xia
}
\seealso{
  \code{\link{mpi.scatter}},
  \code{\link{mpi.gather.Robj}},
}
\examples{
\donttest{
#assume that there are three children running
mpi.bcast.cmd(x<-mpi.scatter.Robj())

xx <- list("parent",rnorm(3),letters[2],1:10)
mpi.scatter.Robj(obj=xx)

mpi.remote.exec(x)

#scatter a matrix to children
dat=matrix(1:24,ncol=3)
splitmatrix = function(x, ncl) lapply(.splitIndices(nrow(x), ncl), function(i) x[i,])
dat2=splitmatrix(dat,3)
mpi.scatter.Robj2child(dat2)
mpi.remote.exec(dat2)
}
}
\keyword{utilities}

