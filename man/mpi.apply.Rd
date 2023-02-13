\name{mpi.apply}
\alias{mpi.apply}
\alias{mpi.iapply}
\title{Scatter an array to children and then apply a FUN}
\description{
An array (length <= total number of children) is scattered to children so that the first 
child calls \code{FUN} with arguments \code{x[[1]]} and \code{...}, the second one 
calls with arguments \code{x[[2]]} and \code{...}, and so on. \code{mpi.iapply} is a 
nonblocking version of \code{mpi.apply} so that it will not consume CPU on parent node.} 
\usage{
mpi.apply(X, FUN, ..., comm=1)  
mpi.iapply(X, FUN, ..., comm=1, sleep=0.01)  
}
\arguments{
  \item{X}{an array}
  \item{FUN}{a function}
  \item{...}{optional arguments to \code{FUN}}
  \item{comm}{a communicator number}
  \item{sleep}{a sleep interval on parent node (in sec)}
}
\value{
A list of the results is returned. Its length is the same as that of \code{x}. In 
case the call \code{FUN} with arguments \code{x[[i]]} and \code{...} fails on ith 
child, corresponding error message will be returned in the returning list.  }

%\seealso{
%  \code{\link{mpi.spawn.Rchildren}},
%  \code{\link{mpi.bcast.cmd}}
%  \code{\link{mpi.bcast.Robj2child}}
%}
\author{
Hao Yu
}
\examples{
\donttest{
#Assume that there are at least 5 children running
#Otherwise run mpi.spawn.Rchildren(nchildren=5)
x=c(10,20)
mpi.apply(x,runif)
meanx=1:5
mpi.apply(meanx,rnorm,n=2,sd=4)
}
}
\keyword{utilities}
