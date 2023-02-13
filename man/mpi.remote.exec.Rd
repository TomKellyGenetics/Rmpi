\name{mpi.remote.exec}
\alias{mpi.remote.exec}
\title{Remote Executions on R children}
\description{
 Remotely execute a command on R children spawned by using childdaemon.R script 
 and return all executed results back to parent. 
} 
\usage{
mpi.remote.exec(cmd, ..., simplify = TRUE, comm = 1, ret = TRUE)  
}
\arguments{
  \item{cmd}{the command to be executed on R children}
  \item{...}{used as arguments to cmd (function command) for passing their 
  (parent) values to R children, i.e., if `myfun(x)' will be executed on R children 
with `x' as parent variable, use mpi.remote.exec(cmd=myfun, x).}
  \item{simplify}{logical; should the result be simplified to a data.frame if possible?}
  \item{comm}{a communicator number.}
  \item{ret}{return executed results from R children if TRUE.}
}
\value{
  return executed results from R children if the argument \code{ret} is 
  set to be TRUE. The value could be a data.frame if values 
  (integer or double) from each child have the same dimension. 
  Otherwise a list is returned.}
\details{
  Once R children are spawned by \code{\link{mpi.spawn.Rchildren}} with the 
  childdaemon.R script, they are waiting for instructions from parent. One can 
  use \code{\link{mpi.bcast.cmd}} to send a command to R children. However it 
  will not return executed results. Hence \code{mpi.remote.exec} can be 
  considered an extension to \code{\link{mpi.bcast.cmd}}.    
}

\section{Warning}{
  \code{mpi.remote.exec} may have difficult guessing invisible results 
  on R children. Use \code{ret = FALSE} instead. 
} 
\seealso{
  \code{\link{mpi.spawn.Rchildren}},
  \code{\link{mpi.bcast.cmd}}
}
\author{
Hao Yu
}
\examples{
\donttest{
mpi.remote.exec(mpi.comm.rank())
 x=5
mpi.remote.exec(rnorm,x)
}
}

\keyword{utilities}
