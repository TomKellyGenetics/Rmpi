\name{mpi.bcast.cmd}
\title{Extension of MPI\_Bcast API}
\alias{mpi.bcast.cmd}
\description{
  \code{mpi.bcast.cmd} is an extension of \code{\link{mpi.bcast}}. 
  It is mainly used to transmit a command from manager to all R workers 
  spawned by using workerdaemon.R script.
}
\usage{
mpi.bcast.cmd(cmd=NULL, ..., rank = 0, comm = 1, nonblock=FALSE, sleep=0.1)
}
\arguments{
  \item{cmd}{a command to be sent from manager.}
  \item{...}{used as arguments to cmd (function command) for passing their 
  (manager) values to R workers, i.e., if `myfun(x)' will be executed on R workers 
with `x' as manager variable, use mpi.bcast.cmd(cmd=myfun, x=x).}
  \item{rank}{the sender}
  \item{comm}{a communicator number}
  \item{nonblock}{logical. If TRUE, a nonblock procedure is used on all receivers so that
they will consume none or little CPUs while waiting.}
  \item{sleep}{a sleep interval, used when nonblock=TRUE. Smaller sleep is, more response receivers are, 
  more CPUs consume}
}
\value{
  \code{mpi.bcast.cmd} returns no value for the sender and an expression of the transmitted command for others.
} 
\details{
  \code{mpi.bcast.cmd} is a collective call. This means all members in a communicator must 
  execute it at the same time. If workers are spawned (created) by using workerdaemon.R (Rprofile script), 
  then they are running \code{mpi.bcast.cmd} in infinite loop (idle state). Hence manager can execute 
  \code{mpi.bcast.cmd} alone to start computation. On the manager, \code{cmd} and \code{...} are put together 
  as a list which is then broadcasted (after serialization) to all workers (using for loop with mpi.send 
  and mpi.recv pair). All workers will return an expression which will be evaluated by either workerdaemon.R, 
  or by whatever an R script based on workerdaemon.R.
  
  If nonblock=TRUE, then on receiving side, a nonblock procedure is used to check if 
  there is a message. If not, it will sleep for the specied amount and repeat itself.
  
  Please use \code{\link{mpi.remote.exec}} if you want the executed results returned from R 
  workers.
 }
\section{Warning}{
  Be caution to use \code{mpi.bcast.cmd} alone by manager in the middle of comptuation. Only all workers in idle 
  states (waiting instructions from manager) can it be used. Othewise it may result miscommunication
  with other MPI calls.
} 

\author{
Hao Yu
}
\seealso{
  \code{\link{mpi.remote.exec}}
}
%\examples{
%}
\keyword{utilities}
