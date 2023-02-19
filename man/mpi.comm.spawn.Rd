\name{mpi.comm.spawn}
\alias{mpi.comm.spawn}
\title{MPI\_Comm\_spawn API }
\description{
\code{mpi.comm.spawn} tries to start \code{nworkers} identical copies of 
\code{workers}, establishing communication with them and returning an 
intercommunicator. The spawned workers are referred to as workers, and the 
process that spawned them is called the manager (manager). The workers have 
their own MPI\_COMM\_WORLD represented by comm 0. To make communication 
possible among manager and workers, all workers should use 
\code{\link{mpi.comm.get.manager}} to find their manager and use 
\code{\link{mpi.intercomm.merge}} to merger an intercomm to a comm.
 }
\usage{
 mpi.comm.spawn(worker, workerarg = character(0),
                nworkers = mpi.universe.size(), info = 0,
                root = 0, intercomm = 2, quiet = FALSE)
}
\arguments{
  \item{worker}{a file name to an executable program.}
  \item{workerarg}{an argument list (a char vector) to worker.}
  \item{nworkers}{number of workers to be spawned.}
  \item{info}{an info number.}
  \item{root}{the root member who spawns workers.}
  \item{intercomm}{an intercomm number.}
  \item{quiet}{a logical. If TRUE, do not print anything unless an error occurs.}
}
\value{
Unless \code{quiet = TRUE}, a message is printed to indicate how many workers are successfully 
spawned and how many failed.
}
\seealso{
  \code{\link{mpi.comm.get.manager}},
  \code{\link{mpi.intercomm.merge}}.
}
\references{
  \url{https://www.open-mpi.org/}
}

\author{
Hao Yu
}
\keyword{interface}
