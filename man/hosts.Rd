\name{lamhosts}
\alias{mpi.is.manager}
\alias{lamhosts}
\alias{mpi.hostinfo}
\alias{worker.hostinfo}
\title{Hosts Information}
\description{
\code{lamhosts} finds the host name associated with its node number. Can be used 
by \code{\link{mpi.spawn.Rworkers}} to spawn R workers on selected hosts. This is 
a LAM-MPI specific function.

\code{mpi.is.manager} checks if it is running on manager or workers.

\code{mpi.hostinfo} finds an individual host information including rank and 
size in a comm.

\code{worker.hostinfo} is executed only by manager and find all manager and workers 
host information in a comm.

}
\usage{
lamhosts()
mpi.is.manager()
mpi.hostinfo(comm = 1)
worker.hostinfo(comm = 1, short=TRUE) 
}
\arguments{
 \item{comm}{a communicator number}
 \item{short}{if true, a short form is printed}
}
\value{
  \code{lamhosts} returns CPUs nodes numbers with their host names.

  \code{mpi.is.manager} returns TRUE if it is on manager and FALSE otherwise.

  \code{mpi.hostinfo} sends to stdio a host name, rank, size and comm.

  \code{worker.hostname} sends to stdio a list of host, rank, size, and comm 
information for all manager and workers. With short=TRUE and 8 workers or more, 
the first 3 and last 2 workers are shown. } 
\seealso{
  \code{\link{mpi.spawn.Rworkers}}
}
\author{
Hao Yu
}
\keyword{utilities}
