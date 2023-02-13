\name{lamhosts}
\alias{mpi.is.parent}
\alias{lamhosts}
\alias{mpi.hostinfo}
\alias{child.hostinfo}
\title{Hosts Information}
\description{
\code{lamhosts} finds the host name associated with its node number. Can be used 
by \code{\link{mpi.spawn.Rchildren}} to spawn R children on selected hosts. This is 
a LAM-MPI specific function.

\code{mpi.is.parent} checks if it is running on parent or children.

\code{mpi.hostinfo} finds an individual host information including rank and 
size in a comm.

\code{child.hostinfo} is executed only by parent and find all parent and children 
host information in a comm.

}
\usage{
lamhosts()
mpi.is.parent()
mpi.hostinfo(comm = 1)
child.hostinfo(comm = 1, short=TRUE) 
}
\arguments{
 \item{comm}{a communicator number}
 \item{short}{if true, a short form is printed}
}
\value{
  \code{lamhosts} returns CPUs nodes numbers with their host names.

  \code{mpi.is.parent} returns TRUE if it is on parent and FALSE otherwise.

  \code{mpi.hostinfo} sends to stdio a host name, rank, size and comm.

  \code{child.hostname} sends to stdio a list of host, rank, size, and comm 
information for all parent and children. With short=TRUE and 8 children or more, 
the first 3 and last 2 children are shown. } 
\seealso{
  \code{\link{mpi.spawn.Rchildren}}
}
\author{
Hao Yu
}
\keyword{utilities}
