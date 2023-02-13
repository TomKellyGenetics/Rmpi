\name{mpi.comm.spawn}
\alias{mpi.comm.spawn}
\title{MPI\_Comm\_spawn API }
\description{
\code{mpi.comm.spawn} tries to start \code{nchildren} identical copies of 
\code{children}, establishing communication with them and returning an 
intercommunicator. The spawned children are referred to as children, and the 
process that spawned them is called the parent (parent). The children have 
their own MPI\_COMM\_WORLD represented by comm 0. To make communication 
possible among parent and children, all children should use 
\code{\link{mpi.comm.get.parent}} to find their parent and use 
\code{\link{mpi.intercomm.merge}} to merger an intercomm to a comm.
 }
\usage{
 mpi.comm.spawn(child, childarg = character(0),
                nchildren = mpi.universe.size(), info = 0,
                root = 0, intercomm = 2, quiet = FALSE)
}
\arguments{
  \item{child}{a file name to an executable program.}
  \item{childarg}{an argument list (a char vector) to child.}
  \item{nchildren}{number of children to be spawned.}
  \item{info}{an info number.}
  \item{root}{the root member who spawns children.}
  \item{intercomm}{an intercomm number.}
  \item{quiet}{a logical. If TRUE, do not print anything unless an error occurs.}
}
\value{
Unless \code{quiet = TRUE}, a message is printed to indicate how many children are successfully 
spawned and how many failed.
}
\seealso{
  \code{\link{mpi.comm.get.parent}},
  \code{\link{mpi.intercomm.merge}}.
}
\references{
  \url{https://www.open-mpi.org/}
}

\author{
Hao Yu
}
\keyword{interface}
