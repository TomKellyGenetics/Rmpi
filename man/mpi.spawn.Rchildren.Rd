\name{mpi.spawn.Rchildren}
\alias{mpi.spawn.Rchildren}
\alias{mpi.close.Rchildren}
\alias{tailchild.log}
\title{Spawn and Close R Children}
\description{
\code{mpi.spawn.Rchildren} spawns R children to those hosts automatically 
chosen by MPI or specific hosts assigned by the argument \code{hosts}.
Those R children are running in R BATCH mode with a specific Rscript file. 
The default Rscript file "childdaemon.R" provides interactive R child 
environments.

\code{mpi.close.Rchildren} shuts down R children spawned by 
\code{mpi.spawn.Rchildren}.

\code{tailchild.log} view (from tail) R child log files (assuming they are all 
in one working directory).
}
\usage{
mpi.spawn.Rchildren(Rscript=system.file("childdaemon.R", package="Rmpi"),
        nchildren=mpi.universe.size(), root = 0, intercomm = 2,
        comm = 1, hosts = NULL, needlog = TRUE, mapdrive=TRUE, quiet = FALSE, 
		nonblock=TRUE, sleep=0.1)

mpi.close.Rchildren(dellog = TRUE, comm = 1)
tailchild.log(nlines = 3, comm = 1)
}
\arguments{
  \item{Rscript}{an R script file used to run R in BATCH mode.}
  \item{nchildren}{number of children to be spawned.}
  \item{root}{the rank number of the member who spawns R children.}
  \item{intercomm}{an intercommunicator number}
  \item{comm}{a communicator number merged from an intercomm.}
  \item{hosts}{NULL or LAM node numbers to specify where R children to be 
spawned.}
  \item{needlog}{a logical. If TRUE, R BATCH outputs will be saved in log files.  
  If FALSE, the outputs will send to /dev/null.}
  \item{mapdrive}{a logical. If TRUE and parent's working dir is on a network, mapping network drive 
is attemped on remote nodes under windows platform.}
  \item{quiet}{a logical. If TRUE, do not print anything unless an error occurs.}
  \item{nonblock}{a logical. If TRUE, a nonblock procedure is used on all children so that
they will consume none or little CPUs while waiting.}
  \item{sleep}{a sleep interval, used when nonblock=TRUE. Smaller sleep is, more response children are, 
  more CPUs consume.}
  \item{dellog}{a logical specifying if R child's log files are deleted or 
not.}
  \item{nlines}{number of lines to view from tail in R child's log files.}
}
\value{
 Unless \code{quiet = TRUE}, \code{mpi.spawn.Rchildren} prints to stdio how many 
children are successfully spawned and where they are running.

 \code{mpi.close.Rchildren} return 1 if success and 0 otherwise.

\code{tailchild.log} returns last lines of R child's log files.
}
\details{
The R children that \code{mpi.spawn.Rchildren} spawns are really running a shell 
program which can be found in \code{system.file("Rchildren.sh",package="Rmpi")} 
which takes a Rscript file as one of its arguments. Other arguments are 
used to see if a log file (R output) is needed and how to name it. The parent 
process id and the comm number, along with host names where R children are running  are 
used to name these log files. 

Once R children are successfully spawned, the 
mergers from an intercomm (default `intercomm = 2') to a comm (default 
`comm = 1') are automatically done on parent and children (should be done 
if the default Rscript is replaced). If additional sets of R children are 
needed, please use `comm = 3', `comm = 4', etc to spawn them. At most a 
comm number up to 10 can be used. Notice that the default comm number for 
R children (using childdaemon.R) is always 1 which is saved as .comm.  
  
To spawn R children to specific hosts, please use the argument \code{hosts} 
with a list of those node numbers (an integer vector). Total node numbers 
along their host names can be found by using \code{\link{lamhosts}}.
Notice that this is LAM-MPI specific. 
} 

\seealso{
  \code{\link{mpi.comm.spawn}},
  \code{\link{lamhosts}}.
}
\author{
Hao Yu
}
\examples{
\donttest{
mpi.spawn.Rchildren(nchildren=2)
tailchild.log()
mpi.remote.exec(rnorm(10))
mpi.close.Rchildren()
}
}
\keyword{utilities}
