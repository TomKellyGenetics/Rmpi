\name{mpi.spawn.Rworkers}
\alias{mpi.spawn.Rworkers}
\alias{mpi.close.Rworkers}
\alias{tailworker.log}
\title{Spawn and Close R Workers}
\description{
\code{mpi.spawn.Rworkers} spawns R workers to those hosts automatically 
chosen by MPI or specific hosts assigned by the argument \code{hosts}.
Those R workers are running in R BATCH mode with a specific Rscript file. 
The default Rscript file "workerdaemon.R" provides interactive R worker 
environments.

\code{mpi.close.Rworkers} shuts down R workers spawned by 
\code{mpi.spawn.Rworkers}.

\code{tailworker.log} view (from tail) R worker log files (assuming they are all 
in one working directory).
}
\usage{
mpi.spawn.Rworkers(Rscript=system.file("workerdaemon.R", package="Rmpi"),
        nworkers=mpi.universe.size(), root = 0, intercomm = 2,
        comm = 1, hosts = NULL, needlog = TRUE, mapdrive=TRUE, quiet = FALSE, 
		nonblock=TRUE, sleep=0.1)

mpi.close.Rworkers(dellog = TRUE, comm = 1)
tailworker.log(nlines = 3, comm = 1)
}
\arguments{
  \item{Rscript}{an R script file used to run R in BATCH mode.}
  \item{nworkers}{number of workers to be spawned.}
  \item{root}{the rank number of the member who spawns R workers.}
  \item{intercomm}{an intercommunicator number}
  \item{comm}{a communicator number merged from an intercomm.}
  \item{hosts}{NULL or LAM node numbers to specify where R workers to be 
spawned.}
  \item{needlog}{a logical. If TRUE, R BATCH outputs will be saved in log files.  
  If FALSE, the outputs will send to /dev/null.}
  \item{mapdrive}{a logical. If TRUE and manager's working dir is on a network, mapping network drive 
is attemped on remote nodes under windows platform.}
  \item{quiet}{a logical. If TRUE, do not print anything unless an error occurs.}
  \item{nonblock}{a logical. If TRUE, a nonblock procedure is used on all workers so that
they will consume none or little CPUs while waiting.}
  \item{sleep}{a sleep interval, used when nonblock=TRUE. Smaller sleep is, more response workers are, 
  more CPUs consume.}
  \item{dellog}{a logical specifying if R worker's log files are deleted or 
not.}
  \item{nlines}{number of lines to view from tail in R worker's log files.}
}
\value{
 Unless \code{quiet = TRUE}, \code{mpi.spawn.Rworkers} prints to stdio how many 
workers are successfully spawned and where they are running.

 \code{mpi.close.Rworkers} return 1 if success and 0 otherwise.

\code{tailworker.log} returns last lines of R worker's log files.
}
\details{
The R workers that \code{mpi.spawn.Rworkers} spawns are really running a shell 
program which can be found in \code{system.file("Rworkers.sh",package="Rmpi")} 
which takes a Rscript file as one of its arguments. Other arguments are 
used to see if a log file (R output) is needed and how to name it. The manager 
process id and the comm number, along with host names where R workers are running  are 
used to name these log files. 

Once R workers are successfully spawned, the 
mergers from an intercomm (default `intercomm = 2') to a comm (default 
`comm = 1') are automatically done on manager and workers (should be done 
if the default Rscript is replaced). If additional sets of R workers are 
needed, please use `comm = 3', `comm = 4', etc to spawn them. At most a 
comm number up to 10 can be used. Notice that the default comm number for 
R workers (using workerdaemon.R) is always 1 which is saved as .comm.  
  
To spawn R workers to specific hosts, please use the argument \code{hosts} 
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
mpi.spawn.Rworkers(nworkers=2)
tailworker.log()
mpi.remote.exec(rnorm(10))
mpi.close.Rworkers()
}
}
\keyword{utilities}
