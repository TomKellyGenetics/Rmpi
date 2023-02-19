\name{mpi.parSim}
\alias{mpi.parSim}
\title{Parallel Monte Carlo Simulation}
\description{
 Carry out parallel Monte Carlo simulation on R workers spawned by 
 using workerdaemon.R script and all executed results are returned back to 
 manager. 
} 
\usage{
mpi.parSim(n=100, rand.gen=rnorm, rand.arg=NULL,statistic, 
nsim=100, run=1, workerinfo=FALSE, sim.seq=NULL, simplify=TRUE, comm=1, ...)  
}
\arguments{
  \item{n}{sample size.}
  \item{rand.gen}{the random data generating function. See the details 
section}
  \item{rand.arg}{additional argument list to \code{rand.gen}.}
  \item{statistic}{the statistic function to be simulated. See the 
details section}
  \item{nsim}{the number of simulation carried on a worker which is 
 counted as one worker job.}
  \item{run}{the number of looping. See the details section.}
  \item{workerinfo}{if TRUE, the numbers of jobs finished by workers 
will be displayed.}
  \item{sim.seq}{if reproducing the same simulation is desirable, set it 
to the integer vector .mpi.parSim generated in previous simulation.}
  \item{simplify}{logical; should the result be simplified to a vector or
matrix if possible?}
  \item{comm}{a communicator number}
  \item{...}{optional arguments to \code{statistic}}
}
\value{
   The returned values depend on values returned by \code{\link{replicate}}
of \code{statistic(rand.gen(n))} and the total number of simulation 
(TNS). If \code{statistic} returns a single value, then the result is a 
vector of length TNS. If \code{statistic} returns a vector (list) of 
length \code{nrow}, then the result is a matrix of dimension 
\code{c(nrow, TNS)}.   
}
\details{
It is assumed that one simulation is carried out as 
\code{statistic(rand.gen(n))}, where \code{rand.gen(n)} can return any 
values as long as \code{statistic} can take them. Additional arguments can 
be passed to \code{rand.gen} by \code{rand.arg} as a list. Optional 
arguments can also be passed to \code{statistic} by the argument 
\code{...}. 

Each worker job consists of \code{replicate(nsim,statistic(rand.gen(n)))}, 
i.e., each job runs \code{nsim} number of simulation. The returned values 
are transported from workers to manager.

The total number of simulation (TNS) is calculated as follows. Let 
worker.num be the total number of workers in a \code{comm} and it is 
\code{mpi.comm.size(comm)-1}. Then TNS=worker.num*nsim*run and the total
number of worker jobs is worker.num*run, where \code{run} is the number of 
looping from manager perspective. If run=1, each worker will run one worker 
job. If run=2, each worker will run two workers jobs on average, and so on. 

The purpose of using \code{run} has two folds. It allows a tuneup 
of worker job size and total number of worker jobs to deal with two 
different cluster environments. On a cluster of workers with equal CPU 
power, \code{run=1} is often enough. But if \code{nsim} is too big, one
can set \code{run=2} and the worker jog size to be \code{nsim/2} so that  
TNS=worker.num*(nsim/2)*(2*run). This may improve R computation 
efficiency slightly. On a cluster of workers with different CPU power, one 
can choose a big value of \code{run} and a small value of \code{nsim} 
so that manager can dispatch more jobs to workers who run faster than 
others. This will keep all workers busy so that load balancing is 
achieved. 

The sequence of workers who deliver results to manager are saved into 
\code{.mpi.parSim}. It keeps track which part of results done by which workers. 
\code{.mpi.parSim} can be used to reproduce the same simulation result if the same
seed is used and the argument \code{sim.seq} is equal to \code{.mpi.parSim}.
 
See the warning section before you use \code{mpi.parSim}.
}

\section{Warning}{
It is assumed that a parallel RNG is used on all workers. Run
\code{mpi.setup.rngstream} on the manager to set up a parallel RNG. Though \code{mpi.parSim} 
works without a parallel RNG, the quality of simulation is not guarantied. 

\code{mpi.parSim} will automatically transfer \code{rand.gen} 
and \code{statistic} to workers. However, any functions that 
\code{rand.gen} and \code{statistic} reply on but are not on workers 
must be transfered to workers before using \code{mpi.parSim}. You 
can use \code{\link{mpi.bcast.Robj2worker}} for that purpose. The same is 
applied to required packages or C/Fortran codes. You can use either 
\code{\link{mpi.bcast.cmd}} or put \code{required(package)} and/or 
\code{dyn.load(so.lib)} into \code{rand.gen} and \code{statistic}.
 
If \code{simplify} is TRUE, sapply style simplication is applied. Otherwise a list of length 
worker.num*run is returned.} 

\seealso{
  \code{\link{mpi.setup.rngstream}}
  \code{\link{mpi.bcast.cmd}}
  \code{\link{mpi.bcast.Robj2worker}}
}
\author{
Hao Yu
}
\keyword{utilities}
