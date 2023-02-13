\name{mpi.parSim}
\alias{mpi.parSim}
\title{Parallel Monte Carlo Simulation}
\description{
 Carry out parallel Monte Carlo simulation on R children spawned by 
 using childdaemon.R script and all executed results are returned back to 
 parent. 
} 
\usage{
mpi.parSim(n=100, rand.gen=rnorm, rand.arg=NULL,statistic, 
nsim=100, run=1, childinfo=FALSE, sim.seq=NULL, simplify=TRUE, comm=1, ...)  
}
\arguments{
  \item{n}{sample size.}
  \item{rand.gen}{the random data generating function. See the details 
section}
  \item{rand.arg}{additional argument list to \code{rand.gen}.}
  \item{statistic}{the statistic function to be simulated. See the 
details section}
  \item{nsim}{the number of simulation carried on a child which is 
 counted as one child job.}
  \item{run}{the number of looping. See the details section.}
  \item{childinfo}{if TRUE, the numbers of jobs finished by children 
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

Each child job consists of \code{replicate(nsim,statistic(rand.gen(n)))}, 
i.e., each job runs \code{nsim} number of simulation. The returned values 
are transported from children to parent.

The total number of simulation (TNS) is calculated as follows. Let 
child.num be the total number of children in a \code{comm} and it is 
\code{mpi.comm.size(comm)-1}. Then TNS=child.num*nsim*run and the total
number of child jobs is child.num*run, where \code{run} is the number of 
looping from parent perspective. If run=1, each child will run one child 
job. If run=2, each child will run two children jobs on average, and so on. 

The purpose of using \code{run} has two folds. It allows a tuneup 
of child job size and total number of child jobs to deal with two 
different cluster environments. On a cluster of children with equal CPU 
power, \code{run=1} is often enough. But if \code{nsim} is too big, one
can set \code{run=2} and the child jog size to be \code{nsim/2} so that  
TNS=child.num*(nsim/2)*(2*run). This may improve R computation 
efficiency slightly. On a cluster of children with different CPU power, one 
can choose a big value of \code{run} and a small value of \code{nsim} 
so that parent can dispatch more jobs to children who run faster than 
others. This will keep all children busy so that load balancing is 
achieved. 

The sequence of children who deliver results to parent are saved into 
\code{.mpi.parSim}. It keeps track which part of results done by which children. 
\code{.mpi.parSim} can be used to reproduce the same simulation result if the same
seed is used and the argument \code{sim.seq} is equal to \code{.mpi.parSim}.
 
See the warning section before you use \code{mpi.parSim}.
}

\section{Warning}{
It is assumed that a parallel RNG is used on all children. Run
\code{mpi.setup.rngstream} on the parent to set up a parallel RNG. Though \code{mpi.parSim} 
works without a parallel RNG, the quality of simulation is not guarantied. 

\code{mpi.parSim} will automatically transfer \code{rand.gen} 
and \code{statistic} to children. However, any functions that 
\code{rand.gen} and \code{statistic} reply on but are not on children 
must be transfered to children before using \code{mpi.parSim}. You 
can use \code{\link{mpi.bcast.Robj2child}} for that purpose. The same is 
applied to required packages or C/Fortran codes. You can use either 
\code{\link{mpi.bcast.cmd}} or put \code{required(package)} and/or 
\code{dyn.load(so.lib)} into \code{rand.gen} and \code{statistic}.
 
If \code{simplify} is TRUE, sapply style simplication is applied. Otherwise a list of length 
child.num*run is returned.} 

\seealso{
  \code{\link{mpi.setup.rngstream}}
  \code{\link{mpi.bcast.cmd}}
  \code{\link{mpi.bcast.Robj2child}}
}
\author{
Hao Yu
}
\keyword{utilities}
