\name{mpi.setup.rngstream}
\alias{mpi.setup.rngstream}
\title{Setup parallel RNG on all children}
\description{
  \code{mpi.setup.rngstream} setups RNGstream on all children. 
}
\usage{
mpi.setup.rngstream(iseed=NULL, comm = 1)
}
\arguments{
  \item{iseed}{An integer to be supplied to \code{set.seed}, or NULL not to set reproducible seeds.} 
  \item{comm}{A comm number.}
}
\value{
  No value returned.
}
\details{
  \code{mpi.setup.rngstream} can be run only on parent node. It can be run later on with the same or
  different iseed.
}
\author{
  Hao Yu
}
\keyword{interface}
\keyword{distribution}
