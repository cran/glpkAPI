\name{initProbGLPK}
\alias{initProbGLPK}
\alias{glp_create_prob}

\title{
  Create a GLPK Problem Object
}

\description{
  Low level interface function to the GLPK function \code{glp_create_prob}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  initProbGLPK()
}

\details{
  Interface to the C function \code{initProb} which calls the GLPK
  function \code{glp_create_prob}.
}

\value{
  The function returns an external pointer object (externalptr).
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich

  Maintainer: Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>
}

\keyword{ optimize }

