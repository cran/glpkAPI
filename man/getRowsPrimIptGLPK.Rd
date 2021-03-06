\name{getRowsPrimIptGLPK}
\alias{getRowsPrimIptGLPK}

\title{
  Retrieve Row Primal Value of all Rows
}

\description{
  This is an advanced version of \code{\link{getRowPrimIptGLPK}}.
}

\usage{
  getRowsPrimIptGLPK(lp)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
}

\details{
  Interface to the C function \code{getRowsPrimIpt} which calls the GLPK
  function \code{glp_ipt_row_prim}.
}

\value{
  The row primal values of all rows are returned.
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\keyword{ optimize }

