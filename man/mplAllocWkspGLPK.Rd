\name{mplAllocWkspGLPK}
\alias{mplAllocWkspGLPK}
\alias{glp_mpl_alloc_wksp}

\title{
  Allocate Translator Workspace
}

\description{
  Low level interface function to the GLPK function \code{glp_mpl_alloc_wksp}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  mplAllocWkspGLPK()
}

\details{
  Interface to the C function \code{mplAllocWksp} which calls the GLPK
  function \code{glp_mpl_alloc_wksp}.
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

