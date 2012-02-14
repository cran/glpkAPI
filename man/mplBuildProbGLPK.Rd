\name{mplBuildProbGLPK}
\alias{mplBuildProbGLPK}
\alias{glp_mpl_build_prob}

\title{
  Build Problem Instance From Model
}

\description{
  Low level interface function to the GLPK function \code{glp_mpl_build_prob}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  mplBuildProbGLPK(wk, lp)
}

\arguments{
  \item{wk}{
    A pointer to a GLPK translocator workspace.
  }
  \item{lp}{
    A pointer to a GLPK problem object.
  }
}

\details{
  Interface to the C function \code{mplBuildProb} which calls the GLPK
  function \code{glp_mpl_build_prob}.
}

\value{
  Returns zero on success, otherwise it returns non-zero.
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich

  Maintainer: Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>
}

\seealso{
  \code{\link{mplAllocWkspGLPK}},
  \code{\link{mplFreeWkspGLPK}},
  \code{\link{mplGenerateGLPK}},
  \code{\link{mplPostsolveGLPK}},
  \code{\link{mplReadDataGLPK}} and
  \code{\link{mplReadModelGLPK}}.
}

\keyword{ optimize }
