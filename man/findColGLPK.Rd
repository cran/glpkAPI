\name{findColGLPK}
\alias{findColGLPK}
\alias{glp_find_col}

\title{
  Find Column by its Name
}

\description{
  Low level interface function to the GLPK function \code{glp_find_col}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  findColGLPK(lp, cname)
}

\arguments{
  \item{lp}{
    A pointer to a GLPK problem object.
  }
  \item{cname}{
    A column name.
  }
}

\details{
  Interface to the C function \code{findCol} which calls the GLPK
  function \code{glp_find_column}.
}

\value{
  Returns the ordinal number of a column, which is assigned the specified cname.
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

