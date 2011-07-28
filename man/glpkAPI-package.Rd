\name{glpkAPI-package}

\alias{glpkAPI-package}
\alias{glpkAPI}

\docType{package}

\title{
  R Interface to C API of GLPK
}

\description{
  A low level interface to the GNU Linear Programming Kit (GLPK).
}

\details{
    \tabular{ll}{
        Package: \tab glpkAPI \cr
        Type: \tab Package \cr
        Version: \tab 1.1.5 \cr
        Date: \tab 2011-07-28 \cr
        Depends: \tab R (>= 2.6.0), methods \cr
        License: \tab GPL-3\cr
        LazyLoad: \tab yes\cr
    }

    Index:
    \preformatted{
glpkAPI-package       R Interface to C API of GLPK
    }

The package \pkg{glpkAPI} provides a low level interface to
the GNU Linear Programming Kit (GLPK).

}

\author{
    Gabriel Gelius-Dietrich

    Maintainer: Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.

  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\keyword{ package }
\keyword{ optimize}

\examples{
# load package
library(glpkAPI)

# preparing the model
lp <- initProbGLPK()

# model data
nrows  <- 5
ncols  <- 8

# constraint matrix
ne <- 14
ia <- c(1, 5, 1, 2, 2, 3, 1, 4, 1, 5, 3, 4, 1, 5)
ja <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 8)
ar <- c(3.0, 5.6, 1.0, 2.0, 1.1, 1.0, -2.0, 2.8,
        -1.0, 1.0, 1.0, -1.2, -1.0, 1.9)

# objective function
obj <- c(1, 0, 0, 0, 2, 0, 0, -1)

# upper and lower bounds of the rows
rlower <- c(2.5, -1000, 4, 1.8, 3)
rupper <- c(1000, 2.1, 4, 5, 15)

# upper and lower bounds of the columns
clower <- c(2.5, 0, 0, 0, 0.5, 0, 0, 0)
cupper <- c(1000, 4.1, 1, 1, 4, 1000, 1000, 4.3)

# direction of optimization
setObjDirGLPK(lp, GLP_MIN)

# add rows and columns
addRowsGLPK(lp, nrows)
addColsGLPK(lp, ncols)

setColsBndsObjCoefsGLPK(lp, c(1:ncols), clower, cupper, obj)
setRowsBndsGLPK(lp, c(1:nrows), rlower, rupper)

# load constraint matrix
loadMatrixGLPK(lp, ne, ia, ja, ar)

# solve lp problem
solveSimplexGLPK(lp)

# retrieve the results
getSolStatGLPK(lp)
getObjValGLPK(lp)
getColsPrimGLPK(lp)
}

