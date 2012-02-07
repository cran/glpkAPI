/* glpkR.h
   R interface to GLPK.
 
   Copyright (C) 2011 Gabriel Gelius-Dietrich, Department for Bioinformatics,
   Institute for Informatics, Heinrich-Heine-University, Duesseldorf, Germany.
   All right reserved.
   Email: geliudie@uni-duesseldorf.de
 
   This file is part of glpkAPI.
 
   GlpkAPI is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
 
   GlpkAPI is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with glpkAPI.  If not, see <http://www.gnu.org/licenses/>.
*/


#include <stdlib.h>
#include <glpk.h>


/* avoid remapping of Rf_<function> to <function> in R header files */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif /* R_NO_REMAP */

/* use strict R headers */
#ifndef STRICT_R_HEADERS
#define STRICT_R_HEADERS
#endif /* STRICT_R_HEADERS */

#include <R.h>
#include <Rinternals.h>


/* -------------------------------------------------------------------------- */
/* NULL */
#define checkIfNil(cp) do { \
    if (R_ExternalPtrAddr(cp) == NULL) \
        Rf_error("You passed a nil value!"); \
} while (0)


/* -------------------------------------------------------------------------- */
/* problem */
#define checkTypeOfProb(cp) do { \
    if ( (TYPEOF(cp) != EXTPTRSXP) || (R_ExternalPtrTag(cp) != tagGLPK) ) \
        Rf_error("You must pass a glpk problem structure!"); \
} while (0)

#define checkProb(p) do { \
    checkIfNil(p); \
    checkTypeOfProb(p); \
} while (0)


/* -------------------------------------------------------------------------- */
/* parameters */
#define checkTypeOfParm(pa) do { \
    if ( (TYPEOF(pa) != EXTPTRSXP) || (R_ExternalPtrTag(pa) != tagGLPKparm) ) \
        Rf_error("You must pass a pointer to an glpk parameter structure!"); \
} while (0)

#define checkParm(p) do { \
    checkIfNil(p); \
    checkTypeOfParm(p); \
} while (0)


/* -------------------------------------------------------------------------- */
/* MathProg */
#define checkTypeOfMathProg(mp) do { \
    if ( (TYPEOF(mp) != EXTPTRSXP) || (R_ExternalPtrTag(mp) != tagMATHprog) ) \
        Rf_error("You must pass a pointer to an MathProg translator workspace!"); \
} while (0)

#define checkMathProg(p) do { \
    checkIfNil(p); \
    checkTypeOfMathProg(p); \
} while (0)
