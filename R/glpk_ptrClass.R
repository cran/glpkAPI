#------------------------------------------------------------------------------#
#                             R interface to GLPK                              #
#------------------------------------------------------------------------------#

#  glpk_ptrClass.R
#  R interface to GLPK.
#
#  Copyright (C) 2011-2012 Gabriel Gelius-Dietrich, Dpt. for Bioinformatics,
#  Institute for Informatics, Heinrich-Heine-University, Duesseldorf, Germany.
#  All right reserved.
#  Email: geliudie@uni-duesseldorf.de
#
#  This file is part of glpkAPI.
#
#  GlpkAPI is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  GlpkAPI is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with glpkAPI  If not, see <http://www.gnu.org/licenses/>.


#------------------------------------------------------------------------------#
#                          definition of class glpkPtr                         #
#------------------------------------------------------------------------------#


# representation of class glpkPtr
setClass(Class = "glpkPtr",
         representation(
              pType = "character",
              ptr   = "externalptr"
         )
         #, contains = "externalptr"
)


#------------------------------------------------------------------------------#

# contructor for class glpkPtr
setMethod(f = "initialize",
          signature = "glpkPtr",
          definition = function(.Object, p, w) {

              .Object@ptr   <- attr(p, which = w, exact = TRUE)
              .Object@pType <- as.character(p)
              
              return(.Object)
          
          }
)


# contructor for pointers to glpk problem structures
glpk_Pointer <- function(pointer) {

    if (is(pointer, "glpk_ptr")) {
        pObj <- new("glpkPtr",
                    p = pointer,
                    w = as.character("glpk_ptr"))
    }
    else {
        pObj <- pointer
    }

    return(pObj)
}

# contructor for pointers to translator workspace
trwks_Pointer <- function(pointer) {

    if (is(pointer, "trwks_ptr")) {
        pObj <- new("glpkPtr",
                    p = pointer,
                    w = as.character("trwks_ptr"))
    }
    else {
        pObj <- pointer
    }

    return(pObj)
}


#------------------------------------------------------------------------------#

# pType
setMethod("pType", signature(object = "glpkPtr"),
          function(object) {
              return(object@pType)
          }
)

setReplaceMethod("pType", signature = (object = "glpkPtr"),
                 function(object, value) {
                     object@pType <- value
                     return(object)
                 }
)


# ptr
setMethod("ptr", signature(object = "glpkPtr"),
          function(object) {
              return(object@ptr)
          }
)


#------------------------------------------------------------------------------#

setMethod("isNULLpointer", signature(object = "glpkPtr"),
    function(object) {
        return(.Call("isNULLptr", PACKAGE = "glpkAPI", ptr(object)))
    }
)

setMethod("isGLPKpointer", signature(object = "glpkPtr"),
    function(object) {
        return(.Call("isGLPKptr", PACKAGE = "glpkAPI", ptr(object)))
    }
)

setMethod("isTRWKSpointer", signature(object = "glpkPtr"),
    function(object) {
        return(.Call("isTRWKSptr", PACKAGE = "glpkAPI", ptr(object)))
    }
)


#------------------------------------------------------------------------------#

setMethod("show", signature(object = "glpkPtr"),
    function(object) {
    
        nc <- NA
        
        if (isNULLpointer(object)) {
            ptrtype <- "NULL"
        }
        else {
            if (isGLPKpointer(object)) {
                ptrtype <- "GLPK problem object"
                nc <- getNumColsGLPK(object)
            }
            else if (isTRWKSpointer(object)) {
                ptrtype <- "MathProg translator workspace"
            }
            else {
                ptrtype <- "unknown"
            }
        }

        cat("object of class ", dQuote("glpkPtr"),
            ": pointer to ", ptrtype, ".\n", sep = "")

        if (!is.na(nc)) {
            if ( (nc < 1) || (nc > 10) ) {
                cat(paste("Number of variables:  ",
                          getNumColsGLPK(object), "\n"))
                cat(paste("Number of constraints:",
                          getNumRowsGLPK(object), "\n"))
            }
            else {
                # make a more illustrative method here
                cat(paste("Number of variables:  ",
                          getNumColsGLPK(object), "\n"))
                cat(paste("Number of constraints:",
                          getNumRowsGLPK(object), "\n"))
            }
        }
        
        cat(paste("Slot ", dQuote("pType"), ": ", pType(object), "\n", sep = ""))
        cat(paste("Slot ", dQuote("ptr"), ":   ", sep = ""))
        print(slot(object, "ptr"), sep = "")
    }
)
