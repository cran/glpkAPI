/* init.c
   R interface to GLPK.
 
   Copyright (C) 2011-2013 Gabriel Gelius-Dietrich, Dpt. for Bioinformatics,
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

#include <R.h>
#include <Rinternals.h>

#include "glpkAPI.h"

#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
    {"isGLPKptr",           (DL_FUNC) &isGLPKptr,           1},
    {"isTRWKSptr",          (DL_FUNC) &isTRWKSptr,          1},
    {"isNULLptr",           (DL_FUNC) &isNULLptr,           1},
    {"initGLPK",            (DL_FUNC) &initGLPK,            0},
    {"delProb",             (DL_FUNC) &delProb,             1},
    {"eraseProb",           (DL_FUNC) &eraseProb,           1},
    {"copyProb",            (DL_FUNC) &copyProb,            3},
    {"initProb",            (DL_FUNC) &initProb,            1},
    {"setProbName",         (DL_FUNC) &setProbName,         2},
    {"getProbName",         (DL_FUNC) &getProbName,         1},
    {"setObjName",          (DL_FUNC) &setObjName,          2},
    {"getObjName",          (DL_FUNC) &getObjName,          1},
    {"createIndex",         (DL_FUNC) &createIndex,         1},
    {"deleteIndex",         (DL_FUNC) &deleteIndex,         1},
    {"setDefaultSmpParm",   (DL_FUNC) &setDefaultSmpParm,   0},
    {"setDefaultIptParm",   (DL_FUNC) &setDefaultIptParm,   0},
    {"setDefaultMIPParm",   (DL_FUNC) &setDefaultMIPParm,   0},
    {"setSimplexParm",      (DL_FUNC) &setSimplexParm,      6},
    {"setInteriorParm",     (DL_FUNC) &setInteriorParm,     3},
    {"setMIPParm",          (DL_FUNC) &setMIPParm,          6},
    {"getSimplexParm",      (DL_FUNC) &getSimplexParm,      0},
    {"getInteriorParm",     (DL_FUNC) &getInteriorParm,     0},
    {"getMIPParm",          (DL_FUNC) &getMIPParm,          0},
    {"setObjDir",           (DL_FUNC) &setObjDir,           2},
    {"getObjDir",           (DL_FUNC) &getObjDir,           1},
    {"addRows",             (DL_FUNC) &addRows,             2},
    {"setRowName",          (DL_FUNC) &setRowName,          3},
    {"getRowName",          (DL_FUNC) &getRowName,          2},
    {"findRow",             (DL_FUNC) &findRow,             2},
    {"addCols",             (DL_FUNC) &addCols,             2},
    {"setColName",          (DL_FUNC) &setColName,          3},
    {"getColName",          (DL_FUNC) &getColName,          2},
    {"findCol",             (DL_FUNC) &findCol,             2},
    {"getNumRows",          (DL_FUNC) &getNumRows,          1},
    {"getNumCols",          (DL_FUNC) &getNumCols,          1},
    {"setColsBnds",         (DL_FUNC) &setColsBnds,         5},
    {"setColsBndsObjCoefs", (DL_FUNC) &setColsBndsObjCoefs, 6},
    {"setColBnd",           (DL_FUNC) &setColBnd,           5},
    {"getColsLowBnds",      (DL_FUNC) &getColsLowBnds,      2},
    {"getColLowBnd",        (DL_FUNC) &getColLowBnd,        2},
    {"getColsUppBnds",      (DL_FUNC) &getColsUppBnds,      2},
    {"getColUppBnd",        (DL_FUNC) &getColUppBnd,        2},
    {"setColKind",          (DL_FUNC) &setColKind,          3},
    {"setColsKind",         (DL_FUNC) &setColsKind,         3},
    {"getColKind",          (DL_FUNC) &getColKind,          2},
    {"getColsKind",         (DL_FUNC) &getColsKind,         2},
    {"getNumInt",           (DL_FUNC) &getNumInt,           1},
    {"getNumBin",           (DL_FUNC) &getNumBin,           1},
    {"setRowsBnds",         (DL_FUNC) &setRowsBnds,         5},
    {"setRhsZero",          (DL_FUNC) &setRhsZero,          1},
    {"setRowBnd",           (DL_FUNC) &setRowBnd,           5},
    {"getRowsLowBnds",      (DL_FUNC) &getRowsLowBnds,      2},
    {"getRowLowBnd",        (DL_FUNC) &getRowLowBnd,        2},
    {"getRowsUppBnds",      (DL_FUNC) &getRowsUppBnds,      2},
    {"getRowUppBnd",        (DL_FUNC) &getRowUppBnd,        2},
    {"getRowType",          (DL_FUNC) &getRowType,          2},
    {"getColType",          (DL_FUNC) &getColType,          2},
    {"setObjCoefs",         (DL_FUNC) &setObjCoefs,         3},
    {"setObjCoef",          (DL_FUNC) &setObjCoef,          3},
    {"getObjCoefs",         (DL_FUNC) &getObjCoefs,         2},
    {"getObjCoef",          (DL_FUNC) &getObjCoef,          2},
    {"loadMatrix",          (DL_FUNC) &loadMatrix,          5},
    {"checkDup",            (DL_FUNC) &checkDup,            5},
    {"sortMatrix",          (DL_FUNC) &sortMatrix,          1},
    {"delRows",             (DL_FUNC) &delRows,             3},
    {"delCols",             (DL_FUNC) &delCols,             3},
    {"setRii",              (DL_FUNC) &setRii,              3},
    {"setSjj",              (DL_FUNC) &setSjj,              3},
    {"getRii",              (DL_FUNC) &getRii,              2},
    {"getSjj",              (DL_FUNC) &getSjj,              2},
    {"scaleProb",           (DL_FUNC) &scaleProb,           2},
    {"unscaleProb",         (DL_FUNC) &unscaleProb,         1},
    {"setRowStat",          (DL_FUNC) &setRowStat,          3},
    {"setColStat",          (DL_FUNC) &setColStat,          3},
    {"stdBasis",            (DL_FUNC) &stdBasis,            1},
    {"advBasis",            (DL_FUNC) &advBasis,            1},
    {"cpxBasis",            (DL_FUNC) &cpxBasis,            1},
    {"warmUp",              (DL_FUNC) &warmUp,              1},
    {"termOut",             (DL_FUNC) &termOut,             1},
    {"solveSimplex",        (DL_FUNC) &solveSimplex,        1},
    {"solveSimplexExact",   (DL_FUNC) &solveSimplexExact,   1},
    {"getObjVal",           (DL_FUNC) &getObjVal,           1},
    {"getSolStat",          (DL_FUNC) &getSolStat,          1},
    {"getColsPrim",         (DL_FUNC) &getColsPrim,         1},
    {"getColPrim",          (DL_FUNC) &getColPrim,          2},
    {"getPrimStat",         (DL_FUNC) &getPrimStat,         1},
    {"getDualStat",         (DL_FUNC) &getDualStat,         1},
    {"getRowStat",          (DL_FUNC) &getRowStat,          2},
    {"getRowsStat",         (DL_FUNC) &getRowsStat,         1},
    {"getRowPrim",          (DL_FUNC) &getRowPrim,          2},
    {"getRowsPrim",         (DL_FUNC) &getRowsPrim,         1},
    {"getRowDual",          (DL_FUNC) &getRowDual,          2},
    {"getRowsDual",         (DL_FUNC) &getRowsDual,         1},
    {"getColStat",          (DL_FUNC) &getColStat,          2},
    {"getColsStat",         (DL_FUNC) &getColsStat,         1},
    {"getColDual",          (DL_FUNC) &getColDual,          2},
    {"getColsDual",         (DL_FUNC) &getColsDual,         1},
    {"getUnbndRay",         (DL_FUNC) &getUnbndRay,         1},
    {"solveInterior",       (DL_FUNC) &solveInterior,       1},
    {"getObjValIpt",        (DL_FUNC) &getObjValIpt,        1},
    {"getSolStatIpt",       (DL_FUNC) &getSolStatIpt,       1},
    {"getColsPrimIpt",      (DL_FUNC) &getColsPrimIpt,      1},
    {"getColPrimIpt",       (DL_FUNC) &getColPrimIpt,       2},
    {"getRowPrimIpt",       (DL_FUNC) &getRowPrimIpt,       2},
    {"getRowsPrimIpt",      (DL_FUNC) &getRowsPrimIpt,      1},
    {"getRowDualIpt",       (DL_FUNC) &getRowDualIpt,       2},
    {"getRowsDualIpt",      (DL_FUNC) &getRowsDualIpt,      1},
    {"getColDualIpt",       (DL_FUNC) &getColDualIpt,       2},
    {"getColsDualIpt",      (DL_FUNC) &getColsDualIpt,      1},
    {"solveMIP",            (DL_FUNC) &solveMIP,            1},
    {"mipStatus",           (DL_FUNC) &mipStatus,           1},
    {"mipObjVal",           (DL_FUNC) &mipObjVal,           1},
    {"mipRowVal",           (DL_FUNC) &mipRowVal,           2},
    {"mipRowsVal",          (DL_FUNC) &mipRowsVal,          1},
    {"mipColVal",           (DL_FUNC) &mipColVal,           2},
    {"mipColsVal",          (DL_FUNC) &mipColsVal,          1},
    {"getNumNnz",           (DL_FUNC) &getNumNnz,           1},
    {"getMatRow",           (DL_FUNC) &getMatRow,           2},
    {"setMatRow",           (DL_FUNC) &setMatRow,           5},
    {"getMatCol",           (DL_FUNC) &getMatCol,           2},
    {"setMatCol",           (DL_FUNC) &setMatCol,           5},
    {"readMPS",             (DL_FUNC) &readMPS,             3},
    {"readLP",              (DL_FUNC) &readLP,              2},
    {"readProb",            (DL_FUNC) &readProb,            2},
    {"writeMPS",            (DL_FUNC) &writeMPS,            3},
    {"writeLP",             (DL_FUNC) &writeLP,             2},
    {"writeProb",           (DL_FUNC) &writeProb,           2},
    {"printSol",            (DL_FUNC) &printSol,            2},
    {"readSol",             (DL_FUNC) &readSol,             2},
    {"writeSol",            (DL_FUNC) &writeSol,            2},
    {"printIpt",            (DL_FUNC) &printIpt,            2},
    {"readIpt",             (DL_FUNC) &readIpt,             2},
    {"writeIpt",            (DL_FUNC) &writeIpt,            2},
    {"printMIP",            (DL_FUNC) &printMIP,            2},
    {"readMIP",             (DL_FUNC) &readMIP,             2},
    {"writeMIP",            (DL_FUNC) &writeMIP,            2},
    {"version",             (DL_FUNC) &version,             0},
    {"bfExists",            (DL_FUNC) &bfExists,            1},
    {"factorize",           (DL_FUNC) &factorize,           1},
    {"bfUpdated",           (DL_FUNC) &bfUpdated,           1},
    {"setBfcp",             (DL_FUNC) &setBfcp,             7},
    {"getBfcp",             (DL_FUNC) &getBfcp,             1},
    {"getBhead",            (DL_FUNC) &getBhead,            2},
    {"getRbind",            (DL_FUNC) &getRbind,            2},
    {"getCbind",            (DL_FUNC) &getCbind,            2},
    {"printRanges",         (DL_FUNC) &printRanges,         4},
    {"mplAllocWksp",        (DL_FUNC) &mplAllocWksp,        1},
    {"mplFreeWksp",         (DL_FUNC) &mplFreeWksp,         1},
    {"mplReadModel",        (DL_FUNC) &mplReadModel,        3},
    {"mplReadData",         (DL_FUNC) &mplReadData,         2},
    {"mplGenerate",         (DL_FUNC) &mplGenerate,         2},
    {"mplBuildProb",        (DL_FUNC) &mplBuildProb,        2},
    {"mplPostsolve",        (DL_FUNC) &mplPostsolve,        3},
    {NULL, NULL, 0}
};


void R_init_glpkAPI(DllInfo *info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
