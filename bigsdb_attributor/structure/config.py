# -*- coding: utf-8 -*-
import logging

logging.error("Add '00' to BURNIN, ADMBURNING and NUMREPS")

MAINPARAMS = """\
#define OUTFILE  {outputfile}
#define INFILE   {inputfile}
#define NUMINDS    {n}
#define NUMLOCI    {n_l}
#define LABEL     {label:d}
#define POPDATA   1
#define POPFLAG   1
#define LOCDATA   0
#define PHENOTYPE 0
#define MARKERNAMES      1
#define MAPDISTANCES     0
#define ONEROWPERIND 1
#define RECESSIVEALLELES 0
#define EXTRACOLS 0
#define MISSING     -9
#define PLOIDY       1
#define MAXPOPS    {maxpops}
#define BURNIN    10
#define NUMREPS   100
"""

EXTRAPARAMS = """\
#define USEPOPINFO  1
#define GENSBACK    2
#define MIGRPRIOR 0.0

#define LINKAGE     0
#define NOADMIX     1
#define ADMBURNIN  5

#define FREQSCORR   0
#define LAMBDA    1.0
#define COMPUTEPROB 1
#define PFROMPOPFLAGONLY 1
#define ANCESTDIST   0
#define STARTATPOPINFO 1
#define METROFREQ    10
#define UPDATEFREQ   1

#define PRINTNET     1
#define PRINTLAMBDA  1
#define PRINTQSUM    1
#define PRINTQHAT    0
#define PRINTLIKES   0
#define INTERMEDSAVE 0
#define ECHODATA     1
"""


class Loci(object):
    MLST = 7
