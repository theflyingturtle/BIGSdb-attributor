#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""Written by Katriel Cohn-Gordon (2016).

STRUCTURE is population genetics software which has a terribly non-
machine-readable output format. This module invokes it using some
configurable options, giving some data in an output directory.

"""

import argparse
import collections
import distutils.spawn
import logging
import os
import re
import shutil
import subprocess
import sys
import tempfile

import pandas as pd
import xlrd
from bigsdb_attributor.structure.config import EXTRAPARAMS, MAINPARAMS

log = logging.getLogger('runner')


class StructureRuntimeError(RuntimeError):
    pass


def run(data, label, max_populations, executable=None, output_directory=None):
    """Run STRUCTURE with the given data and options.

    `data` should be the path to a pandas dataframe in the correct format for STRUCTURE.

    Returns the path to the output file generated by STRUCTURE.

    """
    # Try to find STRUCTURE binary.
    structure = executable or distutils.spawn.find_executable('structure')
    if structure is None:
        raise StructureRuntimeError(
            'Could not find STRUCTURE binary; is it on your $PATH?',
        )
        # TODO(propagate this through multiprocessing)

    # Set up output directory.
    if not os.path.isdir(output_directory):
        os.mkdir(output_directory)

    def path(f):
        return os.path.join(output_directory, f)

    # Parse STRUCTURE input dataframe to extract various bits of data.
    if not os.path.isfile(data):
        raise StructureRuntimeError('Could not find file {}.'.format(data))
    df = pd.read_csv(data, index_col=0, sep='\t')
    n, n_l = len(df), sum(c.startswith('Locus_') for c in df.columns)

    # Set up mainparams and extraparams.
    with open(path('mainparams'), 'w') as mainparams, \
            open(path('extraparams'), 'w') as extraparams:
        # TODO(K): I removed the -1 from maxpops. Why was it there?
        mainparams.write(MAINPARAMS.format(
            maxpops=max_populations,
            inputfile=data,
            outputfile=path(
                'structure_results',
            ),
            n=n,
            n_l=n_l,
            label=label,
        ))
        extraparams.write(EXTRAPARAMS)
        mainparams.flush()
        extraparams.flush()

        # Run STRUCTURE.
        with open(path('structure_runtime.log'), 'w') as runtime:
            structure = subprocess.Popen(
                [
                    os.path.expanduser(structure),
                    '-m', path('mainparams'),
                    '-e', path('extraparams'),
                ],
                stdout=runtime, stderr=sys.stderr,
            )
            structure.communicate()

        return path('structure_results') + '_f'
