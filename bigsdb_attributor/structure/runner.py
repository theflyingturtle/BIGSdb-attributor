#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""Written by Katriel Cohn-Gordon (2016).

STRUCTURE is population genetics software which has a terribly non-
machine-readable output format. This module invokes it using some
configurable options, giving some data in an output directory.

"""
# TO DO(K) force structure to add seed.txt to output directory
import distutils.spawn
import logging
import os
import subprocess
import sys

import pandas as pd
import tqdm
from bigsdb_attributor.structure.config import (
    BURNIN, EXTRAPARAMS,
    MAINPARAMS, NUMREPS
)

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
    data = os.path.abspath(data)
    if not os.path.isdir(output_directory):
        os.mkdir(output_directory)

    def path(kind, f):
        if kind == 'input':
            return os.path.join(os.path.dirname(data), f)
        elif kind == 'output':
            return os.path.join(output_directory, f)
        else:
            raise ValueError('Wrong kind')

    # Parse STRUCTURE input dataframe to extract various bits of data.
    if not os.path.isfile(data):
        raise StructureRuntimeError('Could not find file {}.'.format(data))
    df = pd.read_csv(data, index_col=0, sep='\t')
    n, n_l = len(df), sum(c.startswith('Locus_') for c in df.columns)

    # Set up mainparams and extraparams.
    with open(path('input', 'mainparams'), 'w') as mainparams, \
            open(path('input', 'extraparams'), 'w') as extraparams:
        # TODO(K): I removed the -1 from maxpops. Why was it there?
        mainparams.write(MAINPARAMS.format(
            maxpops=max_populations,
            inputfile=data,
            outputfile=path(
                'output',
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
        with open(path('output', 'structure_runtime.log'), mode='wb', buffering=0) as runtime, \
                tqdm.tqdm(total=BURNIN + NUMREPS) as pbar:
            structure = subprocess.Popen(
                [
                    os.path.expanduser(structure),
                    '-m', path('input', 'mainparams'),
                    '-e', path('input', 'extraparams'),
                ],
                stdout=subprocess.PIPE,
                stderr=sys.stderr,
                cwd=output_directory,
            )

            # This is effectively `structure.communicate()` but lets us inspect the lines, which is great for tqdm
            started_MCMC = False
            itercount = 0
            rc = structure.poll()
            while rc != 0:
                while True:
                    line = structure.stdout.readline()
                    if not line:
                        break
                    runtime.write(line)
                    if not started_MCMC:
                        if b'starting MCMC' in line:
                            started_MCMC = True
                        else:
                            continue
                    line_header = line.split(b':')[0].strip()
                    if line_header.isdigit() and int(line_header) > itercount:
                        itercount = int(line_header)
                        pbar.update(1)
                rc = structure.poll()

            structure.communicate()

        return path('output', 'structure_results') + '_f'
