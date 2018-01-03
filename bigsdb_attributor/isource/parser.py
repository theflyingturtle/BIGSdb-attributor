# -*- coding: utf-8 -*-
"""iSource results files aren't great, but let's try to make something of
them."""

import glob
import logging
import os

import pandas as pd

logger = logging.getLogger('iSource.parser')

try:
    from pandas.errors import EmptyDataError
except ImportError:
    # old pandas version
    from pandas.io.common import EmptyDataError


def check_run_succeeded(g_path):
    if not os.path.isfile(g_path):
        # Results don't exist. Let's see if we can find the cause.
        for logfile in glob.glob(os.path.join(
                os.path.dirname(g_path), '*.log',
        )):
            if os.path.isfile(logfile):
                break
        else:
            raise IOError(
                "Path {} does not exist; I couldn't find a log file".format(
                    g_path,
                ),
            )

        with open(logfile) as loglines:
            errors = '\n'.join(
                line for line in loglines if line.startswith('ERROR')
            )
        if errors:
            raise IOError(
                'Path {} does not exist. Probable cause: \n{}'.format(
                    g_path, errors,
                ),
            )
        else:
            raise IOError(
                'Path {} does not exist and I have no idea why.'.format(
                    g_path,
                ),
            )


def parse(g_path):
    check_run_succeeded(g_path)
    # TODO(katriel) fix ERROR:self_attribution:No columns to parse from file
    try:
        g_values = pd.read_csv(g_path, sep='\t', index_col=None, header=None)
    except EmptyDataError:
        logging.error('No columns to read from file %s', g_path)
        raise

    # Expect an f_foo in the same directory as g_foo
    f_path = os.path.join(
        os.path.dirname(g_path),
        os.path.basename(g_path).replace(
            'g_',
            'f_',
            1,
        ),
    )
    assert os.path.isfile(f_path), 'Expecting f_'
    with open(f_path) as f:
        no_populations = next(f).count('\t')
        open(
            '/tmp/bar',
            'a',
        ).write(
            '{}: {}, {}\n'.format(
                g_path,
                g_values.shape,
                no_populations,
            ),
        )
        assertion = 'isource shapes for {} went wrong: g_values has shape'\
                    '{} which is inconsistent with having {} populations'
        assert not (
            g_values.shape[1] %
            no_populations
        ), assertion.format(
            g_path, g_values.shape, no_populations,
        )
    no_test_isolates = g_values.shape[1] / no_populations
    g_values = pd.DataFrame(
        g_values.T[0].reshape(
            (no_test_isolates, no_populations),
        ),
    )

    # iSource counts from 0 but we relabelled from 1. I hope this is right...
    # Looks like this is wrong, how can we be sure that iSource is labelling
    # things correctly? Need to figure out the order in their file format
    g_values.columns += 1

    # Rename test isolates to their original labels
    ixmap_path = os.path.join(
        os.path.dirname(g_path),
        'isource_input_to_label.csv',
    )
    ixmap = pd.Series.from_csv(ixmap_path)
    assert ixmap.value_counts()[0] == len(
        g_values,
    ), 'isource index mapping shapes went wrong'
    g_values.index = ixmap[ixmap == 0].index

    return {'InferredAncestry': g_values}
