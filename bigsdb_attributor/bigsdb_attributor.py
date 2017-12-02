# -*- coding: utf-8 -*-

import logging
import pandas as pd

"""Written by Melissa Jansen van Rensburg and Katriel Cohn-Gordon (2017).

Takes test and reference sets from BIGSdb, converts to STRUCTURE/iSource format, runs STRUCTURE/iSource, and parses outputs.
"""


def read_test_and_ref_files(testdata, refdata):
    # Open data and reference files
    logging.info("Opening %s (test data)", testdata)
    test_data = pd.read_excel(testdata, index_col=None)
    test_data['received_date'] = pd.to_datetime(test_data['received_date'], dayfirst=True)
    test_data['private_isolation_date'] = pd.to_datetime(test_data['private_isolation_date'], dayfirst=True)
    # TODO (if not present, copy from received or isolation date)

    logging.info("Opening %s (reference set)", refdata)
    ref = pd.read_excel(reffile, index_col=None)
    ref['received_date'] = pd.to_datetime(ref['received_date'], dayfirst=True)

    # Merge dataframes and indicate source of isolate (data vs reference).  Flatten MultiIndex.
    combined_df = pd.concat([test_data, ref], keys=['test','ref'], axis=0).reset_index()
    return combined_df
    

def read_and_validate(testdata, refdata):
    combined = read_test_and_ref_files(testdata, refdata)
    import ipdb; ipdb.set_trace()


def main():
    read_test_and_ref_files()
    run_attribution_alg_and_parse_outputs()
    graph()
