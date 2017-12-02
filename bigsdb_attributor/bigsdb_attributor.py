# -*- coding: utf-8 -*-

import logging
import pandas as pd
import sys

"""Written by Melissa Jansen van Rensburg and Katriel Cohn-Gordon (2017).

Takes test and reference sets from BIGSdb, converts to STRUCTURE/iSource format, runs STRUCTURE/iSource, and parses outputs.
"""


def read_test_and_ref_files(testdata, refdata):
    # Open data and reference files
    logging.info("Opening %s (test data)", testdata)
    test_data = pd.read_excel(testdata, index_col=None)
    test_data['received_date'] = pd.to_datetime(test_data['received_date'], dayfirst=True)
    if 'private_isolation_date' in test_data:
        test_data['private_isolation_date'] = pd.to_datetime(test_data['private_isolation_date'], dayfirst=True)
    else:
        # TODO (if not present, copy from received or isolation date)
        logging.warn("private_isolation_date column not found, using isolation date instead")
        test_data['private_isolation_date'] = pd.to_datetime(test_data['isolation_date'], dayfirst=True)

    logging.info("Opening %s (reference set)", refdata)
    ref = pd.read_excel(refdata, index_col=None)
    ref['received_date'] = pd.to_datetime(ref['received_date'], dayfirst=True)

    # Merge dataframes and indicate source of isolate (data vs reference).  Flatten MultiIndex.
    combined_df = pd.concat([test_data, ref], keys=['test','ref'], axis=0).reset_index()
    combined_df.rename(columns={'level_0':'dataset'}, inplace=True)  # Rename former MultiIndex column
    return combined_df


def validate_and_fixup(combined_df):
    # Quit if any essential headers are missing
    prefix = ['year', 'month', 'received_date', 'age_yr', 'sex']
    MLST = ['aspA', 'glnA', 'gltA', 'glyA', 'pgm', 'tkt', 'uncA', 'ST (MLST)', 'clonal_complex (MLST)']
    required_fields = prefix + MLST

    missing_headers = [field for field in required_fields if field not in combined_df.columns]
    if not missing_headers:
        logging.info("\nAll required fields present.  Moving on...")
    else:
        logging.fatal("\n Could not find headers corresponding to {}. Please try again.".format(missing_headers))
        sys.exit(1)




def read_and_validate(testdata, refdata):
    combined = read_test_and_ref_files(testdata, refdata)
    combined = validate_and_fixup(combined)
    import ipdb; ipdb.set_trace()


def main():
    read_test_and_ref_files()
    run_attribution_alg_and_parse_outputs()
    graph()
