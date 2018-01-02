# -*- coding: utf-8 -*-

import logging
import os
import pandas as pd
import re
import sys

"""Written by Melissa Jansen van Rensburg and Katriel Cohn-Gordon (2017).

Takes test and reference sets from BIGSdb, converts to STRUCTURE/iSource
format, runs STRUCTURE/iSource, and parses outputs.

"""


def read_test_and_ref_files(testdata, refdata):
    # Open data and reference files
    logging.debug("Opening %s (test data)", testdata)
    test_data = pd.read_excel(testdata, index_col=None)
    test_data['received_date'] = pd.to_datetime(
        test_data['received_date'], dayfirst=True)
    if 'private_isolation_date' in test_data:
        test_data['private_isolation_date'] = pd.to_datetime(
            test_data['private_isolation_date'], dayfirst=True)
    else:
        # TODO (if not present, copy from received or isolation date)
        logging.warn(
            "private_isolation_date column not found, using isolation date"
            " instead")
        test_data['private_isolation_date'] = pd.to_datetime(
            test_data['isolation_date'], dayfirst=True)

    logging.debug("Opening %s (reference set)", refdata)
    ref = pd.read_excel(refdata, index_col=None)
    ref['received_date'] = pd.to_datetime(ref['received_date'], dayfirst=True)

    # Merge dataframes and indicate source of isolate (data vs reference).
    # Flatten MultiIndex.
    combined_df = pd.concat([test_data, ref], keys=[
                            'test', 'ref'], axis=0).reset_index()
    # Rename former MultiIndex column
    combined_df.rename(columns={'level_0': 'dataset'}, inplace=True)
    return combined_df


def validate_and_fixup(combined_df):
    # Quit if any essential headers are missing
    prefix = ['year', 'month', 'received_date', 'age_yr', 'sex']
    MLST = [
        'aspA',
        'glnA',
        'gltA',
        'glyA',
        'pgm',
        'tkt',
        'uncA',
        'ST (MLST)',
        'clonal_complex (MLST)']
    required_fields = prefix + MLST
    missing_headers = [
        field for field in required_fields if field not in combined_df.columns]
    if not missing_headers:
        logging.debug("All required fields present.  Moving on...")
    else:
        logging.fatal(
            "\n Could not find headers corresponding to {}. Please try again."
            .format(missing_headers))
        sys.exit(1)

    # Drop rows with mixed allelic profiles (search for ; in gene columns)
    # Find rows with a semi-colon in MLST columns
    has_semicolon = lambda series: series.astype(str).str.contains(";")
    mixed_profiles = combined_df[MLST[:-2]].apply(has_semicolon).any(axis='columns')
    # Drop these rows and log the ids removed
    if mixed_profiles.any():
        ids_to_drop = combined_df[mixed_profiles]['id'].astype(str).str.cat(sep=', ')
        logging.warn("Mixed profiles detected, dropping ids corresponding to %s", ids_to_drop)
    combined_df = combined_df[~mixed_profiles]

    # Fill missing values
    combined_df = combined_df.fillna({
        'clonal_complex (MLST)': 'Unassigned',
        'ST (MLST)': 'Not determined',
        'year': 'Unknown',
        'month': 'Unknown',
        #    'received_date': 'Unknown',
        'source': 'Unknown',
        'aspA': '-9',
        'glnA': '-9',
        'gltA': '-9',
        'glyA': '-9',
        'pgm': '-9',
        'tkt': '-9',
        'uncA': '-9',
    })

    # Detect any ids present in both datasets (shouldn't have test isolates in ref dataset)
    n_duplicate_ids = len(combined_df[combined_df.duplicated('id')].index)
    if n_duplicate_ids == 0:
        logging.debug("No duplicate ids detected in test and reference datasets.")
    else:
        logging.warning("Duplicate id(s) were detected in test and reference datasets.")

    return combined_df


def prepare_for_structure(combined_df, sourcelookup):
    # Create the new STRUCTURE data frame
    structure_df = pd.DataFrame()

    # Copy across relevant data and relabel columns for STRUCTURE
    structure_df[["Label", "PopData", "PopFlag", "Locus_1", "Locus_2", "Locus_3",
                  "Locus_4", "Locus_5", "Locus_6", "Locus_7"]] = \
              combined_df[['id', 'source', 'dataset', 'aspA', 'glnA', 'gltA',
                           'glyA', 'pgm', 'tkt', 'uncA']]

    # Get PopData from source
    logging.debug("Extracted PopData from 'source' column using host source groups defined in %s",
                  sourcelookup)

    # Read in CSV containing look-up table of BIGSdb source fields to host source
    logging.debug("Reading %s", sourcelookup)
    source_xref = pd.DataFrame.from_csv(sourcelookup)

    # Map PopData in STRUCTURE dataframe using look-up table
    modified_structure_df = structure_df['PopData'].map(source_xref['aggregated'])
    structure_df['PopData'] = modified_structure_df

    # Convert PopData to integers
    logging.debug("PopData text converted to integers (required by STRUCTURE):")
    
    # Summarise unique host sources and assign an integer
    integers, reversed_integers = {}, {}
    populations = structure_df.groupby("PopFlag")['PopData'].unique().to_dict()
    for v, k in enumerate(populations['ref'], start=1):
        integers[str(k)] = str(v)
        reversed_integers[v] = str(k)

    # Assign everything in the test set to 0.
    # (0 is an invalid number for STRUCTURE, but that's OK because it won't read
    # this zero, because it only appears in the test set which has PopFlag set to 0.)
    test_populations = {k: 0 for k in populations['test']}
    assert not(test_populations.keys() & integers.keys()), \
        "PopData appears in both ref and test population."
    integers.update(test_populations)

    # Replace string in PopData with corresponding integer
    for k, v in integers.items():
        logging.debug("\t{:>20}: {}".format(k, v))

    # Update STRUCTURE data frame
    structure_df['PopData'] = structure_df['PopData'].map(integers)

    # Set PopFlag based on original dataset (i.e. ON for refs and OFF for tests)
    structure_df.replace({'PopFlag' : {'ref': 1, 'test': 0}}, inplace=True)
    
    # Make sure all values are integers
    modified_structure_df = structure_df.astype(int)
    structure_df = modified_structure_df

    # Remove Pandas index and set to "Label"
    structure_df.set_index("Label", inplace=True)

    # Remove headers for "Label", "PopData", and "PopFlag"
    locus = re.compile("Locus_\d+")
    structure_df.columns = [c if locus.match(c) else "" for c in structure_df.columns]
    structure_df.index.name = ""

    return structure_df, reversed_integers


def read_and_validate(testdata, refdata):
    combined = read_test_and_ref_files(testdata, refdata)
    combined = validate_and_fixup(combined)
    return combined
