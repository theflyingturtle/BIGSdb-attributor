# -*- coding: utf-8 -*-

import bidict
import logging
import os
import pandas as pd
import re
import sys
import textwrap

from matplotlib import pyplot as plt
from bigsdb_attributor.helpers import colour_of

"""Written by Melissa Jansen van Rensburg and Katriel Cohn-Gordon (2017).

Takes test and reference sets from BIGSdb, converts to STRUCTURE/iSource
format, runs STRUCTURE/iSource, and parses outputs.

"""


def read_test_and_ref_files(testdata, refdata):
    # Open data and reference files
    logging.debug('Opening %s (test data)', testdata)
    test_data = pd.read_excel(testdata, index_col=None)
    test_data['received_date'] = pd.to_datetime(
        test_data['received_date'], dayfirst=True,
    )
    if 'private_isolation_date' in test_data:
        test_data['private_isolation_date'] = pd.to_datetime(
            test_data['private_isolation_date'], dayfirst=True,
        )
    else:
        # TODO (if not present, copy from received or isolation date)
        logging.warn(
            'private_isolation_date column not found, using isolation date'
            ' instead',
        )
        test_data['private_isolation_date'] = pd.to_datetime(
            test_data['isolation_date'], dayfirst=True,
        )

    logging.debug('Opening %s (reference set)', refdata)
    ref = pd.read_excel(refdata, index_col=None)
    ref['received_date'] = pd.to_datetime(ref['received_date'], dayfirst=True)

    return test_data, ref


def validate_and_fixup(test, ref):
    # Merge dataframes and indicate source of isolate (data vs reference).
    # Flatten MultiIndex.
    combined_df = pd.concat(
        [test, ref], keys=[
            'test', 'ref',
        ], axis=0,
    ).reset_index()
    # Rename former MultiIndex column
    combined_df.rename(columns={'level_0': 'dataset'}, inplace=True)

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
        'clonal_complex (MLST)',
    ]
    required_fields = prefix + MLST
    missing_headers = [
        field for field in required_fields if field not in combined_df.columns
    ]
    if not missing_headers:
        logging.debug('All required fields present.  Moving on...')
    else:
        logging.fatal(
            '\n Could not find headers corresponding to {}. Please try again.'
            .format(missing_headers),
        )
        sys.exit(1)

    # Drop rows with mixed allelic profiles (search for ; in gene columns)
    # Find rows with a semi-colon in MLST columns
    def has_semicolon(series):
        return series.astype(str).str.contains(';')
    mixed_profiles = combined_df[
        MLST[:-2]
    ].apply(has_semicolon).any(axis='columns')
    # Drop these rows and log the ids removed
    if mixed_profiles.any():
        ids_to_drop = combined_df[mixed_profiles]['id'].astype(
            str,
        ).str.cat(sep=', ')
        logging.warn(
            'Mixed profiles detected, dropping ids corresponding to %s', ids_to_drop,
        )
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
        logging.debug(
            'No duplicate ids detected in test and reference datasets.',
        )
    else:
        logging.warning(
            'Duplicate id(s) were detected in test and reference datasets.',
        )

    return combined_df


def prepare_for_structure(combined_df, sourcelookup):
    # Create the new STRUCTURE data frame
    structure_df = pd.DataFrame()

    # Copy across relevant data and relabel columns for STRUCTURE
    structure_df[[
        'Label', 'PopData', 'PopFlag', 'Locus_1', 'Locus_2', 'Locus_3',
        'Locus_4', 'Locus_5', 'Locus_6', 'Locus_7',
    ]] = \
        combined_df[[
            'id', 'source', 'dataset', 'aspA', 'glnA', 'gltA',
            'glyA', 'pgm', 'tkt', 'uncA',
        ]]

    # Get PopData from source
    logging.debug(
        "Extracted PopData from 'source' column using host source groups defined in %s",
        sourcelookup,
    )

    # Read in CSV containing look-up table of BIGSdb source fields to host source
    logging.debug('Reading %s', sourcelookup)
    source_xref = pd.read_csv(sourcelookup, index_col=0)

    # Map PopData in STRUCTURE dataframe using look-up table
    modified_structure_df = structure_df['PopData'].map(
        source_xref['aggregated'],
    )
    structure_df['PopData'] = modified_structure_df

    # Convert PopData to integers
    logging.debug(
        'PopData text converted to integers (required by STRUCTURE):',
    )

    # Summarise unique host sources and assign an integer
    integers = bidict.bidict()
    populations = structure_df.groupby('PopFlag')['PopData'].unique().to_dict()
    for v, k in enumerate(populations['ref'], start=1):
        integers[str(k)] = str(v)

    # Assign everything in the test set to 0.
    # (0 is an invalid number for STRUCTURE, but that's OK because it won't read
    # this zero, because it only appears in the test set which has PopFlag set to 0.)
    test_populations = {k: 0 for k in populations['test']}
    assert not(test_populations.keys() & integers.keys()), \
        'PopData appears in both ref and test population.'
    integers.update(test_populations)

    # Replace string in PopData with corresponding integer
    for k, v in integers.items():
        logging.debug('\t{:>20}: {}'.format(k, v))

    # Update STRUCTURE data frame (need .get since `not isinstance(bidict, dict)`,
    # which confuses pandas type sniffing)
    structure_df['PopData'] = structure_df['PopData'].map(integers.get)

    # Set PopFlag based on original dataset (i.e. ON for refs and OFF for tests)
    structure_df.replace({'PopFlag': {'ref': 1, 'test': 0}}, inplace=True)

    # Make sure all values are integers
    structure_df = structure_df.astype(int)

    # Remove Pandas index header and set to "Label"
    structure_df.set_index('Label', inplace=True)

    # Remove headers for "Label", "PopData", and "PopFlag"
    locus = re.compile('Locus_\d+')
    structure_df.columns = [
        c if locus.match(
            c,
        ) else '' for c in structure_df.columns
    ]
    structure_df.index.name = ''

    return structure_df, integers


def postprocess(test_data, ancestries, outdir):
    # Merge original test set dataframe with inferred ancestries dataframe; specify columns with attribution data
    # Set index for test set to prepare for merge
    test_data.set_index('id', inplace=True)

    # Merge data
    anc_data = pd.merge(
        test_data, ancestries, how='inner',
        right_index=True, left_index=True,
    )

    # Differentiate Oxfordshire and Tyneside isolates
    # Add column indicating site based on isolate name
    anc_data['site'] = anc_data['isolate'].str[:3]

    # Convert site id to full name
    anc_data['site'] = anc_data['site'].replace(
        {'OXC': 'Oxfordshire', 'NWC': 'Tyneside'},
    )

    # Get list of sources ordered from most to least common
    total_means = anc_data[ancestries.columns].mean(
    ).sort_values(ascending=False)

    # Fill dates from private data
    anc_data['received_date'].fillna(
        anc_data['private_isolation_date'], inplace=True,
    )

    # Refresh the year column
    anc_data['year'] = anc_data['received_date'].dt.year

    # Get time period (in years)
    start = anc_data['received_date'].min().strftime('%B %Y')
    finish = anc_data['received_date'].max().strftime('%B %Y')

    # Log table of probabilities
    no_test_isolates = len(anc_data)

    # Generate corresponding bar graph coloured according to source
    filename = 'OverallAttribution.svg'
    title_total = 'Estimated proportion of human disease isolates attributed to animal and environmental '\
                  'sources.  Probabilistic assignment of {} isolates collected between {} and {} in Oxfordshire '\
                  'was carried out using the noadmixture model in STRUCTURE.'
    title_total = title_total.format(no_test_isolates, start, finish)
    title_total = '\n'.join(textwrap.wrap(title_total))
    plot(
        total_means,
        os.path.join(outdir, filename),
        xlabel='Sources',
        ylabel='Proportion of isolates',
        title=title_total,
        kind='bar',
    )
    logging.debug(
        'Probabilistic assignment of %s isolates collected in Oxfordshire between %s and %s:\n%s'
        '\nCorresponding graph saved as %s.',
        no_test_isolates,
        start,
        finish,
        total_means.to_string(),
        filename,
    )


def plot(
    df,
    filename,
    xlabel='',
    ylabel='',
    title='',
    **plot_args
):

    fig = df.plot(color=list(df.index.map(colour_of)), **plot_args)

    # Axis labels
    fig.set_xlabel(xlabel)
    fig.set_ylabel(ylabel)

    # Axis ticks
    fig.xaxis.set_ticks_position('none')
    fig.yaxis.set_ticks_position('left')

    # Figure title
    plt.title(title)

    # Rotate labels on x-axis
    plt.xticks(rotation='45', ha='right')

    # Write file
    plt.savefig(filename, bbox_inches='tight', dpi=300, format='svg')

    return fig
