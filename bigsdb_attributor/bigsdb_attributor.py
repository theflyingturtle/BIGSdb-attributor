# -*- coding: utf-8 -*-

import bidict
import collections
import logging
import os
import pandas as pd
import re
import sys
import textwrap

from matplotlib import pyplot as plt
from bigsdb_attributor.helpers import colour_of
from bigsdb_attributor.helpers import plot_individual_ancestry
from bigsdb_attributor.structure import parser as structure_parser
from bigsdb_attributor.structure import runner as structure_runner
from bigsdb_attributor.isource import parser as isource_parser
from bigsdb_attributor.isource import runner as isource_runner

"""Written by Melissa Jansen van Rensburg and Katriel Cohn-Gordon (2017).

Takes test and reference sets from BIGSdb, converts to STRUCTURE/iSource
format, runs STRUCTURE/iSource, and parses outputs.

"""


def parse_structure(path, mapping):
    inferred_ancestry = structure_parser.parse(path)['InferredAncestry']
    inferred_ancestry.columns = inferred_ancestry.columns.astype(
        str,
    ).map(mapping.inv.get)
    return inferred_ancestry


def parse_isource(path, mapping):
    logging.debug('Hackily parsing iSource results file %s', path)
    groups, rows = mapping
    inferred_ancestry = isource_parser.parse(path)['InferredAncestry']
    inferred_ancestry.columns = inferred_ancestry.columns.astype(
        str,
    ).map(groups.inv.get)

    assert len(rows) == len(
        inferred_ancestry,
    ), 'isource index mapping shapes went wrong'

    inferred_ancestry.index = inferred_ancestry.index.map(rows.get)
    inferred_ancestry.index.name = 'ST'
    return inferred_ancestry


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


def validate_and_fixup(test, ref, drop_missing=False):
    # Merge dataframes and indicate source of isolate (data vs reference).
    # Flatten MultiIndex.
    combined_df = pd.concat(
        [test, ref], keys=[
            'test', 'ref',
        ], axis=0,
    ).reset_index()
    # Rename former MultiIndex column
    combined_df.rename(columns={'level_0': 'dataset'}, inplace=True)

    # TO DO - revise essential headers - no month header; only need time headers to do time analyses
    # - simplest would be to do overall attribution and then have a breakdown over time option that
    # would require at least year and/or DOI

    # Quit if any essential headers are missing
    prefix = ['year', 'received_date', 'age_yr', 'sex']
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

    if drop_missing:
        logging.warn('BAD JUJU')
        isolates_with_missing_allele = (
            combined_df[[
                'aspA', 'glnA', 'gltA', 'glyA', 'pgm', 'tkt', 'uncA',
            ]].astype(str) == '-9'
        ).any(axis=1)
        combined_df = combined_df[~isolates_with_missing_allele]

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


def remap_col_by_sourcelookup(df, group, col, sourcelookup):
    logging.debug(
        'Remapping column %s into integers according to its %s, aggregating by %s',
        col,
        group,
        sourcelookup,
    )

    logging.debug('Reading %s', sourcelookup)
    source_xref = pd.read_csv(sourcelookup, index_col=0)

    df[col] = df[col].map(
        source_xref['aggregated'],
    )

    logging.debug('Remapped column %s to integers', col)
    # TO DO - script fails if source is not filled in (specifically for humans) - need to completely
    # fill human source column (perhaps warn if blank/not human); need to fail if ref set includes
    # blanks for source
    integers = bidict.bidict()
    populations = df.groupby(group)[col].unique().to_dict()
    for v, k in enumerate(populations['ref'], start=1):
        integers[str(k)] = str(v)

    # Assign everything in the test set to 0.
    # (0 is an invalid number for STRUCTURE, but that's OK because it won't read
    # this zero, because it only appears in the test set.)
    test_populations = {k: 0 for k in populations['test']}
    assert not(test_populations.keys() & integers.keys()), \
        ('%s appears in both ref and test population.' % col)
    integers.update(test_populations)

    # Replace col with corresponding integer
    for k, v in integers.items():
        logging.debug('\t{:>20}: {}'.format(k, v))
    df[col] = df[col].map(integers.get)

    return integers


def prepare_for_isource(combined_df, sourcelookup):
    # Create the new iSource data frame
    iSource_df = pd.DataFrame()

    # Copy across relevant data and relabel columns for iSource
    iSource_df[[
        'id', 'ST', 'aspA', 'glnA', 'gltA',
        'glyA', 'pgm', 'tkt', 'uncA', 'group', 'dataset',
    ]] = \
        combined_df[[
            'id', 'ST (MLST)', 'aspA', 'glnA', 'gltA',
            'glyA', 'pgm', 'tkt', 'uncA', 'source', 'dataset',
        ]]

    integers = remap_col_by_sourcelookup(
        iSource_df, group='dataset', col='group', sourcelookup=sourcelookup,
    )

    del iSource_df['dataset']
    iSource_df.set_index('ST', inplace=True)
    iSource_df = iSource_df.astype(int)

    test_isolates = iSource_df[iSource_df['group'] == 0]
    row_number_to_id = dict(enumerate(test_isolates['id']))
    del iSource_df['id']

    return iSource_df, (integers, row_number_to_id)


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

    integers = remap_col_by_sourcelookup(
        structure_df, group='PopFlag', col='PopData', sourcelookup=sourcelookup,
    )

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


# TODO(katriel) these should really be on Structure/iSource classes with a common interface, so that
# we can store state (e.g. population mappings) without having to pass it back and forth through the
# cli. Future work.
mode = collections.namedtuple('mode', 'prepare run parse'.split())
runners = dict(
    structure=mode(
        prepare=prepare_for_structure,
        run=structure_runner.run,
        parse=parse_structure,
    ),
    isource=mode(
        prepare=prepare_for_isource,
        run=isource_runner.run,
        parse=parse_isource,
    ),
)


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
    # start = anc_data['received_date'].min().strftime('%B %Y')
    # finish = anc_data['received_date'].max().strftime('%B %Y')

    # Log table of probabilities
    no_test_isolates = len(anc_data)

    # Generate corresponding bar graph coloured according to source

    # TO DO append "Probabilistic assignments carried out using noadmixture model in
    # STRUCTURE/iSource" to figure title as applicable
    filename = 'OverallAttribution.svg'
    title_total = 'Estimated proportion of {} human disease isolates attributed to animal and '\
                  'environmental sources.'
    title_total = title_total.format(no_test_isolates)
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
        'Probabilistic assignment of %s human disease isolates to host sources:\n%s'
        '\nCorresponding graph saved as %s.',
        no_test_isolates,
        total_means.to_string(),
        filename,
    )

    # Graph assignment probabilities for individual isolates
    # TO DO position legend BELOW x-axis label
    # Save figures as pngs or svgs?
    filename = 'IndividualAncestries.svg'
    source_order = total_means.index.values.tolist()
    title_inds = 'Source probabilities for {} human disease isolates. Isolates are represented as '\
        'vertical bars coloured according to estimated probability for each source. '\
        'Isolates are ordered horizontally by most likely source. Sources are separated '\
        'by black vertical lines.'
    title_inds = title_inds.format(no_test_isolates)
    title_inds = '\n'.join(textwrap.wrap(title_inds))

    plot_individual_ancestry(
        anc_data,
        source_order,
        os.path.join(outdir, filename),
        title=title_inds,
        xlabel='Isolates',
        ylabel='Source probability',
    )

    logging.debug(
        'Graph of assignment probabilities for individual isolates saved as %s',
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
