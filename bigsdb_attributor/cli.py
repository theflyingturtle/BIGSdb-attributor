# -*- coding: utf-8 -*-
import argparse
import logging
import os
import shutil
import sys

import coloredlogs


from .bigsdb_attributor import read_test_and_ref_files,\
    validate_and_fixup,\
    postprocess, \
    runners


def parse_args():
    parser = argparse.ArgumentParser(
        description='Takes BIGS data exports for data and reference sets, '
        'converts to STRUCTURE/iSource format, and runs STRUCTURE/iSource for '
        'host attribution.  Input spreadsheets MUST have same headers.',
    )
    parser.add_argument(
        '--datafile',
        required=True,
        help='BIGSdb data export of isolates to be analysed in xlsx format',
    )
    parser.add_argument(
        '--reffile',
        required=True,
        help='BIGSdb data export of reference set in xlsx format',
    )
    parser.add_argument(
        '--output-directory',
        help='output directory to create',
        default='output',
    )
    parser.add_argument(
        '--overwrite',
        help='Overwrite output directory if it already exists',
        action='store_true',
    )
    parser.add_argument(
        '--logfile',
        help='name of log file (default: %(default)s)',
        default='Prep.log',
    )
    parser.add_argument(
        '--sourcelookup',
        required=True,
        help='CSV file containing two columns mapping allowed source values'
        'from PubMLST to aggregated attribution source.  Headers must be'
        "'original' and 'aggregated' in that order.",
    )
    parser.add_argument(
        '--mode',
        help='Attribution program to use',
        required=True,
        choices=[
            'structure',
            'isource',
        ],
    )
    parser.add_argument(
        '--attributor-path',
        help='Full path to STRUCTURE or iSource',
        required=True,
    )

    return parser.parse_args()


def setup_output_directory(output, overwrite):
    if os.path.isdir(output):
        if overwrite:
            logging.info(
                "Directory called '%s' already exists. Clearing it.",
                output,
            )
            shutil.rmtree(output)
        else:
            logging.fatal(
                "Directory called '%s' already exists, choose a new name"
                'or use the overwrite option.',
                output,
            )
            sys.exit(1)

    logging.debug("Creating new directory called '%s'", output)
    os.mkdir(output)


def run(args, combined):
    logging.info('Stop! Population genetics time.')
    data, mapping = runners[args.mode].prepare(combined, args.sourcelookup)
    data_file = os.path.join(args.output_directory, 'runner-input.tsv')
    data.to_csv(data_file, sep='\t')

    logging.debug('Running...')
    results_path = runners[args.mode].run(
        data_file,
        label=1,
        # -1 because we don't want to count reference (i.e. 0) as a pop
        max_populations=len(mapping) - 1,
        output_directory=args.output_directory,
        executable=args.attributor_path,
    )

    logging.debug('Run finished; parsing results')
    inferred_ancestry = runners[args.mode].parse(results_path, mapping)
    return inferred_ancestry


def main():
    # Initial setup
    # or logging.basicConfig(level=logging.DEBUG)
    coloredlogs.install(level='DEBUG')
    logging.captureWarnings(True)
    args = parse_args()
    setup_output_directory(args.output_directory, args.overwrite)
    fh = logging.FileHandler(os.path.join(args.output_directory, args.logfile))
    logging.getLogger().addHandler(fh)

    # Read and validate data
    testdata, refdata = read_test_and_ref_files(args.datafile, args.reffile)
    # only STRUCTURE can handle missing data
    drop_missing = (args.mode != 'structure')
    combined = validate_and_fixup(testdata, refdata, drop_missing=drop_missing)

    # Go go gadget population genetics
    inferred_ancestry = run(args, combined)

    # Write inferred ancestries to CSV
    inferred_ancestry_path = os.path.join(
        args.output_directory, 'inferred-ancestry.csv',
    )
    inferred_ancestry.to_csv(inferred_ancestry_path)

    # Log location of CSV containing inferred ancestries of test isolates
    logging.info(
        'Inferred ancestries for test isolates extracted and processed. '
        'Saved as %s. '
        'Text and visual summaries of results follow.',
        inferred_ancestry_path,
    )

    postprocess(testdata, inferred_ancestry, args.output_directory)


if __name__ == '__main__':
    main()
