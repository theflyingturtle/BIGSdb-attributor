import argparse
import coloredlogs
import logging
import os
import shutil
import sys

from .bigsdb_attributor import read_and_validate, prepare_for_structure
from bigsdb_attributor.structure import runner as structure_runner, parser as structure_parser


def parse_args():
    parser = argparse.ArgumentParser(
        description="Takes BIGS data exports for data and reference sets, "
        "converts to STRUCTURE/iSource format, and runs STRUCTURE/iSource for "
        "host attribution.  Input spreadsheets MUST have same headers.")
    parser.add_argument(
        "--datafile",
        required=True,
        help="BIGSdb data export of isolates to be analysed in xlsx format")
    parser.add_argument(
        "--reffile",
        required=True,
        help="BIGSdb data export of reference set in xlsx format")
    parser.add_argument(
        "--output-directory",
        help="output directory to create",
        default="output")
    parser.add_argument(
        "--overwrite",
        help="Overwrite output directory if it already exists",
        action="store_true")
    parser.add_argument(
        "--logfile",
        help="name of log file (default: %(default)s)",
        default="Prep.log")
    parser.add_argument(
        "--sourcelookup",
        required=True,
        help="CSV file containing two columns mapping allowed source values"
        "from PubMLST to aggregated attribution source.  Headers must be"
        "'original' and 'aggregated' in that order.")
    parser.add_argument(
        "--mode",
        help="Attribution program to use",
        required=True,
        choices=[
            "structure",
            'isource'])
    parser.add_argument(
        "--attributor-path",
        help="Full path to STRUCTURE or iSource",
        required=True)

    return parser.parse_args()


def setup_output_directory(output, overwrite):
    if os.path.isdir(output):
        if overwrite:
            logging.info(
                "Directory called '%s' already exists. Clearing it.",
                output)
            shutil.rmtree(output)
        else:
            logging.fatal(
                "Directory called '%s' already exists, choose a new name"
                "or use the overwrite option.",
                output)
            sys.exit(1)

    logging.debug("Creating new directory called '%s'", output)
    os.mkdir(output)


def main():
    # Initial setup
    coloredlogs.install(level='DEBUG')  # or logging.basicConfig(level=logging.DEBUG)
    logging.captureWarnings(True)
    args = parse_args()
    setup_output_directory(args.output_directory, args.overwrite)
    fh = logging.FileHandler(os.path.join(args.output_directory, args.logfile))
    logging.getLogger().addHandler(fh)

    # Read and validate data
    combined = read_and_validate(args.datafile, args.reffile)

    # Write STRUCTURE data frame to file
    logging.info("Stop! STRUCTURE time")
    structured = prepare_for_structure(combined, args.sourcelookup)
    structure_file = os.path.join(args.output_directory, "STRUCTUREInput.tsv")
    structured.to_csv(structure_file, sep="\t")
    results_path = structure_runner.run(structure_file,
                                        label=1,
                                        max_populations=structured.iloc[:, 0].nunique(),
                                        output_directory=args.output_directory)

    log.info("STRUCTURE finished; parsing results")
    inferred_ancestry = structure_parser.parse(results_path)["InferredAncestry"]
    import ipdb
    ipdb.set_trace()


if __name__ == '__main__':
    main()
