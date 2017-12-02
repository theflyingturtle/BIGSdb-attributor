import argparse
import logging
import shutil
import sys
import os

from .bigsdb_attributor import read_and_validate


def parse_args():
    parser = argparse.ArgumentParser(description="Takes BIGS data exports for data and reference sets, converts to STRUCTURE/iSource format, and runs STRUCTURE/iSource for host attribution.  Input spreadsheets MUST have same headers.")
    parser.add_argument("--datafile", required=True, help="BIGSdb data export of isolates to be analysed in xlsx format")
    parser.add_argument("--reffile", required=True, help="BIGSdb data export of reference set in xlsx format")
    parser.add_argument("--output-directory", help="output directory to create", default="output")
    parser.add_argument("--overwrite", help="Overwrite output directory if it already exists", action="store_true")
    parser.add_argument("--logfile", help="name of log file (default: %(default)s)", default="Prep.log")
    parser.add_argument("--sourcelookup", required=True, help="CSV file containing two columns mapping allowed source values from PubMLST to aggregated attribution source.  Headers must be 'original' and 'aggregated' in that order.")
    parser.add_argument("--mode", help="Attribution program to use", required=True, choices=["structure", 'isource'])
    parser.add_argument("--attributor-path", help="Full path to STRUCTURE or iSource", required=True)

    return parser.parse_args()


def setup_output_directory(output, overwrite):
    if os.path.isdir(output):
        if overwrite:
            logging.info("Directory called '%s' already exists. Clearing it.", output)
            shutil.rmtree(output)
        else:
            logging.fatal("Directory called '%s' already exists, choose a new name or use the overwrite option.", output)
            sys.exit(1)

    logging.info("Creating new directory called '%s'", output)
    os.mkdir(output)


def main():
    logging.basicConfig(level=logging.DEBUG)
    print('hello')
    args = parse_args()
    setup_output_directory(args.output_directory, args.overwrite)
    fh = logging.FileHandler(os.path.join(args.output_directory, args.logfile))
    logging.getLogger().addHandler(fh)
    read_and_validate(args.datafile, args.reffile)

if __name__ == '__main__':
    main()
