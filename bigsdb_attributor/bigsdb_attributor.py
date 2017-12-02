# -*- coding: utf-8 -*-

"""Written by Melissa Jansen van Rensburg and Katriel Cohn-Gordon (2017).

Takes test and reference sets from BIGSdb, converts to STRUCTURE/iSource format, runs STRUCTURE/iSource, and parses outputs.
"""

def main():
	read_test_and_ref_files()
	run_attribution_alg_and_parse_outputs()
	graph()
