# -*- coding: utf-8 -*-

"""Top-level package for BIGSdb-attributor."""

import matplotlib

__author__ = """Melissa Jansen van Rensburg and Katriel Cohn-GOrdon"""
__email__ = 'm.jansenvr@imperial.ac.uk'
__version__ = '0.1.0'

# This has to go here, because it gets set globally the moment that you import pyplot and the
# default doesn't work if you are using a non-framework install of Python (i.e. the normal way of
# using Python in a virtualenv on macOS). This is the first thing that Python will execute when
# running anything from this package, so by putting this here we're safe from future pyplot imports.
matplotlib.use('Agg')
