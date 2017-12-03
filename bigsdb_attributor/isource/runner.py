import os
import subprocess
import sys

import pandas as pd

ISOURCE_RESULTS = "isource_results"


class ISourceRuntimeError(RuntimeError):
    pass


def run(data, label, max_populations, executable=None, output_directory=None):
    """Run iSource with the given data and options.

    `data` should be a pandas dataframe in the correct format for iSource.

    Returns the path to the output file generated by iSource.

    """
    # Try to find iSource binary
    if executable is None:
        raise ISourceRuntimeError("Could not find iSource binary.")

    if not os.path.isdir(output_directory):
        os.mkdir(output_directory)

    def path(f=""):
        os.path.join(output_directory, f)

    # Parse iSource input dataframe to extract various bits of data
    if not os.path.isfile(data):
        raise ISourceRuntimeError("Could not find file {}".format(data))

    with open(path("isource_runtime.log"), "w") as runtime:
        isource_p = subprocess.Popen([os.path.expanduser(executable),
                                      data,
                                      ISOURCE_RESULTS,
                                      "1000",  # number of iterations
                                      "1",  # thinning,
                                      "1",  # Dirichlet uniform prior
                                      ],
                                     stdout=runtime,
                                     stderr=sys.stderr,
                                     cwd=path())
        isource_p.communicate()

    return path("g_" + ISOURCE_RESULTS)
