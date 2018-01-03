=================
BIGSdb-attributor
=================


.. image:: https://img.shields.io/pypi/v/bigsdb_attributor.svg
        :target: https://pypi.python.org/pypi/bigsdb_attributor

.. image:: https://img.shields.io/travis/theflyingturtle/bigsdb_attributor.svg
        :target: https://travis-ci.org/theflyingturtle/bigsdb_attributor

.. image:: https://readthedocs.org/projects/bigsdb-attributor/badge/?version=latest
        :target: http://bigsdb-attributor.readthedocs.io/en/latest/?badge=latest
        :alt: Documentation Status

.. image:: https://pyup.io/repos/github/theflyingturtle/BIGSdb-attributor/shield.svg
     :target: https://pyup.io/repos/github/theflyingturtle/BIGSdb-attributor/
     :alt: Updates

.. image:: https://pyup.io/repos/github/theflyingturtle/BIGSdb-attributor/python-3-shield.svg
     :target: https://pyup.io/repos/github/theflyingturtle/BIGSdb-attributor/
     :alt: Python 3


Attribute human disease isolates to host sources using STRUCTURE or iSource. Currently work in progress.

Installing
----------

```
git clone https://github.com/theflyingturtle/BIGSdb-attributor
python3 BIGSdb-attributor/setup.py install
bigsdb_attributor --help
```

Developing
----------

```
git clone https://github.com/theflyingturtle/BIGSdb-attributor
source .../path/to/virtualenv/bin/activate[.fish]
pip install -r BIGSdb-attributor/requirements-dev.txt
python3 BIGSdb-attributor/setup.py develop
bigsdb_attributor --help
```

* Free software: GNU General Public License v3

Credits
---------

This package was created with Cookiecutter_ and the `audreyr/cookiecutter-pypackage`_ project template.

.. _Cookiecutter: https://github.com/audreyr/cookiecutter
.. _`audreyr/cookiecutter-pypackage`: https://github.com/audreyr/cookiecutter-pypackage
