#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""The setup script."""

from setuptools import find_packages, setup

with open('README.rst') as readme_file:
    readme = readme_file.read()

with open('HISTORY.rst') as history_file:
    history = history_file.read()

requirements = [
    'bidict',
    'coloredlogs',
    'matplotlib==2.1.0',
    'pandas==0.21.0',
    'tqdm==4.19.4',
    'xlrd==1.1.0',
]

setup_requirements = [
    # TODO(theflyingturtle): put setup requirements
    # (distutils extensions, etc.) here
]

test_requirements = [
    # TODO: put package test requirements here
]

setup(
    name='bigsdb_attributor',
    version='0.1.0',
    description='Attribute human disease isolates to host sources'
    'using STRUCTURE or iSource',
    long_description=readme + '\n\n' + history,
    author='Melissa Jansen van Rensburg',
    author_email='m.jansenvr@imperial.ac.uk',
    url='https://github.com/theflyingturtle/bigsdb_attributor',
    packages=find_packages(),
    entry_points={
        'console_scripts': [
            'bigsdb_attributor=bigsdb_attributor.cli:main',
        ],
    },
    include_package_data=True,
    install_requires=requirements,
    license='GNU General Public License v3',
    zip_safe=False,
    keywords='bigsdb_attributor',
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
    ],
    test_suite='tests',
    tests_require=test_requirements,
    setup_requires=setup_requirements,
)
