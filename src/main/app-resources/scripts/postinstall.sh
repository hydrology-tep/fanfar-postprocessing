#!/bin/bash

#
# Add R libraries/packages and other necessary SW components to the develop and run-time environment.
# Prefereably, install the main part of the major SW components first via yum to reduce any
# incompability that may arise when conda upgrades/downgrades or installs additional library
# dependecies. And not all libraries/packages are available from the conda channels.
# If more SW components have been added, also add the installation dependency to pom.xml and the
# user guide for setting up a sandbox environment.
#

/opt/anaconda/bin/conda install -y --file /application/dependencies/R/packages.list

# May need to setup a separate conda environment when using cairo and dependency to jpeg=9 since gdal seems to require jpeg=8
/opt/anaconda/bin/conda create --name cairo-env --file /application/dependencies/R/cairo-env.list
