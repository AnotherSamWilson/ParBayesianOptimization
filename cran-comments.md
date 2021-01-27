
## Test environments
* local Windows 10 x64, R 4.0.0
* Windows Server 2008 R2 SP1 32/64 bit (Rhub)
* Ubuntu Linux 16.04 LTS (Rhub)
* Fedora Linux (Rhub)
* Ubuntu 14.04.5, R 3.6.2 (travis-ci)


## R CMD check results
There were no errors or notes. Only warnings explained that I am the maintainer and the package is currently archived.

## Downstream dependencies
There are no downstream dependencies.

## Changes
Removed Plotly from dependencies - this package was causing a warning in check because it was not being used.
