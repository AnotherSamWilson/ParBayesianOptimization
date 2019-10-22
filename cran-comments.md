
## Test environments
* local Windows 10 x64, R 3.5.2
* Windows Server 2008 R2 SP1 32/64 bit (Rhub)
* Ubuntu 14.04.5, R 3.5.2 (travis-ci)
* Ubuntu Linux 16.04 LTS (Rhub)
* Fedora Linux (Rhub)


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are no downstream dependencies.

## Changes
Sink is removed on function exit.
Gaussian Process now uses beta length = # parameters.

## Comments
Provided option to plot progress.  
Depreciated beta parameter.  
Improved documentation (vignettes and function documentation)
