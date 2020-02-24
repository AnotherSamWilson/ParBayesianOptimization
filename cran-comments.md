
## Test environments
* local Windows 10 x64, R 3.6.1
* Windows Server 2008 R2 SP1 32/64 bit (Rhub)
* Ubuntu Linux 16.04 LTS (Rhub)
* Fedora Linux (Rhub)
* Ubuntu 14.04.5, R 3.6.2 (travis-ci)


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are no downstream dependencies.

## Changes
Changed Gaussian Process package to DiceKriging. predict method is much faster.
Added errorHandling parameter - bayesOpt() and addIterations() should now return results no matter what, unless errorHandling = 'stop'
Added otherHalting parameter.
