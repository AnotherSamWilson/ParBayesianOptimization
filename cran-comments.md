
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
#### Meta
Package was removed because suggested package was not available on checking machine, which threw a warning when vignettes were built. Made vignettes and examples execution conditional on availability of suggested package. This doesn't affect the readability or educational value of the vignettes or examples.

#### Documentation
* Added missing value fields to .Rd files of exported functions, and improved the documentation of existing value fields. 
* Added testable examples to all exported functions that were missing any. 
* Reset any options that were changed by vignettes.
