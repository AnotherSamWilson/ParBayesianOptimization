
## Test environments
* local Windows 10 x64, R 3.5.2
* ubuntu 14.04.5 (on travis-ci), R 3.5.2

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are no downstream dependencies.

## Changes
* Made %dopar% preschedule FALSE 12/6/2018
* updateGP no longer runs in parallel to fix issues with macOS 2/24/2019
* Reduced R version depencency to 3.4. 2/24/2019