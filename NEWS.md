## ParBayesianOptimization 1.0.0


# Major Changes
* Depreciated BayesianOptimization(). New package structure uses bayesOpt() 2020-02-10
* Added a `NEWS.md` file to track changes to the package.
* Package was accepted by CRAN on 12/3/2018  
* Implemented plotProgress 10/01/2019
* Depreciated beta parameter. Automatically set to 0. 10/01/2019



# Minor Changes
* Made %dopar% preschedule FALSE 12/6/2018
* Minor changes to documentation 12/6/2018
* updateGP no longer runs in parallel to fix issues with macOS 2/24/2019
* Reduced R version depencency to 3.4. 2/24/2019
* Improved error handling 3/9/2019
* Sink now works on Solaris 7/22/2019
* Same kernel is now provided to GauPro. This allows re-scaling every iteration 10/01/2019
