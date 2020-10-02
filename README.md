## High Frequency Data Quality Checks

This package brings a series of convenience functions to monitor data quality during the data collection when running a survey with kobotoolbox (or any `xlsform` compatible platform). 

Those can be performed periodically during the data collection process to check for possible errors and provide meaningful inputs to the enumerators. All these functions do not have to be ran at the same period of time. They are provided there to help data supervisor to build reports.

The package an adaptation in R of the [Stata package for running high-frequency checks on research data at Innovations for Poverty Action](https://github.com/PovertyAction/high-frequency-checks)


The package vignette offers a practical [data quality monitoring template](articles/HFC.html)

Install from github with `devtools::install_github("unhcr/HighFrequencyChecks")`.

The packages includes a series of controls calling for:

 * Corrective actions:

   * Correct set-up of data collection devices and encoding of the forms
   * Data collected according the sampling plan

 * Pro-active actions:
   * Ensuring enumerators rigorous work standards
   * Promoting enumerators productivity
 
#### Building Site documentation 

`devtools::document()`

`devtools::check(document = FALSE)`

`pkgdown::build_site()`


If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/PYannick/HighFrequencyChecks/issues). 
