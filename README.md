## High Frequency Data Quality Checks

This package brings a series of convenience functions to monitor data quality during the data collection when running a survey with kobotoolbox (or any `xlsform` compatible platform). 

Those can be performed periodically during the data collection process to check for possible errors and provide meaningful inputs to the enumerators. All these functions do not have to be ran at the same period of time. They are provided there to help data supervisor to build reports.

The package an adaptation in R of the [Stata package for running high-frequency checks on research data at Innovations for Poverty Action](https://github.com/PovertyAction/high-frequency-checks)


The package vignette offers a practical [data quality monitoring template](articles/HFC.html)

Install from github with `devtools::install_github("Edouard-Legoupil/HighFrequencyChecks")`.

The packages includes the following check:

### Enumerator Productivity
 * Number of surveys per day by enumerator
 * Build tracking sheet
 * Identify  enumerators with very low or high productivity
 * Identify  enumerators who pick up less than a certain number answers per specific questions
 * Identify enumerators who made a survey below a certain number of minutes
 * Assess number of survey per day of data collection
 * Assess number of survey per day of data collection per consent status
 * Percentage of survey per consent status by enumerator
 * Average interview duration by enumerator
 

### Checking for logical errors in data
 * Duplicates in unique ID
 * Interviews that do not end on the same day as they started
 * Interviews which end before they start
 * Interviews made before the first day of data collection 
 * Interviews made in the future
 * Check for potential errors on the dates are not marked for deletion which can lead to weird duration

### Checks on the surveys content
 * Number of distinct values per questions
 * Number of other distinct values (for the questions with a possibility of other)

### Checking Spatial Data

 * check that the recorded site is the actual one
 * check that the survey is done within a certain buffer meter buffer from a sample point



#### Building Site documentation 

> devtools::document()

> devtools::check(document = FALSE)


> pkgdown::build_site()


If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/PYannick/HighFrequencyChecks/issues). 
