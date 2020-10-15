## High Frequency Data Quality Checks

`HighFrequencyCheck` can be used to detect programming errors, surveyor errors, data fabrication, poorly understood questions, and other issues happening during Household Survey data collection. The results of these checks can also be useful in improving the survey, identifying enumerator effects, and assessing the reliability of your outcome measures. It allows teams to catch survey issues and collection mistakes in time to correct them and ensure high-quality data. Such checks are based are among best practices from both the [World Bank](https://dimewiki.worldbank.org/wiki/High_Frequency_Checks) and [Innovations for Poverty Action](https://www.povertyactionlab.org/resource/data-quality-checks).

This package brings a series of convenience functions to monitor data quality during the data collection when running a survey with [KoboToolbox]() (or any `xlsform` compatible platform). it is an adaptation in R of the [Stata package](https://github.com/PovertyAction/high-frequency-checks) from Innovations for Poverty Action. 

Those can be performed periodically during the data collection process to check for possible errors and provide meaningful inputs to the enumerators. All these functions do not have to be ran at the same period of time. They are provided there to help data supervisor to build reports.

The packages includes a series of controls calling for:

 * Corrective actions:

   * Correct set-up of data collection devices and encoding of the forms
   * Data collected according the sampling plan

 * Pro-active actions:  
 
   * Ensuring enumerators rigorous work standards
   * Promoting enumerators productivity
   
## Usage

The package [vignette](articles/HFC.html) offers introduce the main functions in the package.

Install from github with `devtools::install_github("unhcr/HighFrequencyChecks")`.


You can then create a new Rstudio project and run the following
``` r
## enable the library
library(HighFrequencyChecks)

## create a standard folder sructure
hfc_projectinit()
```

You can then follow the instructions in the `R/run-check.R` script.


#### Building package documentation 

`devtools::document()`

`devtools::check(document = FALSE)`

`pkgdown::build_site()`


If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/unhcr/HighFrequencyChecks/issues). 



> This package is part of `unhcrverse`, a set of packages to ease the production of statistical evidence and data stories. You can install them all with the following:

```r
## Use UNHCR Open data  - https://unhcr.github.io/unhcrdatapackage/docs/
remotes::install_github('unhcr/unhcrdatapackage’)

## API to connect to internal data source - https://unhcr-web.github.io/hcrdata/docs/
remotes::install_github('unhcr-web/hcrdata’)

## Perform High Frequency Check https://unhcr.github.io/HighFrequencyChecks/docs/
remotes::install_github('unhcr-web/HighFrequencyChecks’)

## Process data crunching for survey dataset - https://unhcr.github.io/koboloadeR/docs/
remotes::install_github('unhcr/koboloadeR’)

## Use UNHCR graphical template- https://unhcr-web.github.io/unhcRstyle/docs/
remotes::install_github('unhcr-web/unhcRstyle')
```
