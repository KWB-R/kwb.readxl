[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/0kvpqifpqdus2rs8/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-readxl/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.readxl.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.readxl)
[![codecov](https://codecov.io/github/KWB-R/kwb.readxl/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.readxl)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.readxl)]()

This package is based on the package readxl. It provides functions that read all Excel sheets as pure text and then try to split each sheet into a set of line ranges that are assumed to represent single tables.

## Installation

```r
#install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("KWB-R/kwb.readxl")
```
