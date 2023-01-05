
# BIC-safetygraphicsif

The BIC-safetygraphicsif R Shiny app provides an interface enabling the usage of the safetyGraphics app with modified Clinical Trial Safety Data.

## Installation

-   assumes R is installed (install the remotes and usethis R-packages)
-   assumed git is installed
-   add CWL=<your_cwl> and CWL\_password=<your_cwl_password> to
    .Renviron file using usethis::edit\_r\_environ()

``` r
remotes::install_github("Bayer-group/BIC-safetygraphicsif")
```

## Launch Rshiny app

``` r
library(safetygraphicsif)
safetygraphicsif::start_app()
```