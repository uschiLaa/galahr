
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galahr

<!-- badges: start -->

<!-- badges: end -->

galahr, a GUI to Assist Learning About High dimensions in R, provides a
GUI to the `tourr` package based on `shiny` and `plotly`. Upload your
dataset and use the input panel to select settings for the tour and
display, move to the results panel to play the tour animation.

## Installation

You can install the development version of galahr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("uschiLaa/galahr")
```

Note that rendering is slow with the latest version of `plotly`
(depending on the browser used). We are working on fixing this and
currently we recommend using `plotly` version 4.8 with the `galahr`
package. You can install this version
with:

``` r
devtools::install_version("plotly", version = "4.8.0", repos = "http://cran.us.r-project.org")
```

## Example

You can launch the Shiny app with the default dataset or your own
dataset. Once it appears in the RStudio Viewer, choose \`Open in
Browser’ for the best layout. The GUI also lets you upload a new
dataset that is read from .csv format.

``` r
library(galahr)
## launching the app with the default dataset
launchApp()
## launching the app with a different dataset
launchApp(GW170817)
```
