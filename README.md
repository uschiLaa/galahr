
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

## Example

You can launch the Shiny app with the default dataset or your own
dataset. The GUI also lets you upload a new dataset that is read from
.csv format.

``` r
library(galahr)
## launching the app with the default dataset
launchApp()
## launching the app with a different dataset
launchApp(GW170817)
```
