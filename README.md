[factcuratoR docs quick link](https://idahoagstats.github.io/factcuratoR/)

# factcuratoR Package:

The purpose of `factcuratoR` is to provide sets of functions to 
help standardize variety testing data for the FACT program.

The goal of the FACT program curation is to generate:

1. trial data
2. trial metadata

that conform to the controlled vocabulary codebooks.

The `factcuratoR` functions fall into a few different workflows: 

- read-in the controlled vocabulary codebooks
- validate column names and contents according to the codebooks
- curate cultivar names
- standardize/ write out files according to the codebooks

## Installation

You can install `factcuratoR` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("IdahoAgStats/factcuratoR")
```

## factcuratoR functions require codebooks that are standardized
See the [Introduction to codebooks vignette](https://idahoagstats.github.io/factcuratoR/articles/intro_codebooks.html)


