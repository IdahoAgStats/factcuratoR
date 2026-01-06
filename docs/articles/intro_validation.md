# Introduction to the validation functions

## Introduction

The purpose of **factcuratoR** is to provide sets of functions to help
standardize variety testing data for curation of variety testing data
sets supporting the WAVE project.

The goal of the WAVE program curation is to generate *trial data* *and*
*trial metadata* that conform to the controlled vocabulary codebooks.

The codebooks specify the variable required for each file (as a single
column in a data frame, matrix, tibble, etc) and the accepted values for
the the variable. See the [Introduction to codebooks
vignette](https://idahoagstats.github.io/factcuratoR/articles/intro_codebooks.html)
for more information on the structure of and formatting for the
codebooks.

## Overview of validation functions

The validation functions check that the data conforms to the controlled
vocabulary codebooks. There are functions to validate:

- variable names. The functions return tables for the curator to check
  that required variables are present and that names are standardized.
- variable values. The functions return tables for the curator to check
  that values for a single variable match controlled vocabularies or are
  in the accepted value range.

Finally,
[`standardize_cols_by_cb()`](https://idahoagstats.github.io/factcuratoR/reference/standardize_cols_by_cb.html)
enables curators to standardize the files (select and order columns)
according to the standards established in the codebooks.

First, load **factcuratoR** and point to the main codebook which must be
named *codebooks_all_db.csv* for the validation functions to find it.

``` r
library(factcuratoR)

rlang::check_installed("here")

codebook_folder <- here::here(
  "tests/testthat/test_controlled_vocab")

knitroutputfolder <- here::here("inst/extdata/intro_validation", "output")
```

## Create some test data

``` r
test_data <- data.frame(location = c("Aberdeen", "Soda Springs", NA, "location_x"),
                        year = c(rep(2020, 3), NA),
                        variety = c("variety_1", "AAC Wildfire", "", NA),
                        rep_temp = 1:4,
                        sourcefile = "test")
```

## Validate trial data

It may be easiest to point the most cleaned up version of the data to a
new variable (e.g. df_validate below) so that the calls to the
validation functions don’t need to be updated every time there is a more
cleaned up version of the data.

``` r
df_validate <- test_data
```

### Check column names

The function `validate_colnames(`) will check the variable names between
the codebook and data. The argument `codebook_name = "trial_data"`
indicates what codebook from the *codebooks_all_db.csv* should be used
for this step.

``` r
colname_valid <- validate_colnames(df_validate, 
                                   codebook_name = "trial_data", 
                                   db_folder = codebook_folder)
#> Joining with `by = join_by(comment)`
#> Status: # A tibble: 3 × 4 comment n required req <chr> <int> <lgl> <int> 1
#> exists in both data and codebook 1 NA NA 2 not present in codebook: trial_data
#> 4 NA NA 3 not present in data 27 TRUE 6
```

The main goal of validating column names is to get the value in the
required column of row 3 to be zero. A value of zero indicates that all
the required columns are present in the data and completely filled out.
The second row in the summary highlights columns that are not in the
codebook.

If required columns appear missing in the initial check, it may be
present in the data set with a differing name. Often this is the case
for many variables; collaborators each have their own unique name for
common variables. When this happens, creating a file to rename files en
masse is the best solution. See
[facthelpeR](https://idahoagstats.github.io/facthelpeR/articles/intro_facthelper.html)
for functions to help with renaming columns. Determining if a variable
is captured by the existing controlled vocabulary is a human decision
made based on your own knowledge and when needed, consultation with
other project participants. When in doubt, ask.

If a required variable is truly missing, then you will need to find the
data. Looking at annual reports is a good starting point, and you may
have to eventually request this information from the collaborator if you
cannot find it elsewhere.

Sometimes, there are also new variables not captured by the existing
vocabulary. In that case, we will need to decide if this variable should
be added to the database. The answer is usually “yes”, but usually a
discussion with project participants is needed for this decision. If
there is a decision to add a new variable, add it to controlled
vocabulary using the correct formatting described in the [Introduction
to codebooks
vignette](https://idahoagstats.github.io/factcuratoR/articles/intro_codebooks.html).

#### Full report of validating column names

``` r
knitr::kable(colname_valid)
```

| colname_data | colname_codebook | required | col_num | comment |
|:---|:---|:---|---:|:---|
| NA | trial | TRUE | 1 | not present in data |
| NA | entry | TRUE | 3 | not present in data |
| NA | plot | TRUE | 4 | not present in data |
| NA | range | TRUE | 6 | not present in data |
| NA | rep | TRUE | 5 | not present in data |
| NA | row | TRUE | 7 | not present in data |
| NA | heading_date | NA | NA | not present in data |
| NA | yield_bu_acre | NA | NA | not present in data |
| NA | yield_lb_plot | NA | NA | not present in data |
| NA | yield_corrected | NA | NA | not present in data |
| NA | stand | NA | NA | not present in data |
| NA | moisture | NA | NA | not present in data |
| NA | height | NA | NA | not present in data |
| NA | falling_number | NA | NA | not present in data |
| NA | grain_protein | NA | NA | not present in data |
| NA | test_weight | NA | NA | not present in data |
| NA | test_weight_cleaned | NA | NA | not present in data |
| NA | lodging | NA | NA | not present in data |
| NA | plump_percent_6_64 | NA | NA | not present in data |
| NA | thin_percent_5_64 | NA | NA | not present in data |
| NA | yield_g_plot | NA | NA | not present in data |
| NA | yield_lb_acre | NA | NA | not present in data |
| NA | yield_kg_ha | NA | NA | not present in data |
| NA | hessian_fly_percent | NA | NA | not present in data |
| NA | emergence | NA | NA | not present in data |
| NA | fdk_rep_xx | NA | NA | not present in data |
| NA | inf_spikelet_rep_xx | NA | NA | not present in data |
| location | NA | NA | NA | not present in codebook: trial_data |
| year | NA | NA | NA | not present in codebook: trial_data |
| rep_temp | NA | NA | NA | not present in codebook: trial_data |
| sourcefile | NA | NA | NA | not present in codebook: trial_data |
| variety | variety | TRUE | 2 | exists in both data and codebook |

#### To interact with the variable names that still need to be fixed

``` r
colname_valid_check <- colname_valid %>% 
  filter(comment == "not present in codebook: trial_data")

col_info <- find_col_info(df_validate, 
                       cols_check = colname_valid_check$colname_data, 
                       by_col = sourcefile)

knitr::kable(col_info)
```

|   n | contained_in | variable   | example  |
|----:|:-------------|:-----------|:---------|
|   3 | test         | location   | Aberdeen |
|   4 | test         | rep_temp   | 1        |
|   4 | test         | sourcefile | test     |
|   3 | test         | year       | 2020     |

### Check column contents

The function
[`confront_data()`](https://idahoagstats.github.io/factcuratoR/reference/confront_data.md)
is a wrapper around
[`validate::confront()`](https://rdrr.io/pkg/validate/man/confront.html)
that checks that column contents match controlled vocabularies or are in
the accepted value range.

Note: The argument `blends` = TRUE will check for blends in the variety
column

``` r
colcontent_valid <- confront_data(df_validate, 
                                  df_type = "trial_data",
                                  db_folder = codebook_folder)
#> Warning: Some issues left to resolve 
#> # A tibble: 2 × 5
#>   required fails   nNA error warning
#>   <lgl>    <int> <int> <int>   <int>
#> 1 TRUE         1     1     6       0
#> 2 NA           0     0    21       0
```

The summary output for validating column contents reports the number of
columns that have fails (that is, a column does not match controlled
vocabularies or is not within the accepted range) and NA values. A
column will return `error = TRUE` if a column is not present in the
data.

The goal of validating column contents is to achieve zero fails for all
columns and for required columns, achieve zero NA values (so that each
observation has a value).

The errors can be fixed by either fixing errors or standardizing
contents in the raw data or adding new controlled vocabularies to the
codebooks. For example, if the name “variety_1” is a real variety name,
then it should be added to the cultivar codebook.

### Full report of validating column contents

``` r
colcontent_summary <- colcontent_valid[["summary"]]
knitr::kable(colcontent_summary)
```

| required | name | items | passes | fails | nNA | error | warning | expression | crop_type |
|:---|:---|---:|---:|---:|---:|:---|:---|:---|:---|
| TRUE | variety | 4 | 1 | 2 | 1 | FALSE | FALSE | variety %vin% db\[\[“cultivar.csv”\]\]\[\[“variety”\]\] | NA |
| TRUE | trial | 0 | 0 | 0 | 0 | TRUE | FALSE | is.character(trial) | NA |
| TRUE | entry | 0 | 0 | 0 | 0 | TRUE | FALSE | entry - 0 \>= -1e-08 & entry - 500 \<= 1e-08 & entry%%1 == 0 | NA |
| TRUE | plot | 0 | 0 | 0 | 0 | TRUE | FALSE | plot - 1 \>= -1e-08 & plot - 1e+06 \<= 1e-08 & plot%%1 == 0 | NA |
| TRUE | range | 0 | 0 | 0 | 0 | TRUE | FALSE | range - 1 \>= -1e-08 & range - 200 \<= 1e-08 & range%%1 == 0 | NA |
| TRUE | rep | 0 | 0 | 0 | 0 | TRUE | FALSE | rep - 1 \>= -1e-08 & rep - 20 \<= 1e-08 & rep%%1 == 0 | NA |
| TRUE | row | 0 | 0 | 0 | 0 | TRUE | FALSE | row - 1 \>= -1e-08 & row - 200 \<= 1e-08 & row%%1 == 0 | NA |
| NA | heading_date | 0 | 0 | 0 | 0 | TRUE | FALSE | heading_date - 1 \>= -1e-08 & heading_date - 300 \<= 1e-08 & heading_date%%1 == 0 | NA |
| NA | yield_bu_acre | 0 | 0 | 0 | 0 | TRUE | FALSE | yield_bu_acre - 0 \>= -1e-08 & yield_bu_acre - 500 \<= 1e-08 | NA |
| NA | yield_lb_plot | 0 | 0 | 0 | 0 | TRUE | FALSE | yield_lb_plot - 0 \>= -1e-08 & yield_lb_plot - 50 \<= 1e-08 | NA |
| NA | yield_corrected | 0 | 0 | 0 | 0 | TRUE | FALSE | yield_corrected - 0 \>= -1e-08 & yield_corrected - 500 \<= 1e-08 | NA |
| NA | stand | 0 | 0 | 0 | 0 | TRUE | FALSE | stand - 0 \>= -1e-08 & stand - 100 \<= 1e-08 | NA |
| NA | moisture | 0 | 0 | 0 | 0 | TRUE | FALSE | moisture - 0 \>= -1e-08 & moisture - 30 \<= 1e-08 | NA |
| NA | height | 0 | 0 | 0 | 0 | TRUE | FALSE | height - 0 \>= -1e-08 & height - 200 \<= 1e-08 | NA |
| NA | falling_number | 0 | 0 | 0 | 0 | TRUE | FALSE | falling_number - 0 \>= -1e-08 & falling_number - 600 \<= 1e-08 & falling_number%%1 == 0 | NA |
| NA | grain_protein | 0 | 0 | 0 | 0 | TRUE | FALSE | grain_protein - 0 \>= -1e-08 & grain_protein - 100 \<= 1e-08 | NA |
| NA | test_weight | 0 | 0 | 0 | 0 | TRUE | FALSE | test_weight - 0 \>= -1e-08 & test_weight - 100 \<= 1e-08 | NA |
| NA | test_weight_cleaned | 0 | 0 | 0 | 0 | TRUE | FALSE | test_weight_cleaned - 0 \>= -1e-08 & test_weight_cleaned - 100 \<= 1e-08 | NA |
| NA | lodging | 0 | 0 | 0 | 0 | TRUE | FALSE | lodging - 0 \>= -1e-08 & lodging - 100 \<= 1e-08 | NA |
| NA | plump_percent_6_64 | 0 | 0 | 0 | 0 | TRUE | FALSE | plump_percent_6_64 - 0 \>= -1e-08 & plump_percent_6_64 - 100 \<= 1e-08 | NA |
| NA | thin_percent_5_64 | 0 | 0 | 0 | 0 | TRUE | FALSE | thin_percent_5_64 - 0 \>= -1e-08 & thin_percent_5_64 - 100 \<= 1e-08 | NA |
| NA | yield_g_plot | 0 | 0 | 0 | 0 | TRUE | FALSE | is.numeric(yield_g_plot) | NA |
| NA | yield_lb_acre | 0 | 0 | 0 | 0 | TRUE | FALSE | is.numeric(yield_lb_acre) | NA |
| NA | yield_kg_ha | 0 | 0 | 0 | 0 | TRUE | FALSE | is.numeric(yield_kg_ha) | NA |
| NA | hessian_fly_percent | 0 | 0 | 0 | 0 | TRUE | FALSE | hessian_fly_percent - 0 \>= -1e-08 & hessian_fly_percent - 100 \<= 1e-08 | NA |
| NA | emergence | 0 | 0 | 0 | 0 | TRUE | FALSE | emergence - 0 \>= -1e-08 & emergence - 100 \<= 1e-08 | NA |
| NA | fdk_rep_xx | 0 | 0 | 0 | 0 | TRUE | FALSE | fdk_rep_xx - 0 \>= -1e-08 & fdk_rep_xx - 100 \<= 1e-08 | NA |
| NA | inf_spikelet_rep_xx | 0 | 0 | 0 | 0 | TRUE | FALSE | inf_spikelet_rep_xx - 0 \>= -1e-08 & inf_spikelet_rep_xx - 100 \<= 1e-08 | NA |

#### To check for validation fails interactively

``` r
var <- c("variety")
colcontent_violate <- 
  validate::violating(test_data, colcontent_valid[[2]][var]) %>% 
  relocate(matches(var))

knitr::kable(colcontent_violate)
```

|     | variety   | location | year | rep_temp | sourcefile |
|:----|:----------|:---------|-----:|---------:|:-----------|
| 1   | variety_1 | Aberdeen | 2020 |        1 | test       |
| 3   |           | NA       | 2020 |        3 | test       |

## Validate metadata

The steps in validating the metadata is the same as validating the trial
data, except the argument `codebook_name = "trials_metadata"`.

``` r
metadata_validate <- test_data
```

### Check variable names

``` r
metadata_colname_valid <- 
  validate_colnames(
    metadata_validate, 
    "trials_metadata",
    db_folder = codebook_folder) %>%
    select(comment, colname_data, colname_codebook, required, col_num) 
#> Joining with `by = join_by(comment)`
#> Status: # A tibble: 3 × 4 comment n required req <chr> <int> <lgl> <int> 1
#> exists in both data and codebook 2 NA NA 2 not present in codebook:
#> trials_metadata 3 NA NA 3 not present in data 25 TRUE 10

knitr::kable(metadata_colname_valid)
```

| comment | colname_data | colname_codebook | required | col_num |
|:---|:---|:---|:---|---:|
| not present in data | NA | trial | TRUE | 1 |
| not present in data | NA | nursery | TRUE | 3 |
| not present in data | NA | latitude | TRUE | 8 |
| not present in data | NA | longitude | TRUE | 9 |
| not present in data | NA | irrigation | TRUE | 10 |
| not present in data | NA | planting_date | TRUE | 11 |
| not present in data | NA | harvest_date | TRUE | 12 |
| not present in data | NA | plot_length | TRUE | 14 |
| not present in data | NA | plot_width | TRUE | 15 |
| not present in data | NA | tillage | TRUE | 31 |
| not present in data | NA | row_spacing_in | FALSE | 16 |
| not present in data | NA | npks_lb_acre | FALSE | 17 |
| not present in data | NA | chemical_trts | FALSE | 18 |
| not present in data | NA | seed_rate_per_acre | FALSE | 19 |
| not present in data | NA | seed_trt | FALSE | 20 |
| not present in data | NA | agronomic_notes | FALSE | 21 |
| not present in data | NA | soil_type | FALSE | 23 |
| not present in data | NA | soil_om | FALSE | 24 |
| not present in data | NA | ph | FALSE | 25 |
| not present in data | NA | n_lbs_acre | FALSE | 26 |
| not present in data | NA | p_ppm | FALSE | 27 |
| not present in data | NA | k_ppm | FALSE | 28 |
| not present in data | NA | s_ppm | FALSE | 29 |
| not present in data | NA | free_lime_pct | FALSE | 30 |
| not present in data | NA | unpublished | FALSE | 33 |
| not present in codebook: trials_metadata | variety | NA | NA | NA |
| not present in codebook: trials_metadata | rep_temp | NA | NA | NA |
| not present in codebook: trials_metadata | sourcefile | NA | NA | NA |
| exists in both data and codebook | location | location | TRUE | 6 |
| exists in both data and codebook | year | year | TRUE | 4 |

### Check variable values

``` r
metadata_colcontent_valid <- confront_data(metadata_validate, 
                                           df_type = "trials_metadata",
                                           db_folder = codebook_folder)
#> Warning: Some issues left to resolve 
#> # A tibble: 2 × 5
#>   required fails   nNA error warning
#>   <lgl>    <int> <int> <int>   <int>
#> 1 FALSE        0     0    15       0
#> 2 TRUE         1     2    10       0

metadata_colcontent_summary <- metadata_colcontent_valid[["summary"]]

knitr::kable(metadata_colcontent_summary)
```

| required | name | items | passes | fails | nNA | error | warning | expression |
|:---|:---|---:|---:|---:|---:|:---|:---|:---|
| TRUE | year | 4 | 3 | 0 | 1 | FALSE | FALSE | year - 1995 \>= -1e-08 & year - 2026 \<= 1e-08 & year%%1 == 0 |
| TRUE | location | 4 | 2 | 1 | 1 | FALSE | FALSE | location %vin% db\[\[“locations.csv”\]\]\[\[“location”\]\] |
| TRUE | trial | 0 | 0 | 0 | 0 | TRUE | FALSE | is.character(trial) |
| TRUE | nursery | 0 | 0 | 0 | 0 | TRUE | FALSE | nursery %vin% db\[\[“nursery.csv”\]\]\[\[“nursery”\]\] |
| TRUE | latitude | 0 | 0 | 0 | 0 | TRUE | FALSE | latitude - 42 \>= -1e-08 & latitude - 49 \<= 1e-08 |
| TRUE | longitude | 0 | 0 | 0 | 0 | TRUE | FALSE | longitude - -124.77 \>= -1e-08 & longitude - -111.05 \<= 1e-08 |
| TRUE | irrigation | 0 | 0 | 0 | 0 | TRUE | FALSE | irrigation %vin% c(“irrigated”, “dryland”) |
| TRUE | planting_date | 0 | 0 | 0 | 0 | TRUE | FALSE | grepl(“\[0-9\]{4}-\|/\[0-9\]{2}-\|/\[0-9\]{2}”, planting_date) |
| TRUE | harvest_date | 0 | 0 | 0 | 0 | TRUE | FALSE | grepl(“\[0-9\]{4}-\|/\[0-9\]{2}-\|/\[0-9\]{2}”, harvest_date) |
| TRUE | plot_length | 0 | 0 | 0 | 0 | TRUE | FALSE | plot_length - 5 \>= -1e-08 & plot_length - 200 \<= 1e-08 \| plot_length %vin% c(-9) |
| TRUE | plot_width | 0 | 0 | 0 | 0 | TRUE | FALSE | plot_width - 2 \>= -1e-08 & plot_width - 50 \<= 1e-08 \| plot_width %vin% c(-9) |
| TRUE | tillage | 0 | 0 | 0 | 0 | TRUE | FALSE | tillage %vin% c(“conventional”, “no till”, “conservation”) |
| FALSE | row_spacing_in | 0 | 0 | 0 | 0 | TRUE | FALSE | row_spacing_in - 1 \>= -1e-08 & row_spacing_in - 50 \<= 1e-08 \| row_spacing_in %vin% c(-9) |
| FALSE | npks_lb_acre | 0 | 0 | 0 | 0 | TRUE | FALSE | is.character(npks_lb_acre) |
| FALSE | chemical_trts | 0 | 0 | 0 | 0 | TRUE | FALSE | nchar(as.character(chemical_trts)) \<= 20 |
| FALSE | seed_rate_per_acre | 0 | 0 | 0 | 0 | TRUE | FALSE | seed_rate_per_acre %vin% c(7e+05, 1e+06) |
| FALSE | seed_trt | 0 | 0 | 0 | 0 | TRUE | FALSE | seed_trt %vin% c(TRUE, FALSE) |
| FALSE | agronomic_notes | 0 | 0 | 0 | 0 | TRUE | FALSE | nchar(as.character(agronomic_notes)) \<= 500 |
| FALSE | soil_type | 0 | 0 | 0 | 0 | TRUE | FALSE | nchar(as.character(soil_type)) \<= 50 |
| FALSE | soil_om | 0 | 0 | 0 | 0 | TRUE | FALSE | soil_om - 0 \>= -1e-08 & soil_om - 100 \<= 1e-08 |
| FALSE | ph | 0 | 0 | 0 | 0 | TRUE | FALSE | ph - 0 \>= -1e-08 & ph - 14 \<= 1e-08 |
| FALSE | n_lbs_acre | 0 | 0 | 0 | 0 | TRUE | FALSE | n_lbs_acre - 0 \>= -1e-08 & n_lbs_acre - 500 \<= 1e-08 |
| FALSE | p_ppm | 0 | 0 | 0 | 0 | TRUE | FALSE | p_ppm - 0 \>= -1e-08 & p_ppm - 200 \<= 1e-08 |
| FALSE | k_ppm | 0 | 0 | 0 | 0 | TRUE | FALSE | k_ppm - 0 \>= -1e-08 & k_ppm - 1000 \<= 1e-08 |
| FALSE | s_ppm | 0 | 0 | 0 | 0 | TRUE | FALSE | s_ppm - 0 \>= -1e-08 & s_ppm - 200 \<= 1e-08 |
| FALSE | free_lime_pct | 0 | 0 | 0 | 0 | TRUE | FALSE | free_lime_pct - 0 \>= -1e-08 & free_lime_pct - 100 \<= 1e-08 |
| FALSE | unpublished | 0 | 0 | 0 | 0 | TRUE | FALSE | unpublished %vin% c(TRUE, FALSE) |

``` r

#Check for any fails interactively:

metadata_var <- c("location")
metadata_colcontent_violate <- 
  validate::violating(test_data, metadata_colcontent_valid[[2]][metadata_var]) %>% 
  relocate(matches(var))

knitr::kable(metadata_colcontent_violate)
```

|     | variety | location   | year | rep_temp | sourcefile |
|:----|:--------|:-----------|-----:|---------:|:-----------|
| 4   | NA      | location_x |   NA |        4 | test       |
