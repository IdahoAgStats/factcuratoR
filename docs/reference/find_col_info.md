# Find column information

For a data.frame, loop through the columns provided (cols_check) and
summarise the number of non-NA entries and the groups that contain
non-NA entries (for example if want to search trial, year, or filename),
and return the first non-NA entry as an example

## Usage

``` r
find_col_info(df, cols_check, by_col)
```

## Arguments

- df:

  A data.frame to summarise

- cols_check:

  A character vector of column names to check

- by_col:

  A bare variable to summarise

## See also

Other validation functions:
[`confront_data()`](https://idahoagstats.github.io/factcuratoR/reference/confront_data.md),
[`create_rules()`](https://idahoagstats.github.io/factcuratoR/reference/create_rules.md),
[`validate_colnames()`](https://idahoagstats.github.io/factcuratoR/reference/validate_colnames.md)
