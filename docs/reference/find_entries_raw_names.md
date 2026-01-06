# Find entries for the cultivar names that need to be checked

Summarises how many times a cultivar name shows up in the raw data per
year

## Usage

``` r
find_entries_raw_names(df_names, data_raw, join_by)
```

## Arguments

- df_names:

  A data.frame that contains the column of names to check

- data_raw:

  A data.frame that contains the raw data (all entries)

- join_by:

  A character string or named character string that is passed to the
  'by' parameter in left_join()

## See also

Other match variety functions:
[`collect_final_matches()`](https://idahoagstats.github.io/factcuratoR/reference/collect_final_matches.md),
[`create_intid()`](https://idahoagstats.github.io/factcuratoR/reference/create_intid.md)
