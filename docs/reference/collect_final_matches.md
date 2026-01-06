# Module to collect all the matches from each stage of the variety matching process

`collect_final_matches()` combines the match output lists and returns a
data frame containing the raw names, matched clean names, and the stage
where they were matched. An output file, 'final_matches.csv', will be
written to the specified output location.

## Usage

``` r
collect_final_matches(
  match_list,
  names_raw,
  knitroutputfolder,
  csv_suffix = NULL,
  is_blends = FALSE
)
```

## Arguments

- match_list:

  A list of the output from each step of the variety matching process,
  i.e., exact, rename, and fuzzy. These must be enclosed in a
  [`list()`](https://rdrr.io/r/base/list.html) call.

- names_raw:

  A data.frame of the names created by
  [`create_intid()`](https://idahoagstats.github.io/factcuratoR/reference/create_intid.md)

- knitroutputfolder:

  A path to the output location

- csv_suffix:

  Optional string denoting the suffix for the .csv name.

- is_blends:

  A logical that specifies whether the varieties are blends. Default is
  FALSE

## Details

Note: If no match has been found, then the returned match is the
original raw name.

## See also

Other match variety functions:
[`create_intid()`](https://idahoagstats.github.io/factcuratoR/reference/create_intid.md),
[`find_entries_raw_names()`](https://idahoagstats.github.io/factcuratoR/reference/find_entries_raw_names.md)
