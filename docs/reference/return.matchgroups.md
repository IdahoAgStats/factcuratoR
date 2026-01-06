# Return the variety names by whether they have a match, no match, or need to be checked

Return the variety names. This function takes a data.frame of variety
names and their potential database matches. Given the columns status,
which denotes whether the variety has a database match, and any_match,
which denotes whether the variety has at least one match, this function
separates the data.frame into a list of 4 data.frames: the varieties
with database matches, the varieties without database matches, the
varieties that still need to be checked manually (with a collaborator),
and the duplicate rows of varieties that already have matches

## Usage

``` r
return.matchgroups(df_any_match, is_blends = FALSE)
```

## Arguments

- df_any_match:

  A data.frame that has been processed by check.anymatch()

- is_blends:

  A logical that specifies whether the varieties are blends. Default is
  FALSE
