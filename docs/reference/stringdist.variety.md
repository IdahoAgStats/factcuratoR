# Fuzzymatch varieties using stringdist_join

Fuzzymatch the varieties from the raw data with the database using an
internal id. This function uses stringdist_join and returns the closest
match (smallest distance) To have enough flexibility without returning
too many fuzzy matches, this function returns the closest match measured
in 2 different ways:

1.  select the best_n varieties with the smallest distance, and then
    select the best_n varieties that have the smallest scaled distance

2.  select the best_n varieties with the smallest scaled distance,
    irrespective of the raw distance See the effect of these different
    selection methods in test_match_variety_fns.R

## Usage

``` r
stringdist.variety(
  raw_data,
  database,
  intid_col = "name",
  best_n,
  method_stringdist
)
```

## Arguments

- raw_data:

  A data.frame

- database:

  A data.frame

- intid_col:

  A string denoting the name of the internal id column used to join the
  raw data and database

- best_n:

  A numeric denoting the number of best matches to select

- method_stringdist:

  A string corresponding to a string metric defined by stringdist e.g.,
  "lw", "jw". See
  [`stringdist-metrics`](https://rdrr.io/pkg/stringdist/man/stringdist-metrics.html)
  for details.
