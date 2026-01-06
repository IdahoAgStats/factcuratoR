# Check for any matches

After a variety name match (exact or fuzzymatch), check whether there
are any exact or approved matches.

## Usage

``` r
check.anymatch(df, group, checkfor = NULL, match_type)
```

## Arguments

- df:

  A data.frame with variety names matched to database names

- group:

  A column name by which to group and search for a match (e.g. to
  determine if there are any matches for the variety column, use:
  variety)

- checkfor:

  A column name to check whether there is a match (This parameter is
  used only for exact matching. For fuzzymatches, the status of the
  match must be manually entered)

- match_type:

  A string that is either 'db' or 'raw', which refers to processing
  database entries (only one entry per cultivar) or raw entries which
  may have many alternate spellings for one cultivar
