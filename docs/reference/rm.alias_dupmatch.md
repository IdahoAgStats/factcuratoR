# Remove alias if there is a match with variety

If a variety is listed in the raw data with an alias, both names may
match to names in the database. This function will remove the alias and
return the listed variety name

## Usage

``` r
rm.alias_dupmatch(match_df)
```

## Arguments

- match_df:

  A data.frame that contains var_id, db_id, and type_db ("alias" or
  "variety")
