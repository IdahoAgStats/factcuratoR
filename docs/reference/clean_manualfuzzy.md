# Clean the file of fuzzy matches with manual checks

Fuzzy matching outputs a file of potential/fuzzy matches. A user must go
in a assign whether the fuzzy match is a true match.

## Usage

``` r
clean_manualfuzzy(match_type = "raw", df)
```

## Arguments

- match_type:

  A string that is either 'db' or 'raw', which refers to processing
  database entries (only one entry per cultivar) or raw entries which
  may have many alternate spellings for one cultivar

- df:

  A data.frame, which can be provided in lieu of the curation_folder and
  filename
