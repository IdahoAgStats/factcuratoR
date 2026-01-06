# Bind the status of fuzzymatches with the fuzzymatches created in this session so that the two stay in sync even if the fuzzymatch function is changed Note: this function may have to be updated to handle multiple auxiliary fuzzymatch files

Bind the status of fuzzymatches with the fuzzymatches created in this
session so that the two stay in sync even if the fuzzymatch function is
changed Note: this function may have to be updated to handle multiple
auxiliary fuzzymatch files

## Usage

``` r
bind_fuzzymatches(output_fuzzymatch_df, aux_fuzzy_status)
```

## Arguments

- output_fuzzymatch_df:

  A data.frame with the newly generated fuzzymatches

- aux_fuzzy_status:

  A data.frame containing the fuzzymatches with is_truematch manually
  checked
