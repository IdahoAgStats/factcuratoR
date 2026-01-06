# Module to process fuzzymatches given an auxiliary file

This function writes out:

- fuzzy_check.csv (A file of fuzzy matches to manually check) !Note:
  Rows for which the source column is only "new_fuzzymatch" must be
  manually pasted into the aux_fuzzy_status file Source column
  containing "csv" denotes that the possible fuzzymatch in the
  aux_fuzzy_status file is blank (or not TRUE or FALSE)

## Usage

``` r
process_fuzzymatch(output_fuzzymatch_df, aux_fuzzy_status, knitroutputfolder)
```

## Arguments

- output_fuzzymatch_df:

  A data.frame with the newly generated fuzzymatches

- aux_fuzzy_status:

  A data.frame containing the fuzzymatches with is_truematch manually
  checked

- knitroutputfolder:

  A path to send output

## Details

- add_fuzzy_to_cv_rename.csv (A file to paste into the main
  controlled_vocab/cv_rename.csv) These are names for which is_truematch
  = TRUE and these names should be manually added to
  controlled_vocab/cv_rename.csv file so they are caught using
  do_exactmatch()

- standardize_new_names.csv (A file of names to standardize for adding
  to the database.)

## See also

Other match variety modules:
[`do_exactmatch()`](https://idahoagstats.github.io/factcuratoR/reference/do_exactmatch.md),
[`find_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/find_fuzzymatch.md),
[`process_std_new_names()`](https://idahoagstats.github.io/factcuratoR/reference/process_std_new_names.md)
