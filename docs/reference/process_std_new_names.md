# Process standardized new names that were created from process_fuzzymatch()

Once the user checks the new names, they can be read in and processed.
The names are checked against the names that still need matches after
fuzzymatching (contained in matches_temp_list) and this function:

1.  Writes out a csv with a message to update the
    auxiliary/standardize_new_names\*.csv (New names to standardize
    arise when the is_truematch status in the fuzzymatching stage moves
    from 'check' to 'TRUE' or 'FALSE')

2.  Writes out a csv with message to add it to the cultivar database

3.  Writes out another csv with message to add it to the cv_rename.csv
    file Note: This function comes after fuzzymatching, just like
    create_new_db_entries(). create_new_db_entries() will not be
    deprecated because it is used in neely_curation/cultivar but it
    processes the standardized new names in a way that is unnecessary
    (finds aliases). Also, this function allows for iterations through
    the matching process.

## Usage

``` r
process_std_new_names(
  output_nomatch_df,
  auxiliary_files,
  filename,
  knitroutputfolder,
  db_folder
)
```

## Arguments

- output_nomatch_df:

  The 'nomatch' data.frame created from process_fuzzymatch

- auxiliary_files:

  Path to the folder containing the completed 'standardize_new_names'
  helper file

- filename:

  Name of the 'standardize_new_names' helper file to be read in

- knitroutputfolder:

  Path to the output folder

- db_folder:

  Path to the codebook folder

## See also

Other match variety modules:
[`do_exactmatch()`](https://idahoagstats.github.io/factcuratoR/reference/do_exactmatch.md),
[`find_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/find_fuzzymatch.md),
[`process_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/process_fuzzymatch.md)
