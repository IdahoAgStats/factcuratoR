# List the variables (column names) in a given database codebook

List the variables (column names) in a given database codebook

A wrapper for list_db_var to maintain backwards compatibility

## Usage

``` r
list_db_var(db_folder, codebook_name, required_only = FALSE, crop_types)

list.db_var(db_folder, codebook_name, required_only = FALSE)
```

## Arguments

- db_folder:

  A string path to the database controlled vocabulary folder

- codebook_name:

  A string denoting the book name. Use
  [`list_db_books()`](https://idahoagstats.github.io/factcuratoR/reference/list_db_books.md)
  to see options

- required_only:

  A logical, FALSE returns all columns; TRUE returns required columns
  only

- crop_types:

  A vector containing the crop_types. This is used to select the
  appropriate traits if codebook_name == "trial_data"

## See also

Other access codebook functions:
[`get_col_index()`](https://idahoagstats.github.io/factcuratoR/reference/get_col_index.md),
[`get_variety_db()`](https://idahoagstats.github.io/factcuratoR/reference/get_variety_db.md),
[`list_db_books()`](https://idahoagstats.github.io/factcuratoR/reference/list_db_books.md),
[`readin_db()`](https://idahoagstats.github.io/factcuratoR/reference/readin_db.md)
