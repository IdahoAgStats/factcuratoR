# Get column index for codebook variables in a list of data frames

This function is used to get the column index numbers for selected
columns. The index vectors are used to set column styling for Excel data
templates, i.e. highlighting required columns. Invalid codebook_names
will throw an error.

## Usage

``` r
get_col_index(
  datalist,
  db_folder,
  codebook_name,
  required_only = TRUE,
  crop_types = NULL
)
```

## Arguments

- datalist:

  A list of dataframes with columns that match codebook variables

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

## Value

Returns a list of integer vectors with the same list names as
`datalist`.

## See also

Other access codebook functions:
[`get_variety_db()`](https://idahoagstats.github.io/factcuratoR/reference/get_variety_db.md),
[`list_db_books()`](https://idahoagstats.github.io/factcuratoR/reference/list_db_books.md),
[`list_db_var()`](https://idahoagstats.github.io/factcuratoR/reference/list_db_var.md),
[`readin_db()`](https://idahoagstats.github.io/factcuratoR/reference/readin_db.md)
