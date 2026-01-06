# Validate column names against codebook

Validate column names against codebook

## Usage

``` r
validate_colnames(
  df,
  codebook_name,
  db_folder,
  crop_types = NULL,
  cb_name_remove = NULL
)
```

## Arguments

- df:

  A data.frame with column names to verify against the codebook names
  against e.g. "trial_data", "trials_metadata"

- codebook_name:

  A string denoting the book name. Use
  [`list_db_books()`](https://idahoagstats.github.io/factcuratoR/reference/list_db_books.md)
  to see options

- db_folder:

  A string path to the database controlled vocabulary folder

- crop_types:

  A vector containing the crop_types. This is used to select the
  appropriate traits if codebook_name == "trial_data"

- cb_name_remove:

  A string denoting the name of the codebook to remove from the data
  (df) For example, if testing "trial_data", remove the names in df that
  correspond to "trials_metadata"

## See also

Other validation functions:
[`confront_data()`](https://idahoagstats.github.io/factcuratoR/reference/confront_data.md),
[`create_rules()`](https://idahoagstats.github.io/factcuratoR/reference/create_rules.md),
[`find_col_info()`](https://idahoagstats.github.io/factcuratoR/reference/find_col_info.md)
