# Standardize the columns in the order specified by the codebook

Contains options to add missing columns or remove unnecessary columns
The ordering follows the column order specified in the codebook Any
extra columns will come after, in alphabetical order.

## Usage

``` r
standardize_cols_by_cb(
  df,
  codebook_name,
  add_missing_cols,
  required_only,
  codebook_cols_only,
  db_folder,
  new_col_fill = "",
  cols_keep = NULL
)
```

## Arguments

- df:

  A data.frame with column names to verify against the codebook names
  against e.g. "trial_data", "trials_metadata"

- codebook_name:

  A string that matches the book name in codebooks_all_db.csv

- add_missing_cols:

  A logical denoting whether to add missing columns

- required_only:

  A logical denoting whether to add only required columns. This
  parameter is only called if add_missing_cols = TRUE. Note: This option
  refers only to missing columns If optional columns exist in the df,
  these columns are retained.

- codebook_cols_only:

  A logical denoting whether to remove any columns not specified by the
  codebook

- db_folder:

  A string path to the database controlled vocabulary folder

- new_col_fill:

  A value that will be used to fill newly created columns

- cols_keep:

  A vector containing names of columns to keep (This is a useful
  argument to use when codebook_cols_only = TRUE, but the user wants to
  retain a few other columns that aren't in the codebook)
