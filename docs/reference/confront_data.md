# Wrapper for create_rules() and validate::confront()

This functions summarises the results and fixes the inability for
confront() to detect NAs with grepl. See create_rules() for more
details.

## Usage

``` r
confront_data(df, df_type, db_folder, blends = FALSE, crop_types = NULL)
```

## Arguments

- df:

  A data.frame to be validated

- df_type:

  A string "trial_data" or "trials_metadata" denoting the type of
  validation rules to generate

- db_folder:

  A string path to the database controlled vocabulary folder

- blends:

  A logical denoting whether to check for blends in the variety column.
  The default is FALSE

- crop_types:

  A vector containing the crop_types. This is used to select the
  appropriate traits if codebook_name == "trial_data"

## See also

Other validation functions:
[`create_rules()`](https://idahoagstats.github.io/factcuratoR/reference/create_rules.md),
[`find_col_info()`](https://idahoagstats.github.io/factcuratoR/reference/find_col_info.md),
[`validate_colnames()`](https://idahoagstats.github.io/factcuratoR/reference/validate_colnames.md)
