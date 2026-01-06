# Create rules from the codebook using validate::validator()

This functions reads in codebooks_all_db.csv and generates rules from
the information provided in this main codebook

## Usage

``` r
create_rules(df_type, db_folder, blends = FALSE, crop_types)

create.rules(df_type, db_folder)
```

## Arguments

- df_type:

  A string "trial_data" or "trials_metadata" denoting the type of
  validation rules to generate

- db_folder:

  A string path to the database controlled vocabulary folder

- blends:

  A logical denoting whether there are blends stored in the variety
  column in df_type = "trial_data"

- crop_types:

  A vector containing the crop_types. This is used to select the
  appropriate traits if codebook_name == "trial_data"

## Details

Note: grepl returns FALSE if the tested object is NA, so NAs are not
correctly detected in grepl checks. Currently, the validator package
does not accept str_detect. As a workaround, for date fields, an extra
check for NAs is implemented.

## See also

Other validation functions:
[`confront_data()`](https://idahoagstats.github.io/factcuratoR/reference/confront_data.md),
[`find_col_info()`](https://idahoagstats.github.io/factcuratoR/reference/find_col_info.md),
[`validate_colnames()`](https://idahoagstats.github.io/factcuratoR/reference/validate_colnames.md)
