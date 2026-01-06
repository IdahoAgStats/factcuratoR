# Read in all codebooks and create a combined cultivar file

Read in all controlled variable codebooks and create a combined cultivar
file in long format.

## Usage

``` r
readin_db(db_folder)

readin.db(db_folder)
```

## Arguments

- db_folder:

  A string path to the database controlled vocabulary folder

## Value

A list of data frames; *\*.csv* files in the controlled vocab folder.

Additionally creates a combined cultivar file, *cultivar.csv*, selecting
*crop, date_added*, and all current variety/alias names for each
*crop_type*. All variety/alias names are gathered in a single column,
linked with a *db_id* number. The *type_db* column tracks whether the
name is an alias or true variety name, and the *intid_db* column
contains the cultivar name in lowercase with all spaces and punctuation
removed, used for matching.

## See also

Other access codebook functions:
[`get_col_index()`](https://idahoagstats.github.io/factcuratoR/reference/get_col_index.md),
[`get_variety_db()`](https://idahoagstats.github.io/factcuratoR/reference/get_variety_db.md),
[`list_db_books()`](https://idahoagstats.github.io/factcuratoR/reference/list_db_books.md),
[`list_db_var()`](https://idahoagstats.github.io/factcuratoR/reference/list_db_var.md)
