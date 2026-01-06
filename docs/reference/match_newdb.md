# Match names with the new db entries (deprecated)

As of 2022/01/11, this function is deprecated Note: It is important that
the "id_newvar" column refers to the same entries between the output
created by create_new_db_entries() and the auxiliary new cultivar
controlled vocabulary names.

## Usage

``` r
match_newdb(auxiliary_files, filename, names_newvarid, ...)
```

## Arguments

- filename:

  A filename that contains the information created by
  create_new_db_entries() and checked and filled in by the collaborators

- names_newvarid:

  A data.frame of variety names that do not have matches. This
  data.frame is a list element named 'names_newvarid' generated using
  create_newdbnames()

- ...:

  Bare variables to select from the new cultivar name
