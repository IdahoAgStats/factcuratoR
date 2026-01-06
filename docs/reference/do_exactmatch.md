# Do the steps in the exact match

Do the steps in the exact match

## Usage

``` r
do_exactmatch(
  db_folder,
  data_intid,
  select_before = Sys.Date(),
  select_crops = NULL,
  match_type = "raw",
  rename_df = FALSE,
  is_blends = FALSE,
  rename_df_path = NULL
)
```

## Arguments

- db_folder:

  A string path to the database controlled vocabulary folder

- data_intid:

  A data.frame created by the function create_intid(). If the data.frame
  has a mix of crop_types, it is best to pass in a column with crop_type
  for each variety. Then, the function can ensure that exact matches are
  for the correct crop_type. If varieties for only one crop_type are
  being matched, the select_crops argument can be used

- select_before:

  A string in the format of Ymd. The function returns cultivars that
  were added to the datebase before this specified date.

- select_crops:

  A regular expression of crops separated by \|. Note that this regex
  will filter on the `crop` column rather than `crop_type`. For example,
  there are entries in the wheat file with crop Triticale, so to capture
  both the input should be `select_crops = "wheat|triticale"`.

- match_type:

  A string, either "raw" or "db, " which denotes the type of matching
  procedure to use. If matching raw variety names, use "raw." If
  matching database names, use "db." The difference is that "raw" groups
  by the var_id and is only looking for one match per var_id. The method
  "db" groups by the intid and is looking to match every entry provided.

- rename_df:

  A logical denoting whether to check the rename (misspelling) file
  Otherwise, the matches are pulled from the database list

  - Currently, all the programs renames are kept in one file, but may
    want to consider putting the renames into different files that are
    collected together See get_cultivar_rename()

- is_blends:

  A logical that specifies whether the varieties are blends. Default is
  FALSE

- rename_df_path:

  The path of the file that contains the variety misspellings

## See also

Other match variety modules:
[`find_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/find_fuzzymatch.md),
[`process_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/process_fuzzymatch.md),
[`process_std_new_names()`](https://idahoagstats.github.io/factcuratoR/reference/process_std_new_names.md)
