# Find fuzzymatch for variety using stringdist using methods "lv" and "jw"

Note: This function returns the fuzzymatches from stringdist.variety()
and then places stricter requirements on variety names that end in a
number with greater than one digit (e.g. the ending number must be an
exact match with a string of numbers in the fuzzymatch). This
requirement is to limit the number of close matches for numeric variety
names. Because of this condition, must be careful when generating var_id
from the raw names to remove any trailing digits that have no meaning
(e.g. +25% refers to seed increase and not to a cultivar name)

## Usage

``` r
find_fuzzymatch(
  var_noexactmatch,
  data_name,
  intid_col,
  select_before = Sys.Date(),
  select_crops = NULL,
  knitroutputfolder = knitroutputfolder,
  db_folder
)
```

## Arguments

- var_noexactmatch:

  A data.frame of the varieties without exact matches - the data.frame
  should contain a column for variety and internal id (intid)

- data_name:

  A string denoting the name of the dataset

- intid_col:

  A string denoting the name of the internal id column used to join the
  raw data and database

- select_before:

  A string in the format of Ymd. The function returns cultivars that
  were added to the datebase before this specified date.

- select_crops:

  A regular expression of crops separated by \|. Note that this regex
  will filter on the `crop` column rather than `crop_type`. For example,
  there are entries in the wheat file with crop Triticale, so to capture
  both the input should be `select_crops = "wheat|triticale"`.

- knitroutputfolder:

  A folder path to send output

- db_folder:

  A string path to the database controlled vocabulary folder

## See also

Other match variety modules:
[`do_exactmatch()`](https://idahoagstats.github.io/factcuratoR/reference/do_exactmatch.md),
[`process_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/process_fuzzymatch.md),
[`process_std_new_names()`](https://idahoagstats.github.io/factcuratoR/reference/process_std_new_names.md)
