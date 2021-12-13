# factcuratoR Package:

The goal of `factcuratoR` is to provide sets of functions to 
help standardize variety testing data for the FACT program.
The functions fall into a few different workflows: 

- read-in the controlled vocabulary
- validate column names and contents according to codebooks
- curate cultivar names

## Installation

You can install `factcuratoR` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("IdahoAgStats/factcuratoR")
```

## Validation and Write functions require codebooks that are standardized
The main codebook should be named `codebooks_all_db.csv`
codebooks_all_db.csv contains:  
- the variables and their descriptions present in each codebook   
- the codebook where the accepted values/ controlled vocabularies are defined
- see example files in tests/testthat/test_controlled_vocab

Usage:
When updating any codebook, the codebooks_all_db.csv must be updated to reflect the change.   
- A newly added variable column would correspond to a new row in codebooks_all_db.csv.   
- A changed variable column name would correspond to change in variable name in an existing row.  Care must be taken as variables may be used in several different codebooks.  Renames should be consistent.  

Columns: 
**value_type**: Denotes the variable type.  
Options:
- categorical (acceptable variables must be specified in values_defined_in or value_range)

- string (validation function will check to ensure is.character())

- integer (validation will check %%1 = 0)

- continuous (acceptable range must be provided in value_range)

- date (validation will check for yyyy-mm-dd format)


**values_defined_in**: If this variable has controlled vocabularies, enter the name of the file that contains the controlled vocabularies    
**value_range**: If this variable does not have controlled vocabularies, enter the accepted values or ranges.  This formatting is used in the qc_validate_fns.R to validate the data.  
  - Entries should be separated by semi-colons (e.g. TRUE; FALSE or 1; 2; 3)
  -  Strings should be quoted (e.g. “value1”; “value2”)  
  -  Range should be in the form: min to max (e.g. 0 to 100) 
  -  Separate range and values using: | (e.g. 0 to 100 | -9)
  -  Due to Excel formatting issues, negative numbers should be prefaced with ‘ (but without closing quotes) (e.g. ‘-180)

