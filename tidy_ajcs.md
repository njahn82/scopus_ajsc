ASJC Transformation from Scopus Journal data
================

``` r
library(tidyverse)
```

In tidy data, the following rules apply

  - Each variable must have its own column.
  - Each observation must have its own row.
  - Each value must have its own
cell.

<https://r4ds.had.co.nz/tidy-data.html>

### Get Scopus Journal list

``` r
u <- "https://www.elsevier.com/__data/assets/excel_doc/0015/91122/ext_list_october_2019.xlsx"
download.file(u, "data/ext_list_october_2019.xlsx")
```

### Tidy Journal Classifications per Journal

``` r
scopus <- readxl::read_xlsx("data/ext_list_october_2019.xlsx")
scopus_jns <- scopus %>% 
  select(1:4, ajcs_all = `All Science Journal Classification Codes (ASJC)`) %>%
  # subject category one per row
  mutate(ajcs = strsplit(ajcs_all, "; ", fixed=TRUE)) %>%
  unnest(ajcs) %>%
  mutate(ajcs = gsub(";", "", ajcs)) %>%
  # issn type 
  gather(key = "issn_type", value = "issn", `Print-ISSN`, `E-ISSN`) %>%
  filter(!is.na(issn)) %>%
  # tidy issn
  mutate(issn = paste(substr(issn, 1, 4), substr(issn, 5,8), sep = "-"))
head(scopus_jns)
```

    ## # A tibble: 6 x 6
    ##   `Sourcerecord id` `Source Title (Medline… ajcs_all  ajcs  issn_type issn 
    ##               <dbl> <chr>                   <chr>     <chr> <chr>     <chr>
    ## 1       18500162600 21st Century Music      1210;     1210  Print-IS… 1534…
    ## 2       21100447128 3 Biotech               1101; 23… 1101  Print-IS… 2190…
    ## 3       21100447128 3 Biotech               1101; 23… 2301  Print-IS… 2190…
    ## 4       21100447128 3 Biotech               1101; 23… 1305  Print-IS… 2190…
    ## 5       21100779062 3D Printing and Additi… 2209; 25… 2209  Print-IS… 2329…
    ## 6       21100779062 3D Printing and Additi… 2209; 25… 2501  Print-IS… 2329…

### Mapping AJSC Codes to Classification

``` r
ajcs_data <- readxl::read_xlsx("data/ext_list_october_2019.xlsx", 
                                  sheet = "ASJC classification codes") 

ajcs_top <-  ajcs_data %>%
  select(top_code = code, top_level = `Description...5`, super_group = Supergroup) %>%
  mutate(top_code = gsub("\\*", "", top_code)) %>%
  mutate(top_code = gsub("00", "", top_code)) 
ajcs_all <- ajcs_data %>%
  select(code = Code, description = `Description...2`) %>%
  mutate(top_code = substr(code, 1,2)) %>%
  mutate(code = as.character(code)) %>%
  filter(!is.na(code)) %>%
  inner_join(ajcs_top, by = "top_code")
ajcs_all
```

    ## # A tibble: 334 x 5
    ##    code  description                top_code top_level          super_group
    ##    <chr> <chr>                      <chr>    <chr>              <chr>      
    ##  1 1000  Multidisciplinary          10       Multidisciplinary  <NA>       
    ##  2 1100  General Agricultural and … 11       Agricultural and … Life Scien…
    ##  3 1101  Agricultural and Biologic… 11       Agricultural and … Life Scien…
    ##  4 1102  Agronomy and Crop Science  11       Agricultural and … Life Scien…
    ##  5 1103  Animal Science and Zoology 11       Agricultural and … Life Scien…
    ##  6 1104  Aquatic Science            11       Agricultural and … Life Scien…
    ##  7 1105  Ecology, Evolution, Behav… 11       Agricultural and … Life Scien…
    ##  8 1106  Food Science               11       Agricultural and … Life Scien…
    ##  9 1107  Forestry                   11       Agricultural and … Life Scien…
    ## 10 1108  Horticulture               11       Agricultural and … Life Scien…
    ## # … with 324 more rows

``` r
# backup
write_csv(ajcs_all, "data/ajsc_hierarchy.csv")
```

### Merge with journal-level data

``` r
scopus_tidy <- scopus_jns %>%
  inner_join(ajcs_all, by = c("ajcs" = "code")) %>%
  # rename
  select(scopus_id = 1, 
         journal_title = 2,
         issn_type, issn,
         ajcs, description,
         top_code, top_level, super_group
         )
head(scopus_tidy)
```

    ## # A tibble: 6 x 9
    ##   scopus_id journal_title issn_type issn  ajcs  description top_code
    ##       <dbl> <chr>         <chr>     <chr> <chr> <chr>       <chr>   
    ## 1   1.85e10 21st Century… Print-IS… 1534… 1210  Music       12      
    ## 2   2.11e10 3 Biotech     Print-IS… 2190… 1101  Agricultur… 11      
    ## 3   2.11e10 3 Biotech     Print-IS… 2190… 2301  Environmen… 23      
    ## 4   2.11e10 3 Biotech     Print-IS… 2190… 1305  Biotechnol… 13      
    ## 5   2.11e10 3D Printing … Print-IS… 2329… 2209  Industrial… 22      
    ## 6   2.11e10 3D Printing … Print-IS… 2329… 2501  Materials … 25      
    ## # … with 2 more variables: top_level <chr>, super_group <chr>

``` r
# backup 
write_csv(scopus_tidy, "data/scopus_asjc_tidy.csv")
```
