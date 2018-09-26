
<!--
IMAGES:
Insert them with: ![alt text](image.png)
You can also resize them if needed: convert image.png -resize 50% image.png
If you want to center the image, go through HTML code:
<div style="text-align:center"><img src ="image.png"/></div>

REFERENCES:
For references: Put all the bibTeX references in the file "references.bib"
in the current folder and cite the references as @key or [@key] in the text.
Uncomment the bibliography field in the above header and put a "References"
title wherever you want to display the reference list.
-->
Preambule
---------

Because the PACS system was adopted after the surveillance had already started, and because all the data collected prior PACS adoption have not been all entered in the PACS system yet, the data are currently in two files, both of them in the `raw_data/PACS` folder of the DropBox Ecomore2 folder:

-   `pre-PACS.xlsx` that contains data from aterial **\#1** to material **\#3718** and which is the data that were entered before adopting PACS at IPL and
-   `PACS.xls` that contains data from material **\#3370** to Material **\#7461** and which is the data that were entered directly into PACS and retrieved from it.

These two files will be loaded into the `prepacs` and `postpacs` data frames respectively and binded together in the `pacs` data frame. After merging with the GPS data (see [here](https://github.com/ecomore2/gps)), the cleaned and reshaped data are saved to disk on `pacs.csv` and `pacs.rds` files in the `cleaned_data` folder of the DropBox Ecomore2 folder.

**NOTE:** the age, date of birth, sample date collection and some village names are corrected are patched.

Packages
--------

Packages currently installed on the system:

``` r
> installed_packages <- rownames(installed.packages())
```

Packages that we need from [CRAN](https://cran.r-project.org):

``` r
> cran <- c("devtools",  # development tools
+           "dplyr",     # data frames manipulation
+           "lubridate", # dealing with dates
+           "magrittr",  # pipe operators
+           "purrr",     # functional programming tools
+           "readxl",    # reading excel files
+           "stringr",   # string operations
+           "tidyr"      # tidying data
+           )
```

Installing these packages when not already installed:

``` r
> to_install <- !cran %in% installed_packages
> if (any(to_install)) install.packages(cran[to_install])
```

We additionally need the `ecomore` package from [GitHub](https://github.com/ecomore2/ecomore):

``` r
> if (! "ecomore" %in% installed_packages)  devtools::install_github("ecomore2/ecomore")
```

Loading the packages for interactive use at the command line:

``` r
> invisible(lapply(c(setdiff(cran, "devtools"), "ecomore"), library, character.only = TRUE))
```

Utilitary functions
-------------------

In the `postpacs` data, some observations have a duplicate with an ID preceded by a "R", meaning that it contains the results of the tests. In these situations, it is of course the observations with the results that should be considered. The function below allows to deal with that. It takes as an input a vector of IDs and returns a logical vector of the same length indicating whether the observation should be kept:

``` r
> keep_observation <- function(x) {
+   grep("R", x, value = TRUE) %>%
+   sub("R", "^", .) %>%
+   sapply(grepl, x) %>% 
+   rowSums() %>% 
+   `!`()
+ }
```

In the `prepacs` data the birth dates sometimes are not in the date format:

-   Because of some problem at importation of the data from excel they sometimes are in the number of days since 1899-12-30.
-   In some other cases, the dates are in MDY format instead of DMY format.
-   Lastly, sometimes the dates do not correspond to real dates at all (e.g. 31st of February).

The following function takes a vector of supposed-to-be dates as an input and deals with the first 2 situations. It tries to convert the data to proper dates. When it fails (third case), it returns an error that can then be handled by the next function. The output of `to_data` is thus a list of the same length as the input vector.

``` r
> to_date <- function(x) {
+   if (grepl("-", x)) return(safely(dmy, mdy)(x)) # tries DMY then MDY
+   ymd("1899-12-30") + as.numeric(x)
+ }
```

As mentioned above, the following function processes the output from `to_date`, replacing errors by `NA`s:

``` r
> easy_fix <- function(x) {
+   result <- x$result[[1]]
+   if (length(x$warnings) > 0 | is.na(result)) return(as.Date(NA))
+   result
+ }
```

The following function combines the 2 previous functions to deal specifically with the birth date data of `prepacs` where the day, month and year are actually separated by `/` instead of `-`. Is thus takes a vector of supposed-to-be dates as an input and returns a vector of the same length, but of proper dates:

``` r
> fix_date <- function(x) {
+   x %>% 
+     lapply(. %>%
+              gsub(" *", "", .) %>%
+              gsub("/", "-", .) %>% 
+              quietly(to_date)() %>% 
+              easy_fix()) %>% 
+     do.call(c, .) # this is an alternative to "unlist" that doesn't drop the Date class
+ }
```

The following function cleans a vector of countries names:

``` r
> fix_country_names <- function(x) {
+   x %>% 
+     tolower %>% 
+     sub("african"   , "africa"         , .) %>% 
+     sub("american"  , "usa"            , .) %>% 
+     sub("belgia[mn]", "belgium"        , .) %>% 
+     sub("[gG]erman*", "germany"        , .) %>% 
+     sub("korean"    , "korea"          , .) %>% 
+     sub("japanese"  , "japan"          , .) %>% 
+     sub("chinese"   , "china"          , .) %>% 
+     sub("french"    , "france"         , .) %>% 
+     sub("vietnamese", "vietnam"        , .) %>% 
+     sub("Foreigner" , "foreign_country", .)
+ }
```

The following function cleans a vector of provinces names:

``` r
> fix_provinces_names <- function(x) {
+   x %>% 
+     sub("bolikhamsay"             , "Bolikhamxai"       , .) %>% 
+     sub("Borlikhamxay"            , "Bolikhamxai"       , .) %>% 
+     sub("hanoi"                   , "Hanoi"             , .) %>% 
+     sub("Huaphan"                 , "Houaphan"          , .) %>% 
+     sub("Khammouan"               , "Khammouane"        , .) %>% 
+     sub("Luangprabang"            , "Louangphabang"     , .) %>% 
+     sub("oudomxai"                , "Oudomxai"          , .) %>% 
+     sub("[sS]alavan"              , "Saravan"           , .) %>% 
+     sub("[vV]ientiane [pP]rovince", "Vientiane Province", .) %>% 
+     sub("Vientiane capital"       , "Vientiane Capital" , .) %>% 
+     sub("xaignabouli"             , "Xaignabouli"       , .) %>% 
+     sub("xekong"                  , "Xekong"            , .) %>% 
+     sub("xiangkhouang"            , "Xiengkhuang"       , .)
+ }
```

The following function removes any year information from the samples IDs:

``` r
> fix_id <- function(x) {
+   as.integer(sub("^\\d*-", "", x))
+ }
```

The following function converts a character vector of age information that may contains entries such as "5 years and 9 months", "1 year and 11 months", "1 year and 1 month", "2 months", "23 days" into an integer vector that contains age in decimal fractions of years:

``` r
> convert2years <- function(x) {
+   require(magrittr) # %>%, %<>% 
+   ym <- which(grepl("and", x))
+   m  <- which(grepl("^\\d* months*", x))
+   d  <- which(grepl("day", x))
+   x[ym] %<>%
+     sub(" *y*e*a*r*s* and ", "-", .) %>% 
+     sub(" months*", "", .) %>%
+     strsplit("-") %>%
+     lapply(as.numeric) %>%
+     sapply(function(x) x[1] + x[2] / 12)
+   x[m] %<>%
+     sub(" months*", "", .) %>%
+     as.numeric() %>%
+     `/`(12)
+   x[d] %<>%
+     sub(" days*", "", .) %>%
+     as.numeric() %>%
+     `/`(365)
+   as.numeric(x)
+ }
```

Reading, cleaning, reshaping data
---------------------------------

Reading `pre-PACS.xlsx`:

``` r
> prepacs <- read_excel("../../raw_data/IPL PACS/pre-PACS.xlsx") %>% 
+   transmute(id                = fix_id(`Material #`),
+             age               =  age,
+             dob               =  fix_date(birthdate),
+             sex               =  sex,
+             nationality       =  Nationality,
+             province          =  Province,
+             district          =  District,
+             village           =  Village,
+             travel_country    = `which country`,
+             where             =  where,
+             onset             = `date onset`,
+             hospitalization   = `date addmit`,
+             consultation      = `date consult`,
+             sample_collection = `date collected blood sample`,
+             pcr               = `result RT PCR`,
+             ns1               = `result NS1`,
+             serotype          = `result serotype`) %>% 
+   mutate(travel_laos   = ifelse(travel_country == "lao", where, NA),
+          travel_abroad = ifelse(travel_country != "lao", where, NA)) %>% 
+   select(id, nationality, sex, age, dob, province, district, village, travel_laos,
+          travel_abroad, onset, hospitalization, consultation, sample_collection,
+          pcr, ns1, serotype) %>% 
+   mutate_if(is.POSIXct, as.Date)
```

Reading `PACS.xlsx`:

``` r
> postpacs <- read_excel("../../raw_data/IPL PACS/PACS.xlsx") %>% 
+   transmute(id                = `Material #`,
+             age               =  Age,
+             dob               = `Date of birth`,
+             sex               =  Gender,
+             home_location     = `Point of origin`,
+             travel_laos       = `Travel in Laos`,
+             travel_abroad     = `Travel abroad`,
+             onset             = `Onset date`,
+             hospitalization   = `Hospitalization date`,
+             consultation      = `Consultation date`,
+             sample_collection = `Sample collection date`,
+             pcr               =  Screening,
+             ns1               = `NS1 antigen`) %>% 
+   separate(home_location, c("country", "province", "district", "village"), "\\.") %>% 
+   select(-country) %>% 
+   filter(keep_observation(id)) %>% 
+   mutate(id = fix_id(sub("R", "", id))) %>% 
+   mutate_if(is.POSIXct, as.Date)
```

Here, compared to `prepacs`, we don't have serotype and nationality. Let's put `prepacs` and `pacs` together:

``` r
> pacs <- prepacs %>% 
+   bind_rows(filter(postpacs, ! id %in% prepacs$id)) %>% 
+   mutate(id          = id,
+          nationality = fix_country_names(nationality),
+          sex         = as.factor(ifelse(sex == "Unknown", NA, tolower(sex))),
+          province    = fix_provinces_names(province),
+          pcr         = as.factor(gsub(" ", "_", tolower(pcr))),
+          ns1         = ns1 %>%
+                          tolower() %>%
+                          gsub(" ", "_", .) %>%
+                          sub("n/a", NA, .) %>%
+                          as.factor(),
+          serotype    = serotype %>%
+                          tolower() %>%
+                          gsub(" ", "_", .) %>%
+                          sub("not_finish"  , "not_finished"  , .) %>%
+                          sub("not_identify", "not_identified", .) %>% 
+                          as.factor())
```

The final data looks like this:

``` r
> str(pacs)
Classes 'tbl_df', 'tbl' and 'data.frame':   7459 obs. of  17 variables:
 $ id               : int  1 2 3 4 5 6 7 8 9 10 ...
 $ nationality      : chr  "laos" "laos" "africa" "foreigner" ...
 $ sex              : Factor w/ 2 levels "female","male": 1 2 1 2 2 1 2 2 2 2 ...
 $ age              : chr  "10" "6" "25" "0" ...
 $ dob              : Date, format: "2012-02-10" NA ...
 $ province         : chr  "Vientiane Capital" "Vientiane Capital" NA "Vientiane Capital" ...
 $ district         : chr  "Sisattanak district" "Hardsaiyfong district" NA "Xaysettha district" ...
 $ village          : chr  "thongkang" "nongheo" "aaa" "phonesinuan" ...
 $ travel_laos      : chr  NA NA NA "Louangphabang" ...
 $ travel_abroad    : chr  NA NA "south africa" NA ...
 $ onset            : Date, format: "2012-03-24" "2012-03-25" ...
 $ hospitalization  : Date, format: "2012-03-25" "2012-03-28" ...
 $ consultation     : Date, format: "2012-03-25" "2012-03-28" ...
 $ sample_collection: Date, format: "2013-03-26" "2012-03-28" ...
 $ pcr              : Factor w/ 4 levels "equivocal","negative",..: 2 2 2 2 4 2 4 2 2 2 ...
 $ ns1              : Factor w/ 5 levels "equivocal","negative",..: 2 2 5 2 5 2 2 2 2 2 ...
 $ serotype         : Factor w/ 7 levels "dengue_1","dengue_2",..: 7 7 7 7 1 7 2 7 7 7 ...
```

Writing to Dropbox's `cleaned_data` folder:

``` r
> write.csv(pacs, "../../cleaned_data/pacs.csv")
> saveRDS(pacs, "../../cleaned_data/pacs.rds")
```

Paching the ages
----------------

``` r
> agefile <- "../../raw_data/IPL PACS/ages_2018-09-25.xlsx"
```

``` r
> agepacs <- read_excel(agefile, "pacs") %>% 
+   rename(id                = `Material #`,
+          age               =  Age,
+          dob               = `Date of birth`,
+          sample_collection = `Sample collection date`) %>% 
+   mutate_if(is.POSIXct, as.Date)
```

``` r
> ageprepacs <- read_excel(agefile, "pre pacs") %>% 
+   transmute(id                = `Material #`,
+             age               =  age,
+             dob               =  birthdate %>% 
+                                    as.numeric() %>% 
+                                    `+`(ymd("1899-12-30")),
+             sample_collection = as.Date(`date collected blood sample`))
Warning in function_list[[i]](value): NAs introduits lors de la conversion
automatique
```

``` r
> age_patch <- bind_rows(ageprepacs, agepacs) %>% 
+   transmute(id                 = fix_id(id),
+             age2               = age,
+             dob2               = dob,
+             sample_collection2 = sample_collection)
```

``` r
> pacs %<>%
+   left_join(age_patch, "id") %>%
+   mutate(age               = convert2years(if_else(is.na(age2), age, age2)),
+          dob               = if_else(is.na(dob2), dob, dob2),
+          sample_collection = if_else(is.na(sample_collection2),
+                                      sample_collection, sample_collection2)) %>% 
+   select(-age2, -dob2, -sample_collection2)
```

Testing the ages
----------------

``` r
> pacs %>%
+   mutate(date2 = if_else(is.na(onset),
+                          if_else(is.na(hospitalization),
+                                  if_else(is.na(consultation),
+                                          sample_collection,
+                                          consultation),
+                                  hospitalization),
+                          onset),
+          age2 = as.numeric((date2 - dob) / 365)) %>% 
+   filter(age2 > age + 1 | age2 < age - 1) %>% 
+   select(id, age, age2, dob, onset, hospitalization, consultation, sample_collection)
# A tibble: 157 x 8
      id   age     age2 dob        onset      hospitalization consultation
   <int> <dbl>    <dbl> <date>     <date>     <date>          <date>      
 1    25    10  2.01e+1 1992-05-03 2012-05-20 2012-05-23      2012-05-24  
 2    55    53  5.07e+1 1961-11-07 2012-06-25 2012-06-26      2012-06-26  
 3    80    22  2.32e+1 1989-05-02 2012-07-14 NA              2012-07-16  
 4    82    27  3.04e+1 1982-02-06 2012-07-08 2012-07-13      2012-07-13  
 5    86     5 -3.32e-1 2012-11-06 2012-07-08 2012-07-13      2012-07-13  
 6    94    54  5.16e+1 1960-12-14 2012-07-16 NA              2012-07-18  
 7   125     8 -8.22e-3 2012-08-01 2012-07-29 2012-07-31      2012-07-31  
 8   144    18  2.02e+1 1992-06-07 2012-08-04 NA              2012-08-08  
 9   152    37  3.58e+1 1976-11-13 2012-08-10 NA              2012-08-12  
10   158    36  3.75e+1 1975-02-14 2012-08-06 NA              2012-08-09  
# ... with 147 more rows, and 1 more variable: sample_collection <date>
```

Patching villages names
-----------------------

Reading the corrected village names:

``` r
> villages_patch <- read_excel("../../raw_data/IPL PACS/Villages a retrouver.xlsx", "Ecomore 2") %>%
+   transmute(id   = id,
+             Bhan = Bhan %>%
+                    str_to_title() %>% 
+                    trimws() %>% 
+                    gsub("  *", " ", .))
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1822 / R1822C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1823 / R1823C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1824 / R1824C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1825 / R1825C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1826 / R1826C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1827 / R1827C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1828 / R1828C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1829 / R1829C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1830 / R1830C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1831 / R1831C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1832 / R1832C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1833 / R1833C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1834 / R1834C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1835 / R1835C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1836 / R1836C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1837 / R1837C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1838 / R1838C12: got 'Nouveau Village: REUNI
avec Nongbouathong-Nua et thai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1844 / R1844C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1845 / R1845C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1846 / R1846C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1847 / R1847C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1848 / R1848C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1849 / R1849C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1850 / R1850C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1851 / R1851C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1852 / R1852C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1853 / R1853C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1854 / R1854C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1855 / R1855C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1856 / R1856C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1857 / R1857C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L1858 / R1858C12: got 'REUNI avec Nongdouang,
thong et nua'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting numeric in A1898 / R1898C1: got 'd'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2021 / R2021C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2022 / R2022C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2023 / R2023C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2024 / R2024C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2025 / R2025C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2026 / R2026C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2027 / R2027C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2028 / R2028C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L2029 / R2029C12: got 'Reunion Nongteng-Nua,
Nongteng-Tai,'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3245 / R3245C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3246 / R3246C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3247 / R3247C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3248 / R3248C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3249 / R3249C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3250 / R3250C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3251 / R3251C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3252 / R3252C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3253 / R3253C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3254 / R3254C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3255 / R3255C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3256 / R3256C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3257 / R3257C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3258 / R3258C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3259 / R3259C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3260 / R3260C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3261 / R3261C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3262 / R3262C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3263 / R3263C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3264 / R3264C12: got 'Reuni Somvan nuea et
Tai'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3383 / R3383C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3384 / R3384C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3385 / R3385C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3386 / R3386C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3387 / R3387C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3388 / R3388C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3389 / R3389C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3390 / R3390C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3391 / R3391C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3392 / R3392C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3393 / R3393C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3394 / R3394C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3395 / R3395C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3396 / R3396C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3397 / R3397C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3398 / R3398C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3399 / R3399C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3400 / R3400C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3401 / R3401C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3402 / R3402C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3403 / R3403C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3404 / R3404C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3405 / R3405C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3406 / R3406C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3407 / R3407C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3408 / R3408C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3409 / R3409C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3410 / R3410C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3411 / R3411C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3412 / R3412C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3413 / R3413C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3414 / R3414C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3415 / R3415C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3416 / R3416C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3417 / R3417C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3418 / R3418C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3419 / R3419C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3420 / R3420C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3421 / R3421C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3422 / R3422C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3423 / R3423C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3424 / R3424C12: got 'Reunion Thatlouang Nua,
thai et kang'
Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
shim, : Expecting logical in L3425 / R3425C12: got 'Reunion Thatlouang Nua,
thai et kang'
```

Testing the village patch:

``` r
> tmp <- villages_patch %>%
+   mutate(has_village = ! is.na(Bhan)) %>% 
+   left_join(select(mutate(pacs, positive = pcr == "positive" | ns1 == "positive"), id, positive), "id")
```

``` r
> with(tmp, table(has_village, positive))
           positive
has_village FALSE TRUE
      FALSE   163  387
      TRUE    329 1473
```

``` r
> tmp %>% filter((! has_village) & positive) %>% select(id) %>% unlist() %>% unname() %>% sort() %>% cat()
3 5 7 13 27 39 53 61 79 91 99 101 108 113 126 140 141 153 157 162 163 177 195 210 228 261 263 304 305 312 321 338 346 347 353 354 357 358 383 392 397 404 405 406 408 412 436 439 444 452 454 457 500 501 502 505 509 511 512 519 527 529 531 534 537 544 547 555 558 562 574 575 622 623 628 647 650 652 673 679 682 690 691 695 725 727 732 733 741 749 755 756 765 771 773 788 806 810 818 821 827 838 844 858 860 861 864 873 874 884 895 928 974 987 989 1010 1011 1031 1036 1037 1042 1046 1047 1054 1056 1058 1059 1064 1065 1069 1092 1096 1101 1103 1111 1118 1120 1122 1126 1137 1141 1142 1146 1161 1162 1166 1171 1176 1183 1186 1189 1195 1196 1197 1200 1201 1202 1215 1217 1226 1228 1239 1242 1246 1262 1274 1284 1285 1295 1299 1326 1334 1348 1352 1353 1354 1355 1382 1390 1394 1397 1399 1402 1404 1410 1413 1418 1420 1425 1438 1441 1450 1452 1460 1461 1463 1475 1478 1488 1491 1497 1503 1515 1529 1555 1559 1560 1565 1583 1594 1601 1610 1615 1623 1631 1639 1648 1669 1673 1678 1680 1688 1720 1732 1739 1765 1773 1779 1789 1794 1804 1811 1815 1820 1836 1858 1885 1891 1899 1929 1931 1932 1937 1945 1955 1966 2027 2029 2079 2105 2109 2113 2150 2165 2174 2194 2202 2274 2276 2279 2280 2350 2356 2376 2380 2421 2431 2432 2433 2434 2435 2444 2445 2456 2467 2475 2505 2521 2540 2557 2558 2559 2582 2591 2592 2653 2690 2691 2709 2717 2761 2762 2763 2779 2783 2822 2823 2824 2826 2830 2864 2866 2867 2924 2932 2933 2934 2935 2946 2951 2958 2959 3000 3058 3059 3061 3062 3073 3076 3077 3080 3084 3087 3089 3092 3093 3095 3098 3102 3113 3114 3119 3120 3123 3124 3125 3130 3134 3135 3157 3172 3173 3174 3175 3177 3178 3188 3189 3216 3220 3221 3223 3224 3241 3246 3260 3262 3263 3285 3341 3350 3390 3396 3397 3416 3477 3478 3481 3551 3585 3588 3597 3600 3608 3620 3624 3637 3646 3704 3720 3748 3752 3753 3755 3759 3760 3762
```

``` r
> tmp %>% filter(has_village & (! positive)) %>% select(id) %>% unlist() %>% unname() %>% sort() %>% cat()
1 2 4 9 10 11 12 15 17 18 20 21 22 24 30 31 41 42 43 44 45 46 48 50 52 54 57 59 60 63 64 67 68 69 70 73 74 75 76 77 78 80 81 82 83 84 85 86 87 88 89 93 95 97 100 102 105 110 115 117 120 121 122 128 129 361 370 398 399 400 401 402 403 445 466 483 492 605 674 680 684 685 719 730 760 1044 1238 1339 1375 1415 1429 1536 1537 1602 1829 1830 1864 1900 1946 2082 2083 2122 2292 2303 2334 2375 2399 2423 2476 2513 2515 2522 2595 2597 2604 2612 2613 2639 2642 2695 2735 2796 2807 2809 2846 2858 2859 2869 2870 2872 2876 2879 2881 2888 2891 2893 2896 2898 2899 2900 2903 2906 2907 2908 2916 2919 2920 2921 2925 2926 2928 2929 2941 2942 2943 2944 2945 2947 2950 2963 2964 2987 2994 2995 2998 3001 3002 3003 3005 3006 3010 3019 3023 3024 3025 3026 3029 3032 3033 3036 3037 3039 3041 3042 3043 3044 3045 3046 3047 3052 3054 3055 3065 3067 3068 3071 3072 3081 3085 3096 3099 3101 3107 3108 3111 3112 3117 3129 3132 3142 3144 3146 3147 3148 3149 3150 3151 3153 3155 3156 3169 3170 3181 3183 3184 3197 3215 3217 3225 3227 3229 3239 3240 3248 3249 3250 3253 3258 3265 3266 3269 3275 3282 3290 3295 3296 3306 3311 3313 3321 3331 3340 3342 3351 3360 3365 3373 3389 3394 3398 3400 3403 3404 3409 3410 3415 3425 3435 3436 3437 3443 3445 3446 3448 3449 3450 3451 3454 3455 3467 3468 3479 3482 3517 3520 3533 3541 3587 3589 3598 3602 3609 3611 3612 3618 3621 3622 3623 3625 3627 3628 3629 3635 3638 3641 3645 3649 3650 3652 3653 3672 3674 3675 3676 3677 3678 3679 3680 3681 3682 3683 3685 3689 3691 3693 3699 3700 3701 3703
```

``` r
> pacs %>%
+   left_join(villages_patch, "id") %>% 
+   mutate(village = if_else(is.na(Bhan), village, Bhan)) %>% 
+   select(-Bhan)
# A tibble: 7,459 x 17
      id nationality sex    age   dob        province   district   village
   <dbl> <chr>       <fct>  <chr> <date>     <chr>      <chr>      <chr>  
 1     1 laos        female 10    2012-02-10 Vientiane… Sisattana… Thong-…
 2     2 laos        male   6     NA         Vientiane… Hardsaiyf… Nongheo
 3     3 africa      female 25    1987-05-27 <NA>       <NA>       aaa    
 4     4 foreigner   male   0     NA         Vientiane… Xaysettha… Phonsi…
 5     5 japan       male   5     NA         <NA>       <NA>       <NA>   
 6     6 laos        female 4     2008-06-06 <NA>       <NA>       <NA>   
 7     7 japan       male   64    NA         <NA>       <NA>       <NA>   
 8     8 laos        male   13    1998-06-17 Vientiane… Chanthabu… Phonsa…
 9     9 laos        male   5     2006-12-12 Vientiane… Xaysettha… Phonxai
10    10 laos        male   14    NA         Vientiane… Xaysettha… Chomma…
# ... with 7,449 more rows, and 9 more variables: travel_laos <chr>,
#   travel_abroad <chr>, onset <date>, hospitalization <date>,
#   consultation <date>, sample_collection <date>, pcr <fct>, ns1 <fct>,
#   serotype <fct>
```
