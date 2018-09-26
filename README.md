
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

These two files will be loaded into the `prepacs` and `postpacs` data frames respectively and binded together in the `pacs` data frame. The cleaned and reshaped data are in the `cleaned_data` folder of the DropBox Ecomore2 folder.

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
