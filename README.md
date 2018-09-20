
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
> cran <- c("dplyr",     # data frames manipulation
+           "lubridate", # dealing with dates
+           "magrittr",  # pipe operators
+           "purrr",     # functional programming tools
+           "tidyr"      # tidying data
+           )
```

Installing these packages when not already installed:

``` r
> to_install <- !cran %in% installed_packages
> if (any(to_install)) install.packages(cran[to_install])
```

Loading the packages for interactive use at the command line:

``` r
> invisible(lapply(cran, library, character.only = TRUE))
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

Reading, cleaning, reshaping data
---------------------------------

Reading `pre-PACS.xlsx`:

``` r
> prepacs <- readxl::read_excel("../../raw_data/IPL PACS/pre-PACS.xlsx") %>% 
+   transmute(id                = `Material #`,
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
> postpacs <- readxl::read_excel("../../raw_data/IPL PACS/PACS.xlsx") %>% 
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
+   mutate(id = sub("R", "", id)) %>% 
+   mutate_if(is.POSIXct, as.Date)
```

Here, compared to `prepacs`, we don't have serotype and nationality. Let's put `prepacs` and `pacs` together:

``` r
> pacs <- bind_rows(prepacs, postpacs) %>% 
+   mutate(nationality = fix_country_names(nationality),
+          sex         = as.factor(ifelse(sex == "Unknown", NA, tolower(sex))),
+          province    = fix_provinces_names(province),
+          pcr         = as.factor(gsub(" ", "_", tolower(pcr))),
+          ns1         = ns1 %>%
+                          tolower() %>%
+                          gsub(" ", "_", .) %>%
+                          gsub("n/a", NA, .) %>%
+                          as.factor(),
+          serotype    = serotype %>%
+                          tolower() %>%
+                          gsub(" ", "_", .) %>%
+                          sub("not_finish", "not_finished", .) %>%
+                          gsub("not_identify", "not_identified", .) %>% 
+                          as.factor())
```

The final data looks like this:

``` r
> str(pacs)
Classes 'tbl_df', 'tbl' and 'data.frame':   8183 obs. of  17 variables:
 $ id               : chr  "2012-1" "2012-2" "2012-3" "2012-4" ...
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
