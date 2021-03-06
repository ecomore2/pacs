
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PACS data

<!-- badges: start -->

<!-- badges: end -->

PACS (Pathogen Asset Control System) is the name of the data system used
at Institut Pasteur du Laos. In the context of the
[Ecomore2](http://www.ecomore.org) project, PACS is the source of
epidemiological data. It basically contains one line per sample (here,
in most of cases, a sample corresponds to a case) with age, gender,
time, space, confirmation test and serotype information.

Because the PACS system was adopted after the surveillance had already
started, and because all the data collected prior PACS adoption have not
been all entered in the PACS system yet, the data are in several files
in the `raw_data/IPL PACS` folder of the DropBox Ecomore2 folder:

  - `pre-PACS.xlsx` that contains data from aterial **\#1** to material
    **\#3718** and which is the data that were entered before adopting
    PACS at IPL;
  - `PACS.xls` that contains data from material **\#3370** to Material
    **\#7463** and which is the data that were entered directly into
    PACS and retrieved from it;
  - `pacs ID 7464-ID8292_2018-10-02.xlsx` weekly update.

In the [cleaning
pipeline](https://ecomore2.github.io/pacs/make_data.html), these files
are loaded into the `prepacs` (`pre-PACS.xlsx`) and `postpacs`
(`PACS.xls` and `pacs ID 7464-ID8292_2018-10-02.xlsx`) data frames and
binded together in the `pacs` data frame. The data are patched with
corrections:

  - `ages_2018-09-25.xlsx` that contains correction patch on ages;
  - `Villages a retrouver.xlsx` that contains correction patch on
    villages names;
  - plus other dates, provinces and districts names that are patched
    directly in the pipeline.

The cleaned and reshaped data are saved to the
[`data/pacs.csv`](https://raw.githubusercontent.com/ecomore2/pacs/master/data/pacs.csv)
CSV file that can be copied and paste to a text file on your computer or
downloaded directly from R into a data
frame:

``` r
if (! "readr" %in% rownames(installed.packages())) install.packages("readr")
pacs <- readr::read_csv("https://raw.githubusercontent.com/ecomore2/pacs/master/data/pacs.csv",
                        col_types = paste(c("icfnD", rep("c", 5), rep("D", 4), rep("f", 3)), collapse = ""))
```

The variables names are meaningful in themselves. A case will be
considered as confirmed if at least one of the `pcr` and `ns1` variables
is positive.

This [summary](https://ecomore2.github.io/pacs/summarize_data.html)
provides a real-time overview of the current state of the PACS data set,
highlighting problems that remain to be fixed.
