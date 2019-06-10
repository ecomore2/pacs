
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pacs

<!-- badges: start -->

<!-- badges: end -->

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

These files will be loaded into the `prepacs` (`pre-PACS.xlsx`) and
`postpacs` (`PACS.xls` and `pacs ID 7464-ID8292_2018-10-02.xlsx`) data
frames and binded together in the `pacs` data frame. After merging with
the GPS data (see [here](https://github.com/ecomore2/gps)), the data are
patched with corrections:

  - `ages_2018-09-25.xlsx` that contains correction patch on ages;
  - `Villages a retrouver.xlsx` that contains correction patch on
    villages names.

The cleaned and reshaped data are saved to the `data\pacs.csv` CSV file
that can be copy-pasted from
[here](https://raw.githubusercontent.com/ecomore2/pacs/master/data/pacs.csv)
or downloaded directly from R into a data
frame:

``` r
if (! "readr" %in% rownames(installed.packages())) install.packages("readr")
pacs <- readr::read_csv("https://raw.githubusercontent.com/ecomore2/pacs/master/data/pacs.csv",
                        col_types = paste(c("icfnD", rep("c", 5), rep("D", 4), rep("f", 3)), collapse = ""))
```

The variables names are meaningful in themselves. A case will be
considered as confirmed if any of the `pcr` and `ns1` is positive.
