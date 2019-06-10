Because the PACS system was adopted after the surveillance had already started, and because all the data collected prior PACS adoption have not been all entered in the PACS system yet, the data are in several files in the `raw_data/PACS` folder of the DropBox Ecomore2 folder:

-   `pre-PACS.xlsx` that contains data from aterial **\#1** to material **\#3718** and which is the data that were entered before adopting PACS at IPL;
-   `PACS.xls` that contains data from material **\#3370** to Material **\#7463** and which is the data that were entered directly into PACS and retrieved from it;
-   `pacs ID 7464-ID8292_2018-10-02.xlsx` weekly update.

These files will be loaded into the `prepacs` and `postpacs` data frames respectively and binded together in the `pacs` data frame. After merging with the GPS data (see [here](https://github.com/ecomore2/gps)), the cleaned and reshaped data are saved to disk on `pacs.csv` and `pacs.rds` files in the `cleaned_data` folder of the DropBox Ecomore2 folder.

**NOTE:** the age, date of birth, sample date collection and some village names are corrected are patched.