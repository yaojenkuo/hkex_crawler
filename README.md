# hkex_crawler
A crawler for HKEX data.

http://sdinotice.hkex.com.hk/filing/di/NSSrchDate.aspx?src=MAIN&lang=EN

- Specify period
- Search All Notices

## Usage

- Open R/RStudio
- Install `rvest`

```r
install.packages("rvest")
library(rvest)
```

- Declare function `hkex_shareholding_disclosure_crawler`
- Call function

```r
test <- hkex_shareholding_disclosure_crawler(start_date = "2017-08-01", end_date = "2017-08-05")
test$record_count
View(test$disclosure_df)
```