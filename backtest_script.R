library(quantmod)

# Get df
result_df <- read.csv("/Users/kuoyaojen/hkex_crawler/hkinsider_2010_2017.csv", stringsAsFactors = FALSE)
colclasses <- lapply(FUN = class, result_df)
col_classes <- c()
for (i in colclasses) {
  col_classes <- c(col_classes, i)
}
col_classes[1] <- "character"
rm(result_df)
result_df <- read.csv("/Users/kuoyaojen/hkex_crawler/hkinsider_2010_2017.csv", colClasses = col_classes)
result_df$date_of_relevant_event <- as.Date(result_df$date_of_relevant_event)

# Get periodic prices
stockcode_relevant_date <- paste(result_df$stock_codes, result_df$date_of_relevant_event)
unique_stockcode_relevant_date <- unique(stockcode_relevant_date)
unique_stockcode_relevant_date_splt <- unlist(strsplit(unique_stockcode_relevant_date, split = "\\s"))
unique_stock_code <- c()
relevant_date <- c()
for (i in 1:length(unique_stockcode_relevant_date_splt)) {
  if (i %% 2 == 0) {
    relevant_date <- c(relevant_date, unique_stockcode_relevant_date_splt[i])
  } else {
    unique_stock_code <- c(unique_stock_code, unique_stockcode_relevant_date_splt[i])
  }
}
unique_stock_code <- paste0(unique_stock_code, ".HK")
relevant_date <- as.Date(relevant_date)
relevant_date_2yr <- c()
for (i in 1:length(relevant_date)) {
  relevant_date_2yr <- c(relevant_date_2yr, seq(relevant_date[i], length.out = 2, by = "2 years")[2])
}
relevant_date_2yr <- as.Date(relevant_date_2yr)
relevant_date <- relevant_date[!(relevant_date_2yr >= Sys.Date() - 1)]
unique_stock_code <- unique_stock_code[!(relevant_date_2yr >= Sys.Date() - 1)]
relevant_dates_lst <- list(
  relevant_date_2q = c(),
  relevant_date_4q = c(),
  relevant_date_6q = c(),
  relevant_date_8q = c()
)
for (i in 1:length(relevant_date)) {
  time_points <- seq(relevant_date[i], length.out = 5, by = "6 months")
  for (j in 1:4) {
    relevant_dates_lst[[j]] <- c(relevant_dates_lst[[j]], time_points[j + 1])
  }
}
backtest_price_lst <- list(
  backtest_price_2q = c(),
  backtest_price_4q = c(),
  backtest_price_6q = c(),
  backtest_price_8q = c()
)
for (i in 1:length(unique_stock_code)) {
  for (j in 1:length(relevant_dates_lst)) {
    tryCatch({
      xts_obj <- suppressWarnings(getSymbols(unique_stock_code[i], from = as.Date(relevant_dates_lst[[j]][i]), to = as.Date(relevant_dates_lst[[j]][i]), env = NULL, warnings = FALSE))
      backtest_price_lst[[j]] <- c(backtest_price_lst[[j]], xts_obj[, 4])
    }, error = function(e) {
      backtest_price_lst[[j]] <- c(backtest_price_lst[[j]], NA)
    })
  }
}