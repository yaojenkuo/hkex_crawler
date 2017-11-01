start_dates <- seq(as.Date("2010-05-01"), length = 8, by = "1 month")
end_dates <- seq(as.Date("2010-06-01"), length = 8, by = "1 month") - 1
start_dates <- as.character(start_dates)
end_dates <- as.character(end_dates)
file_names <- paste0("hkinsider-", start_dates, "-", end_dates, ".csv")
setwd('/Users/kuoyaojen/hkex_crawler')
for (i in 1:length(start_dates)) {
  month_data <- multiple_page_crawler(start_dates[i], end_dates[i])
  write.csv(month_data, file = file_names[i], row.names = FALSE)
  Sys.sleep(runif(1) * 10)
}