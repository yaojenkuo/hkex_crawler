# Packages
library(rvest)

# Function
hkex_shareholding_disclosure_crawler <- function(start_date, end_date) {
  hkex_notice_url <- "http://sdinotice.hkex.com.hk/filing/di/NSAllFormDateList.aspx?sa1=da&scsd=%s%%2f%s%%2f%s&sced=%s%%2f%s%%2f%s&src=MAIN&lang=EN"
  start_date <- unlist(strsplit(start_date, split = "-"))
  end_date <- unlist(strsplit(end_date, split = "-"))
  hkex_notice_url <- sprintf(hkex_notice_url, start_date[3], start_date[2], start_date[1], end_date[3], end_date[2], end_date[1])
  
  # get record counts
  url_doc <- read_html(hkex_notice_url)
  record_cnt_css <- "#lblRecCount"
  record_cnt <- url_doc %>%
    html_nodes(css = record_cnt_css) %>%
    html_text() %>%
    as.integer
  
  # get url pages
  how_many_pages <- record_cnt %/% 50 + 1
  url_page_n <- paste0(hkex_notice_url, "&pg=", 1:how_many_pages)
  
  # css_selectors for the 11 columns/variables
  cols_css <- paste0("#grdPaging .tbCell:nth-child(", 1:11, ")")
  res_list <- list(form_serial_no = c(),
                   date_of_relevant_event = c(),
                   name_of_listed_coporation = c(),
                   name_of_substantial_shareholder = c(),
                   reason_for_disclosure = c(),
                   no_of_shares_bought_sold = c(),
                   avg_price_per_share = c(),
                   no_of_shares_interested = c(),
                   percentage_of_issued_share_capital = c(),
                   interests_in_shares_of_associated_corp = c(),
                   interests_in_debentures = c()
  )
  
  # loop through all records
  for (i in 1:length(url_page_n)) {
    page_url <- url_page_n[i]
    page_doc <- read_html(page_url)
    for (j in 1:length(res_list)) {
      res <- page_doc %>%
        html_nodes(css = cols_css[j]) %>%
        html_text()
      res_list[[j]] <- c(res_list[[j]], res)
    }
  }
  
  # added record counts into res_list
  res_list$record_cnt <- record_cnt
  
  # make a df
  disclosure_df <- data.frame(
    form_serial_no = res_list$form_serial_no,
    date_of_relevant_event = res_list$date_of_relevant_event,
    name_of_listed_coporation = res_list$name_of_listed_coporation,
    name_of_substantial_shareholder = res_list$name_of_substantial_shareholder,
    reason_for_disclosure = res_list$reason_for_disclosure,
    no_of_shares_bought_sold = res_list$no_of_shares_bought_sold,
    avg_price_per_share = res_list$avg_price_per_share,
    no_of_shares_interested = res_list$no_of_shares_interested,
    percentage_of_issued_share_capital = res_list$percentage_of_issued_share_capital,
    interests_in_shares_of_associated_corp = res_list$interests_in_shares_of_associated_corp,
    interests_in_debentures = res_list$interests_in_debentures,
    stringsAsFactors = FALSE
  )
  return(list(record_count = res_list$record_cnt,
              disclosure_df = disclosure_df))
}