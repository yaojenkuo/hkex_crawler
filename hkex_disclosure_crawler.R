# Packages
library(rvest)

# Function: single_page_crawler()
single_page_crawler <- function(url) {
  
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
  
  page_doc <- read_html(url)
  for (i in 1:length(res_list)) {
    res <- page_doc %>%
      html_nodes(css = cols_css[i]) %>%
      html_text()
    res_list[[i]] <- c(res_list[[i]], res)
  }
  
  return(res_list)
}

# Function: get_urls()
get_urls <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (as.integer(start_date) > as.integer(end_date)) {
    return("Start date must be smaller than end date!")
  } else if (as.integer(start_date) < 17350 & as.integer(end_date) < 17350) {
    # old url
    return("http://sdinotice.hkex.com.hk/filing/di/NSAllFormDateList.aspx?sa1=da&scsd=%s%%2f%s%%2f%s&sced=%s%%2f%s%%2f%s&src=MAIN&lang=EN")
  } else if (as.integer(start_date) < 17350 & as.integer(end_date) >= 17350) {
    new_and_old_urls <- list(
      new_url = "http://sdinotice.hkex.com.hk/di/NSAllFormDateList.aspx?sa1=da&scsd=%s/%s/%s&sced=%s/%s/%s&src=MAIN&lang=EN",
      old_url = "http://sdinotice.hkex.com.hk/filing/di/NSAllFormDateList.aspx?sa1=da&scsd=%s%%2f%s%%2f%s&sced=%s%%2f%s%%2f%s&src=MAIN&lang=EN"
    )
    return(new_and_old_urls)
  } else if (as.integer(start_date) >= 17350) {
    # new url
    return("http://sdinotice.hkex.com.hk/di/NSAllFormDateList.aspx?sa1=da&scsd=%s/%s/%s&sced=%s/%s/%s&src=MAIN&lang=EN")
  }
}


# Call functions
target_url <- "http://sdinotice.hkex.com.hk/di/NSAllFormDateList.aspx?sa1=da&scsd=18/03/2016&sced=18/04/2017&src=MAIN&lang=EN"
result <- share_dis_crawler(target_url)
