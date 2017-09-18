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
get_urls <- function(start_date_chr, end_date_chr) {
  start_date <- as.Date(start_date_chr)
  end_date <- as.Date(end_date_chr)
  if (as.integer(start_date) > as.integer(end_date)) {
    return("Start date must be smaller than end date!")
  } else if (as.integer(start_date) < 17350 & as.integer(end_date) < 17350) {
    
    # old urls
    start_date <- unlist(strsplit(start_date_chr, split = "-"))
    end_date <- unlist(strsplit(end_date_chr, split = "-"))
    old_url <- "http://sdinotice.hkex.com.hk/filing/di/NSAllFormDateList.aspx?sa1=da&scsd=%s%%2f%s%%2f%s&sced=%s%%2f%s%%2f%s&src=MAIN&lang=EN"
    old_url <- sprintf(old_url, start_date[3], start_date[2], start_date[1], end_date[3], end_date[2], end_date[1])
    
    # get record counts
    url_doc <- read_html(old_url)
    record_cnt_css <- "#lblRecCount"
    record_cnt <- url_doc %>%
      html_nodes(css = record_cnt_css) %>%
      html_text() %>%
      as.integer
    how_many_pages <- record_cnt %/% 50 + 1
    url_page_n <- paste0(old_url, "&pg=", 1:how_many_pages)
    return(url_page_n)
  } else if (as.integer(start_date) < 17350 & as.integer(end_date) >= 17350) {
    start_date_old <- unlist(strsplit(start_date_chr, split = "-"))
    end_date_old <- c("2017", "07", "02")
    start_date_new <- c("2017", "07", "03")
    end_date_new <- unlist(strsplit(end_date_chr, split = "-"))
    new_url <- "http://sdinotice.hkex.com.hk/di/NSAllFormDateList.aspx?sa1=da&scsd=%s/%s/%s&sced=%s/%s/%s&src=MAIN&lang=EN"
    old_url <- "http://sdinotice.hkex.com.hk/filing/di/NSAllFormDateList.aspx?sa1=da&scsd=%s%%2f%s%%2f%s&sced=%s%%2f%s%%2f%s&src=MAIN&lang=EN"
    new_url <- sprintf(new_url, start_date_new[3], start_date_new[2], start_date_new[1], end_date_new[3], end_date_new[2], end_date_new[1])
    old_url <- sprintf(old_url, start_date_old[3], start_date_old[2], start_date_old[1], end_date_old[3], end_date_old[2], end_date_old[1])
    
    # get record counts
    url_page_n <- c()
    for (url in c(old_url, new_url)) {
      url_doc <- read_html(url)
      record_cnt_css <- "#lblRecCount"
      record_cnt <- url_doc %>%
        html_nodes(css = record_cnt_css) %>%
        html_text() %>%
        as.integer
      how_many_pages <- record_cnt %/% 50 + 1
      url_page_n <- c(url_page_n, paste0(url, "&pg=", 1:how_many_pages))
    }
    return(url_page_n)
  } else if (as.integer(start_date) >= 17350) {
    # new url
    start_date <- unlist(strsplit(start_date_chr, split = "-"))
    end_date <- unlist(strsplit(end_date_chr, split = "-"))
    new_url <- "http://sdinotice.hkex.com.hk/di/NSAllFormDateList.aspx?sa1=da&scsd=%s/%s/%s&sced=%s/%s/%s&src=MAIN&lang=EN"
    new_url <- sprintf(new_url, start_date[3], start_date[2], start_date[1], end_date[3], end_date[2], end_date[1])
    # get record counts
    url_doc <- read_html(new_url)
    record_cnt_css <- "#lblRecCount"
    record_cnt <- url_doc %>%
      html_nodes(css = record_cnt_css) %>%
      html_text() %>%
      as.integer
    how_many_pages <- record_cnt %/% 50 + 1
    url_page_n <- paste0(new_url, "&pg=", 1:how_many_pages)
    return(url_page_n)
  }
}

# Function: multiple_page_crawler()
multiple_page_crawler <- function(start_date, end_date) {
  urls <- get_urls(start_date, end_date)
  if (class(urls) == "list") {
    # old urls
    old_url <- urls$old_url
    
    # new urls
    new_url <- urls$new_url
    
  } else {
    # get record counts
    url_doc <- read_html(urls)
    record_cnt_css <- "#lblRecCount"
    record_cnt <- url_doc %>%
      html_nodes(css = record_cnt_css) %>%
      html_text() %>%
      as.integer
    
    final_res_list <- list(
      form_serial_no = c(),
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
      page_n_list <- single_page_crawler(page_url)
      for (j in names(final_res_list)) {
        final_res_list[[j]] <- c(final_res_list[[j]], page_n_list[[j]])
      }
    }
    return(final_res_list)
  }
}

# Call functions
target_url <- "http://sdinotice.hkex.com.hk/di/NSAllFormDateList.aspx?sa1=da&scsd=18/03/2016&sced=18/04/2017&src=MAIN&lang=EN"
result <- share_dis_crawler(target_url)
test <- get_urls("2017-07-03", "2017-07-04")
