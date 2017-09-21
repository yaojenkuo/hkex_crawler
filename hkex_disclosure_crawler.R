# Packages
library(rvest)
library(quantmod)

# Function: single_page_crawler()
single_page_crawler <- function(url) {
  
  # css_selectors for the 11 columns/variables
  cols_css <- paste0("#grdPaging .tbCell:nth-child(", 1:11, ")")
  res_list <- list(form_serial_no = c(),
                   date_of_relevant_event = c(),
                   name_of_listed_corporation = c(),
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

# Function: get_stock_code()
get_stock_code <- function(listed_corp_name, start_date_chr, end_date_chr) {
  stock_code_query_url <- "http://sdinotice.hkex.com.hk/di/NSSrchCorpList.aspx?sa1=cl&scsd=%s/%s/%s&sced=%s/%s/%s&srchCorpName=%s&cn=1&src=MAIN&lang=EN"
  start_date <- unlist(strsplit(start_date_chr, split = "-"))
  end_date <- unlist(strsplit(end_date_chr, split = "-"))
  listed_corp_name_url <- listed_corp_name %>%
    gsub(pattern = "  - H Shares", ., replacement = "") %>%
    gsub(pattern = "\\s", ., replacement = "+")
  if (grepl(pattern = "Guangdong Join-Share Financing", listed_corp_name)) {
    return("01543")
  } else {
    stock_code_query_url <- sprintf(stock_code_query_url, start_date[3], start_date[2], start_date[1], end_date[3], end_date[2], end_date[1], listed_corp_name_url)
    stock_code <- read_html(stock_code_query_url) %>%
      html_nodes(css = ".tbCell:nth-child(1)") %>%
      html_text()
    return(stock_code)
  }
}

# Function: get_ohlcs_on_specific_dates()
get_ohlc_on_specific_date <- function(stock_code, specific_date) {
  stock_code_w_hk <- paste0(stock_code, ".HK")
  tryCatch({
    xts_obj <- getSymbols(stock_code_w_hk, from = specific_date, to = specific_date, env = NULL)
    ohlc_ls <- list(
      ohlc_open = as.numeric(xts_obj[, 1]),
      ohlc_high = as.numeric(xts_obj[, 2]),
      ohlc_low = as.numeric(xts_obj[, 3]),
      ohlc_close = as.numeric(xts_obj[, 4])
    )
    return(ohlc_ls)
  }, warning = function(w) {
    ohlc_ls <- list(
      ohlc_open = NA,
      ohlc_high = NA,
      ohlc_low = NA,
      ohlc_close = NA
    )
    return(ohlc_ls)
  }, error = function(e) {
    ohlc_ls <- list(
      ohlc_open = NA,
      ohlc_high = NA,
      ohlc_low = NA,
      ohlc_close = NA
    )
    return(ohlc_ls)
  })
}

# FunctionL get_52_week_info()
get_52_week_info <- function(stock_code, specific_date) {
  stock_code_w_hk <- paste0(stock_code, ".HK")
  specific_date_format <- base::as.Date(specific_date)
  from_date <- specific_date_format - 52*7
  from_date <- as.character(from_date)
  tryCatch({
    xts_obj <- getSymbols(stock_code_w_hk, from = from_date, env = NULL)
    fifty_two_week <- list(
      fifty_two_week_high = max(as.numeric(xts_obj[, 2]), na.rm = TRUE),
      fifty_two_week_low = min(as.numeric(xts_obj[, 3]), na.rm = TRUE),
      fifty_two_week_volume = mean(as.numeric(xts_obj[, 5]), na.rm = TRUE)
    )
    return(fifty_two_week)
  }, warning = function(w) {
    fifty_two_week <- list(
      fifty_two_week_high = NA,
      fifty_two_week_low = NA,
      fifty_two_week_volume = NA
    )
    return(fifty_two_week)
  }, error = function(e) {
    fifty_two_week <- list(
      fifty_two_week_high = NA,
      fifty_two_week_low = NA,
      fifty_two_week_volume = NA
    )
    return(fifty_two_week)
  })
}

# Function: multiple_page_crawler()
multiple_page_crawler <- function(start_date, end_date) {
  # get_urls() is called here
  urls_to_crawl <- get_urls(start_date, end_date)
  final_res_list <- list(
    form_serial_no = c(),
    date_of_relevant_event = c(),
    name_of_listed_corporation = c(),
    name_of_substantial_shareholder = c(),
    reason_for_disclosure = c(),
    no_of_shares_bought_sold = c(),
    avg_price_per_share = c(),
    no_of_shares_interested = c(),
    percentage_of_issued_share_capital = c(),
    interests_in_shares_of_associated_corp = c(),
    interests_in_debentures = c()
  )
  # loop through all records, single_page_crawler() is called here
  for (i in 1:length(urls_to_crawl)) {
    page_url <- urls_to_crawl[i]
    page_n_list <- single_page_crawler(page_url)
    for (j in names(final_res_list)) {
      final_res_list[[j]] <- c(final_res_list[[j]], page_n_list[[j]])
    }
  }
  
  # make a df
  df <- data.frame(
    form_serial_no = final_res_list$form_serial_no,
    date_of_relevant_event = final_res_list$date_of_relevant_event,
    name_of_listed_corporation = final_res_list$name_of_listed_corporation,
    name_of_substantial_shareholder = final_res_list$name_of_substantial_shareholder,
    reason_for_disclosure = final_res_list$reason_for_disclosure,
    no_of_shares_bought_sold = final_res_list$no_of_shares_bought_sold,
    avg_price_per_share = final_res_list$avg_price_per_share,
    no_of_shares_interested = final_res_list$no_of_shares_interested,
    percentage_of_issued_share_capital = final_res_list$percentage_of_issued_share_capital,
    interests_in_shares_of_associated_corp = final_res_list$interests_in_shares_of_associated_corp,
    interests_in_debentures = final_res_list$interests_in_debentures,
    stringsAsFactors = FALSE
  )
  
  # make a clean df
  # if avg_price_per_share is empty then delete that row
  avg_price_per_share_starts_w_space <- grepl(pattern = "^\\s", df$avg_price_per_share)
  clean_df <- df[!avg_price_per_share_starts_w_space, ]
  
  # keep the Long only data rows
  short_lending_pool_pattern <- "(S)|(P)"
  is_short_or_lending_pool <- grepl(pattern = short_lending_pool_pattern, clean_df$no_of_shares_interested)
  clean_df <- clean_df[!is_short_or_lending_pool, ]
  
  # split currency and amount
  # for avg_price_per_share column
  currency_amount <- unlist(strsplit(clean_df$avg_price_per_share, split = "\\s"))
  currency <- c()
  amount <- c()
  for (i in 1:length(currency_amount)) {
    if (i %% 2 == 1) {
      currency <- c(currency, currency_amount[i])
    } else {
      amount <- c(amount, currency_amount[i])
    }
  }
  clean_df$avg_price_per_share_currency <- currency
  clean_df$avg_price_per_share_num <- as.numeric(amount)
  # drop rows where avg_price_per_share_currency that is not HKD
  clean_df <- clean_df[clean_df$avg_price_per_share_currency == "HKD", ]
  clean_df$avg_price_per_share <- clean_df$avg_price_per_share_num
  drop_cols <- c("avg_price_per_share_currency", "avg_price_per_share_num")
  clean_df <- clean_df[, !(names(clean_df) %in% drop_cols)]
  
  # split (L) notation and amount
  # no_of_shares_bought_sold
  # no_of_shares_interested
  gsub_cols <- c("no_of_shares_bought_sold", "no_of_shares_interested")
  for (gsub_col in gsub_cols) {
    clean_df[, gsub_col] <- clean_df[, gsub_col] %>%
      gsub(pattern = "\\(L\\)", ., replacement = "") %>%
      gsub(pattern = ",", ., replacement = "") %>%
      as.numeric()
  }
  
  # convert date_of_relevant_event to Date format then to character
  clean_df$date_of_relevant_event <- base::as.Date(clean_df$date_of_relevant_event, format = "%d/%m/%Y")
  clean_df$date_of_relevant_event <- as.character(clean_df$date_of_relevant_event)

  # get_stock_code() is called here
  unique_names_of_listed_corps <- unique(clean_df$name_of_listed_corporation)
  stock_codes <- c()
  for (uniq_name in unique_names_of_listed_corps) {
    stock_code <- get_stock_code(uniq_name, start_date, end_date)
    if (identical(stock_code, character(0))) {
      stock_codes <- c(stock_codes, NA)
    } else {
      stock_codes <- c(stock_codes, stock_code)
    }
  }
  stock_code_ref_df <- data.frame(
    unique_names_of_listed_corps = unique_names_of_listed_corps,
    stock_codes = stock_codes
  )
  
  # merge clean_df with stock_code_ref_df
  clean_df_w_stock_codes <- merge(clean_df, stock_code_ref_df, by.x = "name_of_listed_corporation", by.y = "unique_names_of_listed_corps", all.x = TRUE)
  clean_df_w_stock_codes$stock_codes <- substr(clean_df_w_stock_codes$stock_codes, start = 2, stop = 5)
  
  # get_ohlcs_on_specific_dates() is applied here
  #mapply_res_mat <- t(mapply(FUN = get_ohlc_on_specific_date, clean_df_w_stock_codes$stock_codes, clean_df_w_stock_codes$date_of_relevant_event))
  #ohlc_df <- data.frame(mapply_res_mat)
  #clean_df_w_stock_codes <- cbind(clean_df_w_stock_codes, ohlc_df)
  #clean_df_w_stock_codes$diff_avg_price_per_share_open_price <- (clean_df_w_stock_codes$avg_price_per_share - clean_df_w_stock_codes$ohlc_open) / clean_df_w_stock_codes$ohlc_open
  #clean_df_w_stock_codes$diff_avg_price_per_share_close_price <- (clean_df_w_stock_codes$avg_price_per_share - clean_df_w_stock_codes$ohlc_close) / clean_df_w_stock_codes$ohlc_open
  
  # get_52_week_info() is applied here
  #mapply_res_mat <- t(mapply(FUN = get_52_week_info, clean_df_w_stock_codes$stock_codes, clean_df_w_stock_codes$date_of_relevant_event))
  #fifty_two_week_df <- as.data.frame(mapply_res_mat, row.names = NULL)
  #clean_df_w_stock_codes <- cbind(clean_df_w_stock_codes, fifty_two_week_df)
  
  # create return object
  # final_res_list is the original data stored in a list
  # clean_df is the cleaned data stored in a dataframe
  return_object <- list(
    original_disclosure_list = final_res_list,
    clean_disclosure_df = clean_df_w_stock_codes
  )
  return(return_object)
}