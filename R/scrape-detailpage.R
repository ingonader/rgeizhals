
## get categories:
#' Title
#'
#' @param detailpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_detailpage_categories <- function(detailpagehtml) {
  ret <- detailpagehtml %>%
    rvest::html_nodes(css = ".gh-data-table__key") %>%
    rvest::html_text()
  return(ret)
}
#get_detailpage_categories(detailpagehtml)


#' Title
#'
#' @param detailpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_keyval_tbl <- function(detailpagehtml) {
  ## get keys (categories):
  keys_from_table <- get_detailpage_categories(detailpagehtml)

  ## get text of all table rows (some containing keys, some not):
  keys_value_text_all <- detailpagehtml %>%
    rvest::html_nodes(css = ".gh-data-table__row") %>%
    rvest::html_text()

  ## get keyval-text that contain keys:
  keys_value_text_sel <- stringr::str_subset(keys_value_text_all, paste(keys_from_table, collapse = "|"))

  ## get keys from keyvalue text:
  keys <- stringr::str_extract(keys_value_text_sel,  paste(keys_from_table, collapse = "|"))

  ## remove keys to get values:
  vals <- stringr::str_replace_all(keys_value_text_sel,  paste(keys, collapse = "|"), "")

  ## remove unnecessary characters:
  vals <- stringr::str_replace_all(vals, "[\n]", "")

  ## make data.frame:
  ret <- tibble::tibble(key = keys, value = vals)

  ## remove duplicates:
  ret <- ret[!duplicated(ret), ]
  return(ret)
}
#get_keyval_tbl(detailpagehtml)


#' Title
#'
#' @param detailpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_prices <- function(detailpagehtml) {
  ## get prices:
  ret <- detailpagehtml %>%
    rvest::html_nodes(css = ".offer__price") %>%            ## get prices
    rvest::html_text()
  ## remove first entry (table header):
  ret <- ret[-1]

  ## convert to numerical:
  ret <- ret %>% stringr::str_extract("^.*?[0-9, ]{1,}") %>%  ## get first occurenc of a number
    stringr::str_replace_all("[^0-9,]", "") %>%               ## get numerical parts only
    stringr::str_replace_all(",", "\\.") %>%                  ## "," comma to "." comma
    as.numeric()
  return(ret)
}
#get_prices(detailpagehtml)


#' Title
#'
#' @param detailpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_price_summary <- function(detailpagehtml) {
  prices <- get_prices(detailpagehtml = detailpagehtml)
  ## sort (just in case):
  prices <- sort(prices)
  ## return summary:
  ret_val <- c(min(prices), prices[2], prices[3], stats::median(prices))
  ret_key <- c("price_min", "price_2nd_min", "price_3rd_min", "price_median")
  ret <- tibble::tibble(
    key = ret_key,
    value = ret_val
  )
  return(ret)
}

#' Title
#'
#' @param detailpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_single_detailpage <- function(detailpagehtml) {
  ## get data:
  ret_keyval <- get_keyval_tbl(detailpagehtml)
  ret_price_summary <- get_price_summary(detailpagehtml)

  ## modify data types (all character, currently):
  ret_price_summary[["value"]] <- as.character(ret_price_summary[["value"]])
  ret <- dplyr::bind_rows(
    ret_keyval,
    ret_price_summary
  )
  return(ret)
}



#' Title
#'
#' @param detailpageurls
#'
#' @return
#' @export
#'
#' @examples
read_all_detailpage_html <- function(detailpageurls) {
  ## get html for all urls:
  ret <- list(
    url = detailpageurls,
    html = purrr::map(detailpageurls, xml2::read_html)
  )
  return(ret)
}

#' Title
#'
#' @param detailpagehtml_list
#'
#' @return
#' @export
#'
#' @examples
get_all_detailpages <- function(detailpagehtml_list) {
  ## get detailpage tibble:
  singlepage_list <- purrr::map(detailpagehtml_list$html, get_single_detailpage)

  ## add url to tibble (to serve as join key later):
  singlepage_list_with_url <- purrr::map2(singlepage_list, detailpagehtml_list$url, ~ dplyr::bind_rows(
    tibble::tibble(key = "url", value = .y),
    .x
  ))

  ## get a list of all keys in all of the detailpage tibbles:
  all_keys <- purrr::map(singlepage_list_with_url, ~ .x[["key"]]) %>%
    unlist() %>% unique()

  ## build tibble by joining all detailpage tibbles to key-tibble,
  ## and transposing wide->long:
  detaildat_wide <- tibble::tibble(key = all_keys)
  detaildat_long <- NULL
  # detaildat_wide_1 <- dplyr::left_join(detaildat_wide,
  #                                      singlepage_list_with_url[[1]],
  #                                      by = "key")
  for (i in seq_along(singlepage_list_with_url)) {
    detaildat_wide_tmp <- dplyr::left_join(detaildat_wide, singlepage_list_with_url[[i]], by = "key")
    detaildat_long_tmp <- tidyr::spread(detaildat_wide_tmp, key = "key", value = "value")
    detaildat_long <- dplyr::bind_rows(detaildat_long, detaildat_long_tmp)
  }
  return(detaildat_long)
}
#get_all_detailpages(detailpagehtml_list)
