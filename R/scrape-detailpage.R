#' Get categories in product detail page
#'
#' Returns all categories (titles) in the detailed product
#' description page. The categories are returned in the order
#' that they appear in on the page, and the categories might
#' not be identical on the detailed product description pages
#' of different products within the same category.
#'
#' @param detailpagehtml html structure from a single geizhals page
#'   listing details of a specific item.
#'
#' @return A character vector with the category names listed
#'   in the specific geizhals page.
#'
#' @examples
#'
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- get_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get categories:
#' get_detailpage_categories(detailpagehtml)
#'
#' @export
get_detailpage_categories <- function(detailpagehtml) {
  ret <- detailpagehtml %>%
    rvest::html_nodes(css = ".gh-data-table__key") %>%
    rvest::html_text()
  return(ret)
}


#' Get categories and their values in product detail page
#'
#' Returns all categories (titles) and their values
#' in the detailed product description page. The categories
#' are returned in the order that they appear in on the page,
#' and the categories might not be identical on the detailed
#' product description pages of different products within
#' the same category.
#'
#' @inheritParams get_detailpage_categories
#'
#' @return A tibble (data.frame) with two columns (key and value),
#'   containing the categories and their values.
#'
#' @examples
#'
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- get_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get categories and their values:
#' get_keyval_tbl(detailpagehtml)
#'
#' @export
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



#' Get price list in product detail page
#'
#' Returns all price values from the price list
#' in the detailed product description page.
#'
#' @inheritParams get_detailpage_categories
#'
#' @return A numeric vector containing the prices.
#'
#' @examples
#'
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- get_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get prices:
#' get_prices(detailpagehtml)
#'
#' @export
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


#' Get a summary of prices in product detail page
#'
#' Returns a summary of all price values from the price list
#' in the detailed product description page. Currently,
#' this summary conains the 3 lowest prices (or \code{NA} if
#' there aren't enough prices on that page), and the median
#' of all prices.
#'
#' @inheritParams get_detailpage_categories
#'
#' @return A tibble (data.frame) with two columns (key and value),
#'   containing the price summary results (key being a descriptive
#'   key like \code{price_min}, value being the respective summary
#'   measure of the prices). The value column is of type
#'   \code{character}, on order to be row-binded to the categories
#'   and their values (which are also of type \code{character}).
#'
#' @examples
#'
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- get_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get prices summary:
#' get_price_summary(detailpagehtml)
#'
#' @export
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

#' Get data from product detail page
#'
#' Returns all categories (titles) and their values
#' in the detailed product description page, as well as
#' a summary of all price values from the price list
#' in the detailed product description page.
#'
#' @inheritParams get_detailpage_categories
#'
#' @return A tibble (data.frame) with two columns (key and value),
#'   containing the categories and their values, as well as the
#'   price summary results (key being a descriptive
#'   key like \code{price_min}, value being the respective summary
#'   measure of the prices). The value column is of type
#'   \code{character}.
#'
#' @examples
#'
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- get_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get data from detailpage:
#' get_single_detailpage(detailpagehtml)
#'
#' @export
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


#' Read html of detailpage urls
#'
#' Retreive the html code for a vector of detailpage urls, returning
#' the urls as well as the html code.
#'
#' @param detailpageurls A character vector containing urls to
#'   sub-pages with detailed product descriptions (as found when following
#'   a link in the listing page).
#'
#' @return A list of length two. The first element, \code{url}, contains
#'   the vector of urls that was passed to the function. The second list
#'   element, \code{html}, contains another list with one entry per url,
#'   containing the html.
#'
#' @examples
#'
#' ## first, get data from all listing pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpages <- get_all_listpages(listpagehtml_list)
#'
#' ## pick only the the first 3 urls (e.g.):
#' wch_urls <- dat_listpages$detailpage_url[1:3]
#' detailpagehtml_list <- read_all_detailpage_html(wch_urls)
#' detailpagehtml_list
#'
#' @export
read_all_detailpage_html <- function(detailpageurls) {
  ## get html for all urls:
  ret <- list(
    url = detailpageurls,
    html = purrr::map(detailpageurls, xml2::read_html)
  )
  return(ret)
}



#' Get data from multiple product detail pages
#'
#' Returns all categories and their values in a list of
#' detailed product description pages, as well as
#' a summary of all price values from the price list
#' in each of the  detailed product description pages.
#' In contrast to the \code{get_single_detailpage}
#' function, the categories describing a product are
#' the columns, and each product is represented as a
#' row in the resulting tibble (data.frame).
#' The tibble has as many columns as there are categories,
#' if a product doesn't feature all categories in its
#' description, this column will be \code{NA}.
#'
#' @param detailpagehtml_list A list of html structure
#'   from multiple geizhals page listing details of a
#'   specific item.
#'
#' @return A tibble (data.frame) with as many columns
#'   as there are distinct categories in all feature
#'   pages, and as many rows as there are products.
#'
#' @examples
#'
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- get_all_listpages(listpagehtml_list)
#' ## pick only the three first detailpage urls:
#' wch_detailpage_urls <- dat_listpage[["detailpage_url"]][1:3]
#' detailpagehtml_list <- read_all_detailpage_html(wch_detailpage_urls)
#' ## get data from all detailpages:
#' dat_detailpages <- get_all_detailpages(detailpagehtml_list)
#' head(dat_detailpages)
#'
#' @export
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
