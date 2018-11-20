#' Parse categories in product detail page
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
#' \dontrun{
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- parse_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get categories:
#' parse_detailpage_categories(detailpagehtml)
#' }
#'
#' @export
parse_detailpage_categories <- function(detailpagehtml) {
  ret <- detailpagehtml %>%
    rvest::html_nodes(css = "#productdata") %>%
    rvest::html_nodes(css = ".gh-data-table__key") %>%
    rvest::html_text()
  return(ret)
}


#' Parse categories and their values in product detail page
#'
#' Returns all categories (titles) and their values
#' in the detailed product description page. The categories
#' are returned in the order that they appear in on the page,
#' and the categories might not be identical on the detailed
#' product description pages of different products within
#' the same category.
#'
#' @inheritParams parse_detailpage_categories
#'
#' @return A tibble (data.frame) with two columns (key and value),
#'   containing the categories and their values.
#'
#' @examples
#' \dontrun{
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- parse_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get categories and their values:
#' parse_keyval_tbl(detailpagehtml)
#' }
#'
#' @export
parse_keyval_tbl <- function(detailpagehtml) {
  ## if there is data...
  if (!is.na(detailpagehtml)) {
    ## ...get keys (categories):
    keys <- parse_detailpage_categories(detailpagehtml)

    ## get values:
    vals <- detailpagehtml %>%
      rvest::html_nodes(css = "#productdata") %>%
      rvest::html_nodes(css = ".gh-data-table__value") %>%
      rvest::html_text()

    ## make data.frame:
    ret <- tibble::tibble(key = keys, value = vals)

    ## remove duplicates:
    ret <- ret[!duplicated(ret), ]
  } else {
    ## if there is no data, just return the same structure with NA's:
    ret <- tibble::tibble(key = NA, value = NA)
  }
  return(ret)
}



#' Parse price list in product detail page
#'
#' Returns all price values from the price list
#' in the detailed product description page.
#'
#' @inheritParams parse_detailpage_categories
#'
#' @return A numeric vector containing the prices.
#'
#' @examples
#' \dontrun{
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- parse_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get prices:
#' parse_prices(detailpagehtml)
#' }
#'
#' @export
parse_prices <- function(detailpagehtml) {
  ## get prices:
  ret <- detailpagehtml %>%
    rvest::html_nodes(css = ".offer__price") %>%            ## get prices
    rvest::html_text()
  ## remove first entry (table header):
  ret <- ret[-1]

  ## convert to numerical:
  ret <- ret %>%
    ## get first occurenc of a number:
    stringr::str_extract("^.*[0-9,]{1,}") %>%
    ## get only the number:
    stringr::str_extract("[0-9]{1,},[0-9]{0,}") %>%
    ## get numerical parts only:
    stringr::str_replace_all("[^0-9,]", "") %>%
    ## "," comma to "." comma:
    stringr::str_replace_all(",", "\\.") %>%
    as.numeric()
  return(ret)
}


#' Calculate a summary of prices in product detail page
#'
#' Returns a summary of all price values from the price list
#' in the detailed product description page. Currently,
#' this summary contains the 3 lowest prices (or \code{NA} if
#' there aren't enough prices on that page), and the median
#' of all prices.
#'
#' @inheritParams parse_detailpage_categories
#'
#' @return A tibble (data.frame) with two columns (key and value),
#'   containing the price summary results (key being a descriptive
#'   key like \code{price_min}, value being the respective summary
#'   measure of the prices). The value column is of type
#'   \code{character}, in order to be row-binded to the categories
#'   and their values (which are also of type \code{character}).
#'
#' @examples
#' \dontrun{
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- parse_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get prices summary:
#' calc_price_summary(detailpagehtml)
#' }
#'
#' @export
calc_price_summary <- function(detailpagehtml) {
  ## if there is html data...
  if (!is.na(detailpagehtml)) {
    prices <- parse_prices(detailpagehtml = detailpagehtml)
    ## sort (just in case):
    prices <- sort(prices)
    ## return summary:
    ret_val <- c(min(prices), prices[2], prices[3], stats::median(prices))
    ret_key <- c("price_min", "price_2nd_min", "price_3rd_min", "price_median")
    ret <- tibble::tibble(
      key = ret_key,
      value = ret_val
    )
  } else {
    ## if there is no html data, just return NA's in a similar structure:
    ret_val <- c(rep(NA, 4))
    ret_key <- c("price_min", "price_2nd_min", "price_3rd_min", "price_median")
    ret <- tibble::tibble(
      key = ret_key,
      value = ret_val
    )
  }
  return(ret)
}

#' Parse data from product detail page
#'
#' Returns all categories (titles) and their values
#' in the detailed product description page, as well as
#' a summary of all price values from the price list
#' in the detailed product description page.
#'
#' @inheritParams parse_detailpage_categories
#'
#' @return A tibble (data.frame) with two columns (key and value),
#'   containing the categories and their values, as well as the
#'   price summary results (key being a descriptive
#'   key like \code{price_min}, value being the respective summary
#'   measure of the prices). The value column is of type
#'   \code{character}.
#'
#' @examples
#' \dontrun{
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- parse_all_listpages(listpagehtml_list)
#' ## get url of a single detail page and read html:
#' url_detailpage <- dat_listpage[["detailpage_url"]][1]
#' detailpagehtml <- xml2::read_html(url_detailpage)
#' ## get data from detailpage:
#' parse_single_detailpage(detailpagehtml)
#' }
#'
#' @export
parse_single_detailpage <- function(detailpagehtml) {
  ## get data:
  ret_keyval <- parse_keyval_tbl(detailpagehtml)
  ret_price_summary <- calc_price_summary(detailpagehtml)

  ## modify data types (all character, currently):
  ret_price_summary[["value"]] <- as.character(ret_price_summary[["value"]])
  ret <- dplyr::bind_rows(
    ret_keyval,
    ret_price_summary
  )
  return(ret)
}


#' Fetch html of detailpage urls
#'
#' Retrieve the html code for a vector of detailpage urls, returning
#' the urls as well as the html code.
#'
#' @param detailpageurls A character vector containing urls to
#'   sub-pages with detailed product descriptions (as found when following
#'   a link in the listing page).
#' @param max_items A numeric (integer) vector of length one, specifying
#'   the maximum number of items to scrape. (Default: \code{Inf}).
#'   If \code{max_items} is smaller than the length of the passed urls
#'   in \code{detailpageurls}, only the first \code{max_items} entries
#'   are fetched.
#'
#' @return A list of length two. The first element, \code{url}, contains
#'   the vector of urls that was passed to the function. The second list
#'   element, \code{html}, contains another list with one entry per url,
#'   containing the html.
#'
#' @examples
#' \dontrun{
#' ## first, get data from all listing pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpages <- parse_all_listpages(listpagehtml_list)
#'
#' ## now, get (first three) detailpages:
#' urls <- dat_listpages$detailpage_url
#' detailpagehtml_list <- fetch_all_detailpage_html(urls, max_items = 3)
#' detailpagehtml_list
#' }
#'
#' @export
fetch_all_detailpage_html <- function(detailpageurls, max_items = Inf) {
  ## check if there are more urls than max_items:
  if (length(detailpageurls) > max_items) {
    detailpageurls <- detailpageurls[1:max_items]
  }
  ## get html for all urls:
  ret <- list(
    url = detailpageurls,
    html = purrr::map(detailpageurls, function(i) {
      message("Fetching detailpage ", i, "...")
      ret <- try(xml2::read_html(i), silent = TRUE)
      ## if it fails, return NA:
      if (class(ret)[1] == "try-error") {
        warning("Something unexpected happened when fetching listpage. \n",
                "(", ret[1], ")\n",
                "Returning NA instead of web page html.")
        ret <- NA
      }

      return(ret)
    })
  )
  message("Done.")
  return(ret)
}



#' Parse data from multiple product detail pages
#'
#' Returns all categories and their values in a list of
#' detailed product description pages, as well as
#' a summary of all price values from the price list
#' in each of the  detailed product description pages.
#' In contrast to the \code{parse_single_detailpage}
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
#' \dontrun{
#' ## get data from multiple geizhals category pages:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' dat_listpage <- parse_all_listpages(listpagehtml_list)
#' ## pick only the three first detailpage urls:
#' wch_detailpage_urls <- dat_listpage[["detailpage_url"]][1:3]
#' detailpagehtml_list <- fetch_all_detailpage_html(wch_detailpage_urls)
#' ## get data from all detailpages:
#' dat_detailpages <- parse_all_detailpages(detailpagehtml_list)
#' head(dat_detailpages)
#' }
#'
#' @export
parse_all_detailpages <- function(detailpagehtml_list) {
  ## get detailpage tibble:
  singlepage_list <- purrr::map(
    detailpagehtml_list$html,
    parse_single_detailpage)

  ## add url to tibble (to serve as join key later):
  singlepage_list_with_url <- purrr::map2(
    singlepage_list, detailpagehtml_list$url,
    ~ dplyr::bind_rows(
      tibble::tibble(key = "url", value = .y),
      .x
  ))

  ## get a list of all keys in all of the detailpage tibbles:
  all_keys <- purrr::map(singlepage_list_with_url, ~ .x[["key"]]) %>%
    unlist() %>% unique()

  ## build tibble by joining all detailpage tibbles to tibble with keys,
  ## and transposing wide to long:
  detaildat_wide <- tibble::tibble(key = all_keys)
  detaildat_long <- NULL
  # detaildat_wide_1 <- dplyr::left_join(detaildat_wide,
  #                                      singlepage_list_with_url[[1]],
  #                                      by = "key")
  for (i in seq_along(singlepage_list_with_url)) {
    detaildat_wide_tmp <- dplyr::left_join(
      detaildat_wide, singlepage_list_with_url[[i]], by = "key")
    detaildat_long_tmp <- tidyr::spread(
      detaildat_wide_tmp, key = "key", value = "value")
    detaildat_long <- dplyr::bind_rows(detaildat_long, detaildat_long_tmp)
  }

  ## since all columns are of type are character, guess the
  ## correct types:
  detaildat_long <- dplyr::mutate_all(detaildat_long,
                                      readr::parse_guess)

  return(detaildat_long)
}
