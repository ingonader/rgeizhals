#' Get product names from geizhals search
#'
#' Returns all product names in a geizhals search results page. The order
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @param listpagehtml html structure from a single geizhals page of search
#'   results as gathered via \code{xml2::read_html()} or via a single entry
#'   of the list of search pages resulting from \code{read_all_listpages}.
#'
#' @return A character vector of the product names appearing in the search
#'   results.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals search page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=hwaeschtr&xf=1027_W%E4rmepumpentrockner%7E1296_10%7E1747_8%7E7641_40%7E7653_9"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' get_product_names(listpagehtml)
#'
#' ## get html of multiple geizhals search pages:
#' listpagehtml_list <- read_all_listpages(url_geizhals)
#' get_product_names(listpagehtml_list[[1]])
#' }
#'
#' @export
get_product_names <- function(listpagehtml) {
  ## get relevant parts of html:
  ret <- listpagehtml %>% rvest::html_nodes(css = ".productlist__item.productlist__name") %>%
    rvest::html_text() %>%
    stringr::str_replace("^\n+", "") %>%  ## replace leading newline characters
    stringr::str_extract("^[^\n]+")       ## extract everything up to the next newine character
  ## remove first entry (string with number of products) and return result:
  ret <- ret[-1]
  return(ret)
}

#' Title
#'
#' @param listpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_ratings <- function(listpagehtml) {
  ## get relevant parts of html:
  ratings_text <- listpagehtml %>% rvest::html_nodes(css = ".productlist__rating") %>%
    rvest::html_text()
  ## extract ratings:
  ret <- ratings_text %>% stringr::str_extract("[0-9]\\.[0-9]") %>% as.numeric()
  ## remove first entry:
  ret <- ret[-1]
  return(ret)
}
#get_ratings(listpagehtml)

#' Title
#'
#' @param listpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_ratings_n <- function(listpagehtml) {
  ## get relevant parts of html:
  ratings_text <- listpagehtml %>% rvest::html_nodes(css = ".productlist__rating") %>%
    rvest::html_text()
  ## extract number of ratings:
  ret <- ratings_text %>% stringr::str_extract("[0-9]+ Bewertung.*") %>%
    stringr::str_replace_all("[^0-9]", "") %>%
    as.numeric()
  ## remove first entry:
  ret <- ret[-1]
  return(ret)
}
#get_ratings_n(listpagehtml)



#' Get numbers of offers from geizhals search
#'
#' Returns the numbers of offers for a product in a single geizhals
#' html page of search results. The order
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @inheritParams get_product_names
#'
#' @return A numeric vector containing the number of offers in the same
#'   order of the products as appearing in the search results returned from
#'   \code{get_product_names}.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals search page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=hwaeschtr&xf=1027_W%E4rmepumpentrockner%7E1296_10%7E1747_8%7E7641_40%7E7653_9"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' get_offers_n(listpagehtml)
#'
#' ## get html of multiple geizhals search pages:
#' listpagehtml_list <- read_all_listpages(url_geizhals)
#' get_offers_(listpagehtml_list[[1]])
#' }
#'
#' @export
get_offers_n <- function(listpagehtml) {
  ## get relevant parts of html:
  ret <- listpagehtml %>% rvest::html_nodes(css = ".productlist__offerscount--standard") %>%
    rvest::html_text()
  ## remove first entry, and convert to numeric:
  ret <- ret[-1] %>% as.numeric()
  return(ret)
}
#get_offers_n(listpagehtml)

#' Title
#'
#' @param listpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_detailpage_urls <- function(listpagehtml) {
  ret <- listpagehtml %>% rvest::html_nodes(css = ".productlist__item.productlist__name") %>%
    rvest::html_nodes(css = "a") %>%
    rvest::html_attr("href")
  ## remove first entry:
  ret <- ret[-1]
  ## add domain:
  ret <- paste0("https://geizhals.at/", ret)
  return(ret)
}
#get_detailpage_urls(listpagehtml)

#' Title
#'
#' @param listpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_single_listpage <- function(listpagehtml) {
  ret <- tibble::tibble(
    prodname = get_product_names(listpagehtml),
    rating = get_ratings(listpagehtml),
    rating_n = get_ratings_n(listpagehtml),
    offers_n = get_offers_n(listpagehtml),
    detailpage_url = get_detailpage_urls(listpagehtml)
  )
  return(ret)
}


#' Title
#'
#' @param listpagehtml
#'
#' @return
#' @export
#'
#' @examples
read_next_listpage <- function(listpagehtml) {
  ## check if there is another page left:
  nextpage <- listpagehtml %>% rvest::html_node(css = ".gh_pag_next_active") %>%
    rvest::html_text()
  ## result: either "vor »", or NA if no page is available.

  if (is.na(nextpage)) ## no next page available
    return(NA)

  ## get url link for next page:
  nextlistpageurl <- listpagehtml %>% rvest::html_node(css = ".gh_pag_next_active") %>%
    rvest::html_attr("href")

  ## add domain (replace "." with geizhals domain::
  nextlistpageurl <- stringr::str_replace( nextlistpageurl, "^\\.", "https://geizhals.at")

  ## read html of that url and return it:
  nextlistpagehtml <- xml2::read_html(nextlistpageurl)
  return(nextlistpagehtml)
}
# get_single_listpage(listpagehtml)
# read_next_listpage(listpagehtml) %>% get_single_listpage()
# read_next_listpage(listpagehtml) %>% read_next_listpage()

#' Title
#'
#' @param firstlistpageurl
#' @param max_pages
#'
#' @return
#' @export
#'
#' @examples
read_all_listpages <- function(firstlistpageurl, max_pages = 10) {
  ## initialize list to store all htmls:
  listpagehtml_list <- list()

  ## get first listpage html:
  listpagehtml_list[[1]] <- xml2::read_html(firstlistpageurl)

  i <- 1
  nextlistpagehtml <- read_next_listpage(listpagehtml_list[[i]])
  while ((!is.na(nextlistpagehtml)) & (i < max_pages)) {
    ## increase counter variable:
    i <- i + 1

    ## store page in list:
    listpagehtml_list[[i]] <- nextlistpagehtml

    ## read next page:
    nextlistpagehtml <- read_next_listpage(listpagehtml_list[[i]])
  }
  return(listpagehtml_list)
}
# listpagehtml_list <- read_all_listpages(url_geizhals)
# str(listpagehtml_list)

#' Title
#'
#' @param listpagehtml_list
#'
#' @return
#' @export
#'
#' @examples
get_all_listpages <- function(listpagehtml_list) {
  purrr::map(listpagehtml_list, get_single_listpage) %>% dplyr::bind_rows()
}
