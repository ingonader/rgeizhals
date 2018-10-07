#' Get product names from geizhals category page
#'
#' Returns all product names in a geizhals page listing all products
#' within a specific  category (i.e., not the generic page-wide search from
#' the search bar, but the page showing all items within a category. Filters
#' might be applied, only results corresponding to that filter will be
#' shown and scraped.) The order of items returned by the function
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @param listpagehtml html structure from a single geizhals page listing
#'   items in a selected category, as gathered via \code{xml2::read_html()}
#'   or via a single entry of the list of search pages resulting from
#'   \code{read_all_listpages}.
#'
#' @return A character vector of the product names appearing in the
#'   geizhals listing page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals search page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
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



#' Get product ratings from geizhals category page
#'
#' Returns all product ratings in a geizhals page listing all products
#' within a specific  category (i.e., not the generic page-wide search from
#' the search bar, but the page showing all items within a category. Filters
#' might be applied, only results corresponding to that filter will be
#' shown and scraped.) The order of items returned by the function
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @inheritParams get_product_names
#'
#' @return A numeric vector of the product ratings appearing in the
#'   geizhals listing page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals search page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' get_ratings(listpagehtml)
#'
#' ## get html of multiple geizhals search pages:
#' listpagehtml_list <- read_all_listpages(url_geizhals)
#' get_ratings(listpagehtml_list[[1]])
#' }
#'
#' @export
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


#' Get number of product ratings from geizhals category page
#'
#' Returns the number of product ratings for each product in a
#' geizhals page listing all products within a specific category
#' (i.e., not the generic page-wide search from
#' the search bar, but the page showing all items within a category. Filters
#' might be applied, only results corresponding to that filter will be
#' shown and scraped.) The order of items returned by the function
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @inheritParams get_product_names
#'
#' @return A numeric vector of the number of product ratings for each item
#'   appearing in the geizhals listing page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals search page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' get_ratings_n(listpagehtml)
#'
#' ## get html of multiple geizhals search pages:
#' listpagehtml_list <- read_all_listpages(url_geizhals)
#' get_ratings_n(listpagehtml_list[[1]])
#' }
#'
#' @export
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
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
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



#' Get urls of detail pages for items in geizhals category page
#'
#' Returns all urls of links to details pages (product details)
#' in a geizhals page listing all products within a specific
#' category (i.e., not the generic page-wide search from
#' the search bar, but the page showing all items within a category. Filters
#' might be applied, only results corresponding to that filter will be
#' shown and scraped.) The order of items returned by the function
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @inheritParams get_product_names
#'
#' @return A character vector containing the urls appearing in the
#'   geizhals listing page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals search page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' get_detailpage_urls(listpagehtml)
#'
#' ## get html of multiple geizhals search pages:
#' listpagehtml_list <- read_all_listpages(url_geizhals)
#' get_detailpage_urls(listpagehtml_list[[1]])
#' }
#'
#' @export
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


#' Get information from geizhals category page
#'
#' Returns information (e.g., product names, product ratings,
#' number of ratings, detail page urls) listed in a geizhals page that
#' is listing all products within a specific  category (i.e., not the
#' generic page-wide search from the search bar, but the page showing
#' all items within a category. Filters might be applied, only results
#' corresponding to that filter will be shown and scraped.)
#' The order of items returned by the function
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @inheritParams get_product_names
#'
#' @return A tibble (data.frame) containing all information scraped
#'   from the geizhals page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals search page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' get_single_listpage(listpagehtml)
#'
#' ## get html of multiple geizhals search pages:
#' listpagehtml_list <- read_all_listpages(url_geizhals)
#' get_single_listpage(listpagehtml_list[[1]])
#' }
#'
#' @export
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


#' Get url of next listpage
#'
#' Returns the url of the next page of listings, or \code{NA} if no more
#' page is availab.e
#'
#' @inheritParams get_product_names
#'
#' @return Character vector of length 1 with the full url of the next page
#'   with product listings, or \code{NA} if no more page is available.
#'
#' @examples
#' \dontrun{
#' ## url of next page:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' get_next_listpage_url(listpagehtml)
#'
#' ## or NA if no next page available:
#' url_geizhals <- "https://geizhals.at/?cat=acam35&pg=3#productlist"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' get_next_listpage_url(listpagehtml)
#' }
#'
#' @export
get_next_listpage_url <- function(listpagehtml) {
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

  return(nextlistpageurl)
}

#' Get next listing page html
#'
#' Takes the html of a listing page as input, extracts the url for
#' the next listing page, reads the html and returns it.
#'
#' @inheritParams get_product_names
#'
#' @return An xml document as returned by \code{xml2::read_html}
#'
#' @examples
#' \dontrun{
#' ## url of next page:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#'
#' ## get information from this listing page:
#' get_single_listpage(listpagehtml)
#'
#' ## get next listing page:
#' read_next_listpage(listpagehtml)
#'
#' #' ## get next listing page and get information:
#' get_single_listpage(read_next_listpage(listpagehtml))
#' }
#' @export
read_next_listpage <- function(listpagehtml) {
  ## check if there is another page left and get url:
  nextlistpageurl <- get_next_listpage_url(listpagehtml)
  if (is.na(nextlistpageurl)) ## no next page available
    return(NA)

  ## read html of that url and return it:
  nextlistpagehtml <- xml2::read_html(nextlistpageurl)
  return(nextlistpagehtml)
}



#' Get html from multiple geizhals category pages
#'
#' Given the url of a geizhals page listing all products
#' within a specific  category (i.e., not the generic page-wide search from
#' the search bar, but the page showing all items within a category),
#' the html code from this and the following pages are returned. Filters
#' might be applied, only results corresponding to that filter will be
#' returned.
#' This list is meant to be processed by the \code{\link{get_all_listpages}}
#' function.
#'
#' @param firstlistpageurl Character vector of lenght 1 containting the
#'   url of a geizhals category page (listing all items of a selected
#'   category).
#' @param max_pages Maximal number of pages to be scraped. Default is 10.
#'
#' @return A list of xml documents.
#'
#' @seealso \code{\link{get_all_listpages}}
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' get_all_listpages(listpagehtml_list)
#' }
#'
#' @export
read_all_listpages <- function(firstlistpageurl,
                               max_pages = 10)
{
  ## initialize list to store all htmls:
  listpagehtml_list <- list()

  ## get first listpage html:
  message("Fetching listing page 1...")
  listpagehtml_list[[1]] <- xml2::read_html(firstlistpageurl)

  i <- 1
  nextlistpagehtml <- read_next_listpage(listpagehtml_list[[i]])
  while ((!is.na(nextlistpagehtml)) & (i < max_pages)) {
    ## increase counter variable:
    i <- i + 1

    ## store page in list:
    listpagehtml_list[[i]] <- nextlistpagehtml

    ## read next page:
    message("Fetching listing page ", i, "...")
    nextlistpagehtml <- read_next_listpage(listpagehtml_list[[i]])
  }
  message("Done.")
  return(listpagehtml_list)
}


#' Get data from multiple geizhals category pages
#'
#' Takes a list of html as input, and returns the information in
#' tabluar form.
#'
#' @param listpagehtml_list List of html structures from a multiple
#'   geizhals pages, each listing items in a selected category,
#'   as gathered via \code{read_all_listpages}.
#'
#' @return A tibble (data.frame) containing all information scraped
#'   from the geizhals pages.
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- read_all_listpages(url_geizhals, max_pages = 2)
#' get_all_listpages(listpagehtml_list)
#' }
#'
#' @export
get_all_listpages <- function(listpagehtml_list) {
  purrr::map(listpagehtml_list, get_single_listpage) %>% dplyr::bind_rows()
}
