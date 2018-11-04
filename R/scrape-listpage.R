#' Parse product names from geizhals category page
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
#'   or via a single entry of the list of listing pages resulting from
#'   \code{fetch_all_listpages}.
#'
#' @return A character vector of the product names appearing in the
#'   geizhals listing page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals category listing page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_product_names(listpagehtml)
#'
#' ## get html of multiple geizhals category listing pages:
#' listpagehtml_list <- fetch_all_listpages(url_geizhals)
#' parse_product_names(listpagehtml_list[[1]])
#' }
#'
#' @export
parse_product_names <- function(listpagehtml) {
  ## get relevant parts of html:
  ret <- listpagehtml %>% rvest::html_nodes(css = ".productlist__item.productlist__name") %>%
    rvest::html_text() %>%
    stringr::str_replace("^\n+", "") %>%  ## replace leading newline characters
    stringr::str_extract("^[^\n]+")       ## extract everything up to the next newine character
  ## remove first entry (string with number of products) and return result:
  ret <- ret[-1]
  return(ret)
}



#' Parse product ratings from geizhals category page
#'
#' Returns all product ratings in a geizhals page listing all products
#' within a specific  category (i.e., not the generic page-wide search from
#' the search bar, but the page showing all items within a category. Filters
#' might be applied, only results corresponding to that filter will be
#' shown and scraped.) The order of items returned by the function
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @inheritParams parse_product_names
#'
#' @return A numeric vector of the product ratings appearing in the
#'   geizhals listing page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals category listing page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_ratings(listpagehtml)
#'
#' ## get html of multiple geizhals category listing pages:
#' listpagehtml_list <- fetch_all_listpages(url_geizhals)
#' parse_ratings(listpagehtml_list[[1]])
#' }
#'
#' @export
parse_ratings <- function(listpagehtml) {
  ## get relevant parts of html:
  ratings_text <- listpagehtml %>% rvest::html_nodes(css = ".productlist__rating") %>%
    rvest::html_text()
  ## extract ratings:
  ret <- ratings_text %>% stringr::str_extract("[0-9]\\.[0-9]") %>% as.numeric()
  ## remove first entry:
  ret <- ret[-1]
  return(ret)
}


#' Parse number of product ratings from geizhals category page
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
#' @inheritParams parse_product_names
#'
#' @return A numeric vector of the number of product ratings for each item
#'   appearing in the geizhals listing page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals category listing page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_ratings_n(listpagehtml)
#'
#' ## get html of multiple geizhals category listing pages:
#' listpagehtml_list <- fetch_all_listpages(url_geizhals)
#' parse_ratings_n(listpagehtml_list[[1]])
#' }
#'
#' @export
parse_ratings_n <- function(listpagehtml) {
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


#' Parse numbers of offers from geizhals listing page
#'
#' Returns the numbers of offers for a product in a single geizhals
#' html page of category listing results. The order
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @inheritParams parse_product_names
#'
#' @return A numeric vector containing the number of offers in the same
#'   order of the products as appearing in the category listing page
#'   returned from \code{parse_product_names}.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals category listing page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_offers_n(listpagehtml)
#'
#' ## get html of multiple geizhals category listing pages:
#' listpagehtml_list <- fetch_all_listpages(url_geizhals)
#' parse_offers_n(listpagehtml_list[[1]])
#' }
#'
#' @export
parse_offers_n <- function(listpagehtml) {
  ## get relevant parts of html:
  ret <- listpagehtml %>% rvest::html_nodes(css = ".productlist__offerscount--standard") %>%
    rvest::html_text()
  ## remove first entry, and convert to numeric:
  ret <- ret[-1] %>% as.numeric()
  return(ret)
}
#parse_offers_n(listpagehtml)


#' Parse price from listing page
#'
#' Returns the listed price for a product in a single geizhals
#' html page of a category listing page. The order
#' might not correspond to the order listed on the webpage, but it is
#' the same order in all related functions.
#'
#' @inheritParams parse_product_names
#'
#' @return A numeric vector containing the price in the same
#'   order of the products as appearing in the listing page
#'   returned from \code{parse_product_names}.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals category page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_listprice(listpagehtml)
#'
#' ## get html of multiple geizhals category listing pages:
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' parse_listprice(listpagehtml_list[[1]])
#' }
#'
#' @export
parse_listprice <- function(listpagehtml) {
  ret <- listpagehtml %>% rvest::html_nodes(css = ".productlist__price") %>%
    rvest::html_text()
  ## remove first entry (category heading):
  ret <- ret[-1]
  ## convert to numerical:
  ret <- ret %>% stringr::str_replace("^\n+", "") %>%  ## replace leading newline characters
    stringr::str_extract("^.*?[0-9,]{1,}") %>%    ## get first occurenc of a number
    stringr::str_replace_all("[^0-9,]", "") %>%               ## get numerical parts only
    stringr::str_replace_all(",", "\\.") %>%                  ## "," comma to "." comma
    as.numeric()
  return(ret)
}

#' Parse urls of detail pages for items in geizhals category page
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
#' @inheritParams parse_product_names
#' @param domain Character vector of length one specifying the domain.
#'   Defaults to \code{"https://geizhals.at"}.
#'
#' @return A character vector containing the urls appearing in the
#'   geizhals listing page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals category listing page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_detailpage_urls(listpagehtml)
#'
#' ## get html of multiple geizhals category listing pages:
#' listpagehtml_list <- fetch_all_listpages(url_geizhals)
#' parse_detailpage_urls(listpagehtml_list[[1]])
#'
#' ## get html from a geizhals.eu page and parse:
#' url_geizhals <- "https://geizhals.eu/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_detailpage_urls(listpagehtml, domain = "https://geizhals.eu")
#' }
#'
#' @export
parse_detailpage_urls <- function(listpagehtml, domain = "https://geizhals.at") {
  ret <- listpagehtml %>% rvest::html_nodes(css = ".productlist__item.productlist__name") %>%
    rvest::html_nodes(css = "a") %>%
    rvest::html_attr("href")
  ## remove first entry:
  ret <- ret[-1]
  ## add domain:
  ret <- paste0(domain, "/", ret)
  return(ret)
}


#' Parse information from geizhals category page
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
#' @inheritParams parse_detailpage_urls
#'
#' @return A tibble (data.frame) containing all information scraped
#'   from the geizhals page.
#'
#' @examples
#' \dontrun{
#' ## get html of a geizhals category listing page via read_html():
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_single_listpage(listpagehtml)
#'
#' ## get html of multiple geizhals category listing pages:
#' listpagehtml_list <- fetch_all_listpages(url_geizhals)
#' parse_single_listpage(listpagehtml_list[[1]])
#'
#' ## get html from a geizhals.eu page and parse:
#' url_geizhals <- "https://geizhals.eu/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_single_listpage(listpagehtml, domain = "https://geizhals.eu")
#' }
#'
#' @export
parse_single_listpage <- function(listpagehtml, domain = "https://geizhals.at") {
  ## check if there is html data:
  if (!is.na(listpagehtml)) {
    ## if data can be extracted, do so:
    ret <- tibble::tibble(
      prodname = parse_product_names(listpagehtml),
      rating = parse_ratings(listpagehtml),
      rating_n = parse_ratings_n(listpagehtml),
      offers_n = parse_offers_n(listpagehtml),
      listprice = parse_listprice(listpagehtml),
      detailpage_url = parse_detailpage_urls(listpagehtml, domain = domain)
    )
  } else {
    ## when no data can be parsed, just return same structure with NAs:
    ## (happens when reading html failed for some reason):
    ret <- tibble::tibble(
      prodname = NA,
      rating = NA,
      rating_n = NA,
      offers_n = NA,
      listprice = NA,
      detailpage_url = NA
    )
  }
  return(ret)
}


#' Parse url of next listpage
#'
#' Returns the url of the next page of listings, or \code{NA} if no more
#' page is available
#'
#' @inheritParams parse_detailpage_urls
#'
#' @return Character vector of length 1 with the full url of the next page
#'   with product listings, or \code{NA} if no more page is available.
#'
#' @examples
#' \dontrun{
#' ## url of next page:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_next_listpage_url(listpagehtml)
#'
#' ## or NA if no next page available:
#' url_geizhals <- "https://geizhals.at/?cat=acam35&pg=3#productlist"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_next_listpage_url(listpagehtml)
#'
#' ## get html from a geizhals.eu page and parse:
#' url_geizhals <- "https://geizhals.eu/?cat=acam35"
#' listpagehtml <- xml2::read_html(url_geizhals)
#' parse_next_listpage_url(listpagehtml, domain = "https://geizhals.eu")
#' }
#'
#' @export
parse_next_listpage_url <- function(listpagehtml, domain = "https://geizhals.at") {
  ## check if there is html present; if not, return NA.
  if (is.na(listpagehtml)) return(NA)

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
  nextlistpageurl <- stringr::str_replace( nextlistpageurl, "^\\.", domain)

  return(nextlistpageurl)
}

#' Fetch html of next listing page
#'
#' Takes the html of a listing page as input, extracts the url for
#' the next listing page, reads the html and returns it.
#'
#' @inheritParams parse_detailpage_urls
#'
#' @return An xml document as returned by \code{xml2::read_html}
#'
#' @examples
#' \dontrun{
#' ## url of geizhals.at page:
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_p01 <- xml2::read_html(url_geizhals)
#' ## get information from this listing page:
#' parse_single_listpage(listpagehtml_p01)
#' ## get next listing page:
#' listpagehtml_p02 <- fetch_next_listpage(listpagehtml_p01)
#' #' ## get next listing page and get information:
#' parse_single_listpage(listpagehtml_p02)
#'
#' ## url of geizhals.eu page:
#' url_geizhals <- "https://geizhals.eu/?cat=acam35"
#' listpagehtml_01 <- xml2::read_html(url_geizhals)
#' ## get information from this listing page:
#' parse_single_listpage(listpagehtml_01, domain = "https://geizhals.eu")
#' ## get next listing page:
#' listpagehtml_p02 <- fetch_next_listpage(listpagehtml, domain = "https://geizhals.eu")
#' #' ## get next listing page and get information:
#' parse_single_listpage(listpagehtml_p02, domain = "https://geizhals.eu")

#' }
#' @export
fetch_next_listpage <- function(listpagehtml, domain = "https://geizhals.at") {
  ## check if there is another page left and get url:
  nextlistpageurl <- parse_next_listpage_url(listpagehtml, domain = domain)
  if (is.na(nextlistpageurl)) ## no next page available
    return(NA)

  ## read html of that url and return it:
  nextlistpagehtml <- try(xml2::read_html(nextlistpageurl), silent = TRUE)
  ## if it fails, return NA:
  if (class(nextlistpagehtml)[1] == "try-error") {
    warning("Something unexpected happened when fetching listpage. \n",
            "(", nextlistpagehtml[1], ")\n",
            "Returning NA instead of web page html.")
    nextlistpagehtml <- NA
  }
  return(nextlistpagehtml)
}



#' Fetch html of multiple geizhals category pages
#'
#' Given the url of a geizhals page listing all products
#' within a specific  category (i.e., not the generic page-wide search from
#' the search bar, but the page showing all items within a category),
#' the html code from this and the following pages are returned. Filters
#' might be applied, only results corresponding to that filter will be
#' returned.
#' This list is meant to be processed by the \code{\link{parse_all_listpages}}
#' function.
#'
#' @inheritParams parse_detailpage_urls
#' @param firstlistpageurl Character vector of length 1 containing the
#'   url of a geizhals category page (listing all items of a selected
#'   category).
#' @param max_pages Maximal number of pages to be scraped. Default is 10.
#'
#' @return A list of xml documents.
#'
#' @seealso \code{\link{parse_all_listpages}}
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' parse_all_listpages(listpagehtml_list)
#'
#' url_geizhals <- "https://geizhals.eu/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2,
#'   domain = "https://www.geizhals.eu")
#' parse_all_listpages(listpagehtml_list, domain = "https://www.geizhals.eu")
#' }
#'
#' @export
fetch_all_listpages <- function(firstlistpageurl,
                               max_pages = 10,
                               domain = "https://geizhals.at")
{
  ## initialize list to store all htmls:
  listpagehtml_list <- list()

  ## get first listpage html:
  ## (no try-catch here; if first page fails, error is in order)
  message("Fetching listing page 1...")
  listpagehtml_list[[1]] <- xml2::read_html(firstlistpageurl)

  i <- 1
  nextlistpagehtml <- fetch_next_listpage(listpagehtml_list[[i]],
                                          domain = domain)
  while ((!is.na(nextlistpagehtml)) & (i < max_pages)) {
    ## increase counter variable:
    i <- i + 1

    ## store page in list:
    listpagehtml_list[[i]] <- nextlistpagehtml

    ## read next page:
    message("Fetching listing page ", i, "...")
    nextlistpagehtml <- fetch_next_listpage(listpagehtml_list[[i]],
                                            domain = domain)
  }
  message("Done.")
  return(listpagehtml_list)
}


#' Parse data from multiple geizhals category pages
#'
#' Takes a list of html as input, and returns the information in
#' tabular form.
#'
#' @inheritParams parse_detailpage_urls
#' @param listpagehtml_list List of html structures from a multiple
#'   geizhals pages, each listing items in a selected category,
#'   as gathered via \code{fetch_all_listpages}.
#'
#' @return A tibble (data.frame) containing all information scraped
#'   from the geizhals pages.
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' parse_all_listpages(listpagehtml_list)
#'
#' url_geizhals <- "https://geizhals.eu/?cat=acam35"
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2,
#'   domain = "https://www.geizhals.eu")
#' parse_all_listpages(listpagehtml_list, domain = "https://www.geizhals.eu")
#' }
#'
#' @export
parse_all_listpages <- function(listpagehtml_list,
                                domain = "https://www.geizhals.at") {
  purrr::map(listpagehtml_list, parse_single_listpage, domain) %>% dplyr::bind_rows()
}
