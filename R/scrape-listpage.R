
#' Title
#'
#' @param listpagehtml
#'
#' @return
#' @export
#'
#' @examples
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
#get_product_names(listpagehtml)


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


#' Title
#'
#' @param listpagehtml
#'
#' @return
#' @export
#'
#' @examples
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
