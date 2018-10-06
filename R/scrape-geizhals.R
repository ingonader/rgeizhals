#' Get data from geizhals list and detail pages
#'
#' Starting from an url, get the information on all items in this
#' list (and the following pages), as well as the information
#' in the detail pages that correspond to these items.
#'
#' @param firstlistpageurl The url of a single geizhals page listing
#'   items in a selected category.
#' @param max_pages Maximal number of pages to be scraped. Default is 10.
#'
#' @return A tibble (data.frame) with all the infromation in
#'   the list page and the corresponding detail pages.
#'   Each row corresponds to one product.
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' dat_gh <- get_geizhals_data(url_geizhals, max_pages = 1)
#' head(dat_gh)
#' }
#'
#' @export
get_geizhals_data <- function(firstlistpageurl, max_pages = 10) {
  ## get all listpages:
  listpagehtml_list <- read_all_listpages(firstlistpageurl,
                                          max_pages = max_pages)
  dat_listpage <- get_all_listpages(listpagehtml_list)

  ## get all detailpages:
  detailpagehtml_list <- read_all_detailpage_html(
    get_all_listpages(listpagehtml_list)$detailpage_url
  )
  dat_detailpage <- get_all_detailpages(detailpagehtml_list)

  ## join listpage data to detailpage data:
  dat_geizhals <- dplyr::left_join(dat_listpage,
                                   dat_detailpage,
                                   by = c("detailpage_url" = "url"))
  return(dat_geizhals)
}
