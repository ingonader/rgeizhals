#' Title
#'
#' @param firstlistpageurl
#'
#' @return
#' @export
#'
#' @examples
get_geizhals_data <- function(firstlistpageurl) {
  ## get all listpages:
  listpagehtml_list <- read_all_listpages(firstlistpageurl)
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
