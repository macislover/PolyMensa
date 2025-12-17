#' Clean weekly menu data from Mensa Polyterrasse
#'
#' This function cleans the raw menu data.
#' It removes irrelevant menu types, extracts the student price, determines
#' whether a menu is served for lunch or dinner, and prepares the data for
#' further filtering and selection.
#'
#' @param df A data.frame with the weekly ETH Mensa Menu with the Columns: day, style, title, description and price.
#' An example Code on how to scrape the ETH Polyterrasse website for the weekly menu is found in the README.Rmd file
#'
#' @return A cleaned data.frame which adds the column meal to indicate whether it is from the lunch or dinner menu.
#'
#' @details
#' Only the student price is retained. Menus labeled as soups or buffet items
#' are removed.
#'
#' @seealso \code{\link{han_hunger}}
#'
#' @examples
#' \dontrun{
#' df_clean <- clean_week(df_raw)
#' }
#' @export
clean_week <- function(df) {

  if (missing(df) || is.null(df)) {
    stop("df must be provided")
  }
  .data <- rlang::.data
  df |>
    dplyr::filter(.data$type != "Hot & Cold 100g" & .data$type != "SUPPE") |>
    dplyr::mutate(
      meal = ifelse(stringr::str_ends(.data$type, "ABEND"), "dinner", "lunch"),
      type = stringr::str_remove(.data$type, " ABEND$")
    ) |>
    dplyr::relocate(.data$meal, .after = .data$type) |>
    dplyr::mutate(
      price = stringr::str_extract(.data$price, "^[^ /]+"),
      price = as.numeric(.data$price)
    )
}
