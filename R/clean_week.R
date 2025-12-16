#' Clean weekly menu data from Mensa Polyterrasse
#'
#' This function cleans the raw menu data returned by \code{fetch_week()}.
#' It removes irrelevant menu types, extracts the student price, determines
#' whether a menu is served for lunch or dinner, and prepares the data for
#' further filtering and selection.
#'
#' @param df A data.frame as returned by \code{fetch_week()}.
#'
#' @return A cleaned data.frame which adds the column meal to indicate whether it is from the lunch or dinner menu.
#'
#' @details
#' Only the student price is retained. Menus labeled as soups or buffet items
#' are removed.
#'
#' @seealso \code{\link{fetch_week}}, \code{\link{han_hunger}}
#'
#' @examples
#' df_raw <- fetch_week(headless = TRUE)
#' df_clean <- clean_week_polyterrasse(df_raw)
#'
#' @export
clean_week <- function(df) {

  if (missing(df) || is.null(df)) {
    stop("df must be provided")
  }

  df |>
    dplyr::filter(type != "Hot & Cold 100g" & type != "SUPPE") |>
    dplyr::mutate(
      meal = ifelse(stringr::str_ends(type, "ABEND"), "dinner", "lunch"),
      type = stringr::str_remove(type, " ABEND$")
    ) |>
    dplyr::relocate(meal, .after = type) |>
    dplyr::mutate(
      price = stringr::str_extract(price, "^[^ /]+"),
      price = as.numeric(price)
    )
}
