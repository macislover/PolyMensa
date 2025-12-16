#' Select a menu from the Mensa Polyterrasse
#'
#' This function selects a single menu item from the weekly menu data
#' of the ETH Zurich Mensa Polyterrasse based on user-defined preferences
#' such as day, meal time, dietary style, excluded ingredients, and price.
#'
#' If multiple menus match the criteria, one is selected at random.
#' If no menu matches, a fallback strategy is applied.
#'
#' @param df A cleaned data.frame as returned by
#'   \code{clean_week()}.
#' @param day Character string specifying the day of the week
#'   (e.g. "Mo", "Di", "Mi").
#' @param time Character string specifying the meal time.
#'   Either \code{"lunch"} or \code{"dinner"}. Default is lunch
#' @param style Optional character vector specifying allowed menu styles
#'   (\code{c("VEGAN", "HOME", "GARDEN", "STREET")}).
#' @param avoid Optional character vector of ingredients or keywords to
#'   exclude based on the menu description.
#' @param max_price Optional numeric value specifying the maximum allowed
#'   student price (CHF). It has to be at least 7 when using it.
#' @param fallback Character string specifying the fallback strategy if no
#'   menu matches the filters. Either \code{"cheapest"} or \code{"any"}. Default
#'   is cheapest
#'
#' @return A one-row data.frame containing the selected menu.
#' If no menu can be selected, an empty data.frame is returned.
#'
#' @details
#' The fallback strategy is applied only if no menu matches all filters.
#' In this case, filtering is reduced to the \code{avoid} criterion only.
#'
#' @seealso \code{\link{fetch_week}}, \code{\link{clean_week}}
#'
#' @examples
#' \dontrun{
#' df_raw <- fetch_week(headless = TRUE)
#' df_clean <- clean_week(df_raw)
#'
#' han_hunger(
#'   df_clean,
#'   day = "Mi",
#'   time = "lunch",
#'   style = "HOME",
#'   avoid = c("Champignons"),
#'   max_price = 10
#' )
#' }
#'
#' @export
han_hunger <- function(df, day, time = "lunch", style = NULL, avoid = NULL, max_price = NULL, fallback ="cheapest"){
  fallback <- match.arg(fallback, choices = c("cheapest","any"))
  time <- match.arg(time, choices = c("lunch","dinner"))
  if (missing(df) || is.null(df)){
    stop("df is missing")
  }
  if (missing(day) || is.null(day)){
    stop("day must be provided")
  }
  if(!is.null(max_price) && max_price < 7){
    stop("max_price must be at least 7")
  }
  d <- df[df$day == day, , drop=FALSE]
  if(nrow(d) == 0){
    stop("No rows found for day = ", day)
  }
  d <- d[d$meal == time, , drop = FALSE]
  if(nrow(d) == 0){
    stop("No rows found for day = ", day, " and time = ", time)
  }
  if(!is.null(max_price)){
    d <- d[d$price <= max_price, , drop = FALSE]
  }
  df_avoid <- d
  if(!is.null(avoid)){
    hit_avoid <- grepl(paste(avoid, collapse = "|"), d$description, ignore.case = TRUE)
    d <- d[!hit_avoid, , drop = FALSE]
    df_avoid <- d
  }
  if(!is.null(style)){
    d <- d[d$type %in% style, , drop = FALSE]
  }
  if (is.null(style) && is.null(avoid) && is.null(max_price)){
    min_p <- min(d$price)
    cheapest_menus <- d[d$price == min_p, , drop = FALSE]
    if(nrow(cheapest_menus) > 1){
      idx <- sample(nrow(cheapest_menus), 1)
      return(cheapest_menus[idx, , drop = FALSE])
    }else{
      return(cheapest_menus[1, , drop = FALSE])
    }
  }
  if(nrow(d) == 1){
    return(d[1, , drop = FALSE])
  }
  if(nrow(d) > 1){
    idx <- sample(nrow(d), 1)
    return(d[idx, , drop = FALSE])
  }
  if(nrow(d) == 0){
    message("No menus applicable with filters. Only filtering 'avoid' and applying fallback='", fallback, "'.")
    if(fallback == "cheapest") {
      min_p <- min(df_avoid$price)
      cheapest_avoid <- df_avoid[df_avoid$price == min_p, , drop = FALSE]
      cheapest_avoid <- cheapest_avoid[sample(nrow(cheapest_avoid), 1), , drop = FALSE]
      return(cheapest_avoid)
    }else{
      return(df_avoid[sample(nrow(df_avoid), 1), , drop = FALSE])
    }
  }
}
