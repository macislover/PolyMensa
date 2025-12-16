#' Fetch weekly menus from the ETH Zurich Mensa Polyterrasse
#'
#' This function uses the \pkg{selenider} package
#' to open the Mensa Polyterrasse website, navigate to the weekly menu view,
#' and scrape all menus from Monday to Friday.
#'
#' The returned data is unprocessed and intended to be cleaned using
#' \code{clean_week()}.
#'
#' @param headless Logical. If TRUE, runs the browser in headless mode.
#' @param timeout Numeric. Timeout (in seconds) for browser interactions.
#'
#' @return A data.frame with one row per menu item and the following columns:
#' \describe{
#'   \item{day}{Day of the week (e.g. "Mo", "Di").}
#'   \item{type}{Menu line or category (e.g. "VEGAN", "HOME").}
#'   \item{meal}{Whether it is the lunch or dinner menu for the day}
#'   \item{title}{Title of the menu item.}
#'   \item{description}{Menu description.}
#'   \item{price}{Price as shown on the website.}
#' }
#' @export
#'
#' @seealso \code{\link{clean_week}}, \code{\link{han_hunger}}
#'
#' @examples
#' fetch_week(headless = TRUE)
#'
#' fetch_week(headless = FALSE, timeout =5)
fetch_week <- function(headless = FALSE, timeout = 10) {
  session <- selenider::selenider_session(
    "chromote",
    timeout = timeout,
    options = selenider::chromote_options(headless = headless)
  )

  selenider::open_url("https://ethz.ch/de/campus/erleben/gastronomie-und-einkaufen/gastronomie/restaurants-und-cafeterias/zentrum/mensa-polyterrasse.html")

  tryCatch({
    selenider::s("#onetrust-accept-btn-handler") |>
      selenider::elem_click()
  }, error = function(e) NULL)


  selenider::ss("#gastro-menus a.eth-link") |>
    selenider::elem_find(selenider::has_text("diese Woche")) |>
    selenider::elem_click()

  week_section <- as.list(selenider::ss("section.cp-week__weekday"))
  rows <- list()
  day_label <- list(length(week_section))

  for (i in seq_along(week_section)) {
    day <- week_section[[i]]
    day_label[i] <- day |> selenider::find_element("h2.cp-menu__dayofweek") |> selenider::elem_text()

    menu_week <- day |> selenider::find_elements("div.cp-week__days div.cp-menu")
    menu_list <- as.list(menu_week)

    for (j in seq_along(menu_list)) {
      menu <- menu_list[[j]]

      type <- menu |> selenider::find_element(".cp-menu__line-small") |> selenider::elem_text()
      title <- menu |> selenider::find_element("h3.cp-menu__title") |> selenider::elem_text()
      description <- menu |> selenider::find_element(".cp-menu__description") |> selenider::elem_text()
      price <- menu |> selenider::find_element(".cp-menu__prices .cp-menu__paragraph") |> selenider::elem_text()

      rows[[length(rows) + 1]] <- list(
        day = day_label[[i]],
        type = type,
        title = title,
        description = description,
        price = price
      )
    }
  }

  df_week <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  df_week
}
