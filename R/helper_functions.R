################################################################################
target_EA <- paste(c("ATS", "BEF", "CYP", "DEM", "EEK",
                     "ESP", "FIM", "FRF", "GRD", "HRK",
                     "IEP", "ITL", "LTL", "LUF", "LVL",
                     "MTL", "NLG", "PTE", "SIT", "SKK"), collapse = '+')

target_nonEA <- paste(c("CHF", "CNY", "CZK", "GBP", "HUF",
                        "JPY", "KRW", "PLN", "RUB", "TRY",
                        "USD", "DKK", "RON", "SEK", "AUD",
                        "BGN", "CAD", "HKD", "NOK", "SGD"), collapse = '+')

################################################################################

#' Bind tables and add a type column with the name of the table
#'
#' Pass a vector of table names, which will be bound together using bind_rows,
#' while adding a new column called type which includes the name of the table
#' from which the rows are from. There is no error checking, if the tables
#' have different column names, tough luck.
#'
#' @param ... names of tables to be bound together
#'
#' @return single dataframe with all the rows from the input tables and an extra
#' type column
bind_exchange_rate_tables <- function(...) {
  tables <- list(...)
    table_names <- as.character(substitute(list(...)))[-1]

  add_type_column <- function(table, name) {
    table |>
      mutate(type = name) |>
      select(type, everything())}

  tables_with_type <- map2(tables, table_names, add_type_column)

  bind_rows(tables_with_type) |>
    rename(temp = currency) |>
    rename(currency = currency_denom,
           currency_denom = temp)
}


#' Calculate year over year growth rates
#'
#' Pass a vector or column and the number of periods in a year (e.g. 12 or 4),
#' to get the yoy values - if there are enough values in the series.
#'
#' @param x numeric vector
#' @param periods integer
#'
#' @return numeric vector of same length as x
year_over_year <- function (x, periods){
  if(length(x)<=periods){ stop("too few rows") }
  else{indexes <- 1:(length(x) - periods)
  c(rep(NA, periods),
           (x[indexes + periods] - x[indexes]) / x[indexes] * 100)}
}

################################################################################


#' Get current number of rows in the database
#'
#' This function is used to determine whether the database tables have been
#' updated. It returns a table with the table names in the schema and the
#' number of rows in each. This can be used to compare the values before
#' and after the update script is run, to trigger an email informing there
#' has been a succesful update.
#'
#' @param con connection to the database
#'
#' @return data frame with columns tabela with the name of the table and
#' vrstice with the number of rows
get_produktivnost_table_row_counts <- function(con) {
  tables <- DBI::dbListTables(con)
  row_counts <- purrr::map_dfr(tables, ~ {
    table <- dplyr::tbl(con,  .x)
    table  |>
      dplyr::summarize(tabela = .x, vrstice = n()) |>
      dplyr::collect()})
}


#' Create the html body of the automatic email informing of any updates
#'
#' Pass the table with the differences before and after the update to the
#' function which creates teh html body to be passed to the
#' `email_produktivnost_changes` function. If diff has no rows, the function
#' returns null and the email function will do nothing.
#'
#' @param diff is the table of differences between the row numbers before and
#' after the update.
#'
#' @return html body if there have been changes, NULL if not
email_produktivnost_update_body <- function (diff) {
  if (nrow(diff) > 0) {
    html_table <- kable(diff, "html") |>
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        font_size = 12)

    email_body <- "To je avtomatsko generirano sporo\u010dilo o posodobitvah tabel te\u010dajev na bazi produktivost.<br><br>"
    email_body <- paste0(email_body, "<b>Naslednje tabele imajo novo \u0161tevilo vrstic:</b><br><br>",
                         html_table, "<br><br>")
    email_body <- paste0(email_body, "<br>Tvoj Umar Data Bot &#129302;")
    email_body} else {
      NULL}
}

#' Send email to list of recipients
#'
#' Pass the html body - e.g. the output from `email_produktivnost_update_body`
#' and a vector of emails for the recipients, and the function sends
#' the email. If the body is NULL, no email gets sent.
#'
#' @param body html email body
#' @param recipient  vector of emails
#'
#' @return nothing, side effect is sent email
email_produktivnost_changes <- function (body, recipient) {
  if (!is.null(body)) {
    text_msg <- gmailr::gm_mime() %>% gmailr::gm_bcc(recipient) %>%
      gmailr::gm_subject("Spremembe tabel te\u010dajev na bazi produktivnost") %>%
      gmailr::gm_html_body(body)
    gmailr::gm_send_message(text_msg)
  }
}

