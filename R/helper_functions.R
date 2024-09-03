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


year_over_year <- function (x, periods){
  if(length(x)<=periods){ stop("too few rows") }
  else{indexes <- 1:(length(x) - periods)
  c(rep(NA, periods),
           (x[indexes + periods] - x[indexes]) / x[indexes] * 100)}
}

################################################################################

# function to get current row numbers
get_produktivnost_table_row_counts <- function(con) {
  tables <- DBI::dbListTables(con)
  row_counts <- purrr::map_dfr(tables, ~ {
    table <- dplyr::tbl(con,  .x)
    table  |>
      dplyr::summarize(tabela = .x, vrstice = n()) |>
      dplyr::collect()})
}

# sestavi telo emaila
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

# pošlje email
email_produktivnost_changes <- function (body, recipient) {
  if (!is.null(body)) {
    text_msg <- gmailr::gm_mime() %>% gmailr::gm_bcc(recipient) %>%
      gmailr::gm_subject("Spremembe tabel te\u010dajev na bazi produktivnost") %>%
      gmailr::gm_html_body(body)
    gmailr::gm_send_message(text_msg)
  }
}

