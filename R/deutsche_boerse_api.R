
# Some basic information for single isins
get_short_static <- function(isin) {
    url_base <- "https://api.boerse-frankfurt.de/v1/data/price_information/shortStatic"

    response <- httr::GET(url_base, query = list(isinsWithOptionalMic = isin))

    content <- httr::content(response, as = "parsed")

    tibble::enframe(unlist(content)) |>
        tidyr::pivot_wider(
            names_from = name,
            values_from = value
        )
}

get_short_static("DE000DL40SR8")


encode_url <- function (url, params) {
    url_base <- paste0("https://api.boerse-frankfurt.de/v1/data/", url)
    full_url <- paste0(url_base, "?", paste0(names(params), "=", params, 
        collapse = "&"))
    return(full_url)
}


# Helper Function for correct request headers
get_ids  <- function(url) {
    
    # Static from the API
    salt <- "w4ivc1ATTGta6njAZzMbkL3kJwxMfEAKDa3MNr"

    timestamp <- lubridate::now()
    # Timestamps for encrypted headers
    milliseconds <- options(digits.secs = 5)
    timelocal <- strftime(lubridate::now(), "%Y-%m-%d %H:%M:%OS", tz ="UTC")
    milliseconds <- options(digits.secs = 3)
    timestr <- strftime(lubridate::now(), "%Y-%m-%dT%H:%M:%OSZ", tz ="UTC")
    
    traceidbase <- stringr::str_c(timestr, url, salt)
    encoded <- enc2utf8(traceidbase)
    traceid <- digest::digest(encoded, algo = "md5", serialize = FALSE)

    xsecuritybase <- strftime(timelocal,"%Y%m%d%H%M")
    encoded <- enc2utf8(xsecuritybase)
    xsecurity <- digest::digest(encoded, algo = "md5", serialize = FALSE)

    return(list(timestr = timestr, 
                traceid = traceid,
                xsecurity = xsecurity))    
}

# Function for creating headers to pass to cURL
create_headers <- function(url) {
    
    ids <- get_ids(url)

    headers <- c(
        `authority` = "api.boerse-frankfurt.de",
        `accept` = "application/json, text/plain, */*",
        `accept-language` = "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7",
        `client-date` = ids$timestr,
        `content-type` = "application/json; charset=UTF-8",
        `origin` = "https://www.boerse-frankfurt.de",
        `referer` = "https://www.boerse-frankfurt.de/",
        `sec-ch-ua` = '"Not?A_Brand";v="8", "Chromium";v="108", "Google Chrome";v="108"',
        `sec-ch-ua-mobile` = "?0",
        `sec-ch-ua-platform` = '"Windows"',
        `sec-fetch-dest` = "empty",
        `sec-fetch-mode` = "cors",
        `sec-fetch-site` = "same-site",
        `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36",
        `x-client-traceid` = ids$traceid,
        `x-security` = ids$xsecurity
    )
    return(headers)
}

# Gets the Data per curl GET.
# Does this work with post? It may not
get_data <- function(url) {
    
    response <- httr::GET(
        url,
        httr::add_headers(.headers = create_headers(url))
    )

    return(response)
}


url <- 'https://api.boerse-frankfurt.de/v1/search/bond_search'

# This function delivers bond data for *ALL* available bonds at once. 
# Set limit with n_bonds and use above URL
bond_search <- function(url, n_bonds = 25) {
    # Body for POST request
    data <- stringr::str_c('{"lang":"de","offset":0,"limit":', n_bonds, ',"sorting":"TURNOVER","sortOrder":"DESC"}')

    # Request POST with headers 
    res <- httr::POST(url,
        httr::add_headers(.headers = create_headers(url)),
        body = data
    )

    # Parse content
    content <- jsonlite::fromJSON(rawToChar(res$content))$data |> 
        dplyr::as_tibble() |> 
        tidyr::unnest_wider(c(overview, performance, keyData, name), names_repair = "minimal") |> 
        tidyr::unnest_wider(c(translations, currency), names_repair = "minimal") |> 
        tidyr::unnest_wider(c(translations), names_repair = "minimal") 

    names(content) <- stringr::str_remove(names(content), "\\.\\d")
    content_subsetted <- subset(content, select=which(!duplicated(names(content)))) 

    return(content_subsetted)
}


results <- bond_search(url, n_bonds = 20000)

# WORKS :)
# This gets historical prices (at least back to year 2000)
get_price_history <- function(isin, 
                            limit = 50,
                            mic = "XFRA",
                            min_date = lubridate::today()-lubridate::days(51),
                            max_date = lubridate::today()-1) {
    
    date_delta <- max_date - min_date
    params <- list(
            "limit" = limit,
            `offset` = '0',
            "isin" = isin,
            "mic" = mic,
            "minDate" = format(min_date, "%Y-%m-%d"),
            "maxDate" = format(max_date, "%Y-%m-%d"),
            "cleanSplit" = "false",
            "cleanPayout" = "false",
            "cleanSubscriptionRights" = "false"
            )

    response <- get_data(encode_url("price_history", params))

    parsed_response <- tibble::tibble(jsonlite::fromJSON(rawToChar(response$content))$data)
    parsed_response$date  <- lubridate::as_date(parsed_response$date)

    return(parsed_response)
}

db <- get_price_history(isin = "DE0005140008")


get_instrument_statics <- function(isin) {
    params <- list("isin" = isin)

    response <- get_data(encode_url("instrument_information", params))

    tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(response$content))))
}

get_company_information <- function(isin) {
    
    params <- list("isin" = isin)

    response <- get_data(encode_url("corporate_information", params))

    tibble::enframe(unlist(jsonlite::fromJSON(rawToChar(response$content))))
}

db_info <- get_company_information(isin = "DE0005140008")

get_related_indices <- function(isin) {
    params <- list("isin" = isin)

    response <- get_data(encode_url("related_indices", params))

    tibble::tibble(jsonlite::fromJSON(rawToChar(response$content))$data)
}

db_indices <- get_related_indices(isin = "DE0005140008")

# TODO
# Equity Summary like bond
# Single Security breakdown





