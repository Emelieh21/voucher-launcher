# Function to add an header
addHeader <- function (..., 
                       color = '#F67B50',
                       moreInfoLink = "https://github.com/Emelieh21/voucher-launcher", 
                       moreInfoText = "About", 
                       logo_src = "https://raw.githubusercontent.com/Emelieh21/voucher-launcher/main/assets/logo-wide.png") { 
  div(tags$header(div(style = paste0("background-color:",color,";padding:15px;width:105%;height:75px;margin-left:-1em;margin-right:2em;"),
                      tags$img(style = "margin-left:20px;margin-top:-0.5em;height:65px;", 
                               src = logo_src), 
                      ...)), absolutePanel(top = 20, right = 25, 
                                           tags$a(style = "color:#fff;font-size:23px;font-weight:400;", 
                                                  href = moreInfoLink, target = "_blank", moreInfoText)))
}

# Function to add loadbar
addLoadbar <- function(loadingText = "Loading...", 
                        color = '#FFE5B4', 
                        top = "0px", alpha = 1) {
  # some styling stuff to see the green loading bar
  div(class = 'wrapper',
      tags$head(tags$style(type="text/css", paste0("
                       #loadmessage {
                       position: fixed;
                       top: ",top,";
                       left: 0px;
                       width: 100%;
                       padding: 5px 0px 5px 0px;
                       text-align: center;
                       font-weight: bold;
                       font-size: 100%;
                       color: #FFFFFF;
                       background-color: ",paste0("rgba(",paste(c(col2rgb(color),alpha),collapse=","),")"),";
                       z-index: 105;}
      "))),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div(loadingText,id="loadmessage")))
}

setUp <- function() {
  dotenv::load_dot_env("local/.env.sandbox")
}

connectToDB <- function() {
  db_user = "voucherlauncher"
  con <- RPostgres::dbConnect(RPostgres::Postgres(), dbname = "kindly-possum-2518.defaultdb", 
                              host = "free-tier5.gcp-europe-west1.cockroachlabs.cloud", 
                              port = 26257, user = db_user, 
                              password = Sys.getenv("POSTGRES_PW"))
}

generateSquareHeaders <- function() {
  headers <- add_headers(.headers = c('Square-Version'='2023-04-19',
                                      'Authorization'= paste("Bearer", Sys.getenv("SQUARE_ACCESS_TOKEN")), 
                                      'Content-Type'='application/json'))
  return(headers)
}

squarePostRequest <- function(url, body) {
  # Get headers
  header <- generateSquareHeaders()
  
  # Run request
  result <- jsonlite::fromJSON(rawToChar(POST(url,
                                              header,
                                              body = body, 
                                              encode = "raw" )$content))
  return(result)
}

squareGetRequest <- function(url, id='') {
  # Get headers
  header <- generateSquareHeaders()
  
  # We need to add the extra slash on the id part, if its not there
  if (id != '') {
    if (!startsWith(id, '/')) {
      id = paste0("/",id)
    }
  }
  
  # Run request
  result <- jsonlite::fromJSON(rawToChar(GET(paste0(url,id),
                                             header)$content))
  return(result)
}

createGiftCard <- function() {
  # Create gift card
  body =  paste0('{
      "idempotency_key": "',UUIDgenerate(),'",
      "location_id": "LTZ7K962QS3DC",
      "gift_card": {
        "type": "DIGITAL"
      }
    }')
  result <- squarePostRequest("https://connect.squareupsandbox.com/v2/gift-cards",
                              body)
  return(result$gift_card$id)
}

putAmountOnGiftcard <- function(gift_card_id, amount) {
  body = paste0('{
    "gift_card_activity": {
      "type": "ACTIVATE",
      "activate_activity_details": {
        "amount_money": {
          "amount": ',amount*100,',
          "currency": "USD"
        },
        "buyer_payment_instrument_ids": [
          "rTRXt7tMvEbBCkwIidv2b9TMEUGZY"
        ]
      },
      "location_id": "LTZ7K962QS3DC",
      "gift_card_id": "',gift_card_id,'"
    },
    "idempotency_key": "084e160a-0083-4446-ad5c-861489a8c511"
  }')
  result <- squarePostRequest("https://connect.squareupsandbox.com/v2/gift-cards/activities",
                              body)
  return(result)
}

getGiftCard <- function(id) {
  result <- squareGetRequest("https://connect.squareupsandbox.com/v2/gift-cards", id)$gift_card
  return(result)
}

chargeGiftCard <- function(gift_card_id, amount) {
  body = paste0('{
    "gift_card_activity": {
      "type": "REDEEM",
      "location_id": "LTZ7K962QS3DC",
      "gift_card_id": "',gift_card_id,'",
      "redeem_activity_details": {
        "amount_money": {
          "amount": ',amount*100,',
          "currency": "USD"
        }
      }
    },
    "idempotency_key": "',UUIDgenerate(),'"
  }')
  result <- squarePostRequest("https://connect.squareupsandbox.com/v2/gift-cards/activities",
                              body)
  return(result)
  
}

getAllGiftCards <- function() {
  result <- squareGetRequest("https://connect.squareupsandbox.com/v2/gift-cards?type=DIGITAL")$gift_cards
  return(result)
}

trackSession <- function(session, business_id) {
  message("Tracking session")
  dat <- as.data.frame(session$token, stringsAsFactors = FALSE)
  names(dat) <- "session_token"
  dat$business_id <- business_id
  dat$time_stamp <- Sys.time()
  con <- connectToDB()
  dbWriteTable(con, "business_tracking", dat, overwrite = FALSE, append = TRUE)
  dbDisconnect(con)
}

auth0_app <- function(app_url, app_name, key, secret) {
  function(app_url) {
    httr::oauth_app(appname = app_name, key = key, secret = secret, redirect_uri = app_url)
  }
}

auth0_api <- function(auth0_url, request, access) {
  httr::oauth_endpoint(base_url = auth0_url, request = request,
                       authorize = "authorize", access = access)
}

has_auth_code <- function(params, state) {
  is.null(params$error) && !is.null(params$code) && params$state == state
}

auth0_server_verify <- function(session, app, api, state) {
  
  u_search <- session[["clientData"]]$url_search
  params <- shiny::parseQueryString(u_search)
  
  if (has_auth_code(params, state)) {
    cred <- httr::oauth2.0_access_token(api, app(redirect_uri), params$code)
    token <- httr::oauth2.0_token(
      app = app(redirect_uri), endpoint = api, cache = FALSE, credentials = cred,
      user_params = list(grant_type = "authorization_code"))
    
    userinfo_url <- sub("authorize", "userinfo", api$authorize)
    resp <- httr::RETRY(
      verb = "GET"
      , url = userinfo_url
      , httr::config(token = token)
      , times = 5
    )
    
    assign("auth0_credentials", token$credentials, envir = session$userData)
    assign("auth0_info", httr::content(resp, "parsed"), envir = session$userData)
  }
  
}

auth0_state <- function(server) {
  paste(sample(c(letters, LETTERS, 0:9), 10, replace = TRUE), collapse = "")
}

#' Information used to connect to Auth0.
#'
#' Creates a list containing all the important information to connect to Auth0
#'   service's API.
#'
#' @param config path to the `_auth0.yml` file or the object returned by
#'   [auth0_config]. If not informed, will try to find the file using
#'   [auth0_find_config_file].
#'
#' @seealso [use_auth0] to create an `_auth0.yml` template.
#'
#' @return A list contaning scope, state, keys, OAuth2.0 app, endpoints,
#'   audience and
#'   remote URL. For compatibility reasons, `remote_url` can be either a parameter
#'   in the root yaml or inside a `shiny_config` parameter.
#'
#' @export
auth0_info <- function(config) {
  if (missing(config)) config <- auth0_config()
  if (!is.list(config) && is.character(config)) config <- auth0_config(config)
  scope <- config$auth0_config$scope
  state <- auth0_state()
  conf <- config$auth0_config
  app <- auth0_app(app_name = config$name, key = conf$credentials$key, secret = conf$credentials$secret)
  api <- auth0_api(conf$api_url, conf$request, conf$access)
  audience <- conf$audience
  rurl <- config$remote_url
  # backward compatibility
  if (is.null(rurl)) rurl <- config$shiny_config$remote_url
  list(scope = scope, state = state, app = app, api = api, audience=audience,
       remote_url = rurl)
}

#' Parse `_auth0.yml` file.
#'
#' Validates and creates a list of useful information from
#'   the `_auth0.yml` file.
#'
#' @param config_file path to the `_auth0.yml` file. If not informed,
#'   will try to find the file using [auth0_find_config_file].
#'
#' @return List containing all the information from the `_auth0.yml` file.
#'
#' @export
auth0_config <- function(config_file) {
  if (missing(config_file)) config_file <- auth0_find_config_file()
  config <- yaml::read_yaml(config_file, eval.expr = TRUE)
  
  # standardise and validate auth0_config
  if (is.null(config$auth0_config)) {
    stop("Missing 'auth0_config' tag in YAML file.")
  }
  config_names <- names(unlist(config$auth0_config))
  required_names <- c("api_url", "credentials.key", "credentials.secret")
  missing_args <- setdiff(required_names, config_names)
  s <- strrep("s", max(length(missing_args) - 1L, 0))
  if (length(missing_args) > 0) {
    msg <- sprintf("Missing '%s' tag%s in YAML file", paste(missing_args, collapse = "','"), s)
    stop(msg)
  }
  # scope
  scp <- config$auth0_config$scope
  if (is.null(scp)) scp <- "openid profile"
  defaults <- list(scope = scp, request = "oauth/token", access = "oauth/token")
  
  for (nm in names(defaults)) {
    if (!nm %in% config_names) {
      config$auth0_config[[nm]] <- defaults[[nm]]
    }
  }
  config
}

#' Auth0 configuration file
#'
#' Create an YAML containing information to connect with Auth0.
#'
#' @param path Directory name. Should be the root of the shiny app
#' you want to add this functionality
#' @param file File name. Defaults to `_auth0.yml`.
#' @param overwrite Will only overwrite existing path if `TRUE`.
#'
#' @details The YAML configuration file has required parameters and extra
#' parameters.
#'
#' The required parameters are:
#' - `auth0_config` is a list contaning at least:
#'   - `api_url`: Your account at Auth0 (e.g. https://jonhdoe.auth0.com).
#'   It is the "Domain" in Auth0 application settings.
#'   - `credentials`: Your credentials to access Auth0 API, including
#'     - `key`: the Client ID in Auth0 application settings.
#'     - `secret`: the Client Secret in Auth0 application settings.
#'
#' The extra parameters are:
#' - `remote_url`: If you are using Shiny-Server or ShinyApps IO service.
#' - `scope`: The information that Auth0 app will access.
#' Defaults to "openid profile".
#' - `request`: Endpoit to request a token. Defaults to "oauth/token"
#' - `access`: Endpoit to access. Defaults to "oauth/token"
#'
#' @export
use_auth0 <- function(path = ".", file = "_auth0.yml", overwrite = FALSE) {
  f <- paste0(normalizePath(path), "/", file)
  if (file.exists(f) && !overwrite) {
    stop("File exists and overwrite is FALSE.")
  }
  ks <- list(key = 'Sys.getenv("AUTH0_KEY")', secret = 'Sys.getenv("AUTH0_SECRET")')
  api_url <- "paste0('https://', Sys.getenv('AUTH0_USER'), '.auth0.com')"
  attr(ks[[1]], "tag") <- "!expr"
  attr(ks[[2]], "tag") <- "!expr"
  attr(api_url, "tag") <- "!expr"
  yaml_list <- list(
    name = "myApp",
    remote_url = "",
    auth0_config = list(api_url = api_url, credentials = ks))
  yaml::write_yaml(yaml_list, f)
}

auth0_ui <- function(ui, info) {
  message("Using local auth0_ui function")
  if (missing(info)) info <- auth0_info()
  function(req) {
    verify <- has_auth_code(shiny::parseQueryString(req$QUERY_STRING), info$state)
    if (!verify) {
      if (grepl("error=unauthorized", req$QUERY_STRING)) {
        redirect <- sprintf("location.replace(\"%s\");", logout_url())
        shiny::tags$script(shiny::HTML(redirect))
      } else {
        
        params <- shiny::parseQueryString(req$QUERY_STRING)
        params$code <- NULL
        params$state <- NULL
        
        query <- paste0("/?", paste(
          mapply(paste, names(params), params, MoreArgs = list(sep = "=")),
          collapse = "&"))
        #print(query) #TEST ####
        if (!is.null(info$remote_url) && info$remote_url != "" && !getOption("auth0_local")) {
          if (query == "/?") {
            redirect_uri <- info$remote_url
          } else {
            redirect_uri <- paste0(info$remote_url, query)
          }
        } else {
          if (grepl("127.0.0.1", req$HTTP_HOST)) {
            redirect_uri <- paste0("http://", gsub("127.0.0.1", "localhost", req$HTTP_HOST, query))
          } else {
            redirect_uri <- paste0("http://", req$HTTP_HOST, query)
          }
        }
        redirect_uri <<- redirect_uri
        #print(redirect_uri) #TEST ####
        
        query_extra <- if(is.null(info$audience)) list() else list(audience=info$audience)
        url <- httr::oauth2.0_authorize_url(
          info$api, info$app(redirect_uri), scope = info$scope, state = info$state,
          query_extra=query_extra
        )
        redirect <- sprintf("location.replace(\"%s\");", url)
        #print(redirect) #TEST ####
        shiny::tags$script(shiny::HTML(redirect))
      }
    } else {
      if (is.function(ui)) {
        ui(req)
      } else {
        ui
      }
    }
  }
}



# # Some payment ids... (apparently it doesn't matter how much the payment was, you can put whatever on a gift voucher...)
# # rTRXt7tMvEbBCkwIidv2b9TMEUGZY = 20 dollar
# # 5oSITxYQRHCLsCUc6XTj5BwRzcFZY = 10 dollar

