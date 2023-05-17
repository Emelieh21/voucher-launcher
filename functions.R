# Function to add an header
addHeader <- function (..., moreInfoLink = "https://github.com/Emelieh21/voucher-launcher", 
                       moreInfoText = "About", 
                       logo_src = "https://raw.githubusercontent.com/Emelieh21/voucher-launcher/main/assets/logo-wide.png") { 
  div(tags$header(div(style = "background-color:#4f3c66;padding:15px;width:105%;height:75px;margin-left:-1em;margin-right:2em;",
                      tags$img(style = "margin-left:20px;margin-top:-0.5em;height:65px;", 
                               src = logo_src), 
                      ...)), absolutePanel(top = 20, right = 25, 
                                           tags$a(style = "color:#fff;font-size:23px;font-weight:400;", 
                                                  href = moreInfoLink, target = "_blank", moreInfoText)))
}

# Function to add loadbar
addLoadbar <- function(loadingText = "Loading...", 
                        color = '#c379b8', ##b3f51b', 
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
  result <- squareGetRequest("https://connect.squareupsandbox.com/v2/gift-cards?type=DIGITAL&state=ACTIVE")$gift_cards
  return(result)
}


# # Some payment ids... (apparently it doesn't matter how much the payment was, you can put whatever on a gift voucher...)
# # rTRXt7tMvEbBCkwIidv2b9TMEUGZY = 20 dollar
# # 5oSITxYQRHCLsCUc6XTj5BwRzcFZY = 10 dollar
gift_card_id <- createGiftCard()
putAmountOnGiftcard(gift_card_id, 5.5)
# getGiftCard(gift_card_id)
# chargeGiftCard(gift_card_id, 3)
# gift_card <- getGiftCard(gift_card_id)
# gift_cards <- getAllGiftCards()
# 
# all_gift_cards <- listAllGiftCards()
