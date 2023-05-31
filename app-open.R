library(shiny)
library(shinyWidgets)
library(shinythemes)
library(RPostgres)
library(uuid)
library(httr)
library(shinyjs)
library(jsonlite)
library(twilio)
library(dotenv)
library(qrcode)

# Parameters
corporate_color_primary <- "#4f3c66"
corporate_color_secondary <- "#c379b8"
original_url <- NULL

# I don't want to create more and more vouchers while I am still testing
generate_vouchers = TRUE

source("functions.R")

# Set up env
setUp()

ui <- fluidPage(theme = shinytheme("journal"),
                # Load all dependencies
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
                tags$head(tags$style(".modal-dialog {width: fit-content !important; max-width: 95% !important; text-align: center !important;}")),
                tags$head(tags$script(src='https://sandbox.web.squarecdn.com/v1/square.js')),
                tags$head(tags$script(src="app.js")),
                # Add header with logo & info link
                addHeader(color = corporate_color_primary),
                # Add loadbar
                addLoadbar(color = corporate_color_secondary),
                # Show business, payment form needs to be there already, so loading it from UI side
                div(style="text-align:center", 
                    uiOutput("business_header"),
                    tags$br(),
                    # Show photo ####
                    div(style="margin-right:40px;", class="inline-box",
                        uiOutput("business_details")
                    ),
                    # Show payment box ####
                    div(class="inline-box",
                        HTML("<em><b>Note</b>: this is a dummy app. No real payment will be made.</em>"),
                        textInput("recipient_email", label = "", placeholder = "Email address of voucher recipient", width = "100%"),
                        HTML("<form id='payment-form'>
                          <span class='input-euro left'>
                            <input type='number' id='voucher-amount' value='20.00'/>
                          </span>
                          <div id='card-container'></div>
                          <button class='btn action-button' id='card-button' type='button'>Pay</button>
                        </form>
                        <div id='payment-status-container'></div>")
                    )
                ) # End of main div
                )
                
server <- function(input, output, session) {
  server_values <- reactiveValues(current_business = NULL)
  # ===================== #
  # Store original url ####
  # ===================== #
  observe({
    # We only want to do this in the beginning
    if (is.null(original_url)) {
      original_url <<- paste0(session$clientData$url_protocol,"//",
                              session$clientData$url_hostname,
                              ifelse(session$clientData$url_port != "",
                                     paste0(":",session$clientData$url_port), ""),
                              session$clientData$url_pathname)
    }
    print(original_url)
  })
  
  # ============ #
  # Parse URL ####
  # ============ #
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query)) {
      if ("business_id" %in% names(query)) {
        business_id <- query[["business_id"]]
        print(business_id)
        
        # Keep track of unique visits
        trackSession(session, business_id)
        
        # Get business info
        con <- connectToDB()
        business <- dbGetQuery(con, paste0("select business_id, business_name, business_description, business_address, business_email, image from businesses where business_id = '",business_id,"'"))
        dbDisconnect(con)
        server_values$current_business <- business 
        
        output$business_header <- renderUI({
          h1(style="text-align:center;", paste0("Get your gift voucher for ",business$business_name[1]))
        })
        output$business_details <- renderUI({
          div(img(src = business$image[1], width = 300),
              div(style = "margin-top: 20px"),
              tags$em(business$business_description[1]),
              div(style = "margin-top: 20px"),
              tags$span(business$business_address[1]))
        })
      }
    } 
  })
  
  observeEvent(input$success, {
    print("Payment successful")
    print(input$success$value)
    
    # Get value
    amount <- as.numeric(input$success$value)
    # Get business_id
    business_id <- server_values$current_business$business_id
    
    # Generate and activate gift card
    if (generate_vouchers) {
      gift_card_id <- createGiftCard()
      message("Gift card created")
      putAmountOnGiftcard(gift_card_id, amount)
      message("Gift card activated and amount added")
    } else {
      gift_card_id <- "gftc:3300392d0c0b4561b6dfe68f35851a72"
    }
    
    # Store business_id and gift_card_id in a table, so we know they are linked.
    # In a real app this would not be necessary as each business would have their own square account key.
    con <- connectToDB()
    dat <- as.data.frame(gift_card_id, stringsAsFactors = FALSE) 
    names(dat) <- "gift_card_id"
    dat$business_id <- business_id
    dat$amount <- amount
    dbWriteTable(con, "gift_cards", dat, overwrite = FALSE, append = TRUE)
    dbDisconnect(con)

    # Generate QR code
    gift_card_file_id <- gsub(":","_",gift_card_id)
    png(paste0("www/",gift_card_file_id,".png"))
    # Generate QR code 
    plot(qr_code(paste0("https://emelieh21.shinyapps.io/voucher-launcher/?gift_card_id=",gift_card_id,
                        "&business_id=",business_id), ecl = "Q"))
    dev.off()
    
    print(input$recipient_email)
    # TODO: Add a step where we send this voucher to the user
    
    # Show user QR code
    showModal(modalDialog(title = "Thank you for your order",
                          HTML(paste0("<h3>Enjoy your gift voucher for ", server_values$current_business$business_name,"!</h3></br>",
                                      "<em>Your voucher has a value of <b>$",amount, "</b> and is valid until <b>",
                                      Sys.Date()+366,"</b>.</em></br>",
                                      "<div style='margin-top:16px;'>Your voucher and a confirmation of your order </br>have been sent to <b>", input$recipient_email,"</b></div>")),
                          img(src = paste0(gift_card_file_id,".png"), class = "img-center", width = 360)
                          )
              )
  })
}

options(shiny.port = 3000)
shiny::shinyApp(ui, server)
