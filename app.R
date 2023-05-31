library(shiny)
library(shinyWidgets)
library(shinythemes)
library(auth0)
library(shinyjs)
library(RPostgres)
library(uuid)
library(openssl)
library(qrcode)
library(dotenv)
library(httr)
library(jsonlite)

# Parameters
corporate_color_primary <- "#4f3c66"
corporate_color_secondary <- "#c379b8"
original_url <- NULL

source("functions.R")

# Set up env
setUp()

# To retrigger the business list
values <- reactiveValues(flag_delete = 0)

ui <- fluidPage(theme = shinytheme("flatly"),
                # Use shinyjs
                useShinyjs(),
                # Make modal dialog a bit wider
                tags$head(tags$style(".modal-dialog {width: fit-content !important;}")),
                # Make urls corporate color
                tags$head(tags$style(paste0(".a-corporate {color: ",corporate_color_secondary," !important;}"))),
                # Add header with logo & info link
                addHeader(color = corporate_color_primary),
                # Add loadbar
                addLoadbar(color = corporate_color_secondary),
                # Set full screen background image
                setBackgroundImage(src = 'gift-ga793e7a9f_1920.jpg'),
                # Button to add your business ####
                actionButton("add_business", "Register your business",
                             style=paste0("float:right;font-family:sans-serif;color:#fff;background-color:",corporate_color_secondary,
                                          ";border-color:",corporate_color_secondary,";font-size:150%;margin:35px;")),
                uiOutput("businesses") # dark blue #224578 dark purple #4f3c66 bright green #8CF4A4
                )
                
server <- function(input, output, session) {
  server_values <- reactiveValues(current_gift_card = NULL,
                                  current_business_id = NULL,
                                  current_businesses = NULL)
  
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
  
  # =========================== #
  # Pop up window to add business ####
  # =========================== #
  observeEvent(input$add_business, {
    print(session$userData$auth0_info)
    showModal(modalDialog(
      title = "Add your business",
      div(
        # Upload photo ####
        div(style="display: inline-block;vertical-align:top;margin-top:20px;margin-left:10px;", 
            div(style = "display:flexbox", id = "image-container", 
                img(src = "logo-icon.png", width = 300, height = 250)),
            fileInput("myFile", "Upload logo", accept = c('image/png', 'image/jpeg')),
            div(style = "margin-top: -20px"), # Reduce space
            HTML("<em style='font-size: 8px;'>* This will be publicly visible</em>")
        ),
        # Add info ####
        div(style="display: inline-block;vertical-align:top;margin-top:20px; margin-left:20px;", 
            textInput("business_name","Business name: "),
            div(style = "margin-top: -20px"), # Reduce space
            HTML("<em style='font-size: 8px;'>* This will be publicly visible</em>"),
            textAreaInput("business_description", "Description: "),
            div(style = "margin-top: -20px"), # Reduce space
            HTML("<em style='font-size: 8px;'>* This will be publicly visible</em>"),
            textAreaInput("business_address", "Address: "),
            div(style = "margin-top: -20px"), # Reduce space
            HTML("<em style='font-size: 8px;'>* This will be publicly visible</em>"),
            textInput("business_email","Business email: "),
            checkboxInput("add_payment_details", "Add payment details", value = TRUE),
            uiOutput("payment_details_output"),
            div(id = "agree_text", style="width:300px; font-size:12px", HTML('<input type="checkbox" name="checkbox" value="check" id="agree" /> I have read and agree to the Terms and Conditions</br>and Privacy Policy'))
            )
        ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "Save & generate QR code",
                     style="color:#fff;background-color:#4f3c66")
      )
    ))
  })
  # React to image file selection ####
  observeEvent(input$myFile, {
    removeUI(
      selector = "#image-container > *"
    )
    inFile <- input$myFile
    if (is.null(inFile)) 
      return()
    b64 <- base64enc::dataURI(file = inFile$datapath, mime = "image/png")
    insertUI(
      selector = "#image-container",
      where = "afterBegin",
      ui = img(src = b64, width = 300)
    )
  })
  # React to payment details checkbox ####
  observeEvent(input$add_payment_details, {
    output$payment_details_output <- renderUI({
               if (input$add_payment_details) {
                 div(textInput("payment_details", "Bank account number :"),
                     div(style = "margin-top: -20px"), # Reduce space
                     HTML("<em style='font-size: 10px;'>* This is a dummy app, please don't put real payment details</em>")
                  )
               } else {
                 return(NULL)
               }
    })
  })
  # React to OK button in "Add your business" ####
  observeEvent(input$ok, {
    # Check if terms and conditions box is checked
    if (input$agree == FALSE) {
        runjs('document.getElementById("agree_text").style.color = "red";') 
    } else {
      
      # Save data to businesss table ####
      con <- connectToDB()
      user <- session$userData$auth0_info$sub
      # In case we do not have a user, we are testing
      if (is.null(user)) {
        user <- "test"
      }
      dat <- as.data.frame(user, stringsAsFactors = FALSE) 
      names(dat) <- "user_name"
      dat$business_id <- UUIDgenerate()
      dat$business_name <- input$business_name
      dat$business_description <- input$business_description
      dat$business_address <- input$business_address
      dat$business_email <- input$business_email
      dat$payment_details <- input$payment_details
      dat$image <- ifelse(is.null(input$myFile), "",
                          base64enc::dataURI(file = input$myFile$datapath, mime = "image/png"))
      dbWriteTable(con, "businesses", dat, overwrite = FALSE, append = TRUE)
      dbDisconnect(con)
      
      # Remove popup
      removeModal()
    }
  })
  # ================================ #
  # Show user his or her own businesses ####
  # ================================ #
  output$businesses <- renderUI({
    print(input$ok) # Reload when new business is saved
    print(values$flag_delete)
    user <- session$userData$auth0_info$sub
    print(user)
    if (is.null(user)) {
      print("Not logged in")
      user <- "test"
    }
    con <- connectToDB()
    businesses <- dbGetQuery(con, paste0("with voucher_total as (
                                            select sum(amount) as voucher_total,
                                                   count(distinct(gift_card_id)) as voucher_count,
                                                   business_id
                                            from gift_cards
                                            group by business_id
                                        ),
                                        visits as (
                                            select count(distinct(session_token)) as count,
                                                   business_id
                                            from business_tracking
                                            group by business_id
                                        )
                                   select b.business_id, 
                                          b.business_name, 
                                          b.business_description as business_info,
                                          coalesce(v.voucher_count,0) as voucher_count,
                                          coalesce(v.voucher_total,0) as voucher_total,
                                          coalesce(visits.count,0) as visit_count
                                   from businesses b
                                   left join voucher_total v ON b.business_id = v.business_id
                                   left join visits ON b.business_id = visits.business_id
                                   where user_name = '",user,"'"))
    dbDisconnect(con)
    server_values$current_businesses <- businesses
    if (nrow(businesses) == 0) {
      return(NULL)
    }
    businesses_html <- h1("Your businesses")
    for (i in c(1:nrow(businesses))) {
      business_id = businesses$business_id[i]
      png(paste0("www/",business_id,".png"))
      url = paste0("https://emelieh21.shinyapps.io/voucher-launcher-open/?business_id=",businesses$business_id[i])
      # Generate QR codes ####
      plot(qr_code(url, ecl = "Q"))
      dev.off()
      print(businesses[i,])
      business_div <- div(style='margin-left:5px;',
        div(style="display: inline-block;vertical-align:top;margin-top:20px; margin-left:20px;", 
            img(src = paste0(business_id,".png"), width = 150),
            HTML("<a class='a-corporate' style='display:block;text-align:center;margin-top:5px;' href='",url,"'>View voucher page</a></br>")),
        div(style="display: inline-block;vertical-align:top;margin-top:20px; margin-left:20px; width:400px;", 
            HTML(paste0("<b>Name: </b>", businesses$business_name[i], "</br>",
                        "<b>Info: </b>", businesses$business_info[i]), "</br>",
                        # TODO: would be more proper if we would store the voucher currency also...
                        "<b>Total voucher amount: </b>$",businesses$voucher_total[i],"</br>",
                        "<b>Number of vouchers sold: </b>",as.integer(businesses$voucher_count[i]),"</br>",
                        "<b>Total page visits: </b>",as.integer(businesses$visit_count[i]),"</br>"
                 ),
            actionButton(paste0("delete_",business_id), "Delete", #icon("trash-alt"),
                         style="font-size:65%;padding-top:5px;padding-bottom:5px;padding-left:10px;padding-right:10px;margin-top:10px;"))
        )
      businesses_html <- paste(businesses_html, business_div, sep="</br>")
    }
    # React to buttons
    lapply(
      X = 1:nrow(businesses),
      FUN = function(i){
        observeEvent(input[[paste0("delete_", businesses$business_id[i])]], {
          print(businesses$business_id[i])
          con <- connectToDB()
          dbSendQuery(con, paste0("delete from businesses where business_id = '",businesses$business_id[i],"'"))
          message("Business removed from DB")
          values$flag_delete = values$flag_delete+1
        }, once = TRUE, autoDestroy = TRUE, ignoreInit = TRUE)
      }
    )
    return(div(style="margin-left:35px;margin-top:2em;",HTML(businesses_html)))
  })
  
  # React to gift voucher scan ####
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query)) {
      if ("gift_card_id" %in% names(query)) {
        gift_card_id <- query[["gift_card_id"]]
      } else {
        return(NULL)
      }
      if ("business_id" %in% names(query)) {
        business_id <- query[["business_id"]]
      }
      # Check if the scanned voucher is linked to the current account
      # In a real app this would not be necessary as each business would have their own square account key.      gift_card <- getGiftCard(gift_card_id)
      businesses <- server_values$current_businesses
      con <- connectToDB()
      gift_cards <- dbGetQuery(con, paste0("select gift_card_id 
                                            from gift_cards 
                                            where business_id IN ('",
                                            paste(businesses$business_id, collapse="','"),"')"))
      if (!gift_card_id %in% gift_cards$gift_card_id) {
        # If this voucher is not related to the current account, we inform the user
        showModal(modalDialog(
          title = "Voucher not found",
          tags$em("We couldn't find this voucher in the current account.")
        ))
        return(NULL)
      }
      
      gift_card <- getGiftCard(gift_card_id)
      gift_card_details <- gift_card[names(gift_card)[!names(gift_card) %in% c('id','balance_money')]]
      
      # We need to be able to access the details from the redeem section
      server_values$current_gift_card <- gift_card
      server_values$current_business_id <- business_id
      
      # Build gift card popup
      gift_card_popup <- HTML(paste0('<div style="padding:12px;">',
                  '<h3 id="giftcard-header">Remaining balance: ',gift_card$balance_money$amount/100,' ',gift_card$balance_money$currency,'</h3></br>',
                  '<div style="font-weight:600;display:inline-block;vertical-align:top;">',
                  # List names of all details
                  paste(names(gift_card_details), collapse=':</br>'),'</div>',
                  '<div style="margin-left:12px;display:inline-block;vertical-align:top;">',
                  # List values of all details
                  paste(gift_card_details, collapse='</br>'),'</div></br>',
                  '<div style=margin-top:36px;>',
                  numericInput("amount_to_redeem", "Amount to redeem", value=0,
                               min = 0, max = gift_card$balance_money$amount/100,
                               step = 0.01),
                  '<button id="redeem_voucher" type="button" style="background-color:',corporate_color_secondary,
                  ';" class="btn btn-default action-button shiny-bound-input" >
                      Redeem voucher
                    </button>',
                  '</div>',
                  uiOutput('info_from_redeem_section'),
                  '</div>'))
      
      showModal(modalDialog(
        title="Gift card found",
        gift_card_popup
      ))
    } 
  })
  
  # React to redeem button
  observeEvent(input$redeem_voucher, {
    amount <- input$amount_to_redeem
    print(amount)
    print(class(amount))
    
    # Load gift card & business id
    gift_card <- server_values$current_gift_card
    business_id <- server_values$current_business_id
    
    # Check if redeem amount exceeds voucher amount
    if (amount > gift_card$balance_money$amount/100) {
      #showNotification("Failed to redeem voucher, the amount is exceeding the gift card balance.")
      output$info_from_redeem_section <- renderUI(tags$em(style='color: red;',
                                                          'Failed to redeem voucher, the amount is exceeding the gift card balance.'))
      updateNumericInput(session, "amount_to_redeem", value=gift_card$balance_money$amount/100)
      return(NULL)
    }
    # Otherwise we charge the voucher
    chargeGiftCard(gift_card$id, amount)
    
    # Update the amount in the popup
    new_amount = gift_card$balance_money$amount/100-amount
    html("giftcard-header", paste0("Remaining balance: ",new_amount," ",gift_card$balance_money$currency))
    
    # Inform user
    output$info_from_redeem_section <- renderUI(
      tags$em(style='color: green;',
              'Voucher has been successfully charged.'))
  })
}
 
# Currently launching the app like this because I need my custom auth0_ui function...
config_file = 'local/_auth0.yml'
disable <- getOption("auth0_disable")
if (!is.null(disable) && disable) {
  shiny::shinyApp(ui, server)
} else {
  if (is.null(config_file)) {
    config_file <- auth0_find_config_file()
  }
  info <- auth0_info(config_file)
  shiny::shinyApp(auth0_ui(ui, info), auth0_server(server, info))
}
