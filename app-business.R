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

source("functions.R")

# Set up env
setUp()

# To retrigger the business list
values <- reactiveValues(flag_delete = 0)

ui <- fluidPage(theme = shinytheme("flatly"),
                # Make modal dialog a bit wider
                tags$head(tags$style(".modal-dialog {width: 700px}")),
                # Add header with logo & info link
                addHeader(),
                # Add loadbar
                addLoadbar(),
                # Set full screen background image
                setBackgroundImage(src = 'gift-ga793e7a9f_1920.jpg'),
                # Use shinyjs
                useShinyjs(),
                # Button to add your business ####
                actionButton("add_business", "Add your business", icon("plus"), 
                             style="font-family:sans-serif;color:#fff;background-color:#4f3c66;border-color:#4f3c66;font-size:200%;margin:35px;"),
                uiOutput("businesses") # dark blue #224578 dark purple #4f3c66 bright green #8CF4A4
                )
                
server <- function(input, output, session) {
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
    con <- connectToDB()
    user <- session$userData$auth0_info$sub
    print(user)
    if (is.null(user)) {
      print("Not logged in")
      user <- "test"
    }
    businesses <- dbGetQuery(con, paste0("
                                   select business_id, business_name, business_description as business_info
                                   from businesses 
                                   where user_name = '",user,"'"))
    dbDisconnect(con)
    if (nrow(businesses) == 0) {
      return(NULL)
    }
    businesses_html <- h1("Your businesses")
    for (i in c(1:nrow(businesses))) {
      business_id = businesses$business_id[i]
      png(paste0("www/",business_id,".png"))
      # Generate QR codes ####
      plot(qr_code(paste0("https://emelieh21.shinyapps.io/voucher-launcher-open/?business_id=",businesses$business_id[i],"&mode=scanned"), ecl = "Q"))
      dev.off()
      business_div <- div(style='margin-left:5px;',
        div(style="display: inline-block;vertical-align:top;margin-top:20px; margin-left:20px;", 
            img(src = paste0(business_id,".png"), width = 150)),
        div(style="display: inline-block;vertical-align:top;margin-top:20px; margin-left:20px; width:400px;", 
            HTML(paste0("<b>Name: </b>", businesses$business_name[i], "</br>",
                        "<b>Info: </b>", businesses$business_info[i]), "</br>"
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
    return(div(style="margin-left:35px;",HTML(businesses_html)))
  })
  
}

shinyApp(ui, server)
#shinyAppAuth0(ui, server, config_file = 'local/_auth0.yml')
