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

connectToDB <- function() {
  db_user = "voucherlauncher"
  con <- RPostgres::dbConnect(RPostgres::Postgres(), dbname = "kindly-possum-2518.defaultdb", 
                              host = "free-tier5.gcp-europe-west1.cockroachlabs.cloud", 
                              port = 26257, user = db_user, 
                              password = readLines("local/pw.txt"))
}
