library(httr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
options(shiny.port = 8080, shiny.host="0.0.0.0")
shiny::runApp("app.R")
# options(shiny.port = 3000, shiny.host="0.0.0.0")
# shiny::runApp("app-open.R")
