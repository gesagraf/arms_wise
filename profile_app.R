if(!require(profvis))install.packages("profvis");library(profvis)
library(shiny)
source("ui.R")
source("server_old.R")

profvis({
  runApp(shinyApp(ui, server))
})

source("server2.R")
# source("ui2.R")
profvis({
  runApp(shinyApp(ui, server))
})

source("server.R") #newest server version
profvis({
  runApp(shinyApp(ui, server))
})
