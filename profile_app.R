if(!require(profvis))install.packages("profvis");library(profvis)
library(shiny)
source("ui.R")
source("server.R")

profvis({
  runApp(shinyApp(ui, server))
})

source("server2.R")
# source("ui2.R")
profvis({
  runApp(shinyApp(ui, server))
})

