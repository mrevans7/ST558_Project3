library(shiny)
library(tidyverse)
library(DT)

shinyServer(function(input, output) {

    #Read in Data
    nola_data <- reactive({
        nola_data <- read_csv("~/Documents/ST558/ST558_Project3/Project3/nola_2016_2019.csv")
    })
    
    #Create Data for Viewing in App
    output$nola_data <- DT::renderDataTable({
        datatable(nola_data_display <- nola_data(),
        options = list(scrollX = TRUE)
        )
    })
})
