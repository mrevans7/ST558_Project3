library(shinydashboard)
library(tidyverse)
library(plotly)

dashboardPage(
    dashboardHeader(title = "ST558 Project 3"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data and App Information", tabName = "App_Info", icon = icon("info")),
            menuItem("Data Exploration", tabName = "Exploration", icon = icon("database")),
            menuItem("Clustering/PCA", tabName = "Clustering_PCA", icon = icon("layer-group")),
            menuItem("Modeling", tabName = "Modeling", icon = icon("chart-line")),
            menuItem("View Data", tabName = "View_Data", icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            #First Tab
            tabItem(tabName = "App_Info",
                    h2("Welcome!"),
                    fluidRow(
                        box(width = 6,
                            title = "Data Information",
                            p("Information about the data goes here. More information about the data can be found ",
                              a("here.", href = "https://www.drivendata.org/competitions/57/nepal-earthquake/page/136/"))
                        ),
                        
                        box(width = 6,
                            title = "App Information",
                            p("Information about the application goes here.")
                        )
                    )
            ),

            #Data Exploration Tab
            tabItem(tabName = "Exploration",
                    h2("Exploration of the Earthquake Data"),
                    fluidRow(
                        box(width = 4,
                            title = "Select Variables",
                            
                            #Select Graph Type
                            selectInput("plot_type", "Plot Type",
                                        c(Scatter = "scatter", Bar = "bar")),
                            
                            #Scatter
                            conditionalPanel(condition = "input.plot_type == 'scatter'",
                                             selectizeInput("numeric_x", "Select X Variable", 
                                                            selected = "age", 
                                                            choices = names(select_if(earthquake, is.numeric))),
                                             selectizeInput("numeric_y", "Select Y Variable", 
                                                            selected = "area_percentage", 
                                                            choices = names(select_if(earthquake, is.numeric))),
                                             checkboxInput("damage", "Color by Damage?")
                            ),
                            
                            #Bar
                            conditionalPanel(condition = "input.plot_type == 'bar'",
                                             selectizeInput("factor", "Select Variables", selected = "secondary_use", 
                                                            choices = names(select_if(earthquake, is.factor))),
                                             checkboxInput("damage2", "Color by Damage?")
                            )
                        ),
                        
                        #Show Plots
                        box(width = 8,
                            plotlyOutput("interactive_plots")
                        )
                    )
            ), 

            #Clustering/PCA Tab
            tabItem(tabName = "Clustering_PCA",
                    h2("Principal Component Analysis"),
                    fluidRow(
                        #Select Variables
                        box(width = 4,
                        varSelectInput("pca_variables", "Select Variables", earthquake_numeric, multiple = TRUE),
                        downloadButton("download_plot", "Download Plot")
                        ),
                        
                        #Show PCA Plot
                        box(width = 8,
                        title = "PCA Plot",
                        plotOutput("PCA_Plot")),
                        
                        #Show PCA*
                        box(width = 12,
                        title = "Individual Components",
                        verbatimTextOutput("PCA")
                        ),
                        
                        #PCA Summary
                        box(width = 12, 
                        title = "PCA Summary",
                        verbatimTextOutput("PCA_Summary")
                        )
                    )
                ),

            #Modeling Tab
            tabItem(tabName = "Modeling"),

            #View Data Tab
            tabItem(tabName = "View_Data",
                    DT::dataTableOutput("earthquake"),
                    downloadButton("download_data", "Download")
            )
        )
    )
)