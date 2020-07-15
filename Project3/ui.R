library(shinydashboard)

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
                            p("Information about the data goes here.")
                        ),
                        
                        box(width = 6,
                            title = "App Information",
                            p("Information about the application goes here.")
                        )
                    )
            ),

            #Data Exploration Tab
            tabItem(tabName = "Exploration"),

            #Clustering/PCA Tab
            tabItem(tabName = "Clustering_PCA"),

            #Modeling Tab
            tabItem(tabName = "Modeling"),

            #View Data Tab
            tabItem(tabName = "View_Data",
                    DT::dataTableOutput("nola_data"))
        )
    )
)