library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinycssloaders)

dashboardPage(
    dashboardHeader(title = "ST558 Project 3"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data and App Information", tabName = "App_Info", icon = icon("info")),
            menuItem("Data Exploration: Visuals", tabName = "Exploration", icon = icon("database")),
            menuItem("Data Exploration: Numerical", tabName = "Exploration_Num", icon = icon("database")),
            menuItem("PCA", tabName = "Clustering_PCA", icon = icon("layer-group")),
            menuItem("Modeling: Random Forest", tabName = "Modeling", icon = icon("chart-line")),
            menuItem("Predictions: Random Forest", tabName = "Predictions1", icon = icon("bullseye")),
            menuItem("Modeling: kNN", tabName = "Modeling2", icon = icon("chart-line")),
            menuItem("Predictions: kNN", tabName = "Predictions2", icon = icon("bullseye")),
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
                            title = "Data Information", status = "primary", solidHeader = TRUE,
                            p("The data used for this project is data about Nepal's 2015 Gorkha earthquake. 
                            It comes from Driven Data, a company that hosts data science competitions to solve social 
                            problems for organizations.", strong("The goal of the project is to predict the amount 
                                                                of damage done to a building"),
                            " based on given characteristics, such as its age, building materials, 
                            and location."),
                            br(),
                            p("The earthquake presented a ton of damage to Nepal. Over 9,000 people died, 16,800 
                              injured, and 2.8 million displaced. Overall, 8 million people were impacted."),
                            withMathJax(),
                            helpText('Impacted Population:  $$\\frac{8,000,000}{27,000,000}\\ = \\frac{3}{10}\\ $$'),
                            p("The inital data for this competition has over 250,000 observations. In order to make 
                            this application more usable, that dataset has been randomly condensed down to 6,515 
                            observations. This will allow models to generate more quickly while still building upon 
                            a significant amount of data."),
                            br(),
                            p("More information about the data and explanations of each variable can be found ",
                              a("here.", 
                                href = "https://www.drivendata.org/competitions/57/nepal-earthquake/page/136/"))
                        ),
                        
                        box(width = 6, status = "primary", solidHeader = TRUE,
                            title = "App Information",
                            p("The application is intended to explore the data about Nepal's earthquake. 
                              It is split into sections to visualize the data, model the data, and use the model 
                              to make predictions. More detailed explanations of each section can be found below:"),
                            p(strong("Data Exploration: Visuals")),
                            p("Create custom scatter and bar plots of the data to explore trends visually."),
                            p(strong("Data Exploration: Numerical Summaries")),
                            p("Look at numerical summaries of the variables in the data."),
                            p(strong("PCA")),
                            p("Examine the principal component analysis for selected variables in the data."),
                            p(strong("Modeling: Random Forest")),
                            p("Select variables and model elements to create a random forest model. This page will 
                            automatically update to show how the model is performing on a test subset of the data."),
                            p(strong("Prediction: Random Forest")),
                            p("Use the random forest model created previously and select values of variables to see 
                            what the model would output as a prediction."),
                            p(strong("Modeling: kNN")),
                            p("Select variables and model elements to create a k - Nearest Neighbor model. This page 
                            will automatically update to show how the model is performing on a test subset of the 
                              data."),
                            p(strong("Prediction: kNN")),
                            p("Use the kNN model created previously and select values of variables to see what the 
                              model would output as a prediction."),
                            p(strong("View Data")),
                            p("Take a look at the data that is being used to create visualizations, models, and 
                              predictions. This page also allows for saving the data to a .csv.")
                        )
                    )
            ),

            #Data Exploration Tab (Visuals)
            tabItem(tabName = "Exploration",
                    h2("Visual Exploration of the Earthquake Data"),
                    fluidRow(
                        box(width = 4, status = "primary", solidHeader = TRUE,
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
                        box(width = 8, status = "primary", solidHeader = TRUE,
                            plotlyOutput("interactive_plots")
                        )
                    )
            ), 

            #Data Exploration Tab (Visuals)
            tabItem(tabName = "Exploration_Num",
                    h2("Numerical Exploration of the Earthquake Data"),
                    fluidRow(
                        box(width = 4,
                            status = "primary", solidHeader = TRUE,
                            title = "Select Numeric Variable",
                            selectInput("num_variables", "Select Variable", names(earthquake_numeric))
                            ),
                        box(width = 8,
                            status = "primary", solidHeader = TRUE,
                            title = "Numerical Summary",
                            tableOutput("summary_table")
                            ),
                        box(width = 4,
                            status = "primary", solidHeader = TRUE,
                            title = "Select Factor Variable",
                            selectInput("factor_variable_1", "Select Variable #1", names(earthquake_factor))
                            ),
                        box(width = 8,
                            status = "primary", solidHeader = TRUE,
                            title = "Count Summary",
                            tableOutput("count_table")
                        )
                    )
            ),
                        
            #Clustering/PCA Tab
            tabItem(tabName = "Clustering_PCA",
                    h2("Principal Component Analysis"),
                    fluidRow(
                        #Select Variables
                        box(width = 4, status = "primary", solidHeader = TRUE,
                            title = "Variables/Download",
                        varSelectInput("pca_variables", "Select Variables", earthquake_numeric, multiple = TRUE,
                                       selected = c("geo_level_1_id", "geo_level_2_id")),
                        downloadButton("download_plot", "Download Plot")
                        ),
                        
                        #Show PCA Plot
                        box(width = 8, status = "primary", solidHeader = TRUE,
                        title = "PCA Plot",
                        plotOutput("PCA_Plot")),
                        
                        #Show PCA
                        box(width = 12, status = "primary", solidHeader = TRUE,
                        title = "Individual Components",
                        verbatimTextOutput("PCA")
                        ),
                        
                        #PCA Summary
                        box(width = 12, status = "primary", solidHeader = TRUE,
                        title = "PCA Summary",
                        verbatimTextOutput("PCA_Summary")
                        )
                    )
                ),

            #Modeling Tab
            tabItem(tabName = "Modeling", 
                    h2("Random Forest Model"),
                    p("Use this page to select variables for a model, see how the model performs on test data, and 
                      submit your own values of predictors to get a prediction."),
                    fluidRow(
                        box(width = 4, 
                            title = "Select Model Variables",
                            status = "primary", solidHeader = TRUE,
                            varSelectInput("rf_variables", "Select Variables", earthquake_use[,-c(1,20)], 
                                           multiple = TRUE,
                                           selected = c("age", "superstructure")),
                            sliderInput("num_trees", "Number of Trees", 50, 500, 200),
                            checkboxInput("limit_trees", "Limit the Number of Trees?")
                            ),
                        box(width = 8,
                            status = "primary", solidHeader = TRUE,
                            title = "Results",
                            footer = "Results based on test data, which is 30% of the initial data.",
                            verbatimTextOutput("RF_Results") %>% withSpinner(color="#008ec1")
                            )
                        )
                    ),
            
            #View Other Model Tab
            tabItem(tabName = "Predictions1",
                    h2("Prediction Using Previously Built Random Forest Model"),
                    p("Note: only adjusting variables selected on the page where the model was built will impact 
                      the prediction. All variables not previously selected will not impact the prediction."),
                    fluidRow(
                        box(width = 4,
                            status = "primary", solidHeader = TRUE,
                            title = "Predict Earthquake Damage",
                            sliderInput("pred_geo1", "Geo Level 1", 
                                        min(earthquake_use$geo_level_1_id), 
                                        max(earthquake_use$geo_level_1_id), 
                                        mean(earthquake_use$geo_level_1_id)),
                            sliderInput("pred_geo2", "Geo Level 2", 
                                        min(earthquake_use$geo_level_2_id), 
                                        max(earthquake_use$geo_level_2_id), 
                                        mean(earthquake_use$geo_level_2_id)),
                            sliderInput("pred_geo3", "Geo Level 3", 
                                        min(earthquake_use$geo_level_3_id), 
                                        max(earthquake_use$geo_level_3_id), 
                                        mean(earthquake_use$geo_level_3_id)),
                            selectInput("pred_count_floors", "Count of Floors", 
                                        levels(earthquake_use$count_floors_pre_eq)),
                            sliderInput("pred_age", "Age of Building",
                                        min(earthquake_use$age), 
                                        max(earthquake_use$age), 
                                        mean(earthquake_use$age)),
                            sliderInput("pred_area", "Normalized Area of Building",
                                        min(earthquake_use$area_percentage), 
                                        max(earthquake_use$area_percentage), 
                                        mean(earthquake_use$area_percentage)),
                            sliderInput("pred_height", "Normalized Height of Building",
                                        min(earthquake_use$height_percentage), 
                                        max(earthquake_use$height_percentage), 
                                        mean(earthquake_use$height_percentage)),
                            selectInput("pred_land", "Land Type", 
                                        levels(earthquake_use$land_surface_condition)),
                            selectInput("pred_foundation", "Foundation Type", 
                                        levels(earthquake_use$foundation_type))
                        ),
                        box(width = 4,
                            status = "primary", solidHeader = TRUE,
                            title = "Predict Earthquake Damage Cont.",
                            selectInput("pred_roof", "Roof Type", 
                                        levels(earthquake_use$roof_type)),
                            selectInput("pred_ground", "Ground Floor Type", 
                                        levels(earthquake_use$ground_floor_type)),
                            selectInput("pred_other_floor", "Other Floor Type", 
                                        levels(earthquake_use$other_floor_type)),
                            selectInput("pred_position", "Position", 
                                        levels(earthquake_use$position)),
                            selectInput("pred_plan", "Plan Configuration", 
                                        levels(earthquake_use$plan_configuration)),
                            selectInput("pred_legal", "Legal Ownership Status", 
                                        levels(earthquake_use$legal_ownership_status)),
                            selectInput("pred_count_fam", "Count of Families", 
                                        levels(earthquake_use$count_families)),
                            selectInput("pred_superstructure", "Superstructure", 
                                        levels(earthquake_use$superstructure)),
                            selectInput("pred_secondary", "Secondary Use", 
                                        levels(earthquake_use$secondary_use))
                        ),
                        box(width = 3,
                            status = "primary", solidHeader = TRUE,
                            title = "Damage Prediction",
                            textOutput("RF_Prediction") %>% withSpinner(color="#008ec1")
                        )
                    )
            ),
            
            #View KNN Model Tab
            tabItem(tabName = "Modeling2",
                    h2("k - Nearest Neighbor Model"),
                    p("Use this page to select variables for a model, see how the model performs on test data, and 
                      submit your own values of predictors to get a prediction."),
                    fluidRow(
                        box(width = 4, 
                            title = "Select Model Variables",
                            status = "primary", solidHeader = TRUE,
                            varSelectInput("knn_variables", "Select Variables", 
                                           earthquake_numeric, 
                                           multiple = TRUE,
                                           selected = c("geo_level_1_id", "geo_level_2_id")),
                            sliderInput("knn_k", "Value of k", 1, 20, 10)
                    ),
                        box(width = 8,
                            status = "primary", solidHeader = TRUE,
                            title = "Results",
                            footer = "Results based on test data, which is 30% of the initial data.",
                            verbatimTextOutput("knn_results") %>% withSpinner(color="#008ec1")
                        )
                    )
            ),
            
            #View KNN Predictions
            tabItem(tabName = "Predictions2",
                    h2("Prediction Using Previously Built Random Forest Model"),
                    p("Note: only adjusting variables selected on the page where the model was built will impact 
                      the prediction. All variables not previously selected will not impact the prediction."),
                    fluidRow(
                        box(width = 4,
                            status = "primary", solidHeader = TRUE,
                            title = "Predict Earthquake Damage",
                            sliderInput("pred_geo1_2", "Geo Level 1", 
                                        min(earthquake_use$geo_level_1_id), 
                                        max(earthquake_use$geo_level_1_id), 
                                        mean(earthquake_use$geo_level_1_id)),
                            sliderInput("pred_geo2_2", "Geo Level 2", 
                                        min(earthquake_use$geo_level_2_id), 
                                        max(earthquake_use$geo_level_2_id), 
                                        mean(earthquake_use$geo_level_2_id)),
                            sliderInput("pred_geo3_2", "Geo Level 3", 
                                        min(earthquake_use$geo_level_3_id), 
                                        max(earthquake_use$geo_level_3_id), 
                                        mean(earthquake_use$geo_level_3_id)),
                        ),
                        box(width = 4,
                            status = "primary", solidHeader = TRUE,
                            title = "Predict Earthquake Damage Cont.",
                            sliderInput("pred_age_2", "Age of Building",
                                        min(earthquake_use$age), 
                                        max(earthquake_use$age), 
                                        mean(earthquake_use$age)),
                            sliderInput("pred_area_2", "Normalized Area of Building",
                                        min(earthquake_use$area_percentage), 
                                        max(earthquake_use$area_percentage), 
                                        mean(earthquake_use$area_percentage)),
                            sliderInput("pred_height_2", "Normalized Height of Building",
                                        min(earthquake_use$height_percentage), 
                                        max(earthquake_use$height_percentage), 
                                        mean(earthquake_use$height_percentage))),
                        box(width = 3,
                            status = "primary", solidHeader = TRUE,
                            title = "Damage Prediction",
                            textOutput("KNN_Prediction") %>% withSpinner(color="#008ec1")
                        )
                    )
            ),

            #View Data Tab
            tabItem(tabName = "View_Data",
                    DT::dataTableOutput("earthquake"),
                    downloadButton("download_data", "Download")
            )
        )
    )
)