#### Shiny Work ####

shinyServer(function(input, output, session) {
    
    #Create Data
    earthquake <- reactive({earthquake_use})
    earthquake_numeric_use <- reactive({earthquake_numeric})
    earthquake_train_use <- reactive({earthquake_train})
    earthquake_test_use <- reactive({earthquake_test})
    
    #Create Plot (Interactive)
    output$interactive_plots <- renderPlotly({
        #get filtered data
        earthquake_plot <- earthquake()
        
        #Create Plots
        #Scatter
        if(input$plot_type == "scatter"){
            plot.1 <- ggplot(data = earthquake_plot, aes_string(x = input$numeric_x, y = input$numeric_y))
            if(input$damage){
                p1_1 <- plot.1 + geom_point(aes(color = damage_grade)) + 
                    labs(title = paste0(input$numeric_y, " vs. ", input$numeric_y))
                ggplotly(p1_1)
            } else {
                p1_2 <- plot.1 + geom_point() +
                    labs(title = paste0(input$numeric_y, " vs. ", input$numeric_y))
                ggplotly(p1_2)
            }
        #Bar
        }else if(input$plot_type == "bar"){
            plot.2 <- ggplot(data = earthquake_plot, aes_string(x = input$factor))
            if(input$damage2){
                p2_1 <- plot.2 + geom_bar(aes(fill = damage_grade)) + 
                    scale_x_discrete(guide = guide_axis(n.dodge = 3)) + 
                    labs(title = paste0("Bar Plot for ", input$factor))
                ggplotly(p2_1)
            } else {
                p2_2 <- plot.2 + geom_bar() + 
                    scale_x_discrete(guide = guide_axis(n.dodge = 3)) + 
                    labs(title = paste0("Bar Plot for ", input$factor))
                ggplotly(p2_2)
            }
        }
    })
    
    #Numerical Summary
    output$summary_table <- renderTable({
      #Get Data
      data <- earthquake() 
      data_use <- data %>% select(input$num_variables)
      #Apply Functions
      table.1 <- sapply(data_use, min)
      table.2 <- sapply(data_use, quantile, probs = .25)
      table.3 <- sapply(data_use, mean)
      table.4 <- sapply(data_use, median)
      table.5 <- sapply(data_use, quantile, probs = .75)
      table.6 <- sapply(data_use, max)
      #Combine
      table.7 <- rbind(table.1, table.2, table.3, table.4, table.5, table.6)
      #Rename rows
      rownames(table.7) <- c("Min.", "25th Percentile", "Mean", "Median", "75th Percentile", "Max.")
      #Output
      table.7
    }, rownames = TRUE)
    
    #Count Summary
    output$count_table <- renderTable({
      #Get Data
      data <- earthquake() 
      data_factor1 <- data %>% select(input$factor_variable_1)
      #Apply Function
      table_count <- table(data_factor1, dnn = c(input$factor_variable_1, "Freq"))
      #Output
      table_count
    })
    
    #PCA Plot
    output$PCA_Plot <- renderPlot({
        PCA_use <- earthquake() %>% select(!!!input$pca_variables)
        PCs <- prcomp(PCA_use, center = TRUE, scale = TRUE)
        biplot(PCs, xlabs = rep(".", nrow(PCA_use)), cex = 1.2)
    })
    
    #Download Plot
    output$download_plot <- downloadHandler(
        filename = "plot.png",
        content = function(file) {
            png(file)
            PCA_use <- earthquake() %>% select(!!!input$pca_variables)
            PCs <- prcomp(PCA_use, center = TRUE, scale = TRUE)
            pca_plot <- biplot(PCs, xlabs = rep(".", nrow(PCA_use)), cex = 1.2)
            dev.off()
        }
    )
    
    #PCA
    output$PCA <- renderPrint({
        PCA_use <- earthquake() %>% select(!!!input$pca_variables)
        PCs <- prcomp(PCA_use, center = TRUE, scale = TRUE)
        PCs
    })
    
    #PCA Sumamry
    output$PCA_Summary <- renderPrint({
        PCA_use <- earthquake() %>% select(!!!input$pca_variables)
        PCs <- prcomp(PCA_use, center = TRUE, scale = TRUE)
        summary(PCs)
    })
    
    #Random Forest Model
    output$RF_Results <- renderPrint({
        #Split Data
        earthquake_train_use <- earthquake_train_use() %>% select(!!!input$rf_variables, damage_grade)
        earthquake_test_use <- earthquake_test_use() %>% select(!!!input$rf_variables, damage_grade)
        
        if(input$limit_trees){
          observe({updateSliderInput(session, "num_trees", max = 300)})
        } else {
          observe({updateSliderInput(session, "num_trees", max = 500)})
        }
        
        #Create Model
        rf_ranger <- ranger(formula = damage_grade ~ ., data = earthquake_train_use, num.trees = input$num_trees, 
                            mtry = ncol(earthquake_train_use)/3)
        
        #Predict
        rfPred <- predict(rf_ranger, earthquake_test_use)
        
        #Create Results
        rf_results <- confusionMatrix(data = rfPred$predictions, reference = earthquake_test_use$damage_grade) 
        rf_results
    })
    
    #Create Predictions Based on Input
    output$RF_Prediction <- renderText({
        input_data <- data.frame("geo_level_1_id" = input$pred_geo1, 
                            "geo_level_2_id" = input$pred_geo2, 
                            "geo_level_3_id" = input$pred_geo3, 
                            "count_floors_pre_eq" = input$pred_count_floors, 
                            "age" = input$pred_age, 
                            "area_percentage" = input$pred_area, 
                            "height_percentage" = input$pred_height, 
                            "land_surface_condition" = input$pred_land, 
                            "foundation_type" = input$pred_foundation, 
                            "roof_type" = input$pred_roof, 
                            "ground_floor_type" = input$pred_ground, 
                            "other_floor_type" = input$pred_other_floor, 
                            "position" = input$pred_position, 
                            "plan_configuration" = input$pred_plan, 
                            "legal_ownership_status" = input$pred_legal, 
                            "count_families" = input$pred_count_fam, 
                            "superstructure" = input$pred_superstructure, 
                            "secondary_use" = input$pred_secondary
        )
        
        #Split
        earthquake_train_use <- earthquake_train_use() %>% select(!!!input$rf_variables, damage_grade)
        
        #Create Model
        rf_ranger <- ranger(formula = damage_grade ~ ., data = earthquake_train_use, num.trees = input$num_trees, 
                            mtry = ncol(earthquake_train_use)/3)
        
        #Predict
        rfPred_input <- predict(rf_ranger, input_data)
        rfPred_input$predictions
    })
    
    #kNN Forest Model
    output$knn_results <- renderPrint({
        #Split Data
        earthquake_train_knn <- earthquake_train_use() %>% select(!!!input$knn_variables, damage_grade)
        earthquake_test_knn <- earthquake_test_use() %>% select(!!!input$knn_variables, damage_grade)
        
        #Create Model
        knn_fit <- train(damage_grade ~ ., data = earthquake_train_knn, method = "knn",
                         preProcess = c("center", "scale"), 
                         tuneGrid = expand.grid(k = c(1 : input$knn_k)))
        
        #Predict
        knn_pred <- predict(knn_fit, newdata = earthquake_test_knn)
        
        #Create Results
        knn_result <- confusionMatrix(knn_pred, earthquake_test_knn$damage_grade) 
        knn_result
    })
    
    #Create Predictions Based on Input
    output$KNN_Prediction <- renderText({
        input_data <- data.frame("geo_level_1_id" = input$pred_geo1_2, 
                                 "geo_level_2_id" = input$pred_geo2_2, 
                                 "geo_level_3_id" = input$pred_geo3_2, 
                                 "age" = input$pred_age_2, 
                                 "area_percentage" = input$pred_area_2, 
                                 "height_percentage" = input$pred_height_2
                                 )
        
        #Split
        earthquake_train_knn <- earthquake_train_use() %>% select(!!!input$knn_variables, damage_grade)
        
        #Create Model
        knn_fit_input <- train(damage_grade ~ ., data = earthquake_train_knn, method = "knn",
                         preProcess = c("center", "scale"), 
                         tuneGrid = expand.grid(k = c(1 : input$knn_k)))
        
        #Predict
        knn_pred_input <- predict(knn_fit_input, newdata = input_data)
        knn_pred_input[1]
    })
    
    #Create Data for Viewing in App
    output$earthquake <- DT::renderDataTable({
        datatable(earthquake_display <- earthquake(),
                  options = list(scrollX = TRUE)
        )
    })
    
    #Download Data
    output$download_data <- downloadHandler(
        filename = "earthquake.csv",
        content = function(file) {
            write.csv(earthquake(), file, row.names = FALSE)
        }
    )
})
