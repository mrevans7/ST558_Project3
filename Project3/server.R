#### Shiny Work ####

shinyServer(function(input, output) {
    
    #Create Data
    earthquake <- reactive({earthquake_use})
    earthquake_numeric_use <- reactive({earthquake_numeric})
    
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
