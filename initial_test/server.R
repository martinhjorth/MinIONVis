#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define server logic to read file every nth seconds
shinyServer(function(input, output, session) {
  fileData <- reactiveFileReader(10000, session, "sequences.labels", read.delim, header = F)
    
  # Data table
  output$filterdata <- DT::renderDataTable(
    data <- loaded_data_subset(),
    server = TRUE,
    filter = "top",
    rownames = FALSE,
    selection = "none",
    options = list(pageLength = 10,
                   autoWidth = TRUE,
                   scrollX = TRUE,
                   lengthMenu = c(5, 10, 25, 50, 100),
                   columnDefs = list(list(width = '125px', targets = "_all"))
                   )
  )
  
  output$plot_type <- renderUI(radioButtons(inputId = "plot_choice", 
                                            label = "Heatmap or barplot?",
                                            choices = c("Heatmap" = "heatmap",
                                                        "Barplot" = "barplot"),
                                            selected = "barplot"
    )
  )
  
  ## Subset data based on user input in Data table
  loaded_data_subset <- reactive({
    data <- fileData()
    colnames(data)[2] <- "Taxonomy"
    rownames(data) <- data[,1]
    data[1] <- NULL
    abund <- data %>%
      group_by(Taxonomy) %>%
      summarise(abund = n()) %>%
      as.data.frame()
    #abund$Abundance <- (abund$abund/sum(abund$abund))*100
    #scaled$Species <- factor(scaled$Species, levels = scaled$Species[order(scaled$Abundance, decreasing = F)])
    tax <- tax_rename(abund[1])
    rownames(tax) <- abund[,1]
    rownames(abund) <- abund[,1]
    abund[1] <- NULL
    data <- load_data(abund = abund, tax = tax, percent = TRUE)
    

    
    #data_tax$Taxonomy <- as.factor(gsub(".__", "",data_tax$Taxonomy))
    #data_sep <- separate(data_tax, Taxonomy, c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = "\\|")
    
    ## Scaling - species so far
    #scaled <- data_sep
    
    
    #scaled$Sample <- as.factor("Sample 1")
    #scaled[,c(1:7,9)] <- as.data.frame(lapply(scaled[,c(1:7,9)], as.factor))
    
    #return(scaled)
    
    # Subset data based on input from data table, inverse if selected
    

    #scaled <- scaled[c(input$filterdata_rows_all), , drop = FALSE]
    #return a new list
    #newabund <- d[rownames(2:19), , drop=FALSE]
    return(data)
  })
  

  output$barplot_UI_group <- renderUI(selectInput(inputId = "tax",
                                         label = "Select taxonomy to aggregate to",
                                         choices = c(colnames(loaded_data_subset()[1:7])),
                                         selected = "Genus"
                                         )
  )

  output$plot <- renderPlot({
    #data <- loaded_data_subset()
  
    # Conditions
    if(input$plot_choice == "heatmap"){
      data <- loaded_data_subset()
      data$Sample <- as.factor("Sample 1")
      
      #TotalCounts <- group_by(data, Species) %>%
      #  summarise(Abundance = sum(abund)) %>%
      #  arrange(desc(Abundance))
      #data$species <- factor(data$Species, levels = rev(TotalCounts$Species))
      #data$Species <- factor(data$Species, levels = data$Species[order(data$abund, decreasing = F)])
      
      data <- data[order(data$abund, decreasing = TRUE),]#Problem - ggplot overrules this?
      data <- data[c(1:as.numeric(input$heat_tax.show)),]#Problem here - takes the first rows. Maybe something like 'unique' might work?
      

    ## Heatmap
    ggplot(data = data, aes_string(x = data$Sample, y = data$Species)) +
      geom_tile(aes_string(fill = data$abund), colour = "white", size = 0.5) +
      theme(
        legend.title = element_blank(),
        axis.title = element_blank()
      )
    
    #
    } else if(input$plot_choice == "barplot"){
      data <- loaded_data_subset()
      data <- data[order(data$abund, decreasing = TRUE),]
      data <- data[c(1:as.numeric(input$bar_tax.show)),]
      
    ## Create the barplot with the specified taxonomy
    ggplot(data = data, aes_string(input$tax, data$abund)) + 
      geom_bar(stat = "identity") + 
      ylab("Read abundance [%]") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 16))
    }
  })
  
})