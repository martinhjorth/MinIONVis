#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(RColorBrewer)
library(data.table)
library(tidyverse)
source("functions.R")

# Define UI for application that draws a histogram
navbarPage(
  title = h4(a(href="javascript:history.go(0)", style="color:#606060", "MinION Visualiser")),
  tabPanel("Data and filtering",
           mainPanel(
             width = 9,
             conditionalPanel(
               condition = "$('html').hasClass('shiny-busy')",
               p("Loading...")
               ),
             tags$hr(),
             DT::dataTableOutput("filterdata", width = "100%")
             )
           ),
              
  tabPanel("Analysis",
             sidebarPanel(
               width = 3,
               position = "left",
               h4("Options"),
               uiOutput("plot_type"),
               ##### Barplot #####
               conditionalPanel(
                 condition = "input.plot_choice == 'barplot'",
                 uiOutput("barplot_UI_group"),
                 sliderInput(inputId = "bar_tax.show",
                             label = "Number of taxa to show",
                             min = 1,
                             max = 50,
                             value = 10,
                             step = 1
                             )
               ),
               ##### Heatmap #####
               conditionalPanel(
                 condition = "input.plot_choice == 'heatmap'",
                 sliderInput(inputId = "heat_tax.show",
                             label = "Number of taxa to show",
                             min = 1,
                             max = 50,
                             value = 10,
                             step = 1
                 )
               )
           ),
           mainPanel(
             width = 9,
             plotOutput("plot")
             #tabsetPanel(id = "plot_type",
                         )
           )
           
           
)