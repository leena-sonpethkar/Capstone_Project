##################################################################################
# Author: Leena Sonpethkar
# Date: 25-Apr-2021
# Filename: ui.R
# Description: Capstone Project - Week7 course project Shiny Application
##################################################################################

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Capstone: SwiftKey Project",
                   
  tabPanel("Text Prediction Model",

    HTML("<strong>Developed By: Leena Sonpethkar</strong>"), 
    br(), 
    HTML("<strong>Date: 25-Apr-2021</strong>"), 
    br(), 
           
             
    # Application title
    titlePanel("Predict the Next Word"),
  
    # Sidebar with a input for incomplete sentence 
    sidebarLayout(
      sidebarPanel(
        h4(p("Please enter partially complete sentense to predict next word")),
        textInput("txtIN", "Enter your sentence here", value = ""),
        br(),
        br(),
        #submitButton("Clear Input")
        #actionButton("clrInput", "Clear Input")
        
      ),
      
      # Show a next predicted word
      mainPanel(
        h4(p("Predicted next word is")),
        tags$style(type='text/css', '#txtPred {background-color: lightyellow; color: blue; bold=true}'), 
        h5(textOutput("txtPred")),
        br(),
        br()
        
        #h4(p("Note:")),
        #tags$style(type='text/css', '#txtNote {background-color: color: black; bold=true}'), 
        #h5(textOutput("txtNote"))
      
      )
    )
    ),
  tabPanel("About",
           mainPanel(
             #includeHTML(file.path(paste(getwd(), "About_Pred_Model.html", sep = "/")))
             includeHTML("D:/PredData/About_Pred_Model.html")
           )
    )
))
