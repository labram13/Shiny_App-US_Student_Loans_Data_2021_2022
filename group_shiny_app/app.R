library(shiny)
library(tidyverse)
library(dplyr)

student_loans <- read_delim("../data/Student Loan Debt by School 2020-2021.csv")

ui <- fluidPage(
    titlePanel("Student Loans 2020-2021"),
    tabsetPanel(type = "tabs",
                tabPanel("Overview:",
                         sidebarLayout(
                           sidebarPanel (
                           ),
                           mainPanel(
                           )
                         )
                ),
                tabPanel("The What and Why",
                         sidebarLayout(
                           sidebarPanel(
                           ),
                           mainPanel(
                           )
                         )
                ),
                tabPanel("Table",
                         sidebarLayout(
                           sidebarPanel(
                    
                ),
                          mainPanel(
                          )
                        )
        
              ),
              tabPanel("Plot",
                       sidebarLayout(
                         sidebarPanel(
                           
                         ),
                         mainPanel(
    
                          )
                        )
              )
    )
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
