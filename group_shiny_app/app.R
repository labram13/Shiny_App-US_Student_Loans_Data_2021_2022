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
                tabPanel("Data: Megan",
                         sidebarLayout(
                           sidebarPanel(
                    
                ),
                          mainPanel(
                          )
                        )
        
              ),
              tabPanel("Data: Mike",
                       sidebarLayout(
                         sidebarPanel(
                           
                         ),
                         mainPanel(
    
                          )
                        )
                      ),
              tabPanel("Data: Nick",
                       sidebarLayout(
                         sidebarPanel(
                           
                         ),
                         mainPanel(
                         )
                       )
              ),
              tabPanel("Conclusion",
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
