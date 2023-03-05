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
              tabPanel("Plot",
                       sidebarLayout(
                         sidebarPanel(h3(strong("Average Loans per State:\n")),
                                      p(),
                                      p("Here, we have calculated the average
                                        loans students have taken out (total
                                        amount divided by number of schools
                                        per state) in each state. By selecting
                                        desired states to view, the average
                                        is arranged from highest to lowest. In
                                        doing so, you are able to see which states, 
                                        in comparison, loaned the most amount
                                        of money to go to school."),
                                      p(),
                                      fluidRow(
                                        column(6,
                                               radioButtons("color", "Choose color palette!",
                                                            choices = c("A", 
                                                                        "B",
                                                                        "C",
                                                                        "D"))
                                        ),
                                        column(6,
                                               uiOutput("checkboxState")
                                        )
                                      ),
                                      p("You have selected the following states: \n"),
                                      uiOutput("selected")
                         ),
                         mainPanel(
                           plotOutput("plot"),
                           textOutput("result")
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
  #-------Megan's code---------
  
  
  #-------Mike's code----------
  plt <- student_loans %>% 
    filter(`Loan Type` == "Total",
           !is.na(`$ of Disbursements`),
           !is.na(State)) %>% 
    group_by(State, School) %>% 
    summarize(total_loans = max(`$ of Disbursements`)) %>% 
    group_by(State) %>% 
    reframe(avg_loan = mean(total_loans))
  output$intro <- renderDataTable({
    student_loans %>% 
      head(5)
  })
  output$checkboxState <- renderUI({
    selectizeInput(
      "State", strong("Select State or Type State abbr. e.g. WA"),
      choices =  unique(student_loans$State),
      multiple = TRUE
    )
  })
  sample <- reactive({
    plt %>% 
      filter(State %in% input$State)
  })
  output$plot <- renderPlot ({
    ggplot(data = sample()) +
      geom_col(aes(x = forcats::fct_reorder(State, desc(avg_loan)), 
                   y = avg_loan, fill = State ),
               labels =scales::comma
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme(legend.position="none")+
      labs(title = "Average Loans Taken Out By Students per State",
           x = "State",
           y = "$ of Average Loans") +
      scale_fill_viridis(discrete = TRUE, option = input$color)
  })
  output$result <- renderText({
    max <- sample() %>% 
      pull(avg_loan) %>% 
      max()
    if(is.infinite(max))
      paste("Please select some states to start showing showing data.")
    else
      ""
  })
  
  
  #-------Nick's code----------
  
  city_state_loans <- student_loans %>%
    group_by(City, State) %>%
    summarize(total_loan = sum(`$ of Disbursements`))
  
  top_schools_by_city <- city_state_loans %>%
    arrange(State, desc(total_loan)) %>%
    group_by(State) %>%
    top_n(10, total_loan) %>%
    arrange(State, desc(total_loan))
  output$checkboxState <- renderUI({
    tagList(
      selectizeInput(
        "State", strong("Select State or Type State abbr. e.g. WA"),
        choices = unique(student_loans$State),
        multiple = TRUE
      ),
      selectizeInput(
        "City", strong("Search by City"),
        choices = NULL,
        multiple = TRUE
      )
    )
  })
  sample <- reactive({
    plt %>%
      filter(State %in% input$State,
             City %in% input$City)
  })
  labs(title = paste("Average Loans Taken Out By Students per State and City",
                     if (length(input$State) > 0) paste0(" in ", paste(input$State, collapse = ", ")),
                     if (length(input$City) > 0) paste0(" - ", paste(input$City, collapse = ", ")),
                     sep = ""))
  output$result <- renderText({
    max <- sample() %>%
      pull(avg_loan) %>%
      max()
    if (is.infinite(max))
      "Please select some states or cities to start showing data."
    else
      paste("Showing data for",
            if (length(input$State) > 0) paste0("state(s) ", paste(input$State, collapse = ", ")),
            if (length(input$State) > 0 && length(input$City) > 0) " and",
            if (length(input$City) > 0) paste0("city(s) ", paste(input$City, collapse = ", ")),
            sep = " ")
  })
  
  
  #-------Nathaniel's code----------
}

shinyApp(ui = ui, server = server)
