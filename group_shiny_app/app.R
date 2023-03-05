library(shiny)
library(tidyverse)
library(dplyr)
library(forcats)
library(viridis)

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
                             selectInput("school-type",
                                         "What school type would you like to see
                                        the average debt for?",
                                         choices = c("Private-Nonprofit", "Proprietary",
                                                     "Public", "Foreign-Public",
                                                     "Foreign-Private"),
                                         selected = "Private-Nonprofit")
                           ),
                           mainPanel(
                             tableOutput("table"),
                             textOutput("tableInfo")
                           )
                         )
              ),
              tabPanel("Data: Mike",
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
  output$table <- renderTable ({
    student_loans %>%
      filter(`School Type` == input$school_type) %>%
      group_by(School) %>%
      filter(!is.na(School), !is.na(Recipients)) %>%
      summarise(avg = mean(Recipients)) %>%
      select(School, `School Type`, avg) %>%
      arrange(desc(avg))
  })
  
  output$tableInfo <- renderPrint ({
    num <- student_loans %>%
      filter(`School Type` == input$school_type) %>%
      nrow()
    cat("There are ", num, " ", input$school_type, " schools.")
  })

  #-------Mike's code----------
  
  # Data manipulated to find average loan per state. 
  avg_loan_per_state <- student_loans %>% 
    filter(`Loan Type` == "Total",
           !is.na(`$ of Disbursements`),
           !is.na(State)) %>% 
    group_by(State, School) %>% 
    summarize(total_loans = max(`$ of Disbursements`)) %>% 
    group_by(State) %>% 
    reframe(avg_loan = mean(total_loans))
  
  # Creates widget to with checkboxes to allow user to type or pick
  # which states they would like to filter. Can select multiple
  # to compare and contrast. 
  output$checkboxState <- renderUI({
    selectizeInput(
      "State", strong("Select State or Type State abbr. e.g. WA"),
      choices =  unique(student_loans$State),
      multiple = TRUE
    )
  })
  
  # Create a reactive response for the column name state of the
  # manipulated data. 
  state_filter <- reactive({
    avg_loan_per_state %>% 
      filter(State %in% input$State)
  })
  
  # Renders the plot with labels. Added a color palette option to give 
  # user visual options. 
  output$plot <- renderPlot ({
    ggplot(data = state_filter()) +
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
  
  # Message that shows up below the graph if nothing is selected. 
  # Message disappears once user has selected some states. 
  
  output$result <- renderText({
    max <- state_filter() %>% 
      pull(avg_loan) %>% 
      max()
    if(is.infinite(max))
      paste("Please select some states to start showing showing data.")
    else
      ""
  })
  
  # Message that shows all the State's the user has chosen. 
  output$selected <- renderUI({
    HTML(paste(input$State, sep ="<br/>"))
  })
  output$top10 <- renderText({
    if (is.null(input$choose)){
      y = plt %>% head(10)
      paste(y$State, sep=",")
    } else {
      x = loan_table()
      y = x %>% head(10)
      paste(y$State)
    }
  })
  #-------Nick's code----------
  
  # city_state_loans <- student_loans %>%
  #   group_by(City, State) %>%
  #   summarize(total_loan = sum(`$ of Disbursements`))
  # 
  # top_schools_by_city <- city_state_loans %>%
  #   arrange(State, desc(total_loan)) %>%
  #   group_by(State) %>%
  #   top_n(10, total_loan) %>%
  #   arrange(State, desc(total_loan))
  # output$checkboxState <- renderUI({
  #   tagList(
  #     selectizeInput(
  #       "State", strong("Select State or Type State abbr. e.g. WA"),
  #       choices = unique(student_loans$State),
  #       multiple = TRUE
  #     ),
  #     selectizeInput(
  #       "City", strong("Search by City"),
  #       choices = NULL,
  #       multiple = TRUE
  #     )
  #   )
  # })
  # sample <- reactive({
  #   plt %>%
  #     filter(State %in% input$State,
  #            City %in% input$City)
  # })
  # labs(title = paste("Average Loans Taken Out By Students per State and City",
  #                    if (length(input$State) > 0) paste0(" in ", paste(input$State, collapse = ", ")),
  #                    if (length(input$City) > 0) paste0(" - ", paste(input$City, collapse = ", ")),
  #                    sep = ""))
  # output$result <- renderText({
  #   max <- sample() %>%
  #     pull(avg_loan) %>%
  #     max()
  #   if (is.infinite(max))
  #     "Please select some states or cities to start showing data."
  #   else
  #     paste("Showing data for",
  #           if (length(input$State) > 0) paste0("state(s) ", paste(input$State, collapse = ", ")),
  #           if (length(input$State) > 0 && length(input$City) > 0) " and",
  #           if (length(input$City) > 0) paste0("city(s) ", paste(input$City, collapse = ", ")),
  #           sep = " ")
  # })

  #-------Nathaniel's code----------
}

shinyApp(ui = ui, server = server)
