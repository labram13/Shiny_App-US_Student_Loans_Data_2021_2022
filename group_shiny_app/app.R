library(shiny)
library(tidyverse)
library(dplyr)
library(forcats)
library(viridis)

student_loans <- read_delim("../data/Student Loan Debt by School 2020-2021.csv")

ui <- fluidPage(
  tags$img(src = "https://honors.uw.edu/wp-content/uploads/2018/08/UW_Top_View.jpg",
           style = 'position: absolute; opacity: 0.2;',
           height="120%", width="120%", align="center"
  ),
  titlePanel("Student Loans 2020-2021"),
  tabsetPanel(type = "tabs",
              tabPanel("Overview:",
                       mainPanel(
                         HTML('<left><img src="https://justbuildwealth.com/wp-content/uploads/2019/02/pay-off-studen-loan-burden-fast.jpg", height = "320px"></left>'),
                         HTML(
                           paste0(
                             h3(strong("The Burden and Impact of Student Loans on Higher Education: 
                                  An Analysis of Student Loan Debt Data from 2020-2021 Academic Year")),'<br/>',
                             h4(em("By Michaelangelo Labrador, Megan Yam, 
                                     Stephen Nicklaus Likin, and Nathaniel Sayasack"), style = "color:black"),'<br/>',
                             p(style="text-align:justify;", "While student loans are essential for many students to pay for college, they also have several disadvantages that can substantially influence students and their decision to attend college.
                                             One significant disadvantage is the debt burden that students endure after graduation, which can have a long-term impact on their financial stability.
                                             Student loan interest rates can cause substantial financial stress and pressure on graduates' budgets, making it difficult to handle other financial commitments such as housing, transportation, and healthcare.
                                             Additionally, students who borrow money for education may feel driven to seek higher-paying jobs rather than their hobbies or interests.
                                             This pressure may limit their employment opportunities and compel them to choose financial security above personal fulfillment.
                                             Furthermore, students with large loan debt may feel obliged to postpone major life milestones such as starting a family, purchasing a home, or furthering their education.
                                             Fear of incurring substantial amounts of debt can also deter students from enrolling in college, particularly those from low-income families who may not have the financial ability to attend college without significant borrowing.
                                             This can limit their economic mobility chances and promote social and economic inequity."), '<br/>',
                             
                                             p("Ultimately, while student loans might enable access to higher education, they also have significant downsides that can long-term influence students' life.
                                             To limit the burden of debt, students must carefully assess the financial consequences of taking out loans and investigate alternate sources of income, such as grants and scholarships.
                                             This study examines data on student loan debt acquired by Andy Kriebel from various universities and colleges throughout the 2020-2021 academic year.
                                             The information in the data collection comprises school names, states, zip codes, school kids, and loan types.
                                             The intended audience for this information varies, but it is precious for parents who may make informed judgments about which schools to send their children to based on the average loan taken out per student.
                                             The data collection can also assist students in planning their post-college lives and determining how much student debt they will have to repay.
                                             The project underlines the need for financial awareness while selecting an institution, especially given the high cost of higher education.
                                             Our project also brings up several questions to be answered using the data set, including which states provide the most/most minor loans, what types of loans are provided, which school within a city offers the most loans/disbursements, and which school provides the most/most minor loans within a specific city, state, or zip code.
                                             Overall, the data collection helps make educated decisions and plan for the financial implications of student loans.
                                             ")
                           )
                         )
                       )
                ),
                tabPanel("Data: Megan",
                         sidebarLayout(
                           sidebarPanel(h3(strong("Average Recipients by School type:\n")),
                                        p(),
                                        p("In this tab, you are able to view the average
                                          amount of loan recipients a school has by the
                                          type of school it is. From this, you can predict
                                          the likelihood of receiving a good financial
                                          aid package from the school, and compare the
                                          amount of loan recipients from one school to
                                          another school of the same type."),
                             selectInput("school_type",
                                         "What school type would you like to see
                                        the amount of average loan recipients for?",
                                         choices = c("Private-Nonprofit", "Proprietary",
                                                     "Public", "Foreign-Public",
                                                     "Foreign-Private"),
                                         selected = "Private-Nonprofit"),
                           ),
                           mainPanel(
                             textOutput("tableInfo"),
                             dataTableOutput("table")
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
  school_type <- student_loans %>% group_by(School) %>%
    filter(!is.na(School), !is.na(Recipients)) %>%
    group_by(School, `School Type`) %>% 
    summarize(average_recipients = mean(Recipients)) %>%
    arrange(desc(average_recipients))
  
  schooltype <- reactive ({
    school_type %>%
      filter(`School Type` %in% input$school_type)
  })
  
  output$table <- renderDataTable ({
    schooltype() 
  })
  
  output$tableInfo <- renderPrint ({
    num <- school_type %>%
      filter(`School Type` %in% input$school_type) %>%
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
