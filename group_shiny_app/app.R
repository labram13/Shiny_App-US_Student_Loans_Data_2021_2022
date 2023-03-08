#Initializing the libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(forcats)
library(viridis)
library(ggplot2)
<<<<<<< HEAD
install.packages("viridis")
#Load the data set into the R.
=======

>>>>>>> 61b24caedcb80b0ea820c50c491c98c6b73f7185
student_loans <- read_delim("../data/Student Loan Debt by School 2020-2021.csv")

#Defining the UI
ui <- fluidPage(
  tags$img(src = "https://honors.uw.edu/wp-content/uploads/2018/08/UW_Top_View.jpg",
           style = 'position: absolute; opacity: 0.2;',
           height="120%", width="120%", align="center"
  ),
  titlePanel("Student Loans in U.S. 2020-2021"),
  tabsetPanel(type = "tabs",
              tabPanel("Overview",
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
                             
                                             ("Ultimately, while student loans might enable access to higher education, they also have significant downsides that can long-term influence students' life.
                                             To limit the burden of debt, students must carefully assess the financial consequences of taking out loans and investigate alternate sources of income, such as grants and scholarships.
                                             This study examines data on student loan debt acquired by "),
                                             tags$a(href="https://www.kaggle.com/datasets/thedevastator/the-schools-that-create-the-most-student-debt?resource=download", "Andy Kriebel "),
                                             ("from various universities and colleges throughout the 2020-2021 academic year.
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
                tabPanel("Average Recipients by School Type",
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
              tabPanel("Average Loans by State",
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
              tabPanel("Average Loans by City",
                       sidebarLayout(
                         sidebarPanel(h3(strong("Average Loans per City: \n")),
                                      p(),
                                      p("Here, you are able to see the average loans students have taken out by city.\n"),
                                      p("To use this, first select the states that the cities you are planning to compare are located in. 
                                        Once you have selected the states, select the cities in the second text box. 
                                        The cities that appear in the text box are already filtered to be from the states you have selected."),
                                      selectizeInput("state", "Select State(s)", choices = unique(student_loans$State), multiple = TRUE),
                                      selectizeInput("city", "Select City(s)", choices = unique(student_loans$City), multiple = TRUE)
                         ),
                         mainPanel(
                           plotOutput("barplot")
                         )
                       )
              ),
              tabPanel("Conclusion",
                       mainPanel(HTML('<left><img src="https://www.commercebank.com/-/media/cb/articles/personal/2019/lg-header-lg-header-got-student-loans-understanding-repayment-options-desktop.png?revision=0dacb695-c1cc-4aa2-8f99-b5d4ad35dce1&modified=20200313180213", height = "320px"></left>'),
                                 fluidRow(
                                   column(12, offset = 2,
                                 HTML(
                                   paste0(
                                     h3(strong("Overall Conclusions")),'<br/>',
                                     p(style="text-align:justify;", "For various reasons, obtaining a student loan for a foreign school might be more complicated than for a public school.
                                         To begin, student loan qualifying rules differ across public and private colleges. 
                                         If you are a resident of the state where the school is situated, or if you attend a public university with a long-standing relationship with the lender, you may find it simpler to apply for a loan for a public school.
                                         Moreover, lenders may see loans to foreign schools as risky for reasons such as language hurdles, cultural differences, and a need for more understanding of the academic quality of the foreign institution. 
                                         Moreover, foreign colleges may not be recognized as qualifying institutions by the lender or the government, resulting in limited loan program alternatives. 
                                         Finally, the cost of attending a foreign school, including expenditures such as flight and lodging, may be greater, resulting in more significant loan amounts and tighter approval conditions. 
                                         In general, acquiring a student loan for a foreign school might be more challenging, necessitating more study and planning. 
                                         This may explain the need for entries for loans for private and foreign institutions. "), '<br/>',
                                     p("Many variables contribute to the East Coast having a higher frequency of student loans than the West Coast. 
                                        For starters, the cost of living on the East Coast is often higher, which includes more tuition and living expenditures. 
                                        Students may need additional loans to cover their education and living expenses. 
                                        Also, on the East Coast, some more prestigious universities and colleges may draw more students and have higher tuition prices.
                                        Because of the more significant population density on the East Coast, there are more students overall, which may lead to more student loans. 
                                        Third, because the employment market on the East Coast is often more competitive, more students may pursue higher education to improve their career chances. 
                                        While numerous variables likely contribute to the East Coast's more significant number of student loans, the higher cost of living and the more substantial number of top colleges are certainly vital causes.
                                      The data set used in this project can be somewhat credible and reliable, as Andy Kriebel collected it from various universities and colleges throughout the 2020-2021 academic year. It does not show any indication of harming any population groups, and its purpose is to provide information to help individuals make informed decisions about higher education and student loans. Therefore, while the data may have limitations, it still offers valuable insights into the impact of student loans on people. 
                                      The possible influence of the COVID-19 pandemic on student loan debt is one trend that may be extrapolated from this data set. The pandemic has caused tremendous economic disruption, with many people losing their employment or decreasing their income. This might lead to more students borrowing significant sums of money to pay for their education, exacerbating graduates' already substantial debt burden. On the other hand, the epidemic may result in more government initiatives to assist with student loan cancellation or relief, reducing the financial obligation many graduates suffer.")
                                   )
                                 )
                                   ),
                                 fluidRow(
                                 column(12, offset = 2,
                                        HTML('<left><img src="https://i.postimg.cc/zvZDc87c/image1.png", height = "500px"></left>'),
                                        HTML('<center><b>East Coast States</b></center>'),
                                        HTML('<right><img src="https://i.postimg.cc/Fs0Df46s/image2.png", height = "500px"></right>'),
                                        HTML('<center><b>West Coast States</b></center>'),
                                 )
                                 )
                       ) ,
              )
  )
)
)

  

          


server <- function(input, output, session) {
  #-------Megan's code---------
  
  # Finds the average amount of loan recipients per school
  avg_recipients <- student_loans %>% 
    group_by(School) %>%
    filter(!is.na(School), !is.na(Recipients)) %>%
    group_by(School, `School Type`) %>% 
    summarize(average_recipients = mean(Recipients)) %>%
    arrange(desc(average_recipients))
  
  # Filters the average amount of loan recipients by 
  # user's input of school type
  schooltype <- reactive ({
    avg_recipients %>%
      filter(`School Type` %in% input$school_type)
  })
  
  # Renders a data table that shows the average amount of loan
  # recipients of a school, with the data filtered out by the
  # reactive variable, schooltype
  output$table <- renderDataTable ({
    schooltype() 
  })
  
  # Message that shows how many schools there are that are displayed
  # in the data table
  output$tableInfo <- renderPrint ({
    num <- avg_recipients %>%
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
  # Store the selected cities
  selected_cities <- reactiveVal(NULL)
  
  # Filter the data based on the selected state and cities and averages the loans per
  # city
  filtered_data <- reactive({
    student_loans %>%
      filter(State %in% input$state) %>%
      group_by(City) %>%
      summarize(`$ of Disbursements` = mean(`$ of Disbursements`)) %>%
      filter(City %in% selected_cities())
  })
  
  # Update the choices for the city based on the selected state
  observeEvent(input$state, {
    choices <- unique(student_loans$City[student_loans$State %in% input$state])
    updateSelectizeInput(session, "city", choices = choices, server = TRUE)
    if (!is.null(selected_cities())) {
      updateSelectizeInput(session, "city", selected = selected_cities()[selected_cities() %in% choices])
    }
  })
  
  # Update the selected cities when the user changes the selection
  observeEvent(input$city, {
    selected_cities(input$city)
  })
  
  # Create the bar plot
  output$barplot <- renderPlot({
    ggplot(filtered_data(), aes(x = reorder(City, -`$ of Disbursements`), y = `$ of Disbursements`, fill = City)) +
      geom_bar(stat = "identity") +
      xlab("City") +
      ylab("Average Student Loan Disbursements") +
      ggtitle(paste("Average Student Loan Disbursements by City in", paste(input$state, collapse = ", "))) +
      scale_y_continuous(labels = scales::dollar_format(big.mark = ",", decimal.mark = ".", prefix = "$", suffix = "", accuracy = 1))
  })
<<<<<<< HEAD

  

=======
>>>>>>> 61b24caedcb80b0ea820c50c491c98c6b73f7185
}
#Start the Shiny App
shinyApp(ui = ui, server = server)
