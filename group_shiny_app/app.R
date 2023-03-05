library(shiny)

ui <- fluidPage(
    titlePanel("Student Loans 2020-2021"),
    tabsetPanel(type = "tabs",
                tabPanel("General Information",
                         sidebarLayout(
                           sidebarPanel (
                           ),
                           mainPanel(
                           )
                         )
                ),
                tabPanel("Plot Data",
                         sidebarLayout(
                           sidebarPanel(
                           ),
                           mainPanel(
                           )
                         )
                ),
                tabPanel("Summary",
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
