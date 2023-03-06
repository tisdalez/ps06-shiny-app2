#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#C:/Users/AquaP/OneDrive/Desktop/Info201/ps06-shiny-app/ps06-shiny-app/app.R

# shiny app

library(shiny)

library(tidyverse)

crime <- read_delim("crime2.csv")

ui <- fluidPage(
  mainPanel(
    titlePanel("Seattle Police Crime Data"),
    tabsetPanel(
      tabPanel("About the data",
               h1("What is this data?"),
               p("This data is a collection of crime data from the ", strong("Seattle 
                 Police Department"), ", the data spans from 2008 and is still updated
                 to this day, although I retrieved the data on ", 
                 strong("February 15th"),", so it doesn't have any data after 
                 that point"),
               em("I am not sure about how recent the data needs to be."),
               h1("What does this application do?"),
               p("The plot plots a sample from 1000 to 100000 crime reports, of
               the ", nrow(crime), " reports in the dataset. The plot charts how
               many of the reports there are from each year, letting you
               get a good of the distribution. without  The table
                 lets the user look through ")),
      
      tabPanel("Plot",sidebarPanel(
        sliderInput("n", "How many reports of crime:",
                    min = 1000,
                    max = nrow(crime),
                    value = 10000),
        radioButtons("color", "Choose color",
                     choices = c("skyblue", "lawngreen", "orangered",
                                          "purple", "gold")),
        uiOutput("againstCategory")
      ),
        mainPanel(plotOutput("plot"),
                 textOutput("textSummary1")),
      ),
      tabPanel("Table",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("show_vars", "Columns in the Crime Data to show:",
                                      names(crime), selected = names(crime))
                 ),mainPanel(DT::dataTableOutput("table"))))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$againstCategory <- renderUI({
    checkboxGroupInput("against", "Who the crime was againt",
                       choices = unique(crime$`Crime Against Category`)
    )
  })
  
  
  output$plot <- renderPlot({
    crime1 <- crime %>% 
      sample_n(input$n) %>% 
      mutate(datech = substr(`Offense Start DateTime`,1,10)) %>% 
      mutate(date = as.Date(datech, format = "%m/%d/%Y")) %>% 
      filter(as.numeric(format(date,'%Y'))>2008) %>% 
      filter(`Crime Against Category` %in% input$against) %>% 
      select(date,`Offense ID`) %>%
      group_by(as.numeric(format(date,'%Y'))) %>% 
      na.omit() %>% 
      summarize(n = n_distinct(`Offense ID`)) 
    ggplot(crime1) +
      geom_line(aes(x =`as.numeric(format(date, "%Y"))`,y = n),col=input$color)+
      ggtitle("Amount of Crime reports per year from the sample")+
      xlab("Year") +
      ylab("Number of Reports that year")
  })

  
    output$table <- DT::renderDataTable({
      DT::datatable(crime[, input$show_vars, drop = FALSE])
  })
    
    output$textSummary1 <- renderText({
      paste("Currently you have selected ", as.character(input$n), " reports. In the reports that 
         and that is ", as.character(signif(input$n/nrow(crime) * 100),digits = 3),"% of the reports 
            in the dataset" 
            )
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
