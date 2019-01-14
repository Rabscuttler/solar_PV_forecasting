library(ggplot2)
library(shiny)
library(tidyverse)
library(lubridate)
library(ggforce)

# UI
ui <- fluidPage(
  titlePanel("Predictions for SunLab Faro Solar PV Site"),
  # Sidebar layout with panel
  sidebarLayout(
    sidebarPanel(
      # Total date range to show
      dateRangeInput(inputId="dates", label="Select date:", 
                     start=as_date("2017-10-01"), end=as_date("2017-10-14"),
                     min=as_date("2017-10-01"), max=as_date("2017-11-30")),
      # Facet zoom to show
      dateRangeInput(inputId="zoom", label="Detail view:", 
                     start=as_date("2017-10-01"), end=as_date("2017-10-3"),
                    min=as_date("2017-10-01"), max=as_date("2017-11-30")),
      textOutput("DateRange"),
      # Choose prediction data to show
      selectInput(inputId="model", label="Choose a predictive model", 
                  # choices=c("min", "fst", "ln", "svr", "nn"),
                  choices=c("Ridge Regression (min)"="min", "Ridge Regression (1se)"="fst",
                  "Linear Regression"="ln", "SVR"="svr", "ANN"="nn"), 
                  selected=F, multiple=F)
    ),
    mainPanel(
      plotOutput(outputId = "zoom_plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  results <- read_csv("sunlab_prediction.csv")
  labels = c("Ridge Regression (min)", "Ridge Regression (1se)", "Linear Regression", "SVR", "ANN")
  
  # Date choosing validation block
  output$DateRange <- renderText({
    validate(need(input$dates[2] > input$dates[1], "Please select an end date after the start date."))
    validate(need(difftime(input$dates[2], input$dates[1], "days") > 2, "Date range must be more than 2 days"))
    paste("Viewing", difftime(input$dates[2], input$dates[1], units="days"),"days \n")
  })
  
  # reactive renderPlot to show charts
  output$zoom_plot <- renderPlot({
    to_plot <- filter(results, Datetime> input$dates[1] & Datetime < input$dates[2])
    ggplot(to_plot) + geom_line(aes(x=Datetime, y=A_Optimal...Power.DC..W.), alpha=0.7) +
      facet_zoom(x = Datetime > input$zoom[1] & Datetime < input$zoom[2], horizontal = FALSE, zoom.size = 0.6) +
      geom_line(aes_string(x="Datetime", y=input$model), linetype="dotted", colour="red") +
      scale_color_manual(name="Data", values=c("black", "red"), labels=c("Actual", input$model)) +
      scale_linetype_manual(name="Data", values=c("solid", "dotted"), labels=c("Actual", input$model)) +
      labs(x="Date, 2017", y="Power (W)") + theme_bw() + theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)