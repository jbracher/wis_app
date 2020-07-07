library(shiny)

# Define UI
shinyUI(fluidPage(

  # Application title
  titlePanel("Evaluation of COVID19 death forecasts"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      uiOutput("inp_select_model"),
      selectInput("select_target", label = "Select target:", choices = paste(1:4, "wk ahead cum death")),
      uiOutput("inp_select_location")

    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot_forecast", height = "300px", brush = brushOpts(id = "brush_forecast", resetOnNew = TRUE),
                  dblclick = "dblclick_forecast"),
       plotOutput("plot_scores", height = "300px", brush = brushOpts("brush_scores", resetOnNew = TRUE), dblclick = "dblclick_scores"),
       plotOutput("plot_scores_summary", height = "300px")
    )
  )
))
