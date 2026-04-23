library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Linear Programming: The Profit Line"),
  sidebarLayout(
    sidebarPanel(
      helpText("1. Set your constraints:"),
      sliderInput("time_limit", "Time Constraint (Hours):", min = 4, max = 12, value = 8),
      sliderInput("space_limit", "Space Constraint (Batches):", min = 2, max = 8, value = 5),
      hr(),
      helpText("2. Slide the Profit Line to find the Max point:"),
      sliderInput("current_profit", "Target Profit ($):", min = 0, max = 200, value = 60, step = 10),
      hr(),
      helpText("Objective: Maximize P = 30x + 20y")
    ),
    
    mainPanel(
      plotOutput("feasiblePlot"),
      wellPanel(
        htmlOutput("solutionText")
      )
    )
  )
)

server <- function(input, output) {
  
  get_intersection <- reactive({
    x_int <- input$time_limit - input$space_limit
    y_int <- input$space_limit - x_int
    if(x_int < 0) return(c(0, input$space_limit))
    if(y_int < 0) return(c(input$time_limit/2, 0))
    return(c(x_int, y_int))
  })
  
  output$feasiblePlot <- renderPlot({
    inter <- get_intersection()
    
    # Feasible Region Polygon
    poly_data <- data.frame(
      x = c(0, min(input$time_limit/2, input$space_limit), inter[1], 0),
      y = c(0, 0, inter[2], min(input$time_limit, input$space_limit))
    )
    
    ggplot() +
      # Feasible Region
      geom_polygon(data = poly_data, aes(x=x, y=y), fill="skyblue", alpha=0.4) +
      # Constraint Lines
      geom_abline(intercept = input$time_limit, slope = -2, color = "red", size = 1, linetype="dashed") +
      geom_abline(intercept = input$space_limit, slope = -1, color = "blue", size = 1, linetype="dashed") +
      # THE PROFIT LINE (30x + 20y = P  =>  y = -1.5x + P/20)
      geom_abline(intercept = input$current_profit / 20, slope = -1.5, color = "darkgreen", size = 2) +
      # Labeling
      scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
      scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
      labs(title = "Sliding the Profit Line",
           subtitle = "Green Line: Current Profit Level | Red/Blue: Constraints",
           x = "Cupcake Batches (x)", y = "Cookie Batches (y)") +
      theme_minimal()
  })
  
  output$solutionText <- renderUI({
    inter <- get_intersection()
    max_p <- 30 * inter[1] + 20 * inter[2]
    
    status_msg <- if(input$current_profit > max_p) {
      "<span style='color:red;'><b>Over Limit:</b> This profit is not possible within the constraints!</span>"
    } else if (input$current_profit == max_p) {
      "<span style='color:green;'><b>Optimal!</b> You've reached the maximum possible profit.</span>"
    } else {
      "<span>Increase the Target Profit to find the edge of the feasible region.</span>"
    }
    
    HTML(paste0("<b>Current Target Profit:</b> $", input$current_profit, "<br/>",
                "<b>Max Possible Profit:</b> $", max_p, "<br/>",
                status_msg))
  })
}

shinyApp(ui = ui, server = server)