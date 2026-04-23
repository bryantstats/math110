library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("The Linear Programming Lab"),
  
  sidebarLayout(
    sidebarPanel(
      # Toggle for Optimization Goal
      radioButtons("goal", "Optimization Goal:",
                   choices = list("Maximize" = "max", "Minimize" = "min"), 
                   selected = "max"),
      hr(),
      
      # Objective Function Coefficients
      h4("Objective: P = Ax + By"),
      numericInput("obj_a", "Coefficient A (x):", 30, min = 1),
      numericInput("obj_b", "Coefficient B (y):", 20, min = 1),
      hr(),
      
      # Constraint 1: c1_x + c1_y <= or >= rhs1
      h4("Constraint 1"),
      fluidRow(
        column(4, numericInput("c1_x", "x coeff", 2)),
        column(4, numericInput("c1_y", "y coeff", 1)),
        column(4, numericInput("rhs1", "Limit", 8))
      ),
      
      # Constraint 2: c2_x + c2_y <= or >= rhs2
      h4("Constraint 2"),
      fluidRow(
        column(4, numericInput("c2_x", "x coeff", 1)),
        column(4, numericInput("c2_y", "y coeff", 1)),
        column(4, numericInput("rhs2", "Limit", 5))
      ),
      hr(),
      sliderInput("profit_level", "Slide Profit/Cost Line:", min = 0, max = 300, value = 50)
    ),
    
    mainPanel(
      plotOutput("lpPlot", height = "500px"),
      wellPanel(
        h4("Vertex Analysis"),
        tableOutput("resultsTable")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive function to calculate intersections and vertices
  calc_data <- reactive({
    # Intersection of C1 and C2
    # c1x + c1y = r1
    # c2x + c2y = r2
    det <- (input$c1_x * input$c1_y) - (input$c1_y * input$c2_x) # simplified for demo
    # Solving using Cramer's rule or simple substitution
    # For a robust app, we'd use matrix inversion:
    A <- matrix(c(input$c1_x, input$c2_x, input$c1_y, input$c2_y), 2, 2)
    B <- c(input$rhs1, input$rhs2)
    
    inter <- tryCatch(solve(A, B), error = function(e) c(NA, NA))
    
    # Define candidates for vertices
    candidates <- data.frame(
      x = c(0, 0, input$rhs1/input$c1_x, input$rhs2/input$c2_x, inter[1]),
      y = c(0, input$rhs1/input$c1_y, 0, 0, inter[2])
    ) %>% filter(!is.na(x) & x >= 0 & y >= 0 & is.finite(x) & is.finite(y))
    
    # Filter for feasibility
    if(input$goal == "max") {
      feasible <- candidates %>%
        filter(input$c1_x * x + input$c1_y * y <= input$rhs1 + 0.001,
               input$c2_x * x + input$c2_y * y <= input$rhs2 + 0.001)
    } else {
      feasible <- candidates %>%
        filter(input$c1_x * x + input$c1_y * y >= input$rhs1 - 0.001,
               input$c2_x * x + input$c2_y * y >= input$rhs2 - 0.001)
    }
    
    feasible %>% mutate(Value = input$obj_a * x + input$obj_b * y) %>% distinct()
  })
  
  output$lpPlot <- renderPlot({
    df <- calc_data()
    
    p <- ggplot() +
      # Constraint Lines
      geom_abline(intercept = input$rhs1/input$c1_y, slope = -input$c1_x/input$c1_y, color="red", size=1) +
      geom_abline(intercept = input$rhs2/input$c2_y, slope = -input$c2_x/input$c2_y, color="blue", size=1) +
      # Objective Line: Ax + By = V -> y = -A/B x + V/B
      geom_abline(intercept = input$profit_level/input$obj_b, slope = -input$obj_a/input$obj_b, 
                  color="darkgreen", size=2, alpha=0.6) +
      coord_cartesian(xlim = c(0, 15), ylim = c(0, 15)) +
      theme_minimal() +
      labs(title = paste("Optimization Lab:", ifelse(input$goal=="max", "Maximization", "Minimization")),
           subtitle = "Green Line: Objective | Red/Blue: Constraints",
           x = "Variable X", y = "Variable Y")
    
    # Add feasible region polygon if possible
    if(nrow(df) >= 3 && input$goal == "max") {
      p <- p + geom_polygon(data = df[chull(df$x, df$y),], aes(x=x, y=y), fill="green", alpha=0.2)
    }
    
    p + geom_point(data = df, aes(x=x, y=y), size=3)
  })
  
  output$resultsTable <- renderTable({
    calc_data() %>% arrange(if(input$goal=="max") desc(Value) else Value)
  })
}

shinyApp(ui, server)