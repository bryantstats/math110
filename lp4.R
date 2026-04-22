library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Linear Programming: Fixing the Shaded Region"),
  sidebarLayout(
    sidebarPanel(
      helpText("1. Primary Constraints:"),
      checkboxInput("show_time", "Enable Time (2x + y <= L)", value = TRUE),
      sliderInput("time_limit", "Time Limit:", min = 4, max = 12, value = 8),
      
      checkboxInput("show_space", "Enable Space (x + y <= L)", value = TRUE),
      sliderInput("space_limit", "Space Limit:", min = 2, max = 8, value = 5),
      hr(),
      
      helpText("2. Toggle Non-Negativity:"),
      checkboxInput("constrain_x", "Enforce x >= 0", value = TRUE),
      checkboxInput("constrain_y", "Enforce y >= 0", value = TRUE),
      hr(),
      
      helpText("3. Objective: Maximize P = 30x + 20y"),
      sliderInput("current_profit", "Target Profit ($):", min = 0, max = 200, value = 60, step = 10)
    ),
    
    mainPanel(
      plotOutput("feasiblePlot"),
      wellPanel(htmlOutput("solutionText"))
    )
  )
)

server <- function(input, output) {
  
  get_feasible_poly <- reactive({
    # 1. Create a grid that covers the full viewable area (including negative)
    # Using a higher resolution grid to ensure smooth shading
    grid <- expand.grid(
      x = seq(-5, 12, length.out = 80), 
      y = seq(-5, 12, length.out = 80)
    )
    
    feasible <- grid
    
    # 2. Filter based on toggles
    if(input$show_time)      feasible <- feasible %>% filter(2*x + y <= input$time_limit)
    if(input$show_space)     feasible <- feasible %>% filter(x + y <= input$space_limit)
    if(input$constrain_x)    feasible <- feasible %>% filter(x >= 0)
    if(input$constrain_y)    feasible <- feasible %>% filter(y >= 0)
    
    # 3. Always bound the grid to the 'viewable' window so the polygon has a shape
    feasible <- feasible %>% filter(x >= -5, x <= 10, y >= -5, y <= 10)
    
    if(nrow(feasible) < 3) return(NULL)
    
    # 4. Use Convex Hull to find the boundary points for geom_polygon
    feasible[chull(feasible$x, feasible$y), ]
  })

  output$feasiblePlot <- renderPlot({
    p <- ggplot() +
      # Fix the coordinate system so the 'shadow' doesn't disappear at the edges
      coord_cartesian(xlim = c(-2, 10), ylim = c(-2, 10)) +
      labs(title = "Feasible Region Shading",
           subtitle = "Toggle x >= 0 or y >= 0 to see the area expand into negative quadrants",
           x = "x (Cupcakes)", y = "y (Cookies)") +
      theme_minimal()
    
    # Draw the shadow (the polygon)
    poly <- get_feasible_poly()
    if(!is.null(poly)) {
      p <- p + geom_polygon(data = poly, aes(x=x, y=y), fill="skyblue", alpha=0.5)
    }
    
    # Constraint Lines
    if(input$show_time) {
      p <- p + geom_abline(intercept = input$time_limit, slope = -2, color = "red", linetype="dashed", size = 1)
    }
    if(input$show_space) {
      p <- p + geom_abline(intercept = input$space_limit, slope = -1, color = "blue", linetype="dashed", size = 1)
    }
    
    # Axis Lines (Black lines show if non-negativity is 'ON')
    if(input$constrain_x) p <- p + geom_vline(xintercept = 0, color = "black", size = 1.2)
    if(input$constrain_y) p <- p + geom_hline(yintercept = 0, color = "black", size = 1.2)
    
    # Target Profit Line
    p <- p + geom_abline(intercept = input$current_profit / 20, slope = -1.5, color = "darkgreen", size = 1.5)
    
    p
  })
  
  output$solutionText <- renderUI({
    poly <- get_feasible_poly()
    if(is.null(poly)) {
      max_p <- "Undefined"
    } else {
      max_p <- paste0("$", round(max(30 * poly$x + 20 * poly$y), 2))
    }
    
    HTML(paste0("<b>Max Profit in Shaded Area:</b> ", max_p))
  })
}

shinyApp(ui = ui, server = server)