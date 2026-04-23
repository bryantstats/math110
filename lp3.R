library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Linear Programming: Minimizing Athlete Costs"),
  sidebarLayout(
    sidebarPanel(
      helpText("1. Adjust Nutrient Requirements (Constraints):"),
      sliderInput("min_protein", "Min Protein Required:", min = 2, max = 10, value = 5),
      sliderInput("min_energy", "Min Energy Required:", min = 5, max = 15, value = 10),
      hr(),
      helpText("2. Slide the Cost Line to find the Min point:"),
      sliderInput("current_cost", "Target Cost ($):", min = 5, max = 40, value = 20, step = 1),
      hr(),
      helpText("Objective: Minimize C = 4x + 3y")
    ),
    
    mainPanel(
      plotOutput("feasiblePlot", height = "500px"),
      wellPanel(
        htmlOutput("solutionText")
      )
    )
  )
)

server <- function(input, output) {
  
  get_vertices <- reactive({
    # Intersection of 2x + y = P and x + 3y = E
    A <- matrix(c(2, 1, 1, 3), 2, 2)
    B <- c(input$min_protein, input$min_energy)
    inter <- tryCatch(solve(A, B), error = function(e) c(NA, NA))
    
    # Boundary vertices for the unbounded region
    v_y_axis <- c(0, max(input$min_protein, input$min_energy/3)) 
    v_x_axis <- c(max(input$min_protein/2, input$min_energy), 0) 
    v_crossing <- inter 
    
    df <- data.frame(
      x = c(v_y_axis[1], v_crossing[1], v_x_axis[1]),
      y = c(v_y_axis[2], v_crossing[2], v_x_axis[2])
    ) %>% mutate(Cost = 4 * x + 3 * y) %>% filter(!is.na(x))
    
    return(df)
  })
  
  output$feasiblePlot <- renderPlot({
    df <- get_vertices()
    
    # Define a LARGE poly region (20x20) that is conceptually unbounded 'up and to the right'
    # We start at the top y-intercept (v_y_axis), go through the crossing point,
    # hit the x-intercept (v_x_axis), then go to (20, 0), (20, 20), (0, 20) and back.
    poly_data <- data.frame(
      x = c(0, df$x, 20, 20, 0),
      y = c(20, df$y, 0, 20, 20)
    )
    
    ggplot() +
      # Shaded 'Safe/Nutritious' Zone (must be first so it is 'behind' the lines)
      geom_polygon(data = poly_data, aes(x=x, y=y), fill="orange", alpha=0.3) +
      
      # Constraint Lines (dashed to show they are limits)
      geom_abline(intercept = input$min_protein, slope = -2, color = "red", size = 1, linetype="dashed") +
      geom_abline(intercept = input$min_energy/3, slope = -1/3, color = "blue", size = 1, linetype="dashed") +
      
      # THE COST LINE (4x + 3y = C => y = -1.33x + C/3)
      geom_abline(intercept = input$current_cost / 3, slope = -4/3, color = "darkgreen", size = 2) +
      
      # Plot formatting
      coord_cartesian(xlim = c(0, 15), ylim = c(0, 15)) +
      scale_x_continuous(breaks = 0:15) +
      scale_y_continuous(breaks = 0:15) +
      labs(title = "Sliding the Cost Line",
           subtitle = "Green: Current Budget | Orange Region: Feasible (Meet Nutritious Goals)",
           x = "Protein Shakes (x)", y = "Energy Bars (y)") +
      theme_minimal()
  })
  
  output$solutionText <- renderUI({
    df <- get_vertices()
    min_cost <- min(df$Cost)
    
    # Tolerance check for 'optimal' status
    status_msg <- if(input$current_cost < min_cost - 0.1) {
      "<span style='color:red;'><b>Under Budget:</b> This spending doesn't meet requirements. Move the cost line up!</span>"
    } else if (abs(input$current_cost - min_cost) < 0.5) {
      "<span style='color:green;'><b>Optimal!</b> You've reached the lowest possible cost.</span>"
    } else {
      "<span>Lower the Target Cost to find the minimum of the orange region.</span>"
    }
    
    HTML(paste0("<b>Current Target Cost:</b> $", input$current_cost, "<br/>",
                "<b>Minimum Possible Cost:</b> $", round(min_cost, 2), "<br/>",
                status_msg))
  })
}

shinyApp(ui = ui, server = server)