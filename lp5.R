library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Linear Programming: Profit Optimization"),
  sidebarLayout(
    sidebarPanel(
      helpText("1. Set Constraints:"),
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
      sliderInput("current_profit", "Target Profit ($):", min = 0, max = 250, value = 60, step = 5)
    ),
    
    mainPanel(
      plotOutput("feasiblePlot"),
      # This wellPanel will now notify you when you hit the max
      wellPanel(
        style = "background-color: #f8f9fa; border: 2px solid #dee2e6;",
        htmlOutput("solutionText")
      )
    )
  )
)

server <- function(input, output) {
  
  # Calculate exact vertices for the shaded region
  get_vertices <- reactive({
    lines <- list()
    if(input$show_time)      lines[[length(lines)+1]] <- c(a=2, b=1, c=input$time_limit)
    if(input$show_space)     lines[[length(lines)+1]] <- c(a=1, b=1, c=input$space_limit)
    if(input$constrain_x)    lines[[length(lines)+1]] <- c(a=1, b=0, c=0)
    if(input$constrain_y)    lines[[length(lines)+1]] <- c(a=0, b=1, c=0)
    
    # Boundary constraints for the plot window
    lines[[length(lines)+1]] <- c(a=1, b=0, c=-2); lines[[length(lines)+1]] <- c(a=1, b=0, c=10)
    lines[[length(lines)+1]] <- c(a=0, b=1, c=-2); lines[[length(lines)+1]] <- c(a=0, b=1, c=10)
    
    pts <- data.frame(x=numeric(), y=numeric())
    for(i in 1:(length(lines)-1)) {
      for(j in (i+1):length(lines)) {
        L1 <- lines[[i]]; L2 <- lines[[j]]
        det <- L1['a']*L2['b'] - L1['b']*L2['a']
        if(det != 0) {
          pts <- rbind(pts, data.frame(
            x = (L1['c']*L2['b'] - L1['b']*L2['c']) / det,
            y = (L1['a']*L2['c'] - L1['c']*L2['a']) / det
          ))
        }
      }
    }
    
    valid_pts <- pts
    if(input$show_time)   valid_pts <- valid_pts %>% filter(2*x + y <= input$time_limit + 1e-7)
    if(input$show_space)  valid_pts <- valid_pts %>% filter(x + y <= input$space_limit + 1e-7)
    if(input$constrain_x) valid_pts <- valid_pts %>% filter(x >= -1e-7)
    if(input$constrain_y) valid_pts <- valid_pts %>% filter(y >= -1e-7)
    
    valid_pts <- valid_pts %>% filter(x >= -2.01, x <= 10.01, y >= -2.01, y <= 10.01)
    if(nrow(valid_pts) < 3) return(NULL)
    valid_pts[chull(valid_pts$x, valid_pts$y), ]
  })

  output$feasiblePlot <- renderPlot({
    p <- ggplot() +
      coord_cartesian(xlim = c(-2, 10), ylim = c(-2, 10)) +
      theme_minimal() +
      labs(title = "Profit Line & Feasible Region", x = "x", y = "y")
    
    verts <- get_vertices()
    if(!is.null(verts)) {
      p <- p + geom_polygon(data = verts, aes(x=x, y=y), fill="skyblue", alpha=0.6)
    }
    
    if(input$show_time) p <- p + geom_abline(intercept = input$time_limit, slope = -2, color = "red", linetype="dashed")
    if(input$show_space) p <- p + geom_abline(intercept = input$space_limit, slope = -1, color = "blue", linetype="dashed")
    if(input$constrain_x) p <- p + geom_vline(xintercept = 0, color = "black", size = 1)
    if(input$constrain_y) p <- p + geom_hline(yintercept = 0, color = "black", size = 1)
    
    p + geom_abline(intercept = input$current_profit / 20, slope = -1.5, color = "darkgreen", size = 1.5)
  })
  
  output$solutionText <- renderUI({
    verts <- get_vertices()
    if(is.null(verts)) return(HTML("<b style='color:orange;'>No valid region found.</b>"))
    
    # Calculate Max Profit
    max_p <- max(30 * verts$x + 20 * verts$y)
    
    # Notification Logic
    if (abs(input$current_profit - max_p) < 0.1) {
      msg <- "<h3 style='color:green; margin:0;'>✨ MAXIMUM PROFIT REACHED! ✨</h3>
              <p>The profit line is at the optimal vertex.</p>"
    } else if (input$current_profit > max_p) {
      msg <- "<h3 style='color:red; margin:0;'>⚠️ INFEASIBLE</h3>
              <p>Target profit is outside the shaded region.</p>"
    } else {
      msg <- "<h3 style='color:black; margin:0;'>Searching...</h3>
              <p>Keep sliding to find the maximum possible profit.</p>"
    }
    
    HTML(paste0(msg, "<b>Target Profit:</b> $", input$current_profit, "<br/>",
                "<b>Theoretical Max:</b> $", round(max_p, 2)))
  })
}

shinyApp(ui = ui, server = server)