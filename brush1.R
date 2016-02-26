library(shiny)
library(miniUI)
library(ggplot2)

ggbrush <- function(data, xvar, yvar) {
  
  ui <- miniPage(
    gadgetTitleBar("Drag to select points"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      plotOutput("show_plot", height="80%"),
      plotOutput("plot", height = "20%", brush = brushOpts(id="brush",
                                                            direction="x"))
    )
  )
  
  server <- function(input, output, session) {
    
    values = reactiveValues(data = data)
    
    # Render the plot
    output$plot <- renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      ggplot(data, aes_string(xvar, yvar)) + geom_point() +
        scale_x_continuous(expand=c(0,0)) + 
        scale_y_continuous(expand=c(0,0)) +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
    })
    
    output$show_plot = renderPlot({
      ggplot(values$data, aes_string(xvar, yvar)) + geom_point()
    })
    observeEvent(input$brush, {
      print(str(input$brush))
      values$data = brushedPoints(data, input$brush)
    })
    
    observeEvent(input$dblClick, {
      values$data = data
    })
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(data, input$brush))
    })
  }
  
  runGadget(ui, server)
}

ggbrush(cars, "speed", "dist")