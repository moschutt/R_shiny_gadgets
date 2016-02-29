library(shiny)
library(miniUI)
library(ggplot2)
library(tidyr)

chartGadget = function(data) {
  
  ui = miniPage(title="Exploritory Chart Gadget", 
                gadgetTitleBar("Exploritory Chart Gadget"),
                conditionalPanel('1 == 0', textInput(inputId='showToggle',
                                                     label = 'If you see this it b broken',
                                                     value = '1')),
                miniContentPanel(
                  conditionalPanel('input.showToggle == "1"',
                                   miniTitleBar('Chart Details', 
                                                right=miniTitleBarButton('detailHide', 
                                                                         'Hide'))
                  ),
                  conditionalPanel('input.showToggle == "0"',
                                   miniTitleBar('Chart Details', 
                                                right=miniTitleBarButton('detailShow', 
                                                                         'Show'))
                  ),
                  conditionalPanel('input.showToggle == "1"',
                                   wellPanel(
                                     radioButtons('chartType',
                                                  'Select Chart Type',
                                                  choices=c('Line'='line', 'Dot (Scatter)'='dot'),
                                                  inline=TRUE
                                     ),
                                     selectInput('xvar',
                                                 'Select X - var',
                                                 list()),
                                     checkboxGroupInput('yvar',
                                                        'Select Y - vars',
                                                        list(), 
                                                        inline=TRUE,
                                                        width='100%')
                                   )
                  ),
                  imageOutput('image1')
                )
  )
  
  server = function(input, output, session) {
    lvars = reactiveValues(xvar = '',
                           dvars = as.list(names(data)),
                           vnames = as.list(names(data)))
    
    output$image1 = renderPlot({
      if ( length(input$yvar) > 0 ) {
        pdata = data %>% gather_('Variable',
                                 'Value',
                                 input$yvar)
        
        g = ggplot(data = pdata,
                   aes_string(x=input$xvar,
                              y='Value',
                              colour='Variable')) 
        print(input$chartType)
        if ( input$chartType == 'dot' ) {
          g = g + geom_point()
        } else {
          g = g + geom_line()
        }
        g
      }
    })
    
    observe({
      updateSelectInput(session,
                        'xvar',
                        choices=c(list(''), as.list(names(data))))
    })
    
    # What to do when Done pressed
    observeEvent(input$done,
                 {stopApp("By!")}
    )
    
    observeEvent(input$detailHide, {updateTextInput(session, 'showToggle', value="0")})
    observeEvent(input$detailShow, {updateTextInput(session, 'showToggle', value="1")})
    
    observeEvent(input$xvar, {
      if( input$xvar != '' ) {
        lvars$xvar = input$xvar
        lvars$vnames = as.list(names(data))
        lvars$vnames[[which(lvars$vnames == input$xvar)]] = NULL
        updateCheckboxGroupInput(session,
                                 'yvar',
                                 choice=lvars$vnames,
                                 inline=TRUE)
      }
    })
  }
  
  runGadget(ui, server)
}

#chartGadget(cars)
#chartGadget(mtcars)
