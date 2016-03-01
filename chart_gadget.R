library(shiny)
library(miniUI)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)

chartGadget = function(data) {
  
  ui = miniPage(title="Exploritory Chart Gadget", 
                gadgetTitleBar("Exploritory Chart Gadget"),
                conditionalPanel('1 == 0', 
                                 textInput(inputId='showToggle',
                                           label = 'If you see this it b broken',
                                           value = '1'),
                                 textInput(inputId='xDateTime',
                                           label='If you see this it is broken',
                                           value = '0')),
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
                                                        width='100%'),
                                     conditionalPanel('input.xDateTime == "1"',
                                                      radioButtons('aggregate',
                                                                   'Aggregate by:',
                                                                   c('None' = 'none',
                                                                     'Day' = 'day',
                                                                     'Hour' = 'hour'),
                                                                   selected='None'))
                                   )
                  ),
                  imageOutput('image1',
                              brush=brushOpts(id="chart_brush",
                                              direction=c("x"),
                                              resetOnNew = TRUE),
                              dblclick = dblclickOpts(id='chart_dblclick')
                  )
                )
  )
  
  server = function(input, output, session) {
    lvars = reactiveValues(xvar = '',
                           dvars = as.list(names(data)),
                           vnames = as.list(names(data)),
                           ddata = data)
    
    agg_data = function(data,
                        direction='long') {
      data = data %>% 
        gather_('Variable',
                'Value',
                input$yvar) 
      
      if ( input$aggregate == 'day' ) {
        
        data = aggregate(as.formula(paste('Value ~', 
                                          input$xvar,
                                          '+ Variable')),
                         data)
        
      }
      if ( direction != 'long' ) {
        return(spread_(data, 'Variable', 'Value'))
      } else {
        data
      }
    }
    output$image1 = renderPlot({
      if ( length(input$yvar) > 0 ) {
        pdata = lvars$ddata %>% agg_data
        
        g = ggplot(data = pdata,
                   aes_string(x=input$xvar,
                              y='Value',
                              colour='Variable')) +
          theme_tufte()
        
        print(input$chartType)
        if ( input$chartType == 'dot' ) {
          g = g + geom_point()
        } else {
          g = g + geom_line()
        }
        g
      }
    })
    
    observeEvent(input$chart_brush, {
      lvars$ddata = brushedPoints(lvars$ddata,
                                  input$chart_brush)
    })
    
    observeEvent(input$chart_dblclick, {
      lvars$ddata = data
    })
    
    observe({
      updateSelectInput(session,
                        'xvar',
                        choices=c(list(''), as.list(names(data))))
    })
    
    # What to do when Done pressed
    observeEvent(input$done,
                 {stopApp(lvars$ddata %>% agg_data(direction='wide'))}
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
