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
                                                                   selected='None',
                                                                   inline=TRUE))
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
    lvars = reactiveValues(vnames = as.list(names(data)),
                           ddata = data.frame())
    
    my_gather = function(data) {
      print(names(data))
      print(input$xvar)
      print(input$yvar)
      data[,c(input$xvar, input$yvar)] %>% 
        gather_('Variable',
                'Value',
                input$yvar)
    }
    
    agg_data = function(d) {
      if ( input$aggregate == 'day' ) {
        d[,input$xvar] = as.Date(d[,input$xvar])
        return (aggregate(as.formula(paste('Value ~', 
                                           input$xvar,
                                           '+ Variable')),
                          d, 
                          mean))
      } else if ( input$aggregate == 'none' ) {
        return(data %>% my_gather)
      }
      else {
        return(d)
      }
    }
    
    output$image1 = renderPlot({
      if ( length(input$yvar) > 0 ) {
        g = ggplot(data = lvars$ddata,
                   aes_string(x=input$xvar,
                              y='Value',
                              colour='Variable')) +
          theme_tufte()
        
        if ( input$chartType == 'dot' ) {
          g = g + geom_point()
        } else {
          g = g + geom_line()
        }
        g
      }
    })
    
    observeEvent(input$aggregate, {
      if ( input$xvar != '' && length(input$yvar) > 0 ) {
        lvars$ddata = lvars$ddata %>% agg_data
      }
    })
    
    observeEvent(input$chart_brush, {
      lvars$ddata = brushedPoints(lvars$ddata,
                                  input$chart_brush)
    })
    
    observeEvent(input$chart_dblclick, {
      lvars$ddata = data %>% my_gather %>% agg_data
    })
    
    # What to do when Done pressed
    observeEvent(input$done,
                 {stopApp(lvars$ddata %>% spread(key='Variable', value='Value'))}
    )
    
    observeEvent(input$detailHide, {updateTextInput(session, 'showToggle', value="0")})
    observeEvent(input$detailShow, {updateTextInput(session, 'showToggle', value="1")})
    
    observeEvent(input$yvar, {
      lvars$ddata = data %>% my_gather
    })
    
    observeEvent(input$xvar, {
      if( input$xvar != '' ) {
        
        if ( 'POSIXct' %in% class(data[,input$xvar]) ) {
          updateTextInput(session, 
                          'xDateTime', 
                          value = '1')
        }
        
        lvars$vnames = as.list(names(data))
        lvars$vnames[[which(lvars$vnames == input$xvar)]] = NULL
        updateCheckboxGroupInput(session,
                                 'yvar',
                                 choice=lvars$vnames,
                                 inline=TRUE)
      }
      

    })
    
    observe({
    updateSelectInput(session,
                      'xvar',
                      choices=c(list(''), as.list(names(data))))
    })
  }
  
  runGadget(ui, server)
}
