#' make a quick shiny app to explore changing a function's parameters
#'
#' @param call A character string, or a list of strings, each that evaluates to a call to a plotting function
#' @param slider_params A list of vectors providing the ranges for each parameter to make a slider for
#' @param header A character string for the app's title
#' @return None.
#' @examples
#' slider_params = list(A = c(1,0,2.5),B = c('a','b','c'))
#' call = function(A,B) {
#'   plot(NA,NA,xlim = c(-1,1),ylim = c(-1,1),main = B)
#'   symbol(0,0,circle = A,add=T,inches=F)
#' }

run_shiny = function(call,slider_params,header='',...){
  # call: a (list) of functions that takes as arguments the names of slider_params, plus additional variables in the workspace
  # slider_params: a list of variables and their ranges or a starting value plus a range.
    # a) 3 elements: c(value,min,max)
    # b) 2 elements: c(min,max)
  # header: The title of the Shiny App

  # parse the slider_params list. In particular, check that numeric elements have a range.
    # If only two elements given, initialize value at their mean.
  slider_params = lapply(slider_params,function(x) {
    if(is.numeric(x) == F) return(x)
    if(length(x) == 3) return(x)
    x = c(mean(x),x)
    return(x)
  })

  # assign a name to the plot if none given.
  if(length(call) == 1) names(call)[1] = 'plot_1'

  runApp(list(
    # layout of the app
    ui = shinyUI(fluidPage(
      headerPanel(header),
      fluidRow(
        column(2,
               tabPanel("Parameters",
                        lapply(names(slider_params),function(param) {
                          if(is.numeric(slider_params[[param]])){
                            sliderInput( param,
                                         param,
                                         min = slider_params[[param]][2],
                                         max = slider_params[[param]][3],
                                         value = slider_params[[param]][[1]]
                                        )
                          } else{
                            selectInput( param,
                                         param,
                                         choices = slider_params[[param]]
                                      )
                          }
                        })
               ),
               tabPanel('Graphics',
                        sliderInput( 'plotHeight',
                                     'plotHeight',
                                     min = 200,
                                     max = 2000,
                                     value = 800
                                  ),
                        sliderInput( 'cex_axes',
                                     'cex_axes',
                                     min = 0.01,
                                     max = 5,
                                     value = 1
                                    )
               )
        ),
        column(1,
               radioButtons('Plot','Plot',names(call))
        ),
        column(10,
               uiOutput('current_plot.ui')
        )
      )
    )),
    # plotting commands
    server = shinyServer(function(input, output) {
      plotHeight = reactive({input$plotHeight})

      output$plot_fun = renderPlot({
        current_call = call[[input$Plot]]
        with(reactiveValuesToList(input),{
          eval(parse(text=current_call))
        })
      })
      output$current_plot.ui = renderUI({
        plotOutput('plot_fun',height = plotHeight())
      })
    })
  ))
}


