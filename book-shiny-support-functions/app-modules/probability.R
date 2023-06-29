
##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

probabilityUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      
      # Application title
      titlePanel("Sannolikhetsfördelningar"),
      withMathJax(),
      tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], processEscapes: true}});"), 
      tags$head(tags$style(
        type = 'text/css',
        # The wellPanel() in mainPanel() creates a div.well that allows for changes in the structure
        'div.well { max-height: 60em; overflow-y: auto; }'
      )),   
      
      
      # Sidebar with a slider input means for the individual groups
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "distribution" %>% ns(),
            label = HTML("Ange en fördelning:"),
            choices = 
              c(
                "Normal" = "norm",
                "t" = "t",
                "Binomial" = "binom",
                "Chi-2" = "chisq"
              )
          ),
          uiOutput(
            ns("distrAttributes")
          ),
          radioButtons(
            inputId = ns("valueOrProb"),
            label = HTML("Beräkning av värde eller sannolikhet"),
            choices = c("Värde", "Sannolikhet")
          ),
          uiOutput(
            ns("probValues")
          ),
          actionButton(
            ns("go"),
            label = HTML("Beräkna från fördelningen"),
            width = "100%"
          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          wellPanel(
            style = "background: white; border: white",
            h2("Visualisering av ett slumpmässigt urval"),
            uiOutput(ns("introduction")),
            withSpinner(plotOutput(ns("plots"))),
            uiOutput(ns("variances"))
          )
          
          
        )
      )
    )
  )
}

## Backend for vizualizations
probabilitySERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){

      valueX <- reactive({
        if(input$valueOrProb == "Värde"){
          functionCall <-
            parse(
              text =
                paste0(
                  "q",
                  input$distribution,
                  "(p = ",
                  input$valueProb,
                  if_else(
                    input$distribution == "norm",
                    paste0(
                      ", mean = ",
                      input$normMean,
                      ", sd = ",
                      input$normSd,
                      ", lower.tail = ",
                      if_else(input$valueSide %in% c("lt", "le"), "TRUE", "FALSE"),
                      ")"
                    ),
                    if_else(
                      input$distribution == "t",
                      paste0(
                        ", df = ",
                        input$tDf,
                        ", lower.tail = ",
                        if_else(input$valueSide %in% c("lt", "le"), "TRUE", "FALSE"),
                        ") * ",
                        input$tSd,
                        " + ",
                        input$tMean
                      ),
                      if_else(
                        input$distribution == "binom",
                        paste0(
                          ", size = ",
                          input$binomSize,
                          ", prob = ",
                          input$binomProb,
                          ", lower.tail = ",
                          if_else(input$valueSide %in% c("lt", "le"), "TRUE", "FALSE"),
                          ")"
                        ),
                        # Chi-square
                        paste0(
                          ", df = ",
                          input$chisqDf,
                          ", lower.tail = ",
                          if_else(input$valueSide %in% c("lt", "le"), "TRUE", "FALSE"),
                          ")"
                        )
                      )
                    )
                  )
                )
            )
          
          value <- eval(functionCall)
        } else {
          functionCall <-
            parse(
              text =
                paste0(
                  "p",
                  input$distribution,
                  "(q = ",
                  input$valueX,
                  if_else(
                    input$distribution == "norm",
                    paste0(
                      ", mean = ",
                      input$normMean,
                      ", sd = ",
                      input$normSd,
                      ", lower.tail = ",
                      if_else(input$valueSide %in% c("lt", "le"), "TRUE", "FALSE"),
                      ")"
                    ),
                    if_else(
                      input$distribution == "t",
                      paste0(
                        ", df = ",
                        input$tDf,
                        ", lower.tail = ",
                        if_else(input$valueSide %in% c("lt", "le"), "TRUE", "FALSE"),
                        ") * ",
                        input$tSd,
                        " + ",
                        input$tMean
                      ),
                      if_else(
                        input$distribution == "binom",
                        paste0(
                          ", size = ",
                          input$binomSize,
                          ", prob = ",
                          input$binomProb,
                          ", lower.tail = ",
                          if_else(input$valueSide %in% c("lt", "le"), "TRUE", "FALSE"),
                          ")"
                        ),
                        # Chi-square
                        paste0(
                          ", df = ",
                          input$chisqDf,
                          ", lower.tail = ",
                          if_else(input$valueSide %in% c("lt", "le"), "TRUE", "FALSE"),
                          ")"
                        )
                      )
                    )
                  )
                )
            )
          
          value <- input$valueX
        }
        
        return(
          list(
            value = value,
            functionCall = functionCall
          )
        )
      }) %>%
        bindEvent(
          input$go,
          ignoreInit = FALSE
        )
      
      output$distrAttributes <- renderUI({
        if(input$distribution == "norm"){
          tagList(
            numericInput(
              session$ns("normMean"),
              label = HTML("Ange normalfördelningens väntevärde:"),
              value = 0,
              step = 0.01
            ),
            numericInput(
              session$ns("normSd"),
              label = HTML("Ange normalfördelningens standardavvikelse:"),
              value = 1,
              min = 0.01,
              step = 0.01
            )
          )
        } else if(input$distribution == "t"){
          tagList(
            numericInput(
              session$ns("tMean"),
              label = HTML("Ange t-fördelningens väntevärde:"),
              value = 0,
              step = 0.01
            ),
            numericInput(
              session$ns("tSd"),
              label = HTML("Ange t-fördelningens standardavvikelse:"),
              value = 1,
              min = 0.01,
              step = 0.01
            ),
            numericInput(
              session$ns("tDf"),
              label = HTML("Ange t-fördelningens frihetsgrader:"),
              value = 15,
              step = 1,
              min = 0
            )
          )
        } else if(input$distribution == "binom"){
          tagList(
            numericInput(
              session$ns("binomSize"),
              label = HTML("Ange binomialfördelningens antal försök:"),
              value = 15,
              step = 1,
              min = 1
            ),
            numericInput(
              session$ns("binomProb"),
              label = HTML("Ange binomialfördelningens sannolikhet att lyckas på ett försök:"),
              value = 0.5,
              min = 0,
              max = 1
            )
          )
        } else if(input$distribution == "chisq"){
          tagList(
            numericInput(
              session$ns("chisqDf"),
              label = HTML("Ange chi-2-fördelningens frihetsgrader:"),
              value = 0
            )
          )
        }
      })
      
      output$probValues <- renderUI({
        if(input$valueOrProb == "Värde"){
          list <- 
            tagList(
              numericInput(
                inputId = session$ns("valueProb"),
                label = HTML("Ange den kända sannolikheten:"),
                value = 0.5,
                min = 0, 
                max = 1,
                step = 0.001
              )
            )
        } else {
          list <- 
            tagList(
              numericInput(
                inputId = session$ns("valueX"),
                label = HTML("Ange det kända värdet:"),
                value = 0,
                step = 0.01
              )
            )
        }
        
        if(input$distribution != "binom"){
          tagList(
            list, 
            p("Kontinuerliga fördelningar gör inte skillnad på < och ≤ samt sannolikheten för exakt ett utfall är alltid 0."),
            radioButtons(
              inputId = session$ns("valueSide"),
              label = HTML("Ange uttryckets olikhetstecken:"),
              inline = TRUE,
              choices = 
                c(
                  "<" = "lt",
                  ">" = "gt"
                )
            )
          )
        } else {
          tagList(
            list, 
            radioButtons(
              inputId = session$ns("valueSide"),
              label = HTML("Ange uttryckets olikhetstecken:"),
              inline = TRUE,
              choices = 
                c(
                  "<" = "lt",
                  "≤" = "le",
                  "=" = "eq",
                  "≥" = "ge",
                  ">" = "gt"
                )
            )
          )
        }
          
      })
      
      
      output$plots <- renderPlot({
        if(input$distribution == "norm"){
          x <- seq(from = input$normMean - 3*input$normSd, to = input$normMean + 3*input$normSd, by = 0.01)
          
          y <- dnorm(x, mean = input$normMean, sd = input$normSd)
          
          data <- 
            data.frame(x = x, y = y)
          
        } else if(input$distribution == "t"){
          x <- seq(from = input$tMean - 3*input$tSd, to = input$tMean + 3*input$tSd, by = 0.01)
          
          y <- dt((x - input$tMean) / input$tSd, df = input$tDf)
          
          data <- 
            data.frame(x = x, y = y)
          
        } else if(input$distribution == "binom"){
          x <- seq(from = 0, to = input$binomSize, by = 1)
          
          y <- dbinom(x, size = input$binomSize, prob = input$binomProb)
          
          data <- 
            data.frame(x = x, y = y)
          
        } else if(input$distribution == "chisq"){
          x <- seq(from = 0, to = input$chisqDf, by = 0.01)
          
          y <- dchisq(x, df = input$chisqDf)
          
          data <- 
            data.frame(x = x, y = y)
          
        }
        
        highlightArea <- 
          data %>% 
          {
            if (input$valueSide  == "le"){
              filter(., x <= (valueX()$value))
            } else if(input$valueSide  == "lt"){
              filter(., x < (valueX()$value))
            } else if(input$valueSide  == "gt"){
              filter(., x > (valueX()$value))
            } else if(input$valueSide  == "ge"){
              filter(., x >= (valueX()$value))
            } else {
              filter(., x == (valueX()$value))
            }
          }
        
        ## Visualization
        p <- ggplot(data) + aes(x = x, y = y)
        
        if(input$distribution == "binom"){
          
          p <- 
            p + geom_bar(stat = "identity") + 
            geom_bar(data = highlightArea, stat = "identity", fill = "orange")
            
        } else {
          
          p <- 
            p + geom_ribbon(data = highlightArea, ymin = 0, aes(ymax = y), fill = "orange") + 
            geom_line(linewidth = 1)
            
        }
        
        p + scale_y_continuous(limits = c(0, max(data$y)*1.1))

      }) %>% 
        bindEvent(input$go)
      
      output$introduction <- renderUI({
        tagList(
          tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']}});"),
          
          htmltools::div(
            paste0(
              "$Pr(X", 
              if_else(input$valueSide == "eq", "=", paste0("\\",input$valueSide)), 
              valueX()$value, 
              ")$ "
              )
            ) %>% 
            withMathJax(),
          
          htmltools::div(
            HTML(
              paste(
                "<span class='tex2jax_ignore'>", 
                paste(
                  valueX()$functionCall, 
                  " = ", 
                  valueX()$functionCall %>% eval(), 
                  sep = " "
                ),
                "</span>"
              )
            )
          )
        )
      }) %>% 
        bindEvent(input$go)
      
      
    }
  )
}

