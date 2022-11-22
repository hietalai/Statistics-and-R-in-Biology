
##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

anovaUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      
      # Application title
      titlePanel("Envägs-ANOVA"),
      withMathJax(),
      tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']], processEscapes: true}});"), 
      tags$head(tags$style(
        type = 'text/css',
        # The wellPanel() in mainPanel() creates a div.well that allows for changes in the structure
        'div.well { max-height: 60em; overflow-y: auto; }'
      )),   
      
      
      # Sidebar with a slider input means for the individual groups
      sidebarLayout(
        sidebarPanel(
          # style = "position:fixed;width:inherit;",
          sliderInput("group_1" %>% ns(),
                      "Medelvärde för Grupp 1",
                      min = -10,
                      max = 10,
                      value = 0,
                      step = 0.05),
          sliderInput("group_2" %>% ns(),
                      "Medelvärde för Grupp 2",
                      min = -10,
                      max = 10,
                      value = 0,
                      step = 0.05),
          sliderInput("group_3" %>% ns(),
                      "Medelvärde för Grupp 3",
                      min = -10,
                      max = 10,
                      value = 0,
                      step = 0.05),
          sliderInput("group_4" %>% ns(),
                      "Medelvärde för Grupp 4",
                      min = -10,
                      max = 10,
                      value = 0,
                      step = 0.05),
          numericInput("sample_size" %>% ns(), 
                       HTML("Ange stickprovsstorleken för varje grupp"),
                       value = 30,
                       min = 1, 
                       step = 1,
                       width = "35%"),
          radioButtons("plot_type" %>% ns(), 
                       "Ange typ av diagram",
                       choiceValues = c("density", "histogram"),
                       choiceNames = c("Densitet", "Histogram"),
                       width = "35%"),
          actionButton(ns("new_sample"), 
                       HTML("Klicka här för ett nytt urval."),
                       width = "100%")
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
anovaSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){

      data_input <- reactive({
        means <- c(input$group_1, input$group_2, input$group_3, input$group_4) 
        
        seed <- today() + input$new_sample
        
        set.seed(seed = seed)
        samples <- sapply(means, FUN = function(x){rnorm(n = input$sample_size, mean = x, sd = 1)}) %>% 
          as.data.frame()
        
        data_samples <- pivot_longer(samples, cols = everything())
      })
      
      output$plots <- renderPlot({
        data_samples <- data_input()
        
        sample_means <- aggregate(value ~ name, data_samples, mean)
        
        if(input$plot_type == "density"){
          ggplot(data = data_samples) + aes(x = value) + geom_density() +
            geom_vline(data = sample_means, aes(xintercept = value), linetype = 2, linewidth = 1.2) +
            geom_vline(aes(xintercept = mean(value)), color = "dark red", linewidth = 1) +
            scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
            facet_grid(rows = vars(name)) + theme_bw() + 
            theme(strip.text.y = element_text(angle = 0, color = "white", size = 14), 
                  strip.background.y = element_rect(fill = "black"),
                  axis.title.y = element_blank()) +
            labs(x = "Värde")  
        } else {
          ggplot(data = data_samples) + aes(x = value) + 
            geom_histogram(binwidth = 0.25, color = "black", fill = "light grey") +
            geom_vline(data = sample_means, aes(xintercept = value), linetype = 2, linewidth = 1.2) +
            geom_vline(aes(xintercept = mean(value)), color = "dark red", linewidth = 1) +
            scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
            facet_grid(rows = vars(name)) + theme_bw() + 
            theme(strip.text.y = element_text(angle = 0, color = "white", size = 14), 
                  strip.background.y = element_rect(fill = "black"),
                  axis.title.y = element_blank()) +
            labs(x = "Värde") 
        }
        
        
      })
      
      output$introduction <- renderUI({
        
        data_samples <- data_input()
        
        withMathJax(
          p("Denna applikation ämnar att försöka visualisera vad ett 
            ANOVA test gör när den jämför fler än två gruppers medelvärden."),
          p("I området till vänster finns reglage som ni kan justera", strong("de sanna"),
            "medelvärdena för vardera grupp och diagrammet nedanför visar fördelningen av vardera slumpmässiga urval om", 
            input$sample_size, "enheter från en sann normalfördelning med varje grupps enskilda medelvärde",
            "och en fix standardavvikelse 1 för alla grupper.",
            "Vi vet alltså att datamaterialet kommer från en population som är normalfördelad med lika",
            "varians vilket är kravet för att använda en ANOVA-modell enligt $X_i \\sim N(\\mu = \\mu_i; \\sigma = 1)$."
          ),
          p("Lek runt med reglagen och undersök vad som händer med respektive kvadratsumma och det resulterande $F_{test}$."),
          h3("Visualisering"),
          p("De streckade linjerna anger stickprovsmedelvärdet för varje grupp, $\\bar{X}_i$.",
            "Den ", span("mörkröda", style = "color:darkred"), 
            "linjen anger hela datamaterialets stickprovsmedelvärde, $\\bar{X}$.")
        )
      })
      
      output$variances <- renderUI({
        data_samples <- data_input()
        sample_means <- aggregate(value ~ name, data_samples, mean)
        sample_sizes <- aggregate(value ~ name, data_samples, length)
        
        SST <- sum((data_samples$value - mean(data_samples$value))^2)
        
        SSgroups <- sum(sample_sizes$value * (sample_means$value - mean(data_samples$value))^2)
        MSgroups <- SSgroups/(nrow(sample_means) - 1)
        
        SSE <- SST-SSgroups
        MSE <- SSE/(nrow(data_samples) - nrow(sample_means))
        
        withMathJax(
          h3("Kvadratsummor"),
          p("Variationen totalt i hela datamaterialet, avståndet mellan varje observation (X_{ij}) och den ", 
            span("mörkröda", style = "color:darkred"),
            "linjen (\\bar{X}), beräknas till:"),
          paste("$${SS}_{total} = \\sum_{i=1}^k\\sum_{j=1}^{n_i}\\left(X_{ij} - \\bar{X} \\right)^2 = ", 
                round(SST, 3), "$$"),
          
          p("Variationen", strong("mellan"),"varje grupp, avståndet mellan de streckade (\\bar{X}_i) och den ", 
            span("mörkröda", style = "color:darkred"),
            "linjen (\\bar{X}), beräknas till:"),
          paste("$${SS}_{groups} = \\sum_{i=1}^k {n_i \\cdot \\left(\\bar{X}_{i} - \\bar{X} \\right)^2} = ", 
                round(SSgroups, 3), "$$"),
          
          p("Variationen", strong("inom"), "varje grupp, avståndet mellan varje observation (X_{ij}) 
         och dess grupps streckade linje (\\bar{X}_i), beräknas till:"),
          paste("$${SS}_{within-groups} = \\sum_{i=1}^k \\left[\\sum_{j=1}^{n_i}
             \\left(X_{ij} - \\bar{X}_i \\right)^2\\right] = ", 
                round(SSE, 3), "$$"),
          
          p("(F_{test}) beräknas för att se om variationen mellan grupperna är 
         avsevärt mycket större än variationen inom grupperna. För detta urval beräknas testvariabeln till:"),
          
          paste("$$F_{test} = \\frac{MS}{MSE} = \\frac{\\frac{SS_{groups}}{k-1}}{\\frac{SS_{error}}{N-k}} = ", 
                round(MSgroups / MSE, 3), "$$")
          
        )
        
        
        
      })
    }
  )
}

