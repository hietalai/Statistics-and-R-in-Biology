
##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

testUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        h2("Kod"),
        aceEditor(ns("code"), mode = "r", height = "200px", value = "2+3"),
        actionButton(ns("eval"), "Kör"),
        p("Endast den sista raden kod som genererar en utskrift, visas i 'Utskrift'.")
      ),
      column(
        width = 6,
        h2("Utskrift"),
        verbatimTextOutput(ns("output"))
      )
    )
  )
}


## Backend for vizualizations
testSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){

      output$output <- renderPrint({
        input$eval
        eval(parse(text = isolate(input$code)))
      })
      
      
    }
  )
}

