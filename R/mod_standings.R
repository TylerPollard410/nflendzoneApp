#' standings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_standings_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' standings Server Functions
#'
#' @noRd 
mod_standings_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_standings_ui("standings_1")
    
## To be copied in the server
# mod_standings_server("standings_1")
