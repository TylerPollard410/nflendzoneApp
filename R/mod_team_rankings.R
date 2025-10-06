#' team_rankings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_team_rankings_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' team_rankings Server Functions
#'
#' @noRd 
mod_team_rankings_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_team_rankings_ui("team_rankings_1")
    
## To be copied in the server
# mod_team_rankings_server("team_rankings_1")
