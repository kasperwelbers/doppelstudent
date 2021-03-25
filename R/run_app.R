#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_doppelstudent <- function(port=6171, ...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options = list(port=port, ...)), 
    golem_opts = list()
  )
}


function(){
  #devtools::install_github('kasperwelbers/tokenbrowser')
  devtools::install_github('kasperwelbers/doppelstudent')
  run_doppelstudent()
}