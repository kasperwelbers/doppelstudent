#' @import shiny 
#' @import shinyWidgets
#' @import shinydashboard
app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    
    dashboardPage(title = 'Persberichten tracker',
                  dashboardHeader(title = 'File upload',
                                  tags$li(class = "dropdown",
                                          tags$li(class = "dropdown", shiny::actionLink('help', 'Help')),
                                          )
                  ),
                  dashboardSidebar(width = 300, collapsed=F,
                                   shinyalert::useShinyalert(),  # Set up shinyalert
                                   shinyjs::useShinyjs(),
                                   sidebarMenu(
                                     div(align='center',
                                     column(width=12,
                                       div(
                                         selectInput('file_style', 'Choose input format', choice=csv_options),
                                         uiOutput('custom_csv')
                                       ),
                                       fileInput('csv_file', label = 'Upload testvision file', accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
                                       textOutput('csv_upload_info')
                                     ),
                                     uiOutput('custom_columns'),
                                     uiOutput('gogogo')
                                     
                                     
                                     )
                                   )),
                  dashboardBody(
                    fluidPage(
                      fluidRow(
                        column(width = 3,  
                               div(style = "height:120px", 
                                h3('Suspicious students'),
                                shiny::p('Students sorted by probability of plagiarism. Select one or multiple rows to filter next table')
                               ),
                               DT::dataTableOutput('suspicious_students')
                        ),
                        column(width = 5, 
                               div(style = "height:120px", 
                                h3('Suspicious answers'),
                                shiny::p('Answers sorted by probability of plagiarism. Select one to view answer with similar answers')
                               ), 
                               DT::dataTableOutput('suspicious_answers')
                        ),
                        column(width=4, 
                          fixedRow(
                              div(style = "height:120px", 
                                  h3('Answer'),
                                  shiny::p('Comparison of selected answer with answers from other students, sorted from most to least similar.')
                              ),
                              div(class='textbox', 
                                  htmlOutput('txt_x'),  
                                  style = "overflow-y: scroll; height: 200px")),
                          fixedRow(
                            h3('Answers from others', align='center'),
                            br(),
                            div(class='textbox', 
                                htmlOutput('txt_y'), 
                                style = "overflow-y: scroll; height: 550px"), 
                          )
                        
                        )
                      )  
                    )
                    
                  )
    )
  )
}

csv_options = list('TestVision'='testvision',
                   'CSV: comma-separated' = 'read_csv', 
                   'CSV: tab-separated' = 'read_tsv',
                   'CSV: European (; with , decimal point)' = 'read_csv2', 
                   'CSV: custom' = 'custom')

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'doppelstudent')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
