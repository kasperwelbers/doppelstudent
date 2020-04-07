#' @import shiny 
#' @import shinyWidgets
#' @import shinydashboard
app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    
    dashboardPage(title = 'Open-ended exam plagiarism scanner',
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
                                         fileInput('csv_file', label = 'Upload file', accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
                                         textOutput('csv_upload_info')
                                       ),
                                       uiOutput('custom_columns'),
                                       br(), br(),
                                     ),
                                     column(width=12, 
                                       br(), br(),
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
                              div(style = "height:150px", 
                                  h3('Compare answers', align='center'),
                                  shiny::p('Comparison of selected answer with answers from other students, sorted from most to least similar.')
                              ),
                              div(class='textbox', 
                                  h4('Answer', align='center'),
                                  wellPanel(
                                  htmlOutput('txt_x'),
                                  br(),
                                  style = "overflow-y: scroll; height: 350px"))),
                          fixedRow(
                            h4('Other answers', align='center'),
                            br(),
                            div(class='textbox', 
                                wellPanel(
                                htmlOutput('txt_y'), 
                                style = "overflow-y: scroll; height: 450px")), 
                          )
                        
                        )
                      )  
                    )
                    
                  )
    )
  )
}

csv_options = list('TestVision'='testvision',
                   'CSV' = 'csv')

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
