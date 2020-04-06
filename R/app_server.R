#' @import shiny
app_server <- function(input, output,session) {
  output$suspicious_answers = DT::renderDataTable(data.table(x=4))
  
  csv_file = reactive({
    if (is.null(input$csv_file)) return(NULL)
    mb = input$csv_file$size / 1000000
    if (mb > 20)
      shinyalert::shinyalert('Suspiciously large file', 'This file is more than 20Mb, which is rather large, so we assume you uploaded the wrong file (unless this is an open-ended anthology writing exam)', type='error')
    
    if (input$file_style == 'testvision') {
      d = readr::read_csv(input$csv_file$datapath)
      output$custom_columns = renderUI(tagList())
    } else {
      if (input$file_style == 'read_csv') d = readr::read_csv(input$csv_file$datapath)
      if (input$file_style == 'read_tsv') d = readr::read_tsv(input$csv_file$datapath)
      if (input$file_style == 'read_csv2') d = readr::read_csv2(input$csv_file$datapath)
      if (input$file_style == 'custom') d = read.csv(input$csv_file$datapath, sep=input$delimiter, quote = input$quote, dec=input$decimal)
      output$custom_columns = renderUI({
        tagList(
          column(width=1),
          column(width=10, align='center',
            br(),
            h4('Select column names'),
            selectizeInput('student', 'Student name', choices=c()),
            selectizeInput('student_id', 'Student ID', choices=c()),
            selectizeInput('question_id', 'Question ID', choices=c())
          )
        )
      })
    }
    output$gogogo = renderUI({
      br()
      actionButton('prepare_data', 'Prepare data')
    })
    d
  })
  
  observeEvent(input$file_style, {
    if (input$file_style == 'custom') {
      output$custom_csv = renderUI({
        fluidRow(
          radioButtons(inline=T, 'delimiter', 'Delimiter', width='100%', choices = list(','=',',';'=':','tab'='\t')),
          radioButtons(inline=T, 'quote', 'Quote', choices = c('"'='"',"'"="'")),
          radioButtons(inline = T, 'decimal', 'Decimal', choices = c('1.0'='.','1,0'=','))
        )
      })
    }
  })
  
  output$csv_upload_info = renderText({
    d = csv_file()
    sprintf('Parsed %s columns and %s rows', ncol(d), nrow(d))
  })
  
  tc = eventReactive(input$prepare_data, {
    #if (is.null(input$csv_file)) return(NULL)
    #mb = input$csv_file$size / 1000000
    #if (mb > 20)
    #  shinyalert::shinyalert('Suspiciously large file', 'This file is more than 20Mb, which is rather large, so we assume you uploaded the wrong file (unless this is an open-ended anthology writing exam)', type='error')
    
    #d = readr::read_csv(input$csv_file$datapath)
    d = csv_file()
    if (is.null(d)) return(NULL)
    req_cols = c('CandidateId','CandidateDisplayName','QuestionId','QuestionName')
    if (!all(req_cols %in% colnames(d))) {
      shinyalert::shinyalert('Invalid input file', 'Oh no! This file does not have the required columns, or cannot be parsed correctly. Did you provide the file directly given by testvision? (If so, please let us know so we can fix this)', type='error')
      return(NULL)
    } else {
      shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")    
      return(get_tcorpus(d))
    }
  })
  
  sim = reactive({get_question_sim(tc())})
  
  suspicious_students = reactive({
    if (is.null(sim())) 
      NULL
    else {
      df = plagiarism_suspects(sim())
      data.table::setnames(df, c('from_candidate','average_z'), c('Student','Z-score'))
    }
  })
  
  output$suspicious_students = DT::renderDataTable(suspicious_students(), options=list(dom = 'Bfrtip', pageLength=25), rownames=F, selection='multiple')
  
  suspicious_answers = reactive({
    if (is.null(sim())) 
      NULL
    else {
      df = subset(sim(), select = c('from_candidate','question','weight','question_z'))
      df = unique(df, by=c('from_candidate','question'))
      
      if (!is.null(input$suspicious_students_rows_selected)){
        .candidate = suspicious_students()$Student[input$suspicious_students_rows_selected]
        df = df[list(from_candidate = .candidate), , on='from_candidate']
      }
      
      #if (length(input$candidate_filter)) {
      #  df = df[list(from_candidate = input$candidate_filter), , on = 'from_candidate']
      #}
      data.table::setnames(df, c('from_candidate','question','weight','question_z'), c('Student', 'Question', 'Similarity', 'Z-score'))
      df
    }
  })
  
  output$suspicious_answers = DT::renderDataTable(suspicious_answers(), options=list(dom = 'Bfrtip', pageLength=25), rownames=F, selection='single')
  
  observeEvent(input$suspicious_answers_rows_selected ,{
    highlight_text(input, output, tc(), sim(), suspicious_answers())
  })
  
  observeEvent(input$help, {
    shinyalert::shinyalert('How does this work?', "All answers to a question are compared with a text similarity measure that looks at the number of overlapping words. Words are weighted based on how often they are used by all students, so that common words in an answer (e.g., 'the', 'it') are less important, and words that are only used by a few students are given more weight. The similarity score ranges from 0 (no overlap) to 1 (perfect overlap). In addition, to account for the fact that certain types of questions tend to have more similar answers, Z-scores are calculated for how much the similarity of two answers deviates from the average for that question.\n\nWhen you view the answers, verbatim overlap of 2 or more words is highlighted, with darker colours for longer phrases. Note that this is not the method used to calculate the similarity (which uses weighted single words), but only used for easier spotting of overlap")
  })
  
}