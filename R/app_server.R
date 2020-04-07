#' @import shiny
app_server <- function(input, output,session) {
  output$suspicious_answers = DT::renderDataTable(data.table(x=4))
  
  observeEvent(input$file_style, {
    if (input$file_style == 'csv') {
      output$custom_csv = renderUI({
        fluidRow(
          column(width=7, radioButtons('delimiter', 'Delimiter', inline=T, width='100%', choices = list(','=',',';'=';','tab'='\t'))),
          column(width=5, radioButtons('quote', 'Quote', inline=T, choices = c('"'='"',"'"="'"))),
          #radioButtons(inline = T, 'decimal', 'Decimal', choices = c('1.0'='.','1,0'=','))
        )
      })
    } else {
      output$custom_csv = renderUI(NULL)
    }
  })
  
  csv_file = reactive({
    if (is.null(input$csv_file)) return(NULL)
    mb = input$csv_file$size / 1000000
    if (mb > 20)
      shinyalert::shinyalert('Suspiciously large file', 'This file is more than 20Mb, which is rather large, so we assume you uploaded the wrong file (unless this is an open-ended anthology writing exam)', type='error')
    
    if (input$file_style == 'testvision') {
      d = readr::read_csv(input$csv_file$datapath)
      output$custom_columns = renderUI(tagList())
    } else {
      if (input$file_style == 'csv') {
        delimiter = if(is.null(input$delimiter)) ',' else input$delimiter
        quote = if(is.null(input$quote)) ',' else input$quote
        print(delimiter)
        if (delimiter == ';') d = readr::read_csv2(input$csv_file$datapath, quote = quote)
        if (delimiter == '\t') d = readr::read_tsv(input$csv_file$datapath, quote=quote)
        if (delimiter == ',') d = readr::read_csv(input$csv_file$datapath, quote=quote)
      }
      output$custom_columns = renderUI({
        tagList(
          column(width=1),
          column(width=10, align='center',
            br(),
            h4('Select column names'),
            selectizeInput('student', 'Student name', choices=c()),
            selectizeInput('question', 'Question (label)', choices=c()),
            selectizeInput('answer', 'Answer', choices=c())
          )
        )
      })
    }
    output$gogogo = renderUI({
      tagList(
        h4('Analyze data', align='center'),
        radioButtons('measure', 'Similarity measure', choices=list('Cosine similarity (symmetic)'='cosine', 'Overlap percentage (assymetric)'='overlap_pct')),
        div(align='center', actionButton('prepare_data', 'Run', width = '50%'))
      )
    })
    d
  })
  
  
  
  #output$csv_upload_info = renderText({
  #  d = csv_file()
  #  sprintf('Parsed %s columns and %s rows', ncol(d), nrow(d))
  #})
  
  observeEvent(csv_file(), {
    d = csv_file()
    output$csv_upload_info = renderText(sprintf('Parsed %s columns and %s rows', ncol(d), nrow(d)))
    
    if (input$file_style == 'csv') {
      cn = colnames(d)
      updateSelectizeInput(session, 'student', choices = cn, selected = get_field_suggestion(c('candidatedisplayname','student'), cn))
      updateSelectizeInput(session, 'question', choices = cn, selected = get_field_suggestion(c('questionname','question'), cn))
      updateSelectizeInput(session, 'answer', choices = cn, selected = get_field_suggestion(c('^answer$','anser'), cn))
    }
  })
  
  tc = eventReactive(input$prepare_data, {
    d = csv_file()
    if (is.null(d)) return(NULL)
    
    if (input$file_style == 'testvision') {
      req_cols = c('CandidateId','CandidateDisplayName','QuestionId','QuestionName')
      if (!all(req_cols %in% colnames(d))) {
        shinyalert::shinyalert('Invalid input file', 'Oh no! This file does not have the required columns, or cannot be parsed correctly.', type='error')
        return(NULL)
      }
      tc = prepare_tc_testvision(d)
    } else {
      if (is.null(input$student) | is.null(input$question) | is.null(input$answer)) {
        shinyalert::shinyalert('Invalid column selection', 'Oh no! You have not yet specified which columns to use', type='error')
        return(NULL)  
      }
      tc = prepare_tc_csv(d, input$student, input$question, input$answer)
    }
  
    shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")    
    return(tc)
  })
  
  sim = reactive({get_question_sim(tc(), input$measure)})
  
  suspicious_students = reactive({
    if (is.null(sim())) 
      NULL
    else {
      df = plagiarism_suspects(sim())
      data.table::setnames(df, c('from_candidate','average_z'), c('Student','Z'))
    }
  })
  
  output$suspicious_students = DT::renderDataTable({
    df = suspicious_students()
    
    df = DT::datatable(df, options=list(dom = 'Bfrtip', 
                                        pageLength=25), 
                       rownames=F, selection='multiple')
    
    cuts = -200:200 / 100
    val = tokenbrowser::scale_col(tokenbrowser::rescale_var(cuts, new_min=-1, new_max=1)^2 * sign(cuts), 
                                  col_range=c('green','white','red'), alpha = 0.2)
    df = DT::formatStyle(df, columns='Z',  
                         backgroundColor = DT::styleInterval(cuts[-1], val))
    df
  })
  
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
      data.table::setnames(df, c('from_candidate','question','weight','question_z'), c('Student', 'Question', 'Sim', 'Z'))
      df
    }
  })
  
  output$suspicious_answers = DT::renderDataTable({
    df = suspicious_answers()
    df = DT::datatable(df, class = "display nowrap",
                       extensions = c('Scroller'),
                       options=list(dom = 'Bfrtip', pageLength=25), rownames=F, selection='single')
    
    cuts = -200:200 / 100
    val = tokenbrowser::scale_col(tokenbrowser::rescale_var(cuts, new_min=-1, new_max=1)^2 * sign(cuts), 
                                  col_range=c('green','white','red'), alpha = 0.2)
    df = DT::formatStyle(df, columns='Z',  
                         backgroundColor = DT::styleInterval(cuts[-1], val))
    
    cuts = 1:100/100
    val = tokenbrowser::scale_col(tokenbrowser::rescale_var(cuts^2, new_min=-1, new_max=1), 
                                  col_range=c('white','red'), alpha = 0.2)
    df = DT::formatStyle(df, columns='Sim',  
                         backgroundColor = DT::styleInterval(cuts[-1], val))
    df
  })
  
  observeEvent(input$suspicious_answers_rows_selected ,{
    highlight_text(input, output, tc(), sim(), suspicious_answers())
  })
  
  observeEvent(input$help, {
    shinyalert::shinyalert('How does this work?', "All answers to a question are compared with a text similarity measure that looks at the number of overlapping words. Words are weighted based on how often they are used by all students, so that common words in an answer (e.g., 'the', 'it') are less important, and words that are only used by a few students are given more weight. The similarity score ranges from 0 (no overlap) to 1 (perfect overlap).\n\nThe second table shows the highest similarity score that a student reached for each question. In addition, to account for the fact that certain types of questions tend to have more similar answers, Z-scores are calculated for how much this score deviates from the average of all students for that question. In the first table, that can be used to filter on specific students, the average Z-scores show which students are overall more likely to give similar answers to others.\n\nWhen you view the answers, verbatim overlap of 2 or more words is highlighted, with darker colours for longer phrases. Note that this is not the method used to calculate the similarity (which uses weighted single words). It's just to make it easier to spot verbatim copy")
  })
  
}