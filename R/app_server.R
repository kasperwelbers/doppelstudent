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
    
    ## this is redundant, since shiny already imposes a limit on 5MB, but I keep it here in case we want to add a message
    mb = input$csv_file$size / 1000000
    if (mb > 5)
      shinyalert::shinyalert('Suspiciously large file', 'This file is more than 20Mb, which is rather large, so we assume you uploaded the wrong file (unless this is an open-ended anthology writing exam)', type='error')
    
    if (input$file_style == 'testvision') {
      
      if (grepl('\\.csv$', input$csv_file$datapath)) 
        d = readr::read_csv(input$csv_file$datapath)
      else
        d = openxlsx::read.xlsx(input$csv_file$datapath)
      output$custom_columns = renderUI(tagList())
    } else {
      if (grepl('\\.csv$', input$csv_file$datapath)) {
        updateSelectInput(session, 'file_style', selected='csv')
        delimiter = if(is.null(input$delimiter)) ',' else input$delimiter
        quote = if(is.null(input$quote)) ',' else input$quote
        print(delimiter)
        if (delimiter == ';') d = readr::read_csv2(input$csv_file$datapath, quote = quote)
        if (delimiter == '\t') d = readr::read_tsv(input$csv_file$datapath, quote=quote)
        if (delimiter == ',') d = readr::read_csv(input$csv_file$datapath, quote=quote)
      }
      if (grepl('\\.xlsx?$', input$csv_file$datapath)) { 
        updateSelectInput(session, 'file_style', selected='xlsx')
        d = openxlsx::read.xlsx(input$csv_file$datapath)
      }
      output$custom_columns = renderUI({
        tagList(
          column(width=1),
          column(width=10, align='center',
            br(),
            h4('Select column names', shiny::actionLink('help_column_select','help')),
          
            selectizeInput('student', 'Student name', choices=c(), multiple=T),
            selectizeInput('question', 'Question (label)', choices=c(), multiple=T),
            selectizeInput('answer', 'Answer', choices=c(), multiple=F)
          )
        )
      })
    }
    output$gogogo = renderUI({
      tagList(
        box(collapsible = T, collapsed = T, title = 'Change settings', width = 12, background = 'black',  
          radioButtons('measure', 'Similarity measure', choices=list('symmetric (cosine similarity)'='cosine', 'asymmetric (% overlap)'='overlap_pct'), selected = 'overlap_pct'),
          radioButtons('ngrams', 'Compare what', inline=F, choices = list('Single words'= 1, 'Three word sequences'=3), selected = 1)
        ),
        br(),
        div(align='center', actionButton('prepare_data', 'Run', width = '50%'))
      )
    })
    d
  })
  
  observeEvent(input$help_column_select, {
    shinyalert::shinyalert('How to select columns', 'Selecting columns is pretty straightforward stuff, but here you can select multiple columns, and you might be wondering why.\n\nTo compare the answers of different students to the same question, the student-question pairs need to be unique. Sometimes this requires using multiple columns (e.g., student name & student ID, first and last name). The values in these columns will then be concatenated')
  })
  
  
  #output$csv_upload_info = renderText({
  #  d = csv_file()
  #  sprintf('Parsed %s columns and %s rows', ncol(d), nrow(d))
  #})
  
  observeEvent(csv_file(), {
    d = csv_file()
    output$csv_upload_info = renderText(sprintf('Parsed %s columns and %s rows', ncol(d), nrow(d)))
    
    if (input$file_style %in% c('csv','xlsx')) {
      cn = colnames(d)
      student_suggestion = union(get_field_suggestion(c('candidatedisplayname','candidatename','student'), cn), 
                                 get_field_suggestion(c('candidateid', 'resultid'), cn))
      question_suggestion = union(get_field_suggestion(c('questionname','question.name','question'), cn),
                                  get_field_suggestion(c('questionid'), cn))
      answer_suggestion = get_field_suggestion(c('^answer$','anser'), cn)
      
      updateSelectizeInput(session, 'student', choices = cn, selected = student_suggestion)
      updateSelectizeInput(session, 'question', choices = cn, selected = question_suggestion)
      updateSelectizeInput(session, 'answer', choices = cn, selected = answer_suggestion)
    }
  })
  
  tc = eventReactive(input$prepare_data, {
    
    d = csv_file()
    if (is.null(d)) return(NULL)
    
    if (input$file_style == 'testvision') {
      testvision_type = if (grepl('\\.csv$', input$csv_file$datapath)) 'csv' else 'xlsx'
      
      if ('kandidaatnaam' %in% colnames(d)) {
        if (testvision_type == 'csv') 
          req_cols = c('kandidaatid','kandidaatweergavenaam','vraagid','vraagnaam','antwoord')
        else
          req_cols = c('kandidaatnaam','vraagnaam','vraagid','antwoord')
      } else {
        if (testvision_type == 'csv') 
          req_cols = c('candidateid','candidatedisplayname','questionid','questionname','answer')
        else
          req_cols = c('candidatename','question.name','questionid','answer')
      }
        
      if (!all(req_cols %in% tolower(colnames(d)))) {
        print(req_cols)
        print(tolower(colnames(d)))
        shinyalert::shinyalert('Invalid input file', 'Oh no! This file does not have the required columns, or cannot be parsed correctly.', type='error')
        return(NULL)
      }
      
      tc = prepare_tc_testvision(input, d, testvision_type)
      
    } else {
      if (is.null(input$student) | is.null(input$question) | is.null(input$answer)) {
        shinyalert::shinyalert('Invalid column selection', 'Oh no! You have not yet specified which columns to use', type='error')
        return(NULL)  
      }
      req_cols = unique(c(input$student,input$question,input$answer))
      if (!all(req_cols %in% colnames(d))) {
        shinyalert::shinyalert('Invalid input file', 'Oh no! This file does not have the required columns, or cannot be parsed correctly.', type='error')
        return(NULL)
      }
      tc = prepare_tc_csv(d, input$student, input$question, input$answer)
    }
    
    if (is.null(tc)) return(NULL)
    shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")    
    return(tc)
  })
  
  sim = eventReactive(tc(), {get_question_sim(tc(), input$measure, input$ngrams)})
  
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
    shinyalert::shinyalert('How does this work?', "All answers to a question are compared with a text similarity measure that looks at the number of overlapping words (or tri-grams: phrases of three words). Words are weighted based on how often they are used by all students, so that common words in an answer (e.g., 'the', 'it') are less important, and words that are only used by a few students are given more weight. The similarity score ranges from 0 (no overlap) to 1 (perfect overlap).\n\nThe second table shows the highest similarity score that a student reached for each question. In addition, to account for the fact that certain types of questions tend to have more similar answers, Z-scores are calculated for how much this score deviates from the average of all students for that question. In the first table, that can be used to filter on specific students, the average Z-scores show which students are overall more likely to give similar answers to others.\n\nWhen you view the answers, verbatim overlap of 2 or more words is highlighted, with darker colours for longer phrases. Note that this is not the method used to calculate the similarity. It's just to make it easier to spot verbatim copy.\n\nFinally, we developed this tool based on what we think works for the type of open-ended questions that we ask, but if you have any suggestions or feature requests, please send me an email (k.welbers@vu.nl) and I'll see what I can do.\n\nHappy hunting!\nKasper Welbers, Wouter van Atteveldt & Arjen Heijstek")
  })
  
}