rm_html <- function(x) {
  x = iconv(x, "UTF-8", "UTF-8", sub='')
  #gsub('<.*?>','',x)
  textclean::replace_html(x)
}

create_unique_label <- function(d, new_column, id_col, name_col) {
  u = unique(data.table::data.table(d[,c(id_col, name_col)]))
  has_duplicate = unique(u[[name_col]][duplicated(u[[name_col]])])
  
  if (length(has_duplicate) > 0) {
    #shinyalert::shinyalert('Duplicate warning', sprintf('You have a duplicate "%s". In the duplicate cases, the ID has been included in the label in the format: "label #id#"', name_col), type='warning')
    u[[name_col]] = ifelse(u[[name_col]] %in% has_duplicate, sprintf('%s #%s#', u[[name_col]], u[[id_col]]), u[[name_col]])
  }
  
  colnames(u) = c(id_col, new_column)
  d[[new_column]] = NULL
  d = merge(d, u, by=id_col)
  d
}

prepare_tc_testvision <- function(d) {
  d = d[d$QuestionType == 'OpenEnded',]
  
  d = create_unique_label(d, 'candidate', 'CandidateId', 'CandidateDisplayName')
  d = create_unique_label(d, 'question', 'QuestionId', 'QuestionName')
  
  a = d[,c('candidate','question','answer')]
  a$candidate = rm_html(a$candidate)
  a$question = rm_html(a$question)
  a$answer = rm_html(a$answer)
  a$answer = stringi::stri_trim(a$answer)
  a$answer[is.na(a$answer)] = ''
  tc = corpustools::create_tcorpus(a, text_col='answer', remember_spaces=T)
  tc$preprocess('token','feature', lowercase = T, as_ascii = T, remove_punctuation = F, remove_stopwords = F)
  tc$tokens$feature[tc$tokens$feature %in% c(',','.')] = NA
  tc
}

prepare_tc_csv <- function(d, student_col, question_col, answer_col) {
  #d = readr::read_csv('~/Downloads/antwoorden open vragen door arjen er uit te halen.csv')
  a = d[,c(student_col, question_col, answer_col)]
  colnames(a) = c('candidate','question','answer')
  a$answer = rm_html(a$answer)
  a$answer = stringi::stri_trim(a$answer)
  a$answer[is.na(a$answer)] = ''
  tc = corpustools::create_tcorpus(a, text_col='answer', remember_spaces=T)
  tc$preprocess('token','feature', lowercase = T, as_ascii = T, remove_punctuation = F, remove_stopwords = F)
  tc$tokens$feature[tc$tokens$feature %in% c(',','.')] = NA
  tc
}

tc_add_idf <- function(tc) {
  dtm = corpustools::get_dtm(tc, feature='feature', weight = 'norm_tfidf', drop_empty_terms = F, context_labels = T, feature_labels=T, ngrams=1)
  dtm = as(dtm, 'dgTMatrix')
  dtm = data.table::data.table(doc_id = rownames(dtm)[dtm@i+1], feature=colnames(dtm)[dtm@j+1], tfidf=dtm@x)  
  tc$tokens = merge(tc$tokens, dtm, by=c('doc_id','feature'))
  tc
}

#' @import data.table
get_question_sim <- function(tc, measure) {
  if (is.null(tc)) return(NULL)
  
  #tc = tc_add_idf(tc)
  g = corpustools::compare_documents(tc, 'feature', meta_cols = 'question', min_similarity = 0.05, measure = measure, return_igraph = F)$d
  
  
  from_i = match(g$from, tc$meta$doc_id)
  to_i = match(g$to, tc$meta$doc_id)
  g$question = tc$meta$question[from_i]
  g$from_candidate = tc$meta$candidate[from_i]
  g$to_candidate = tc$meta$candidate[to_i]

  g = data.table::as.data.table(g)
  data.table::setorderv(g, 'weight', -1)
  
  g_agg = unique(g, by=c('from_candidate','question'))
  g_agg = g_agg[, list(question_mean_weight=mean(weight), question_sd_weight=sd(weight)), by='question']
  g = merge(g, g_agg, by=c('question'))
  
  #g[, question_mean_weight := mean(weight), by='question']  
  #g[, question_sd_weight := sd(weight), by='question']  
  g$question_z = round((g$weight - g$question_mean_weight) / g$question_sd_weight, 2)
  g[, weight := round(weight, 2)]
  data.table::setorderv(g, cols='question_z', order=-1)
  #corpustools::browse_texts(tc, doc_ids = c(215,100))
  #hist(g$weight)
  g  
}

plagiarism_suspects <- function(g) {
  pp = subset(g, select = c('from_candidate', 'question_z','question'))
  data.table::setorderv(pp, cols='question_z', order= -1)
  pp = unique(pp, by=c('from_candidate','question'))
  pp = pp[,list(average_z = round(mean(question_z),2)), by='from_candidate']
  data.table::setorderv(pp, cols = 'average_z', order = -1)
  pp
}

get_field_suggestion <- function(opts, cn) {
  for (o in opts) {
    g = grepl(o, cn, ignore.case = T)
    if (sum(g) > 0) 
      return(cn[g][1])
  }
  return(NULL)
}

#' @import data.table
highlight_text <- function(input, output, tc, sim, sa, max_ngrams=5) {
  ## sa = suspicious answers
  if (max_ngrams < 2) stop('max_ngrams must be at least 2')
  if (is.null(sa) || is.null(input$suspicious_answers_rows_selected)) {
    output$txt_x = shiny::renderText('')
    output$txt_y = shiny::renderText('')
    return(NULL)
  } 
  
  
  .student = sa$Student[input$suspicious_answers_rows_selected]
  .question = sa$Question[input$suspicious_answers_rows_selected]
  edges = sim[list(.student, .question),,on=c('from_candidate','question')]
  
  x_docs = unique(edges$from)
  y_docs = unique(edges$to)

  tc = subset(tc, subset_meta = doc_id %in% union(x_docs, y_docs))
  for (i in 2:max_ngrams)
    tc$preprocess('feature', new_column = paste0('ngram', i), ngrams=i)
  
  
  x = droplevels(tc$get(doc_id = x_docs))
  x_meta = droplevels(tc$get_meta(doc_id = x_docs))
  x_meta$Question = .question
  print(x_meta)
  
  y = droplevels(tc$get(doc_id = y_docs))
  y_meta = droplevels(tc$get_meta(doc_id = y_docs))
  
  y_meta = merge(y_meta, 
                 data.table::data.table(candidate=edges$to_candidate, Similarity=edges$weight), by='candidate')
  data.table::setorderv(y_meta, 'Similarity', -1)
  
  ## workaround for bug in tokenbrowser (solved in 0.1.3, but not yet on cran)
  y$doc_id = match(y$doc_id, unique(y$doc_id))
  y_meta$doc_id = 1:nrow(y_meta)
  
  
  y$highlight = NA
  ng=5
  for (ng in 2:max_ngrams) {
    x_feature = x[[paste0('ngram', ng)]]
    y_feature = y[[paste0('ngram', ng)]]
    
    y_match = which(y_feature %in% levels(x_feature))
    if (ng > 1) {
      y_match = unique(unlist(lapply(y_match, function(i) {
        i_ngram = (i-ng+1):i
        i_ngram = i_ngram[i_ngram > 0]
        same_doc = y$doc_id[i] == y$doc_id[i_ngram]
        i_ngram[same_doc]
      })))
    }
    y$highlight[y_match] = (ng / max_ngrams)
  }
  y$highlight[!is.na(y$highlight)] = tokenbrowser::rescale_var(y$highlight[!is.na(y$highlight)], new_min = -1, new_max = 1, x_min = 0, x_max = 1)
  
  
  
  ## alternative, highlight by tfidf (requires tc_add_idf above)
  #y$highlight = NA
  #y_match = y$feature %in% unique(x$feature)
  #y$highlight = ifelse(y_match, y$tfidf, NA)
  #y$highlight[!is.na(y$highlight)] = tokenbrowser::rescale_var(y$highlight[!is.na(y$highlight)]^2, new_min = -1, new_max = 1)
  
  #browser()
  

  #x$token = tokenbrowser::highlight_tokens(x$token, x$highlight, col = 'lightyellow')
  #y$token = tokenbrowser::highlight_tokens(y$token, y$highlight, col = 'lightyellow')

  
  #x$token = tokenbrowser::colorscale_tokens(x$token, x$highlight, alpha = 0.3, col_range = c('lightyellow','blue'), span_adjacent = T, doc_id=x$doc_id)
  y$token = tokenbrowser::colorscale_tokens(y$token, y$highlight, alpha = 0.3, col_range = c('lightyellow','blue'), span_adjacent = T, doc_id = y$doc_id)
  
  
  if ('space' %in% colnames(x)) {
    x$token = paste(x$token, x$space, sep='')
    y$token = paste(y$token, y$space, sep='')
  }
  
  colnames(x_meta)[colnames(x_meta) == 'candidate'] = 'Student'
  colnames(y_meta)[colnames(y_meta) == 'candidate'] = 'Student'
  #x$doc_id = x_meta$Question
  #x_meta$doc_id = x_meta$Question
  xdoc = tokenbrowser::wrap_documents(x, subset(x_meta, select = c('doc_id','Student','Question')))
  #xdoc = tokenbrowser:::add_tag(tokenbrowser:::wrap_tokens(x), 'answer')
  #xdoc = tokenbrowser:::wrap_tokens(x)
  ydoc = tokenbrowser::wrap_documents(y, subset(y_meta, select = c('doc_id','Student','Similarity')))
  if (length(xdoc) > 0) xdoc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', xdoc)
  if (length(ydoc) > 0) ydoc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', ydoc)
  
  
  ## max 2 consequtive hard enter
  xdoc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', xdoc)
  ydoc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', ydoc)
  
  ## remove \s (used in udpipe)
  xdoc = gsub('\\s', '', xdoc, fixed=T)
  ydoc = gsub('\\s', '', ydoc, fixed=T)
  
  output$txt_x = shiny::renderText(xdoc)
  output$txt_y = shiny::renderText(ydoc)
}




