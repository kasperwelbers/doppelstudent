rm_html <- function(x) {
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

get_tcorpus <- function(d) {
  #d = readr::read_csv('~/Downloads/antwoorden open vragen door arjen er uit te halen.csv')
  d = d[d$QuestionType == 'OpenEnded',]
  
  d = create_unique_label(d, 'candidate', 'CandidateId', 'CandidateDisplayName')
  d = create_unique_label(d, 'question', 'QuestionId', 'QuestionName')

  a = d[,c('candidate','question','answer')]
  a$answer = rm_html(a$answer)
  
  a$answer = stringi::stri_trim(a$answer)
  a$answer[is.na(a$answer)] = ''
  tc = corpustools::create_tcorpus(a, text_col='answer', remember_spaces=T)
  tc$preprocess('token','feature', lowercase = T, as_ascii = T, remove_punctuation = F, remove_stopwords = F)
  tc$tokens$feature[tc$tokens$feature %in% c(',','.')] = NA
  tc
}

#' @import data.table
get_question_sim <- function(tc) {
  if (is.null(tc)) return(NULL)
  
  g = corpustools::compare_documents(tc, 'feature', meta_cols = 'question', min_similarity = 0.05, measure = 'overlap_pct', return_igraph = F)$d
  
  from_i = match(g$from, tc$meta$doc_id)
  to_i = match(g$to, tc$meta$doc_id)
  g$question = tc$meta$question[from_i]
  g$from_candidate = tc$meta$candidate[from_i]
  g$to_candidate = tc$meta$candidate[to_i]

  g = data.table::as.data.table(g)
  g[, question_mean_weight := mean(weight), by='question']  
  g[, question_sd_weight := sd(weight), by='question']  
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

function() {
  tc = get_tcorpus()
  sim = get_sim(tc)
  tc
  sim
}

calc_sim <- function(x, y) {
  d = readr::read_csv('~/Downloads/antwoorden open vragen door arjen er uit te halen.csv')
  a = process_raw_input(d)
  
  candidates = colnames(a)[3:ncol(a)]
  n = length(candidates)

  length(candidates)
  ncol(a)
  
  maxsim = sapply(1:n, function(i) {
    txt = a[q, 2+i, drop=T]
    txt = rm_html(txt)
    sim = tryCatch(textreuse::align_local(txt,txt)$score, error = function(e) NA)
  })
    
  d = data.frame(i = rep(1:n, each=n),
                 j = rep(1:n, n))
  d = d[d$i < d$j,]
  
  x = purrr::map2_dfr(d$i, d$j, function(i, j) {
    text1 = a[q,2+i,drop=T]
    text2 = a[q,2+j,drop=T]
    sim = tryCatch(textreuse::align_local(text1,text2), error = function(e) NULL)
    if (is.null(sim)) return(NULL)
    drow = tidyr::tibble(i=i, j=j, overlap=sim$score, overlap_i = sim$score / maxsim[i], overlap_j = sim$score / maxsim[j])
    drow$highest = max(drow$overlap_i, drow$overlap_j)
    drow
  })
  
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
  
  y = droplevels(tc$get(doc_id = y_docs))
  y_meta = droplevels(tc$get_meta(doc_id = y_docs))
  
  ## workaround for bug in tokenbrowser (solved in 0.1.3, but not yet on cran)
  y$doc_id = match(y$doc_id, unique(y$doc_id))
  y_meta$doc_id = 1:nrow(y_meta)
  
  
  y$highlight = NA
  for (ng in 2:max_ngrams) {
    x_feature = x[[paste0('ngram', ng)]]
    y_feature = y[[paste0('ngram', ng)]]
    
    x_match = which(x_feature %in% levels(y_feature))
    y_match = which(y_feature %in% levels(x_feature))
    if (ng > 1) {
      x_match = unique(unlist(lapply(x_match, function(x) (x-ng+1):x)))
      y_match = unique(unlist(lapply(y_match, function(x) (x-ng+1):x)))
      x_match = x_match[x_match > 0]
      y_match = y_match[y_match > 0]
    }
    y$highlight[y_match] = (ng / max_ngrams)^3
  }

  
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
  xdoc = tokenbrowser::wrap_documents(x, subset(x_meta, select = c('doc_id','Student')))
  ydoc = tokenbrowser::wrap_documents(y, subset(y_meta, select = c('doc_id','Student')))
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


