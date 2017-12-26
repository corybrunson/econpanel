# Expand a survey page ID to its URL
survey_id_url <- function(id) {
  paste0("http://www.igmchicago.org/surveys/", id)
}

# Restrict a survey page URL to its ID
survey_url_id <- function(url) {
  gsub("^http://www.igmchicago.org/surveys/", "", url)
}

# Expand a survey page ID to its file name
survey_id_file <- function(id) {
  here("data-raw/surveys", paste0(id, ".html"))
}

# task prompt
prompt_task <- function(...) {
  message(...)
  invisible(readline("When done, hit 'Return'/'Enter'."))
}

# prompt if file is missing or old
prompt_file <- function(file, days = 1) {
  if (!file.exists(file)) {
    prompt_task(
      "The survey file '", file, "' does not exist.\n",
      "Refresh it from the website."
    )
  }
  if (Sys.time() > file.mtime(file) + lubridate::days(days)) {
    prompt_task(
      "The survey file '", file, "' is over ", days, " day(s) old.\n",
      "If necessary, refresh it from the website."
    )
  }
}

# Scrape downloaded JS home page
read_surveys <- function(panel = "USA") {
  
  # get corresponding HTML file name
  panel <- match.arg(tolower(panel), c("usa", "europe"))
  file <- here("data-raw", switch(
    panel,
    usa = "igm-economic-experts-panel.html",
    europe = "european-economic-experts-panel.html"
  ))
  prompt_file(file)
  
  # read surveys from saved HTML file
  doc <- XML::htmlParse(file)
  topics <- XML::xpathSApply(doc, "//h2/a", XML::xmlValue)
  links <- unname(XML::xpathSApply(doc, "//h2/a/@href"))
  dates <- XML::xpathSApply(doc, "//h6", XML::xmlValue)
  XML::free(doc)
  
  # keep only standard surveys (exclude special surveys)
  keep <- grep("/surveys/", links)
  
  data.frame(
    topic = topics[keep],
    id = links[keep] %>% survey_url_id(),
    date = dates[keep] %>%
      gsub(pattern = "^[A-Za-z]+, ([A-Z].*[0-9]) [0-9]{1,2}\\:.*$",
           replacement = "\\1") %>%
      gsub(pattern = " ([0-9]+)[a-z]{2},", replacement = " \\1,") %>%
      as.Date(format = "%B %d, %Y"),
    stringsAsFactors = FALSE
  )
  
}

# Combine ".surveyQuestion" and "p" info into vector of question texts
list_questions <- function(ques, p, url) {
  
  # Extract texts
  ques_text <- as.vector(ques %>% html_text())
  p_text <- if (length(p) > 6) {
    as.vector(p[1:(length(p) - 6)] %>% html_text())
  } else c()
  
  # Clean texts
  re <- if (length(ques) == 1) "^\\n *([A-Z].*)$" else {
    "^\\n *Question [A-D]:\\n *([A-Z].*)$"
  }
  ques_text <- gsub(re, "\\1", ques_text)
  p_text <- p_text[gsub("\\n", "", p_text) != ""]
  
  # p must contain exactly as many statements as are missing from ques
  ques_grep <- grep("^\\n *(Question [A-D]:\\n *){0,1}", ques_text)
  if (length(p_text) != length(ques_grep)) {
    print(url)
    stop()
  }
  
  # Fill missing ques entries with p
  if (length(ques_grep) > 0) ques_text[ques_grep] <- p_text
  
  # More cleaning
  ques_text <- gsub("\\n", " ", ques_text)
  ques_text <- gsub(" *$", "", ques_text)
  
  ques_text
}

# Put last name first
name_alph <- function(name) {
  paste(gsub('^(.*) ([^ ]+)$', '\\2, \\1', name))
}

# Collapse NA vote options
pool_na <- function(vote) {
  vote[!grepl('[Aa]gree|Uncertain', vote)] <- NA
  vote
}

# Scrape a survey page for panelist responses, separating multiple questions
read_responses <- function(id) {
  
  file <- survey_id_file(id)
  prompt_file(file)
  
  page <- read_html(file)
  
  # Count the number of questions
  ques <- html_nodes(page, ".surveyQuestion")
  p <- html_nodes(page, "p")
  ques_text <- list_questions(ques, p, url)
  
  # Collect responses for each question
  dats <- lapply(0:(length(ques) - 1), function(r) {
    
    # Collect and clean fields
    panelist <- page %>%
      html_nodes(paste0("#sort", r, " .response-name")) %>%
      html_text() %>%
      gsub(pattern = "(\\n|\\t)", replacement = "") %>%
      name_alph
    uni <- page %>%
      html_nodes(paste0("#sort", r, " .parent-row td:nth-child(2)")) %>%
      html_text()
    edit <- page %>%
      html_nodes(paste0("#sort", r,
                        " .tablesorter-childRow td:nth-child(1)")) %>%
      html_text()
    vote <- page %>%
      html_nodes(paste0("#sort", r, " span")) %>%
      html_text() %>%
      gsub(pattern = "(\\n|\\t)", replacement = "")
    conf <- page %>%
      html_nodes(paste0("#sort", r, " .confCell")) %>%
      html_text() %>%
      as.numeric()
    comm <- page %>%
      html_nodes(paste0("#sort", r, " .gridComment")) %>%
      html_text() %>%
      gsub(pattern = "(\\n|\\t)", replacement = "")
    
    # Shift cells back when late joiners are added
    late <- grep("^Joined", edit)
    if(length(late) > 0) {
      vote <- vote[-late]
      conf <- conf[-late]
      comm <- comm[-late]
    }
    
    # Use only revotes when they are cast
    revote <- grep("^Revote", edit)
    if(length(revote) > 0) {
      vote <- vote[-(revote - 1)]
      conf <- conf[-(revote - 1)]
      comm <- comm[-(revote - 1)]
    }
    
    # Combine into a data frame
    res <- data.frame(
      panelist = panelist,
      uni = uni,
      vote = pool_na(vote),
      confidence = conf,
      comment = comm,
      question = if(length(ques) > 1) LETTERS[r + 1] else "",
      statement = ques_text[r + 1],
      stringsAsFactors = FALSE
    )
    
    # Remove rows with missing panelists
    res <- dplyr::filter(res, panelist != "")
    
    res
  })
  
  # Check that no more responses remain
  panelist <- page %>%
    html_nodes(paste0("#sort", length(ques), " .response-name a"))
  if(length(panelist) > 0)
    stop("Conflicting question and response counts")
  
  dplyr::bind_rows(dats)
}

update_data <- function(data, surveys) {
  
  new_rows <- which(!(surveys$id %in% data$id))
  
  peb <- dplyr::progress_estimated(length(new_rows), 2)
  for (i in new_rows) {
    peb$tick()$print()
    new_data <- data.frame(
      id = surveys$id[i],
      date = surveys$date[i],
      topic = surveys$topic[i],
      read_responses(surveys$id[i]),
      stringsAsFactors = FALSE
    )
    data <- dplyr::bind_rows(data, new_data)
  }
  
  # Factorize categorical variables
  for (var in c("panelist", "uni", "vote", "statement")) {
    data[[var]] <- as.factor(data[[var]])
  }
  
  data
}
