rm(list = ls())

if (grepl("/data-raw$", getwd())) setwd("..")
stopifnot(grepl("econpanel$", getwd()))

library(rvest)

# Functions

# Expand a survey page ID to its URL
survey_url <- function(id) {
  paste0("http://www.igmchicago.org/surveys/", id)
}

# Restrict a survey page URL to its ID
survey_id <- function(url) {
  gsub("^http://www.igmchicago.org/surveys/", "", url)
}

# Scrape the home page for survey page URLs
get_surveys <- function(panel = "USA") {
  
  # get corresponding URL
  panel <- match.arg(tolower(panel), c("usa", "europe"))
  url <- c(
    usa = "http://www.igmchicago.org/igm-economic-experts-panel",
    europe = "http://www.igmchicago.org/european-economic-experts-panel"
  )[panel] %>% unname()
  
  # get surveys from URL
  page <- read_html(url)
  
  # SelectorGadget
  elts <- html_nodes(page, "h2 a")
  dates <- html_nodes(page, "h6")
  
  data.frame(
    topic = elts %>% html_text(),
    id = elts %>% html_attr("href") %>% survey_id(),
    date = dates %>% html_text() %>%
      gsub(pattern = "^[A-Za-z]+, ([A-Z].*[0-9]) [0-9]{1,2}\\:.*$",
           replacement = "\\1") %>%
      gsub(pattern = " ([0-9]+)[a-z]{2},", replacement = " \\1,") %>%
      as.Date(format = "%B %d, %Y")
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
pool.na <- function(vote) {
  vote[!grepl('[Aa]gree|Uncertain', vote)] <- NA
  vote
}

# Scrape a survey page for panelist responses, separating multiple questions
get_responses <- function(url) {
  
  page <- read_html(url)
  
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
      vote = pool.na(vote),
      confidence = conf,
      comment = comm,
      question = if(length(ques) > 1) LETTERS[r + 1] else "",
      statement = ques_text[r + 1]
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

# Scrape the website for new surveys and append to existing dataset
update_data <- function(data, surveys, prompt = TRUE, chill = TRUE) {
  
  if (prompt) {
    if (Sys.Date() < max(as.Date(surveys$date, format = "%B %d, %Y")) + 14) {
      res <-
        readline("Last question is < 1 fortnight old; update anyway? (y/n)")
      if (grepl("^[Yy]", res[1])) {
        res <- TRUE
      } else {
        if (!grepl("^[Nn]", res[1]))
          warning("Not a yes/no response; assuming no")
        res <- FALSE
      }
    } else res <- TRUE
  }
  
  # Scrape, bind, and augment data for each survey
  for (i in 1:nrow(surveys)) {
    if (surveys$id[i] %in% data$id) next
    url <- survey_url(id = surveys$id[i])
    new_data <- get_responses(url)
    new_data <- cbind(id = surveys$id[i],
                      date = surveys$date[i],
                      topic = surveys$topic[i],
                      new_data)
    data <- dplyr::bind_rows(data, new_data)
    if (chill) {
      Sys.sleep(5)
      print(paste0("Extracted data for '", surveys$topic[i], "'..."))
    }
  }
  
  # Factorize categorical variables
  for (var in c("panelist", "uni", "vote", "statement")) {
    data[[var]] <- as.factor(data[[var]])
  }
  
  data
}

# Scrape the website for all available surveys (fresh)
get_data <- function(surveys, ...) {
  data <- data.frame()
  update_data(data, surveys, prompt = FALSE, ...)
}

# Script for USA panel

#data_file <- "data/igm.rds"
#if (file.exists(data_file)) igm <- readRDS(data_file)
if (file.exists("data/igm.rda")) load("data/igm.rda")

# get surveys
surveys <- get_surveys()

if (!exists("igm")) {
  # scrape fresh
  igm <- get_data(surveys)
} else {
  # scrape for updates
  igm <- update_data(igm, surveys)
}

# sort by date > question > panelist
igm <- dplyr::arrange(igm, date, question, panelist)

#saveRDS(igm, data_file)
devtools::use_data(igm, overwrite = TRUE)

# Script for Europe panel

#data_file <- "data/eigm.rds"
#if (file.exists(data_file)) igm <- readRDS(data_file)
if (file.exists("data/eigm.rda")) load("data/eigm.rda")

# get surveys
surveys <- get_surveys(panel = "Europe")

if (!exists("eigm")) {
  # scrape fresh
  eigm <- get_data(surveys)
} else {
  # scrape for updates
  eigm <- update_data(eigm, surveys)
}

# sort by date > question > panelist
eigm <- dplyr::arrange(eigm, date, question, panelist)

#saveRDS(eigm, data_file)
devtools::use_data(eigm, overwrite = TRUE)
