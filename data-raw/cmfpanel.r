if (grepl("/data-raw$", getwd())) setwd("..")
stopifnot(grepl("econpanel$", getwd()))

library(rvest)

# Functions

# Expand a survey page ID to its URL
survey_url <- function(id) {
  paste0("http://cfmsurvey.org/surveys/", id)
}

# Restrict a survey page URL to its ID
survey_id <- function(url) {
  gsub("http://cfmsurvey.org/surveys/(.*)$", "\\1", url)
}

# Scrape the home page for survey page URLs
get_surveys <- function(chill = TRUE) {
  
  pages <- list()
  page <- read_html("http://cfmsurvey.org/surveys")
  i <- 0
  while ((page %>% html_nodes("h2 a") %>% length) > 0) {
    if (chill) {
      Sys.sleep(5)
      print(paste0("Extracted page ", i, " of surveys"))
    }
    pages <- c(pages, list(page))
    i <- i + 1
    page <- read_html(paste0("http://cfmsurvey.org/surveys?page=", i))
  }
  
  # SelectorGadget
  topics <- unlist(lapply(lapply(pages, html_nodes, css = "h2 a"), html_text))
  ids <- unlist(lapply(lapply(pages, html_nodes, css = "h2 a"),
                          html_attr, name = "href"))
  dates <- unlist(lapply(lapply(pages, html_nodes, css = "p span"),
                         html_attr, name = "content"))

  data.frame(
    topic = gsub("[ |\t|\n|\r]+", " ", topics),
    id = gsub("^/surveys/", "", ids),
    date = format(as.Date(dates), format = "%B %d, %Y")
  )
}

# Put last name first
name_alph <- function(name) {
  paste(gsub('^(.*) ([^ ]+)$', '\\2, \\1', name))
}

# Standardize Likert answers
std_answer <- function(answer) {
  answer <- as.character(answer)
  answer[grep("[Nn]either.*[Nn]or", answer)] <- "Neither agree nor disagree"
  answer[grep("[Nn]o [Oo]pinion", answer)] <- "No opinion"
  answer[grep("[Ss]trongly[^A-Za-z]*[Dd]isagre", answer)] <- "Strongly disagree"
  answer[grep("[Ss]trongly[^A-Za-z]*[Aa]gre", answer)] <- "Strongly agree"
  answer[grep("^[^A-Za-z]*[Dd]isagre", answer)] <- "Disagree"
  answer[grep("^[^A-Za-z]*[Aa]gre", answer)] <- "Agree"
  answer
}

# Scrape a survey page for panelist responses, separating multiple questions
get_responses <- function(url) {
  
  page <- read_html(url)
  
  # Get the question briefs (table tabs)
  briefs <- page %>%
    html_nodes("div h3") %>%
    html_text() %>%
    gsub(pattern = "(^\n +)|( +$)", replacement = "")
  
  # Get response data frames (tables)
  dats <- page %>%
    html_nodes(css = "div table") %>%
    html_table()
  stopifnot(length(dats) + 2 == length(briefs))
  
  # Get the question texts
  questions <- page %>%
    html_nodes(".node-question p") %>%
    html_text()
  if (all(nchar(questions) <= 20)) {
    questions <- page %>%
      html_nodes(".node-question div") %>%
      html_text()
  }
  wh_q <- which(nchar(questions) <= 20 & grepl("[Qq]uestion", questions))
  if (length(wh_q) > 0) {
    questions[wh_q] <- paste(questions[wh_q], questions[wh_q + 1])
    questions <- questions[-(wh_q + 1)]
  }
  questions <- questions[!grepl(
    "^[ \n\r\t]*([Qq]uestion.[0-9]|[A-Za-z]+ [Qq]uestion)[ \n\r\t:]*$",
    questions
  )]
  if (length(questions) + 2 > length(briefs)) {
    questions <- questions[
      grepl("Question", questions) &
        nchar(questions) > 20 &
        !grepl("=+", questions)
      ]
  }
  questions <- gsub("^.*[Qq]uestion[^A-Za-z0-9]*[0-9]*:*[^A-Za-z0-9]*", "",
                    questions)
  questions <- gsub("[ \r\n\t]+$", "", questions)
  stopifnot(length(briefs) == length(questions) + 2)
  
  # Append question info
  dats <- lapply(1:length(questions), function(i) {
    dplyr::mutate(
      dats[[i]],
      question = if(length(questions) > 1) as.character(i) else "",
      brief = briefs[i + 1],
      statement = questions[i]
    )
  })
  
  # Omit questions that use a scale other than agreement--disagreement
  wh_likert <- c()
  for (i in 1:length(dats)) {
    if (all(grepl("[Aa]gree|No [Oo]pinion", dats[[i]]$Answer))) {
      wh_likert <- c(wh_likert, i)
    } else {
      print(paste("Question", i, "from the survey at", url, "was omitted."))
    }
  }
  dats <- dats[wh_likert]
  
  # Combine into single data frame
  dats %>% dplyr::bind_rows() %>%
    dplyr::transmute(participant = name_alph(gsub("\n.*$", "", Participant)),
                     affiliation = gsub("^.*\n", "", Participant),
                     answer = std_answer(Answer),
                     confidence = `Confidence level`,
                     comment = gsub("[ |\t|\n|\r]+", " ", Comment),
                     question = question,
                     brief = brief,
                     statement = statement)
  
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
  for (j in 1:nrow(surveys)) {
    if (surveys$id[j] %in% data$id) next
    url <- survey_url(id = surveys$id[j])
    new_data <- get_responses(url)
    new_data <- cbind(id = surveys$id[j],
                      date = surveys$date[j],
                      topic = surveys$topic[j],
                      new_data)
    data <- dplyr::bind_rows(data, new_data)
    if (chill) {
      Sys.sleep(5)
      print(paste0("Extracted data for '", surveys$topic[j], "'."))
    }
  }
  
  # Factorize categorical variables
  for (var in c("participant", "affiliation", "answer",
                "question", "statement")) {
    data[[var]] <- as.factor(data[[var]])
  }
  
  data
}

# Scrape the website for all available surveys (fresh)
get_data <- function(surveys, ...) {
  data <- data.frame()
  update_data(data, surveys, prompt = FALSE, ...)
}

# Script

if (file.exists("data/cfm.rda")) load("data/cfm.rda")

# get surveys
surveys <- get_surveys()

if (!exists("cfm")) {
  # scrape fresh
  cfm <- get_data(surveys)
} else {
  # scrape for updates
  cfm <- update_data(cfm, surveys)
}

# sort by date > question > panelist
cfm <- dplyr::arrange(cfm, date, question, participant)

devtools::use_data(cfm, overwrite = TRUE)

rm(list = ls())
