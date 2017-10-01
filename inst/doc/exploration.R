## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE,
                      fig.width=9, out.width='90%', fig.align='center')
library(econpanel)
library(dplyr)
library(ggtern)

## ----Planet Money data---------------------------------------------------
data(planetmoney)

## ----table of Planet Money votes-----------------------------------------
# piping "%>%" implemented in magrittr, imported in dplyr
knitr::kable(planetmoney %>% select(-Brief) %>% distinct)

## ----Likert plot of Planet Money votes, fig.height=4---------------------
n <- unique(rowSums(planetmoney[, c("Bad", "Debatable", "Good")]))
stopifnot(length(n) == 1)
planetmoney_likert <-
  likert::likert(
    summary = planetmoney[, c("Brief", "Bad", "Debatable", "Good")] %>%
      transmute(Item = Brief,
                Bad = Bad * 100 / n,
                Debatable = Debatable * 100 / n,
                Good = Good * 100 / n)
  )
plot(planetmoney_likert)

## ----ternary plot of Planet Money votes, fig.height=6--------------------
# introduce political party variable
planetmoney <- planetmoney %>%
  mutate(Party = ifelse(grepl("Clinton|Sanders", Candidates),
                        ifelse(grepl("Trump|Rubio|Cruz", Candidates),
                               "Both", "Democrat"), "Republican"))
planetmoney_plot <- ggplot(data = planetmoney,
                           aes(x = Good, z = Bad, y = Debatable,
                               label = Brief, color = Party)) +
  coord_tern() +
  geom_mask() +
  geom_point(size = 8, alpha = .25) +
  scale_color_manual(values = c("purple", "blue", "red")) +
  geom_text() +
  theme_bw() + theme_showarrows() + theme_rotate(-90)
print(planetmoney_plot)

## ----IGM data------------------------------------------------------------
data(igm)

## ----table of Greece-relevant votes--------------------------------------
igm_greece <- igm %>%
  filter(grepl("Gree(k|ce)", topic) | grepl("Gree(k|ce)", statement))
knitr::kable(igm_greece %>%
               select(date, topic, question, statement) %>%
               distinct)

## ----Likert plot of Greece-relevant votes, fig.height=3.5----------------
# construct a matrix of Likert responses
igm_matrix <- reshape2::dcast(igm_greece,
                              panelist ~ topic + question,
                              value.var = "vote")
rownames(igm_matrix) <- igm_matrix$panelist
igm_matrix$panelist <- NULL
# factorize the entries
levs <- c("Strongly Disagree", "Disagree",
          "Uncertain",
          "Agree", "Strongly Agree")
for (j in 1:ncol(igm_matrix))
  igm_matrix[[j]] <- factor(igm_matrix[[j]], levels = levs)
# Likertify
igm_likert <- likert::likert(as.data.frame(igm_matrix))
plot(igm_likert)

## ----principal components plot of IGM panelists, fig.height=5------------
igm_matrix <- as.matrix(igm_matrix)
# restrict to authors who answered each question
igm_matrix <- igm_matrix[apply(igm_matrix, 1, function(x) all(!is.na(x))), ]
# numericize
vote_numbers <- c(`Strongly Disagree` = -1.5,
                  Disagree = -1,
                  Uncertain = 0,
                  Agree = 1,
                  `Strongly Agree` = 1.5)
igm_matrix[1:length(igm_matrix)] <- vote_numbers[as.vector(igm_matrix)]
class(igm_matrix) <- "numeric"
igm_pca <- prcomp(igm_matrix)
# percent variance explained by each PC
igm_var <- (cumsum(igm_pca$sdev ^ 2) / sum(igm_pca$sdev ^ 2)) %>% as.table
names(igm_var) <- colnames(igm_pca$rotation)
knitr::kable(t(as.matrix(igm_var)), digits = 3)

## ----principal components biplot of IGM panelists, fig.height=7----------
# construct a PCA biplot
stack_names <- gsub("^([^,]*), ([A-Z])[^,]*$", "\\2. \\1", rownames(igm_pca$x))
ggbiplot::ggbiplot(igm_pca, obs.scale = 1, var.scale = 1,
                   labels = stack_names, labels.size = 4, alpha = 1) +
  theme_bw()

## ----ternary plot of IGM answers, fig.height=6---------------------------
# trichotimize votes
igm$vote_tern <- as.character(igm$vote)
igm$vote_tern[grep("Agree", igm$vote_tern)] <- "Agree"
igm$vote_tern[grep("Disagree", igm$vote_tern)] <- "Disagree"
# tally the votes for each question
panel <- reshape2::dcast(igm,
                         topic + question + statement ~ vote_tern,
                         value.var = "vote_tern",
                         fun.aggregate = length)
# construct a ternary plot
igm_plot <- ggplot(data = panel,
                   aes(x = Agree, z = Disagree, y = Uncertain)) +
  coord_tern(expand = TRUE) +
  geom_mask() +
  geom_point(alpha = .25, aes(size = Agree + Disagree + Uncertain)) +
  scale_size_area(name = "Voting panelists") +
  theme_bw() + theme_showarrows() + theme_rotate(-90)
print(igm_plot)

