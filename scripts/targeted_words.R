# This script is for counting targeted words in all the
# review papers of "Pyrogeography


# To run this script, the working directory need to be set as
# "./articles". It should be working as "./data/articles"
# but somewhow it's not working !! No clue, most likely 
# related to Rstudio version of my laptop!
#############################################################################
# Required library
#############################################################################

library(stringr)  
library(dplyr)
library(pdftools)
library(tm)

# This function will grab the targeted word
# Initially I converted all the articles to pdf file


pyrogeography_word_counts <- function(text, pyrogeography_words) {
  text <- tolower(text)
  counts <- sapply(pyrogeography_words, function(word) {
    word_count <- str_count(text, regex(paste0(word)))
    return(sum(word_count))
  })
  return(counts)
}

# Loading all the articles at once

file_list <- list.files(path = getwd(), 
                        pattern = "\\.pdf$", 
                        full.names = FALSE)


file_list


###################################################################
# Target words, new words can be added here
###################################################################

pyrogeography_words <- c("human", "fire", "climate",
                         "fire regime", "change",
                         "effect", "pattern", 
                         "pyrogeography",
                         "anthropocene", "pyrodiversity",
                         "pyrome", "spatial",
                         "temporal")

# Initialize an empty list to store the counts for each file
file_counts <- list()


# Loop through the file list and process each file
for (i in 1:length(file_list)) {
  file_text <- pdf_text(file_list[i])
  
  # Count the occurrences of target words in the file and store the counts in a named list
  counts <- pyrogeography_word_counts(file_text, 
                                      pyrogeography_words)
  file_counts[[file_list[i]]] <- counts
   
  # Print the counts for each word in the current file
  cat("File:", file_list[i], "\n")
  for (j in 1:length(pyrogeography_words)) {
    cat(pyrogeography_words[j], ":", counts[j], "\n")
  }
  cat("\n")
}


print(file_counts)

class(file_counts)

total_count <- apply(as.data.frame(file_counts), 1, sum)

# Converting the list to data frame

data_frame <- t(as.data.frame(file_counts))

# Shaping the data frame for plotting

reshaped_data_frame <- reshape2::melt(data_frame, value.name = "frequencey")
names(reshaped_data_frame)

# Renaming the variables

final_data_for_plotting <- reshaped_data_frame %>%
  rename(targeted_words = Var2) %>%
  rename(individual_article = Var1) %>%
  group_by(targeted_words) %>%
  mutate(total_counts = sum(frequencey))

View(final_data_for_plotting)


#######################################################################
# This part is for filtering the most frequent words in all 
# the pdf files
# The source of the following codes 
# https://www.youtube.com/watch?v=1ODVAOWkajw
########################################################################

reading_each_file <- sapply(file_list, pdf_text) # reading all files

length(reading_each_file) # checking how many of them I read 

lapply(reading_each_file, length) # how many files per each files


# Creating the pdf database

pdfdatabase <- Corpus(URISource(file_list),
                      readerControl = list(reader = readPDF))



pdfdatabase



most_frequent_word <- TermDocumentMatrix(pdfdatabase,
                                         control = list(removePunctuation = TRUE,
                                                        stopwords = TRUE,
                                                        tolower = TRUE,
                                                        stemming = FALSE,
                                                        removeNumbers = TRUE,
                                                        bounds = list(global = c (3, Inf))))




inspect(most_frequent_word[1:20, ])

class(most_frequent_word)

View(most_frequent_word)

most_frequent_word

at_least_three_times <- findFreqTerms(most_frequent_word,
                   lowfreq = 3, highfreq = Inf)



sorted_at_least_three_times <- sort(apply(as.matrix(most_frequent_word[at_least_three_times,]), 1, sum), 
          decreasing = TRUE)

class(sorted_at_least_three_times)

View(sorted_at_least_three_times)

most_frequent_words <- as.data.frame(sorted_at_least_three_times)

#View(most_frequent_words)

most_frequent_words <- most_frequent_words %>%
  rename(frequency = sorted_at_least_three_times) %>%
  filter(frequency >= 1000)

#View(most_frequent_words)

#write.csv(most_frequent_words,
          #file = "./data/most_frequent_words.csv",
          #row.names = TRUE)
