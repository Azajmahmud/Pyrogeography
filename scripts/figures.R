######################################
# Pyrogeography
# Azaj Mahmud

# This script is for producing figures 
# and saving the filtered data as csv file!



source("./scripts/targeted_words.R")

#############################################################################
# This script is for creating plots
#############################################################################

############################################################################
# Required libraries
############################################################################
library(ggplot2)



############################################################################
# Reading the data
###########################################################################

percentage_of_publication_by_field <- read.csv("./data/percentage_of_publication_by_fields.csv")
number_of_publication_by_year <- read.csv("./data/number_of_publication_by_year.csv")


##############################################################
# Number of publications by disciplines
#############################################################

percentage_of_publications_by_fields_plot <- ggplot(percentage_of_publication_by_field, aes(x = reorder(web_of_science_categories,
                                                                                                        -record_count), 
                                                                                            y = record_count, 
                 fill = web_of_science_categories)) +
  geom_bar(stat = "identity") +
  labs(x = "Web of Science Categories",
       y = "Record Count",
       fill = "Web of Science Categories") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold")) 


ggsave("./results/percentage_of_publication_by_fields.pdf",
       percentage_of_publications_by_fields_plot, height = 230,
       width = 250, units = "mm", dpi = 300)


################################################################################################
# Number of publications by year
###############################################################################################


number_of_publication_by_year_plot <- ggplot(number_of_publication_by_year, aes(x = year, y = number_of_publication)) +
  geom_line() +
  labs( x = "Year",
        y = "Number of Publications") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, max(number_of_publication_by_year$number_of_publication), by = 2), labels = scales::comma) +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"))

ggsave("./results/number_of_publication_by_year_plot.pdf",
       number_of_publication_by_year_plot, height = 220,
       width = 250, units = "mm", dpi = 300)


#############################################################
# Targeted words plots
#############################################################

targeted_words_plot <- ggplot(final_data_for_plotting,
                              aes(x = reorder(targeted_words, -frequencey),
                                  y = frequencey,
                                color = targeted_words)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Targeted words",
       y = "Mean frequuency",
       color = "Targeted words") +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45, 
                                   face = "bold",
                                   hjust = 1),
        axis.text.y = element_text(size = 8,
                                   face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))


targeted_words_plot


ggsave("./results/targeted_words_plot.pdf",
       plot = targeted_words_plot, height = 200,
       width = 260, units = "mm", dpi = 300)


total_counts_of_targeted_words <- ggplot(final_data_for_plotting,
                                         aes(x = reorder(targeted_words, - total_counts), 
                                             y = total_counts,
                                             fill = targeted_words)) +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() +
  labs(x = "Targeted words",
       y = "Total counts",
       fill = "Targeted words") +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45, 
                                   face = "bold",
                                   hjust = 1),
        axis.text.y = element_text(size = 8,
                                   face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))

total_counts_of_targeted_words

ggsave("./results/total_counts_of_targeted_words.pdf",
       plot = total_counts_of_targeted_words, height = 200,
       width = 260, units = "mm", dpi = 300)

############################################################
# Saving the most frequent and targeted words as csv
############################################################

write.csv(most_frequent_words, 
          file = "./results/most_frequent_words.csv",
row.names = TRUE)

write.csv(final_data_for_plotting, 
file = "./results/targeted_words_for_plotting.csv")

#########################################################################
