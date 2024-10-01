
library(dplyr)
library(purrr)
library(wordcloud2)
library(ggplot2)
library(stringr)

getwd()

#UNITAS_answers <- read.csv("csv_outputs/UNITAS_answers_cleaned.csv")
UNITAS_answers_cleaned <- readRDS("r_helper_objects/UNITAS_answers_cleaned.RDS")


num_speeches = UNITAS_answers_cleaned %>% 
  filter(keyTopics5 != "") %>% nrow()


### WORD CLOUD
# key topics
words1 <- UNITAS_answers_cleaned %>% 
  filter(keyTopics5 != "") %>%
  pull(keyTopics5) %>% 
  tolower() %>% 
  paste(collapse = ", ") %>% 
  strsplit(", ") %>% unlist() %>% 
  table() %>% 
  as.data.frame() %>% 
  set_names("theme", "Freq")
words2 <- UNITAS_answers_cleaned %>% 
  filter(keyTopics5B != "") %>%
  pull(keyTopics5B) %>% 
  tolower() %>% 
  paste(collapse = ", ") %>% 
  strsplit(", ") %>% unlist() %>% 
  table() %>% 
  as.data.frame() %>% 
  set_names("theme", "Freq")

if(F){
  words1 %>% arrange(desc(Freq))
  # Create a word cloud with a eye shape
  wordcloud2(words1, size=0.4)
  figPath = "/Users/edbaker/UN_projects/UNITAS/GA23/rCode/eye.png"
  wordcloud2(words1, figPath = figPath, size = 1.5, color = "skyblue")
}


### GRAPHS
lollipop_plot = function(var="keyTopics5"){
  words <- UNITAS_answers_cleaned %>% 
    filter(!!sym(var)!= "") %>%
    pull(!!sym(var)) %>% 
    tolower() %>% 
    paste(collapse = ", ") %>% 
    strsplit(", ") %>% unlist() %>%
    gsub("[[:punct:]]", "", .) %>%
    table() %>% 
    as.data.frame() %>% 
    set_names("theme", "Freq")
  words %>%
    arrange(desc(Freq)) %>% 
    head(15) %>% 
    # Plot the lollipop chart
    ggplot(aes(x = reorder(theme, Freq), y = Freq)) +  # Reorder words based on frequency
    geom_segment(aes(xend = theme, y = 0, yend = Freq), color = "skyblue", linewidth = 1) +  # Create the stems
    geom_point(color = "blue", size = 4) +  # Create the lollipop heads
    coord_flip() +  # Flip the coordinates for a horizontal lollipop chart
    theme_minimal() +  # Use a minimal theme
    labs(title = NULL,
         x = NULL,
         y = "Frequency") +
    theme(text = element_text(size = 12))
}

if(F){
  lollipop_plot("keyTopics5")
  lollipop_plot("keyTopics5B")
  lollipop_plot("countries")
  lollipop_plot("concerns")
  lollipop_plot("crises")
  lollipop_plot("criticise")
  lollipop_plot("praise")
  lollipop_plot("historicalEvents")
  lollipop_plot("alliances")
  c("historicalEvents", "sanctionsSupporrt", "sanctionsCriticize")
}

bar_plot = function(var, group=1){ #var="conflict"; group="unctad_development_status"
  df = UNITAS_answers_cleaned %>% 
    filter(!!sym(var) != "")
  ncountries = nrow(df)
  df = df %>% 
    {
      if(group==1) {group_by(., !!sym(var))} else {filter(., !!sym(group) != "other") %>% group_by(!!sym(group), !!sym(var))}
    } %>% 
    summarise(count = n(), .groups = 'drop_last') %>%
    #ungroup() %>%  # for % as % of total ungroup here
    mutate(percent = count / sum(count)) %>% 
    ungroup()   #for % as e.g. % of women that mentioned gender issues ungroup here
  
  plot = df %>% 
    ggplot(aes(x = !!sym(var))) +
    {if(group == 1){
      geom_bar(aes(y = percent, group=1), fill = "skyblue", color = "black", stat = "identity")
    }} +
    {if(group != 1){
      geom_bar(aes(y = percent, group=!!sym(group), fill=!!sym(group)), color = "black", width=.5, position = "dodge", stat = "identity")
    }} +
    scale_y_continuous(labels=scales::percent) +
    labs(title = paste0(var, " (", ncountries, " countries)"), 
         x = NULL, 
         y = "Percentage") +  # Add labels to the axes and the plot
    {if(group != 1){
      theme(legend.position = "bottom") #, legend.title=element_blank())
    }} +
    theme(text = element_text(size = 12))
  return(plot)
}
if(F){
  bar_plot("conflict", group="region")
  bar_plot("conflict", group=1)
}
