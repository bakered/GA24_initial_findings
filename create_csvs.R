library(dplyr)


############# DATA MAIPULATION
# create by_giver from search terms occurrences
search_terms_all_documents_occurrences <- read.csv("~/UN_projects/GA24/csv_outputs/search_terms_all_documents_occurrences.csv")
by_giver <- search_terms_all_documents_occurrences %>%
  group_by(grouping, concept, permutations, text_id, ISO3, Country, Gender, region, LDC, SIDS_UNCTAD_plus, LLDC) %>%
  summarise(
    sentence_sentiment_mean = mean(sentence_sentiment, na.rm = TRUE),
    character_buffer_sentiment_mean = mean(character_buffer_sentiment, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup()
n_words <- read.csv("~/UN_projects/GA24/csv_outputs/minimum_transformed_summary_stats.csv") %>% 
  select(text_id, n_words)
by_giver = by_giver %>% 
  left_join(n_words, by="text_id") %>% 
  mutate(count_per_1000_words = (count*1000)/n_words)
write.csv(by_giver, "~/UN_projects/GA24/csv_outputs/all_search_terms_counts_by_giver.csv", row.names = FALSE)

# map_sentiments
minimum_transformed_sentiments <- read.csv("~/UN_projects/GA24/csv_outputs/minimum_transformed_sentiments.csv")
map_sentiment_data = minimum_transformed_sentiments %>% 
  select(ISO3, avg_sentiment_w_neutral)
write.csv(map_sentiment_data, "~/UN_projects/GA24/csv_outputs/map_sentiment_data.csv", row.names = FALSE)

# developing_search_terms_counts_by_permutations
n_words_group <- read.csv("~/UN_projects/GA24/csv_outputs/minimum_transformed_summary_stats.csv") %>% 
  select(region, n_words) %>% filter(region!="Developed", region!="other") %>% pull(n_words) %>% sum(na.rm=T)
n_countries_group <- metadata %>% filter(region!="Developed", region!="other", web_filepath!="") %>% pull(ISO3) %>% unique() %>% length()
developing_search_terms_counts_by_permutations = by_giver %>% 
  filter(region!="Developed", region!="other") %>% 
  group_by(grouping, concept, permutations) %>%
  summarise(
    sentence_sentiment_mean = mean(sentence_sentiment_mean, na.rm = TRUE),
    character_buffer_sentiment_mean = mean(character_buffer_sentiment_mean, na.rm = TRUE),
    count = sum(count),
    n_words = sum(n_words),
    perc_group_mentioned = length(unique(ISO3))/n_countries_group
  ) %>%
  ungroup() %>% 
  mutate(count_per_speech = count/n_countries_group,
         count_per_1000_words = (count*1000)/n_words_group)
write.csv(developing_search_terms_counts_by_permutations, "~/UN_projects/GA24/csv_outputs/developing_search_terms_counts_by_permutations.csv", row.names = FALSE)
# developing_search_terms_counts_by_permutations %>% View()

# developed_search_terms_counts_by_permutations
n_words_group <- read.csv("~/UN_projects/GA24/csv_outputs/minimum_transformed_summary_stats.csv") %>% 
  select(region, n_words) %>% filter(region=="Developed") %>% pull(n_words) %>% sum(na.rm=T)
n_countries_group = (metadata %>% filter(region=="Developed", web_filepath!="") %>% pull(ISO3) %>% unique() %>% length())
developed_search_terms_counts_by_permutations = by_giver %>% 
  filter(region=="Developed") %>% 
  group_by(grouping, concept, permutations) %>%
  summarise(
    sentence_sentiment_mean = mean(sentence_sentiment_mean, na.rm = TRUE),
    character_buffer_sentiment_mean = mean(character_buffer_sentiment_mean, na.rm = TRUE),
    count = sum(count),
    n_words = sum(n_words),
    perc_group_mentioned = length(unique(ISO3))/n_countries_group
  ) %>%
  ungroup() %>% 
  mutate(count_per_speech = count/n_countries_group,
         count_per_1000_words = (count*1000)/n_words_group)
write.csv(developed_search_terms_counts_by_permutations, "~/UN_projects/GA24/csv_outputs/developed_search_terms_counts_by_permutations.csv", row.names = FALSE)
# developed_search_terms_counts_by_permutations %>% View()


# developing_search_terms_counts_by_concept
n_words_group <- read.csv("~/UN_projects/GA24/csv_outputs/minimum_transformed_summary_stats.csv") %>% 
  select(region, n_words) %>% filter(region!="Developed", region!="other") %>% pull(n_words) %>% sum(na.rm=T)
n_countries_group = (metadata %>% filter(region!="Developed", region!="other", web_filepath!="") %>% pull(ISO3) %>% unique() %>% length())
developing_search_terms_counts_by_concept = by_giver %>% 
  filter(region!="Developed", region!="other") %>% 
  group_by(grouping, concept) %>%
  summarise(
    sentence_sentiment_mean = mean(sentence_sentiment_mean, na.rm = TRUE),
    character_buffer_sentiment_mean = mean(character_buffer_sentiment_mean, na.rm = TRUE),
    count = sum(count),
    n_words = sum(n_words),
    perc_group_mentioned = length(unique(ISO3))/n_countries_group
  ) %>%
  ungroup() %>% 
  mutate(count_per_speech = count/n_countries_group,
         count_per_1000_words = (count*1000)/n_words_group)
write.csv(developing_search_terms_counts_by_concept, "~/UN_projects/GA24/csv_outputs/developing_search_terms_counts_by_concept.csv", row.names = FALSE)
# developing_search_terms_counts_by_concept %>% View()

# developed_search_terms_counts_by_concept
n_words_group <- read.csv("~/UN_projects/GA24/csv_outputs/minimum_transformed_summary_stats.csv") %>% 
  select(region, n_words) %>% filter(region=="Developed") %>% pull(n_words) %>% sum(na.rm=T)
n_countries_group = (metadata %>% filter(region=="Developed", web_filepath!="") %>% pull(ISO3) %>% unique() %>% length())
developed_search_terms_counts_by_concept = by_giver %>% 
  filter(region=="Developed") %>% 
  group_by(grouping, concept) %>%
  summarise(
    sentence_sentiment_mean = mean(sentence_sentiment_mean, na.rm = TRUE),
    character_buffer_sentiment_mean = mean(character_buffer_sentiment_mean, na.rm = TRUE),
    count = sum(count),
    n_words = sum(n_words),
    perc_group_mentioned = length(unique(ISO3))/n_countries_group
  ) %>%
  ungroup() %>% 
  mutate(count_per_speech = count/n_countries_group,
         count_per_1000_words = (count*1000)/n_words_group)
write.csv(developed_search_terms_counts_by_concept, "~/UN_projects/GA24/csv_outputs/developed_search_terms_counts_by_concept.csv", row.names = FALSE)
# developed_search_terms_counts_by_concept %>% View()

