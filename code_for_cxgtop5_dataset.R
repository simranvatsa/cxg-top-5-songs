options(scipen=999)

library(tidyverse)
library(rtweet)
library(fuzzyjoin)
library(readxl)

# needs a Twitter developer account and API tokens to work
crazy_ex_top_5 <- search_tweets("#CrazyExTop5", n = 1000, include_rts = FALSE) 

cleaned_rankings <- crazy_ex_top_5 %>% 
  pull(text) %>% 
  str_extract_all("[12345][^{]+[12345][^{]+(\\z)") %>% # regex
  unlist() %>% 
  str_remove_all("^\\n") %>% # more regex
  str_remove_all("(\\n)[^\\n15]+$") %>%  # ... more regex
  str_remove_all("[^\x01-\x7F]") %>% # i'm going to stop pointing out how much regex i used now
  str_replace_all("([a-z\\?!\\.]) {1,}([12345])", "\\1\n\\2") %>%
  str_split("\\n") %>% 
  unlist() %>%
  .[str_detect(., "([12345](\\.|\\))) [A-Za-z]")] %>% 
  str_trim()

rank_song_df <- cleaned_rankings %>% 
  str_to_lower() %>% 
  tibble(cleaned_tweets = .) %>% 
  separate(cleaned_tweets, c("rank", "song_name"), sep = "\\.|\\) ") %>%
  mutate(song_name = str_remove_all(song_name, "[[:punct:]]")) %>% 
  mutate(song_name = str_remove_all(song_name, "httpst") %>% str_trim()) %>% 
  mutate(rank = str_sub(rank, start = 1, end = 1))

songs_df <- read_csv("cxg_song_list.csv") %>%  
  # "cxg_song_list.csv": a list of the songs that i scraped and cleaned from wikipedia, it's in this repo
  mutate(song_name_lower = str_to_lower(song_name)) %>% 
  mutate(song_name_lower = str_remove_all(song_name_lower, "[[:punct:]]"))

m <- rank_song_df %>%
  stringdist_left_join(songs_df, by = c(song_name = "song_name_lower"), max_dist = 3) # "fuzzy matching" accounted for small spelling errors

write_csv(m, "fuzzymatched_songs.csv")

# From here, I opened the .csv I created in Google Sheets and manually input song names to data points that hadn't been matched,
# either because there was extra text in the song names, the songs were named wrongly, or there were significant spelling errors.
# I saved the fruits of my manual labor to "CrazyExTop5_rankings.xlsx", also in the repo.

final_data <- read_excel("CrazyExTop5_rankings.xlsx")
final_data <- final_data %>% 
  left_join(songs_df, by = c(song_name.y = "song_name_lower"))

final_data <- final_data %>% 
  select(song_name, rank) %>% 
  mutate(song_name = str_remove_all(song_name, '"')) %>% 
  filter(!is.na(song_name))

cleaned_final_data <- final_data %>% 
  count(song_name, rank) %>%
  spread(key = rank, value = n) %>% 
  replace_na(list(`1` = 0, `2` = 0, `3` = 0, `4` = 0, `5` = 0)) %>% 
  mutate(total_score = 5*`1` + 4*`2` + 3*`3` + 2*`4` + 1*`5`) %>% 
  arrange(desc(total_score))

write_csv(cleaned_final_data, "final_crazy_ex_top_5_rankings.csv")
