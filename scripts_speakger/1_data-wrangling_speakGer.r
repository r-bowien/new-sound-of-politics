
# title: Data wrangling - The New Sound of Politics
# author: Robert Bowien
# affiliation: University College Dublin


# This document shows the data wrangling for the Project "The New Sound of Politics".

# First, the relevant libraries and data are loaded. Then, time-series data from the GLES is identified and isolated.


library(ggplot2)
library(dplyr)
library(stringfish)
library(stringr)



# Part 1: Social media 


gles_data <- haven::read_dta("data/ZA6832_v2-0-0.dta")
gles_korresp <- readxl::read_xlsx("data/ZA6832_v2-0-0_Korrespondenzliste.xlsx")




# nrow(gles_data)
# nrow(gles_korresp)

print(gles_korresp[grepl("Pol. Mediennutzun", gles_korresp$Variablenlabel),c("Variablenname", "Variablenlabel")])

print(gles_korresp[grepl("Wahlabsicht", gles_korresp$Variablenlabel),c("Variablenname", "Variablenlabel")], n= 50)
# colnames(gles_data)

info_source_data <- gles_data |> subset(select = c("field_start", "field_end", "sample", "version", "lfdn", "lfdn_od", "m0001_1", "m0001_2", "m0001_3", "s0002", "e0003a_1", "e0003a_2", "e0003b_1", "e0003b_2")) # m0001_1 does not yet include social media, social media in there since 2012




# Now, we have a first look at the data. Mainly interesting to us is the data that included social media as a seperate category. We want to know:
# - from when on is social media included?
# - how is the data distributed for the oldest and the latest year?
# - how did it develop over time?

print(info_source_data$m0001_1[1:10])
print(info_source_data$m0001_2[1:10])
print(info_source_data$m0001_3[1:10])

info_source_data[info_source_data$m0001_3 %in% c(-99:-71), ] <- NA

info_source_3 <- info_source_data[!is.na(info_source_data$m0001_3), ]

print(info_source_3$m0001_3[1:10])


# pasting together columns for vote intention (e0003a_1 + e0003a_2 and e0003b_1 + e0003b_2) as they are mutually exclusive and should be merged to one column for better handling. The same applies to the field_start and field_end columns, which are merged to one column called time_period.)

# 1. declaring -94 as missing value in both columns
info_source_3$e0003a_1[info_source_3$e0003a_1 == -94] <- NA
info_source_3$e0003a_2[info_source_3$e0003a_2 == -94] <- NA
info_source_3$e0003b_1[info_source_3$e0003b_1 == -94] <- NA
info_source_3$e0003b_2[info_source_3$e0003b_2 == -94] <- NA

info_source_3 <- info_source_3 |> mutate(vote_intention_erststimme = str_remove(paste0(e0003a_1, e0003a_2), "NA"), vote_intention_zweitstimme = str_remove(paste0(e0003b_1, e0003b_2), "NA"))

info_source_3 <- info_source_3 |> mutate(vote_intention_erststimme = case_when(
  vote_intention_erststimme == "1" ~ "1",#"CDU/CSU",
  vote_intention_erststimme == "4" ~ "2",#"SPD",
  vote_intention_erststimme == "322" ~ "3",#"AfD",
  vote_intention_erststimme == "5" ~ "4",#"FDP",
  vote_intention_erststimme == "6" ~ "5",#"Bündnis 90/Die Grünen",
  vote_intention_erststimme == "7" ~ "6",#"Die Linke",
  TRUE ~ NA_character_
))
# recode to have vote intention other parties are all values larger than 7
info_source_3$vote_intention_erststimme[!is.na(info_source_3$vote_intention_erststimme) & info_source_3$vote_intention_erststimme > 7] <- 7


# recode vote intention columns to factors with the correct labels
info_source_3$vote_intention_erststimme <- factor(info_source_3$vote_intention_erststimme,
  levels = c("1", "2", "3", "4", "5", "6", "7"),
  labels = c("CDU/CSU", "SPD", "FDP", "Bündnis 90/Die Grünen", "Die Linke", "AfD", "Other parties")
)

# Now the same for Zweitstimme

info_source_3 <- info_source_3 |> mutate(vote_intention_zweitstimme = str_remove(paste0(e0003a_1, e0003a_2), "NA"), vote_intention_zweitstimme = str_remove(paste0(e0003b_1, e0003b_2), "NA"))

info_source_3 <- info_source_3 |> mutate(vote_intention_zweitstimme = case_when(
  vote_intention_zweitstimme == "1" ~ "1",#"CDU/CSU",
  vote_intention_zweitstimme == "4" ~ "2",#"SPD",
  vote_intention_zweitstimme == "322" ~ "3",#"AfD",
  vote_intention_zweitstimme == "5" ~ "4",#"FDP",
  vote_intention_zweitstimme == "6" ~ "5",#"Bündnis 90/Die Grünen",
  vote_intention_zweitstimme == "7" ~ "6",#"Die Linke",
  TRUE ~ NA_character_
))
# recode to have vote intention other parties are all values larger than 7
info_source_3$vote_intention_zweitstimme[!is.na(info_source_3$vote_intention_zweitstimme) & info_source_3$vote_intention_zweitstimme > 7] <- 7


# recode vote intention columns to factors with the correct labels
info_source_3$vote_intention_zweitstimme <- factor(info_source_3$vote_intention_zweitstimme,
  levels = c("1", "2", "3", "4", "5", "6", "7"),
  labels = c("CDU/CSU", "SPD", "FDP", "Bündnis 90/Die Grünen", "Die Linke", "AfD", "Other parties")
)



head(info_source_3[, c("e0003a_1", "e0003a_2", "vote_intention_erststimme", "e0003b_1", "e0003b_2", "vote_intention_zweitstimme")])
# renaming columns for better handling
colnames(info_source_3)[colnames(info_source_3) == "m0001_3"] <- "media_info_source"
colnames(info_source_3)[colnames(info_source_3) == "s0002"] <- "birth_year"

# save subset of relevant columns
info_source_3 <- info_source_3[, c("field_start", "field_end", "sample", "version", "lfdn", "lfdn_od", "media_info_source", "birth_year", "vote_intention_erststimme", "vote_intention_zweitstimme")]



info_source_3 <- info_source_3 |> mutate(
    birth_year = as.integer(str_extract(birth_year, "\\d{4}")),
    age        = as.integer(str_extract(field_start, "\\d{4}")) - birth_year,
    age_group  = case_when(
      age <= 29 ~ "18-29",
      age >= 30 & age <= 39 ~ "30-39",
      age >= 40 & age <= 49 ~ "40-49",
      age >= 50             ~ "50+",
      TRUE                  ~ NA_character_
    ),
    time_period = as.Date(field_start))

# recode media_info_source so that other internet providers are recoded to other (7)

info_source_3$media_info_source[info_source_3$media_info_source == 5] <- 7#"Other Internetproviders"
info_source_3$media_info_source[info_source_3$media_info_source == 6] <- 5#"Personal 
info_source_3$media_info_source[info_source_3$media_info_source == 7] <- 6#"Personal 

info_source_3 <- info_source_3 |> mutate(
    media_info_source = factor(media_info_source,
      levels = 1:6,
      labels = c(
        "TV",
        "Newspaper",
        "Radio",
        "Social media",
        "Personal conversation",
        "Other source"
      )
    )
)

unique(info_source_3$media_info_source)



# write.csv(info_source_3, "data/sm_relevance.csv")
saveRDS(info_source_3, "data/sm_relevance.rds")

# From when on is social media included?

min(info_source_3$field_start)
unique(info_source_3$field_start)
length(unique(info_source_3$lfdn_od))
nrow(info_source_3)


# Part 2: Bundestag speeches

# Here, the speeches in the Bundestag are cleaned and filtered as good as possible.
# However, the structure and lack of tidyness of the data pose challenging problems, some of which could not be resolved to adhere to the highest scientific standards.


bt_speeches_dat_speakger <- read.csv("data/Bundestag.csv")

bt_speeches <- bt_speeches_dat_speakger |> filter(Period == 20)

bt_meta <- read.csv("data/all_mps_meta.csv", sep = ";")

bt_speeches$speech_id <- seq_len(nrow(bt_speeches))
bt_speeches[, c("Session", "Date", "Interjection", "MPID", "Party", "speech_id")] |> head()

bt_speeches_meta <- merge(bt_speeches, bt_meta, by="MPID", all.x = TRUE) |> arrange(speech_id)
bt_speeches_meta[, c("Session", "Date", "Interjection", "MPID", "Party", "speech_id")] |> head()

bt_speeches_nochair <-  bt_speeches_meta[bt_speeches_meta$Chair == "False", ]

bt_speeches_nointerjections <- bt_speeches_nochair[bt_speeches_nochair$Interjection == "False", ]
bt_speeches_nointerjections[, c("Session", "Date", "Interjection", "MPID", "Party", "speech_id")] |> head()



bt_speeches_nounidentified <- bt_speeches_nointerjections |> dplyr::filter(MPID != 0)
bt_speeches_nounidentified[, c("Session", "Date", "Chair", "Interjection", "MPID", "Party", "speech_id")] |> head()


bt_speeches_combined <- bt_speeches_nounidentified |> group_by(MPID, Date, Chair, Session, Period, Party, Name) |>
summarise(Speech = paste(Speech, collapse="")) # if wanted, insert pattern in collapse = "" to see where the splits occur afterwards. 

bt_speeches_combined |> nrow() |> print()
bt_speeches_combined$Date |> min() |> print()
names(bt_speeches_nounidentified)


# names_bt_members <- bt_meta$Name |> unique() |> paste0(collapse = "|")
names_bt_members_list <- unique(bt_meta$Name) |> list()
names_bt_members <- split(names_bt_members_list[[1]], ceiling(seq_along(names_bt_members_list[[1]])/300))


names_bt_members_rgx <- sapply(names_bt_members, function(names){
  gsub(" \\(.+\\)", "", paste0(names, collapse="|"))
})


# Disclaimer: following regex written by author
remove_regex_base = "(\\d{4} )?Deutscher Bundestag – \\d{2} ?. Wahlperiode – \\d+ ?. Sitzung ?. Berlin, ((Mon|Diens|Donners|Frei|Sams|Sonn)tag|Mittwoch), den \\d+ ?. (Januar|Februar|M(ä|ae)rz|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember) \\d{4,}( \\d*)?"

# Disclaimer: following regex written by claude
lookaround_regexes_rm_names_b4 = sapply(names_bt_members_rgx, function(regex) {paste0(paste0(
    "(?:Präsident\\s+Dr\\.\\s+)?",        # "Präsident Dr." is optional
    "(?:", regex, ")",              # any name from the pipe-separated list
    "\\s+Deutscher\\s+Bundestag",
    "\\s+[–-]\\s+\\d+\\s*\\.\\s*Wahlperiode",
    "\\s+[–-]\\s+\\d+\\s*\\.\\s*Sitzung",
    "\\s*\\.\\s*Berlin,",
    "\\s+\\w+,\\s+den\\s+\\d+\\s*\\.\\s*\\w+\\s+\\d{4}",
    "\\s+\\d+"
  ))})

# Disclaimer: following regex written by perplexity
lookaround_regexes_rm_names_bhind = sapply(names_bt_members_rgx, function(regex) {paste0("(?:", "(?:(?:\\d{2,4} )?Deutscher Bundestag – \\d{2} ?. Wahlperiode – \\d+ ?. Sitzung ?. Berlin, (?:(?:Mon|Diens|Donners|Frei|Sams|Sonn)tag|Mittwoch), den \\d+ ?. (?:Januar|Februar|M(?:ä|ae)rz|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember) \\d{4,}(?: \\d*)?) (?:Alterspr(?:ä|ae)sident|Parl\\. Staatssekret(?:ä|ae)r)? ?(?:\\w{2,4}\\. ){0,2}) ?(Alterspr(ä|ae)sident|Parl. Staatssekret(ä|ae)r)? ?(\\w{2,4}\\. ){0,2}(", regex,")")})

bt_speeches_combined_clean <- bt_speeches_combined

removeRegexes <- function(column, regex, n_threads = 4){
  for (n in seq_along(regex)){
    rgx <- regex[n]
    print(paste0("rgx group: ", n, " of ", length(regex)))
    # Source - https://stackoverflow.com/a/78270004
    # Posted by LMc
    # Retrieved 2026-04-08, License - CC BY-SA 4.0
    column <- sf_gsub(column, regex(rgx, ignore_case = TRUE), "", nthreads = n_threads)
  
  }
  return(column)
}

bt_speeches_combined_clean$Speech <- removeRegexes(bt_speeches_combined_clean$Speech, lookaround_regexes_rm_names_b4, 8)

bt_speeches_combined_clean$Speech <- removeRegexes(bt_speeches_combined_clean$Speech, lookaround_regexes_rm_names_bhind, 8)

bt_speeches_combined_clean$Speech <- removeRegexes(bt_speeches_combined_clean$Speech, remove_regex_base, 8)


write.csv(bt_speeches_combined_clean, "data/bundestag_cleaned_period20.csv")
