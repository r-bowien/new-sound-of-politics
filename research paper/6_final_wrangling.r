library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(sandwich)

# --- GLES survey data wrangling ---
gles_data <- haven::read_dta("data/ZA6832_v2-0-0.dta")

info_source_data <- gles_data |> subset(select = c(
  "field_start", "field_end", "sample", "version", "lfdn", "lfdn_od",
  "m0001_1", "m0001_2", "m0001_3", "s0002",
  "e0003a_1", "e0003a_2", "e0003b_1", "e0003b_2"
))

info_source_data[info_source_data$m0001_3 %in% c(-99:-71), ] <- NA
info_source_3 <- info_source_data[!is.na(info_source_data$m0001_3), ]

info_source_3$e0003a_1[info_source_3$e0003a_1 == -94] <- NA
info_source_3$e0003a_2[info_source_3$e0003a_2 == -94] <- NA
info_source_3$e0003b_1[info_source_3$e0003b_1 == -94] <- NA
info_source_3$e0003b_2[info_source_3$e0003b_2 == -94] <- NA

info_source_3 <- info_source_3 |> mutate(
  vote_intention_erststimme = str_remove(paste0(e0003a_1, e0003a_2), "NA"),
  vote_intention_zweitstimme = str_remove(paste0(e0003b_1, e0003b_2), "NA")
)

recode_vote <- function(x) {
  case_when(
    x == "1"   ~ "1", # CDU/CSU
    x == "4"   ~ "2", # SPD
    x == "322" ~ "3", # AfD
    x == "5"   ~ "4", # FDP
    x == "6"   ~ "5", # Bündnis 90/Die Grünen
    x == "7"   ~ "6", # Die Linke
    TRUE        ~ NA_character_
  )
}

vote_levels  <- c("1", "2", "3", "4", "5", "6", "7")
vote_labels  <- c("CDU/CSU", "SPD", "AfD", "FDP", "Bündnis 90/Die Grünen", "Die Linke", "Other parties")

info_source_3 <- info_source_3 |> mutate(
  vote_intention_erststimme  = factor(recode_vote(vote_intention_erststimme),  levels = vote_levels, labels = vote_labels),
  vote_intention_zweitstimme = factor(recode_vote(vote_intention_zweitstimme), levels = vote_levels, labels = vote_labels)
)

colnames(info_source_3)[colnames(info_source_3) == "m0001_3"] <- "media_info_source"
colnames(info_source_3)[colnames(info_source_3) == "s0002"]   <- "birth_year"

info_source_3 <- info_source_3[, c(
  "field_start", "field_end", "sample", "version", "lfdn", "lfdn_od",
  "media_info_source", "birth_year", "vote_intention_erststimme", "vote_intention_zweitstimme"
)]

info_source_3 <- info_source_3 |> mutate(
  birth_year  = as.integer(str_extract(birth_year, "\\d{4}")),
  age         = as.integer(str_extract(field_start, "\\d{4}")) - birth_year,
  age_group   = case_when(
    age <= 29                 ~ "18-29",
    age >= 30 & age <= 39    ~ "30-39",
    age >= 40 & age <= 49    ~ "40-49",
    age >= 50                 ~ "50+",
    TRUE                      ~ NA_character_
  ),
  time_period = as.Date(field_start)
)

info_source_3$media_info_source[info_source_3$media_info_source == 5] <- 7
info_source_3$media_info_source[info_source_3$media_info_source == 6] <- 5
info_source_3$media_info_source[info_source_3$media_info_source == 7] <- 6

info_source_3 <- info_source_3 |> mutate(
  media_info_source = factor(media_info_source,
    levels = 1:6,
    labels = c("TV", "Newspaper", "Radio", "Social media", "Personal conversation", "Other source")
  )
)

saveRDS(info_source_3, "data/sm_relevance.rds")

# --- Bundestag speeches ---
topic_df <- readRDS("data/topic_proportions_dpi3.rds")

bt_speeches <- topic_df$merged_sentences
bt_speeches <- bt_speeches |> filter(is.na(top_topic_name) | !grepl("procedural", top_topic_name))

bt_speeches_long <- bt_speeches |>
  pivot_longer(
    cols = c(Anti.Elitism_pred, People.Centrism_pred, Left.Wing.Host.Ideology_pred, Right.Wing.Host.Ideology_pred),
    names_to  = "populist_dimension",
    values_to = "score"
  )
bt_speeches_long$Date <- as.Date(bt_speeches_long$Date)

saveRDS(bt_speeches_long, "data/bt_speeches_long.rds")

sum_stat <- subset(bt_speeches, select = c("Party", "n_party_members")) |> unique()
print(sum_stat)

bt_speeches <- bt_speeches |> group_by(Name, Party) |> mutate(mean_pop_per_speaker = mean(populist_mean))
bt_speeches |> arrange(desc(mean_pop_per_speaker)) |> subset(select = c(Name, mean_pop_per_speaker, Party, sentence))
most_populist_speakers <- bt_speeches[bt_speeches$mean_pop_per_speaker == max(bt_speeches$mean_pop_per_speaker), ]

make_subset <- function(dim_name) {
  df           <- bt_speeches_long[bt_speeches_long$populist_dimension == dim_name, ]
  df           <- df[!is.na(df$top_topic), ]    # speeches dropped by DFM have NA top_topic; remove before glm
  df$date_num  <- as.numeric(df$Date)           # days from epoch → continuous time trend
  df$Party     <- relevel(factor(df$Party), ref = "SPD")
  df$top_topic <- factor(df$top_topic)
  df
}

ae_d <- make_subset("Anti.Elitism_pred")
pc_d <- make_subset("People.Centrism_pred")
lw_d <- make_subset("Left.Wing.Host.Ideology_pred")
rw_d <- make_subset("Right.Wing.Host.Ideology_pred")

m_ae <- glm(score ~ date_num + Party + top_topic, data = ae_d, family = binomial)
m_pc <- glm(score ~ date_num + Party + top_topic, data = pc_d, family = binomial)
m_lw <- glm(score ~ date_num + Party + top_topic, data = lw_d, family = binomial)
m_rw <- glm(score ~ date_num + Party + top_topic, data = rw_d, family = binomial)

saveRDS(m_ae, "data/m_ae.rds")
saveRDS(m_pc, "data/m_pc.rds")
saveRDS(m_lw, "data/m_lw.rds")
saveRDS(m_rw, "data/m_rw.rds")

vcov_ae <- vcovCL(m_ae, cluster = ae_d$speech_id)
vcov_pc <- vcovCL(m_pc, cluster = pc_d$speech_id)
vcov_lw <- vcovCL(m_lw, cluster = lw_d$speech_id)
vcov_rw <- vcovCL(m_rw, cluster = rw_d$speech_id)

saveRDS(list(ae = vcov_ae, pc = vcov_pc, lw = vcov_lw, rw = vcov_rw), "data/model_vcovs.rds")
