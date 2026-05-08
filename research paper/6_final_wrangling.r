library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(lubridate)
library(lme4)

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
  age         = as.integer(str_extract(field_end, "\\d{4}")) - birth_year,
  age_group   = case_when(
    age <= 29                 ~ "18-29",
    age >= 30 ~ "30+", #& age <= 39    ~ "30-39",
    # age >= 40 & age <= 49    ~ "40-49",
    # age >= 50                 ~ "50+",
    TRUE                      ~ NA_character_
  ),
  time_period = as.Date(field_end)
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

# --- Bundestag speeches data wrangling and analysis---
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
  df           <- df[!is.na(df$top_topic), ]                          # drop DFM-excluded speeches
  df           <- df[!grepl("^Other", df$top_topic_name), ]           # drop residual keyATM topics
  df$date_num  <- scale(as.numeric(df$Date))[, 1]  # standardise: fixes large-eigenvalue warning from glmer
  df$Party     <- relevel(factor(df$Party), ref = "SPD")
  df$top_topic <- factor(df$top_topic)                                 # droplevels implicit in factor()
  df
}

ae_d <- make_subset("Anti.Elitism_pred")
pc_d <- make_subset("People.Centrism_pred")
lw_d <- make_subset("Left.Wing.Host.Ideology_pred")
rw_d <- make_subset("Right.Wing.Host.Ideology_pred")

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
m_ae <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = ae_d, family = binomial, control = ctrl, nAGQ = 0)
m_pc <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = pc_d, family = binomial, control = ctrl, nAGQ = 0)
m_lw <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = lw_d, family = binomial, control = ctrl, nAGQ = 0)
m_rw <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = rw_d, family = binomial, control = ctrl, nAGQ = 0)

saveRDS(m_ae, "data/m_ae.rds")
saveRDS(m_pc, "data/m_pc.rds")
saveRDS(m_lw, "data/m_lw.rds")
saveRDS(m_rw, "data/m_rw.rds")



count_ae <- bt_speeches[bt_speeches$Anti.Elitism_pred == 1,] |> nrow()
percent_ae <- count_ae / nrow(bt_speeches)
count_pc <- bt_speeches[bt_speeches$People.Centrism_pred == 1,] |> nrow()
percent_pc <- count_pc / nrow(bt_speeches)
count_lw <- bt_speeches[bt_speeches$Left.Wing.Host.Ideology_pred == 1,] |> nrow()
percent_lw <- count_lw / nrow(bt_speeches)
count_rw <- bt_speeches[bt_speeches$Right.Wing.Host.Ideology_pred == 1,] |> nrow()
percent_rw <- count_rw / nrow(bt_speeches)

print(c(percent_ae, percent_pc, percent_lw, percent_rw))


# --- Shared dimension labels for descriptive figures ---
dim_labels <- c(
  "Anti.Elitism_pred"            = "Anti-Elitism",
  "People.Centrism_pred"         = "People-Centrism",
  "Left.Wing.Host.Ideology_pred" = "Left-Wing Host-Ideology",
  "Right.Wing.Host.Ideology_pred" = "Right-Wing Host-Ideology"
)

# --- Figure 4: Speaker-level lollipop ---
speaker_summary <- bt_speeches |>
  group_by(Name, Party) |>
  summarise(
    mean_pop    = mean(populist_mean, na.rm = TRUE),
    n_sentences = n(),
    .groups     = "drop"
  ) |>
  filter(n_sentences >= 100, !is.na(Party), Party != "")

party_medians <- speaker_summary |>
  group_by(Party) |>
  summarise(median_pop = median(mean_pop, na.rm = TRUE), .groups = "drop")

fig_speaker_pop <- bind_rows(
  speaker_summary |> group_by(Party) |> slice_max(mean_pop, n = 2),
  speaker_summary |> group_by(Party) |> slice_min(mean_pop, n = 2)
) |>
  distinct(Name, Party, .keep_all = TRUE) |>
  left_join(party_medians, by = "Party") |>
  ungroup() |>
  arrange(Party, mean_pop) |>
  mutate(Name = factor(Name, levels = unique(Name)))

saveRDS(fig_speaker_pop, "data/fig_speaker_pop.rds")

# --- Figure 10: Mean populism rate by topic × party ---
topic_nice_labels <- c(
  "agriculture"             = "Agriculture",
  "culturemedia"            = "Culture & Media",
  "digitalizationtech"      = "Digitalization",
  "economy"                 = "Economy",
  "eudcationscience"        = "Education & Science",
  "environmentclimate"      = "Environment & Climate",
  "financetax"              = "Finance & Tax",
  "foreign"                 = "Foreign Affairs",
  "government"              = "Government",
  "party"                   = "Party Affairs",
  "regional"                = "Regional",
  "security"                = "Security",
  "social"                  = "Social Policy",
  "transportinfrastructure" = "Transport",
  "migration"               = "Migration",
  "ukraine"                 = "Ukraine"
)

fig_topic_party_pop <- bt_speeches_long |>
  filter(
    !is.na(top_topic_name), !grepl("procedural|Other", top_topic_name),
    !is.na(Party), Party != ""
  ) |>
  mutate(
    topic_label = topic_nice_labels[str_remove(top_topic_name, "^\\d+_")],
    dim_label   = dim_labels[populist_dimension]
  ) |>
  group_by(topic_label, Party, dim_label) |>
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") |>
  filter(!is.na(topic_label), !is.na(dim_label))

saveRDS(fig_topic_party_pop, "data/fig_topic_party_pop.rds")

# Topic coefficient labels for regression table (maps top_topicN -> nice name)
topic_coef_labels <- bt_speeches |>
  filter(!is.na(top_topic), !is.na(top_topic_name)) |>
  select(top_topic, top_topic_name) |>
  distinct() |>
  arrange(top_topic) |>
  mutate(
    short_name = str_remove(top_topic_name, "^\\d+_"),
    nice_name  = {
      lkp <- topic_nice_labels[short_name]
      ifelse(!is.na(lkp), lkp, str_to_title(short_name))
    },
    coef_name  = paste0("top_topic", top_topic)
  )
saveRDS(topic_coef_labels, "data/topic_coef_labels.rds")

# --- Figure 11: Social media share vs. populism over time ---
sm_over_time <- info_source_3 |>
  group_by(time_period) |>
  summarise(
    sm_share = mean(media_info_source == "Social media", na.rm = TRUE),
    .groups  = "drop"
  ) |>
  filter(!is.na(time_period))

pop_over_time <- bt_speeches_long |>
  filter(!is.na(Date), !is.na(score)) |>
  mutate(
    quarter   = floor_date(Date, "quarter"),
    dim_label = dim_labels[populist_dimension]
  ) |>
  group_by(quarter, dim_label) |>
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") |>
  filter(!is.na(dim_label))

# Scale social media share onto the populism axis for dual-axis rendering
sm_range  <- range(sm_over_time$sm_share,   na.rm = TRUE)
pop_range <- range(pop_over_time$mean_score, na.rm = TRUE)
sm_scale  <- diff(pop_range) / diff(sm_range)
sm_offset <- pop_range[1] - sm_range[1] * sm_scale
sm_over_time$sm_scaled <- sm_over_time$sm_share * sm_scale + sm_offset

saveRDS(
  list(sm = sm_over_time, pop = pop_over_time, sm_scale = sm_scale, sm_offset = sm_offset),
  "data/fig_time_trends.rds"
)
