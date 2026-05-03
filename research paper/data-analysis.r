---
title: data analysis - The New Sound of Politics
authors:
  - name: Robert Bowien
    affiliation: University College Dublin
    roles: writing
    corresponding: true
bibliography: references.bib
---



# Descriptive Data analysis

## Social Media relevance

# The next step is to calculate the proportion of social media as most important information source and then plot the development over time.



library(ggplot2)
library(dplyr)
library(stringr)




data_sm_relevance <- read.csv("data/sm_relevance.csv")


# Use field_start as the time variable; treat m0001_3 as a factor
df_shares <- data_sm_relevance |>
  mutate(
    time_period = as.Date(field_start),
    value = factor(m0001_3,
      levels = 1:7,
      labels = c(
        "TV",
        "Newspaper (incl. online)",
        "Radio",
        "Social media",
        "Other internetmedia (e.g. e-mail-provider, blogs)",
        "Personal communication",
        "Other source"
      )
    )
  ) |>
  count(time_period, value) |>
    group_by(time_period) |>
  mutate(share = n / sum(n)) |>
  ungroup()

# Define a colour palette for the distinct values
n_vals <- n_distinct(df_shares$value)
palette <- setNames(
  colorRampPalette(c("#2166ac", "#4dac26", "#d01c8b", "#f1a340", "#762a83", "#e66101", "#1b7837"))(n_vals),
  levels(df_shares$value)
)
 

# Build the plot
p <- ggplot(df_shares, aes(x = time_period, y = share, colour = value, group = value)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months",
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = palette, name = "Source") +
  labs(
    title    = "Relevance of different media as political information source over time",
    subtitle = "Each line represents one response category",
    x        = "Survey Wave Start Date",
    y        = "Share"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(colour = "grey40", margin = margin(b = 8)),
    legend.position  = "right",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Save
ggsave("social_media_relevance_overtime.png", plot = p, width = 10, height = 6, dpi = 150)
message("Plot saved to social_media_relevance_overtime.png")




reference_year <- 2020
 
# Parse s0002: str_extract pulls the 4-digit year from both plain values ("1979")
# and censored strings ("1956 und frueher"). The latter are treated as born in
# that year or earlier, so using the extracted year gives a conservative age estimate.
df <- data_sm_relevance %>%
  mutate(
    birth_year = as.integer(str_extract(s0002, "\\d{4}")),
    age        = reference_year - birth_year,
    age_group  = cut(age,
      breaks = c(14, 29, 39, 49, 59, 69, Inf),
      labels = c("15-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      right  = TRUE
    )
  ) %>%
  filter(!is.na(age_group))
 
# Compute share of social media (m0001_3 == 4) per wave and age group
df_counts <- df %>%
  mutate(time_period = as.Date(field_start)) %>%
  group_by(time_period, age_group) %>%
  summarise(
    share = mean(m0001_3 == 4, na.rm = TRUE),
    .groups = "drop"
  )
 
# Colour palette
age_palette <- c(
  "15-29" = "#2166ac",
  "30-39" = "#4dac26",
  "40-49" = "#d01c8b",
  "50-59" = "#f1a340",
  "60-69" = "#762a83",
  "70+"   = "#b2182b"
)
 
# Build plot
p <- ggplot(df_counts, aes(x = time_period, y = share, colour = age_group, group = age_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months",
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  scale_colour_manual(values = age_palette, name = "Altersgruppe") +
  labs(
    title    = "Anteil soziale Medien als Hauptinformationsquelle nach Altersgruppe",
    subtitle = "Anteil der Befragten, die soziale Medien (m0001_3 = 4) angaben",
    x        = "Erhebungszeitraum (Beginn)",
    y        = "Anteil"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", margin = margin(b = 8)),
    legend.position  = "right",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )
 
# Save
ggsave("plot_socialmedia_age.png", plot = p, width = 10, height = 6, dpi = 150)
message("Plot saved to plot_socialmedia_age.png")



df_youngvsold <- data_sm_relevance %>%
  mutate(
    birth_year = as.integer(str_extract(s0002, "\\d{4}")),
    age        = reference_year - birth_year,
    age_group  = case_when(
      age >= 18 & age <= 29 ~ "Jung (18-29)",
      age >= 30             ~ "Aelter (30+)",
      TRUE                  ~ NA_character_
    ),
    time_period = as.Date(field_start),
    source = factor(m0001_3,
      levels = 1:7,
      labels = c(
        "Fernsehen (inkl. Mediathek)",
        "Zeitung (inkl. Onlineangebot)",
        "Radio (inkl. Webradio)",
        "Soziale Medien",
        "Andere Internetanbieter",
        "Persoenliches Gespraech",
        "Andere Quelle"
      )
    )
  ) %>%
  filter(!is.na(age_group), !is.na(source))
 
# Compute share of each source within age group x wave
df_counts <- df_youngvsold %>%
  group_by(time_period, age_group, source) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_period, age_group) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()
 
# Colour palette per source
source_palette <- c(
  "Fernsehen (inkl. Mediathek)"   = "#1f78b4",
  "Zeitung (inkl. Onlineangebot)" = "#33a02c",
  "Radio (inkl. Webradio)"        = "#ff7f00",
  "Soziale Medien"                = "#e31a1c",
  "Andere Internetanbieter"       = "#6a3d9a",
  "Persoenliches Gespraech"       = "#b15928",
  "Andere Quelle"                 = "#a6a6a6"
)
 
# Plot — faceted by age group, one line per source
p <- ggplot(df_counts, aes(x = time_period, y = share, colour = source, group = source)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ age_group, ncol = 1) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months",
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  scale_colour_manual(values = source_palette, name = "Informationsquelle") +
  labs(
    title    = "Informationsquellen nach Altersgruppe ueber Zeit",
    subtitle = "Anteil der Befragten pro Quelle, aufgeteilt nach Jung (18-29) vs. Aelter (30+)",
    x        = "Erhebungszeitraum (Beginn)",
    y        = "Anteil"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", margin = margin(b = 8)),
    legend.position  = "right",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 12)
  )
 
# Save
ggsave("plot_sources_by_agegroup.png", plot = p, width = 11, height = 9, dpi = 150)
message("Plot saved to plot_sources_by_agegroup.png")






# create a plot that shows party vote intention by age group
df_vote_intention <- data_sm_relevance %>%
  mutate(
    birth_year = as.integer(str_extract(s0002, "\\d{4}")),
    age        = reference_year - birth_year,
    age_group  = case_when(
      age <= 29 ~ "young (18-29)",
      age >= 30 & age <= 39 ~ "30er (18-29)",
      age >= 40 & age <= 49 ~ "40er (18-29)",
      age >= 50             ~ "older (30+)",
      TRUE                  ~ NA_character_
    ),
    time_period = as.Date(field_start),
    vote_intention = factor(e0003a_1,
      levels = 1:7,
      labels = c(
        "CDU/CSU",
        "SPD",
        "AfD",
        "FDP",
        "Die Linke",
        "Bündnis 90/Die Grünen",
        "Andere Partei"
      )
    )
  ) %>%
  filter(!is.na(age_group), !is.na(vote_intention))

plot <- ggplot(df_vote_intention, aes(x = time_period, fill = vote_intention)) +
  geom_bar(position = "fill") +
  facet_wrap(~ age_group, ncol = 1) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months",
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = source_palette, name = "Parteipräferenz") +
  labs(
    title    = "Parteipräferenz nach Altersgruppe über Zeit",
    subtitle = "Anteil der Befragten pro Parteipräferenz, aufgeteilt nach Jung (18-29) vs. Älter (30+)",
    x        = "Erhebungszeitraum (Beginn)",
    y        = "Anteil"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", margin = margin(b = 8)),
    legend.position  = "right",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 12)
  )

ggsave(plot, filename = "plot_vote_intention_by_agegroup.png", width = 11, height = 9, dpi = 150)


# plot social media relevance over time by vote intention


df_counts <- data_sm_relevance |>
  group_by(time_period, vote_intention_zweitstimme, media_info_source) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(time_period, vote_intention_zweitstimme) |>
  mutate(share = n / sum(n)) |>
  ungroup() |> filter(!is.na(vote_intention_zweitstimme))

 
# Plot — faceted by age group, one line per source
p2 <- ggplot(df_counts, aes(x = time_period, y = share, colour = media_info_source, group = media_info_source)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ vote_intention_zweitstimme, ncol = 1) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months",
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  # scale_colour_manual(values = source_palette, name = "Vote intention") +
  labs(
    title    = "Political information source by vote intention (Zweitstimme) over time",
    subtitle = "Share of respondents per source, split by vote intention",
    x        = "Survey period (start)",
    y        = "Share"
  ) +
  theme_minimal(oase_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", margin = margin(b = 8)),
    legend.position  = "right",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 12)
  )

# Save
# ggsave("plot_sources_by_vote_intention.png", plot = p2, width = 11, height = 9, dpi = 150)
# p2








## Bundestag Speeches


load the data


bt_speeches <- read.csv("data/all_data_combined.csv")


calculate summary statisitics




bt_speeches <- bt_speeches |> group_by(Party) |> mutate(n_party_members = length(unique(MPID)))

sum_stat = subset(bt_speeches, select = c("Party", "n_party_members")) |> unique()

print(sum_stat)
print(bt_speeches[bt_speeches$n_party_members == max(bt_speeches$n_party_members),]$Party |> unique())



# QTA methods


bt_speeches|> head()





# PopBERT evaluation


bt_speeches <- bt_speeches |> mutate(populist_mean = (Anti.Elitism + People.Centrism + Left.Wing.Host.Ideology + Right.Wing.Host.Ideology)/4)

bt_speeches <- bt_speeches |> mutate(Party = gsub("[^a-zA-Z0-9äöüß/ ]", "", Party))

saveRDS(bt_speeches, "data/bt_speeches.rds")




colnames(bt_speeches)
score_over_time_perparty <- bt_speeches |> filter(!grepl("fraktionslos", Party))
nrow(score_over_time_perparty) - nrow(bt_speeches)



score_over_time_plot <- ggplot(score_over_time_perparty, aes(x=Date, y=Anti.Elitism, group=Party, color=Party)) +
geom_point() +
geom_smooth() +
facet_wrap( ~ Party)
score_over_time_plot |> ggsave(filename="plot_pop_score_overtime_byparty.png")




bt_speeches <- bt_speeches |> group_by(MPID) |> mutate(mean_pop_per_speaker = mean(populist_mean))

bt_speeches |> arrange(desc(mean_pop_per_speaker)) |> subset(select=c(MPID, mean_pop_per_speaker, Party, sentence))
most_populist_speakers <- bt_speeches[bt_speeches$mean_pop_per_speaker == max(bt_speeches$mean_pop_per_speaker),]

View(most_populist_speakers)



# regression analysis with dv = populist_mean, iv = relevance_sm, control for party_affiliation, speaker_fixed_effects

topic_proportions_base <- readRDS("data/topic_proportions_digitization.rds")
topic_weighted <- readRDS("data/topic_populism_soft_weighted_digitization.rds")
topic_proportions <- topic_proportions_base$merged

p <- ggplot(topic_proportions, aes(x=top_topic, y=populist_mean)) +
  geom_point()

ggsave(p, filename = "plot_populism_by_topic.png", width = 10, height = 6, dpi = 150)

