# Topic modelling of bundestag speeches of the 20th legislative period

library(tidyr)
library(dplyr)
library(stringfish)
library(stringr)
library(keyATM)
library(quanteda.textstats)
library(quanteda)





# Speech-level data: original text from DIP API, used as keyATM documents
bt_speeches_dpi <- read.csv("data/bt_speeches_dpi_api.csv")
colnames(bt_speeches_dpi)[colnames(bt_speeches_dpi) == "topic"] <- "dpi_topic"
bt_speeches_dpi$Date <- as.Date(bt_speeches_dpi$Date)
bt_speeches_dpi <- bt_speeches_dpi |> mutate(Party = case_when(
  Party == "BSW (Gruppe)"          ~ "BSW",
  Party == "Die Linke (Gruppe)"    ~ "Die Linke",
  Party == "BÜNDNIS 90/DIE GRÜNEN" ~ "B90/Die Grünen",
  TRUE ~ Party
))

# Sentence-level data: PopBERT scores, joined back to speeches after topic modelling
bt_sentences <- read.csv("scripts_not_render/all_data_combined.csv")
bt_sentences <- bt_sentences |> filter(nchar(sentence) >= 10)
bt_sentences <- bt_sentences |> group_by(Party) |> mutate(n_party_members = length(unique(Name)))
bt_sentences <- bt_sentences |> mutate(
  populist_mean = (Anti.Elitism + People.Centrism + Left.Wing.Host.Ideology + Right.Wing.Host.Ideology) / 4
)
colnames(bt_sentences)[colnames(bt_sentences) == "topic"] <- "dpi_topic"
bt_sentences <- bt_sentences |> mutate(Party = case_when(
  Party == "BSW (Gruppe)"          ~ "BSW",
  Party == "Die Linke (Gruppe)"    ~ "Die Linke",
  Party == "BÜNDNIS 90/DIE GRÜNEN" ~ "B90/Die Grünen",
  TRUE ~ Party
))

cat("Speeches:", nrow(bt_speeches_dpi), "| Sentences:", nrow(bt_sentences), "\n")

# Disclaimer: following comment was added by Claude Code.
# Corpus at speech level — speeches are the natural topical unit for keyATM.
# bt_speeches_dpi$speech_id (sequential 1..N) is used as corpus docid.
# bt_sentences$speech_id = Python's pandas index = bt_speeches_dpi$X (original R row names);
# the join after topic modelling matches bt_sentences$speech_id against merged_speeches$X.
bt_corpus <- corpus(bt_speeches_dpi, text_field = "Speech", docid_field = "speech_id")


toks <- tokens(
  bt_corpus,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = FALSE
) |>
  tokens_remove(c(stopwords("de"), "kolleginnen", "kollegen", "frau", "herr", "damen", "herren", "geehrte",
  "geehrten", "liebe", "vielen", "dank", "mal", "geehrter", "dr", "dass", "verehrten", "verehrte", "herzlichen", "dank",
  "ja", "schon", "stimmen", "enthaltungen", "angenommen", "abstimmung", "gestimmt", "ungültige", "neinstimmen", "jastimmen",
  "gibt", "geht", "ganz", "immer", "dafür", "müssen", "brauchen", "heute", "gerade", "viele"))
# überall: gibt mehr menschen müssen dafür immer ganz viel mehr brauchen



collocations <- textstat_collocations(toks, min_count = 10)
print(collocations[order(-collocations$count),][1:50,])

toks <- tokens_compound(
  toks,
  phrase(c(
    "wer stimmt dagegen",
    "wer stimmt dafür",
    "wer enthält sich",
    "wer stimmt",
    "wer enthält",
    "CDU CSU",
    "CDU csu-fraktion",
    "fraktionen SPD bündnis 90 grünen fdp",
    "fraktion bündnis 90 grünen",
    "bündnis 90 grünen",
    "fraktion CDU CSU",
    "fraktion linke",
    "fraktion SPD",
    "fraktion AfD",
    "fraktion FDP",
    "wladimir putin",
    "europäische union",
    "europäischen union",
    "europäischer union",
    "europäischer rat",
    "europäisches parlament",
    "europäischer gerichtshof",
    "europäische kommission",
    "europäischen kommission",
    "europäische zentralbank",
    "deutschen bundestag",
    "deutscher bundestag",
    "deutsche bundesregierung",
    "demokratische parteien",
    "demokratischen parteien",
    "demokratischen fraktionen",
    "demokratische fraktionen",
    "bürgerinnen bürger",
    "bürger innen",
    "zuschauer innen",
    "erneuerbare energie",
    "erneuerbare energien",
    "erneuerbaren energien",
    "soziale gerechtigkeit",
    "hartz iv",
    "milliarden euro",
    "millionen euro",
    "unserem land",
    "kommen bitte schluss",
    "soldatinnen soldaten",
    "unseres landes",
    "millionen menschen",
    "unserer gesellschaft",
    "olaf scholz",
    "bundesrepublik deutschland",
    "letzten jahren",
    "seit jahren",
    "letzten jahr",
    "mehr geld"
  ))
)


toks <- toks |> tokens_remove(c("wer_stimmt_dagegen", "wer_stimmt_dafür", "wer_stimmt", "wer_enthält", "wer_enthält_sich"))



dfm <- dfm(toks)
dfm_trim <- dfm_trim(dfm, min_docfreq = 25, min_termfreq = 35)
dfm_trim_clean <- dfm_subset(dfm_trim, ntoken(dfm_trim) > 0)
keyatm_obj <- keyATM_read(dfm_trim_clean)

# disclaimer: github copilot generated most of the keywords for the topic analysis, the user adjusted where necessary.
topics_digitization = list(
              agriculture = c("feld", "landwirtschaft", "bauern", "nahrungsmittel", "lebensmittel"),
              culturemedia = c("kultur", "feiern", "medien", "fernsehen", "radio", "zeitungen"),
              digitalizationtech = c("internet", "digital", "online", "netz", "plattform", "digitalisierung"),
              economy = c("arbeit", "unternehmen", "wirtschaft", "job", "arbeitsplätze", "lohn", "industrie"),
              eudcationscience = c("schule", "bildung", "universität", "forschung", "wissenschaft", "lehrer"),
              environmentclimate = c("klima", "umwelt", "energie", "klimaschutz", "klimawandel", "erneuerbare", "erneuerbare_energien", "erneuerbaren_energien"),
              financetax = c("finanz", "geld", "haushalt", "schulden", "etat", "steuern", "steuer"),
              foreign = c("ausland", "international", "europäisch", "diplomatie", "nato", "china", "amerika", "usa"),
              government = c("regierung", "bundesregierung", "kanzler", "minister", "bundestag", "parlament", "olaf_scholz"),
              party = c("partei", "parteien", "fraktion", "abgeordnete", "mitglied", "wahl", "wahlkampf", "cdu", "csu", "spd", "bündnis_90_grünen", "linke", "afd", "fdp"),
              regional = c("region", "länder", "bundesländer", "kommunen", "städte", "dörfer"),
              security = c("sicherheit", "polizei", "terrorismus", "kriminalität", "sicher"),
              social = c("sozial", "rente", "gesundheit", "pflege", "arbeitslosigkeit", "familie", "soziale_gerechtigkeit"),
              transportinfrastructure = c("verkehr", "infrastruktur", "bahn", "auto", "flughafen", "brücken", "öffentlichen", "autobahn", "schiene", "fahrrad"),
              # additional topics not from the digitization paper
              migration = c("migration", "flüchtlinge", "asyl", "integration", "grenzen", "integrieren"),
              ukraine = c("ukraine", "russland", "krieg", "putin", "angriffskrieg", "invasion", "drohnenkrieg"),
              procedural = c("tagesordnung", "ergebnis", "antrag", "gesetz", "debatte", "schriftführer", "schriftführerinnen", "drucksache", "präsident", "präsidentin", "ausschuss", "rede", "plenum", "wort", "zwischenfrage", "intervention", "protokoll", "tagesordnungspunkt", "beschlussempfehlung", "aussprache")
              )

saveRDS(keyatm_obj, file = "data/keyatm_obj3.rds")

model_path <- "data/keyatm_model_digitization3.rds"
if (file.exists(model_path)) {
  keyatm_model_digitization <- readRDS(model_path)
} else {
  keyatm_model_digitization <- keyATM(
    keyatm_obj,
    model = "base",
    no_keyword_topics = 2,
    keywords = topics_digitization,
    options = list(seed = 420)
  )
  saveRDS(keyatm_model_digitization, model_path)
}

top_words(keyatm_model_digitization, n=20)


# Disclaimer: following code was partly created by Claude Sonnet 4.6 through iterations of prompting

create_topic_df <- function(keyatm_model, bt_speeches_dpi, bt_sentences, dfm) {
  # keyATM theta rownames may be NULL in some package versions; fall back to dfm docnames.
  # docnames(dfm) == corpus docnames == bt_speeches_dpi$speech_id (sequential 1..N).
  topic_df <- as.data.frame(keyatm_model$theta)
  doc_ids  <- rownames(keyatm_model$theta)
  if (is.null(doc_ids)) doc_ids <- docnames(dfm)
  topic_df$speech_id <- as.integer(doc_ids)

  # Attach topic proportions to speech-level data and derive top_topic
  merged_speeches <- bt_speeches_dpi |>
    left_join(topic_df, by = "speech_id")
  topic_cols <- grep("^(\\d+_|Other_)", names(merged_speeches), value = TRUE)
  has_topics <- rowSums(!is.na(merged_speeches[, topic_cols])) > 0
  merged_speeches$top_topic <- NA_integer_
  merged_speeches$top_topic[has_topics] <- max.col(
    merged_speeches[has_topics, topic_cols], ties.method = "first"
  )
  merged_speeches$top_topic_name <- NA_character_
  merged_speeches$top_topic_name[has_topics] <- topic_cols[merged_speeches$top_topic[has_topics]]

  # Propagate speech-level topic columns to sentence level.
  # bt_sentences$speech_id = Python's idx (the unnamed R row-index "X" column).
  # merged_speeches$X carries those same values, so join on X from the right side.
  speech_join_cols <- c("X", topic_cols, "top_topic", "top_topic_name")
  merged_sentences <- bt_sentences |>
    left_join(merged_speeches[, speech_join_cols], by = c("speech_id" = "X"))

  return(list(merged_sentences = merged_sentences, merged_speeches = merged_speeches))
}

merged_digitization <- create_topic_df(
  keyatm_model    = keyatm_model_digitization,
  bt_speeches_dpi = bt_speeches_dpi,
  bt_sentences    = bt_sentences,
  dfm             = dfm_trim_clean
)

cat("Sentences with missing top_topic:", sum(is.na(merged_digitization$merged_sentences$top_topic)), "\n")

saveRDS(merged_digitization, "data/topic_proportions_dpi3.rds")

# Disclaimer: following Code was created by Claude Sonnet 4.6
# Prompt: "Here you find the code for my topic modelling. At the bottom is the start of a keyness analysis. Adjust the last part of the code so that the keyness analysis is split by party. Also save the result so that I can load it and easily create a keyness plot after loading the result in another file."
# --- Keyness analysis split by party ---
# For each party, compute keyness of that party's documents vs. all others.
# Uses the full (untrimmed) dfm so no documents are silently dropped.

# party_var aligned to speech-level DFM (docnames are speech_ids as strings)
speech_party_lookup <- setNames(bt_speeches_dpi$Party, as.character(bt_speeches_dpi$speech_id))
party_var <- speech_party_lookup[docnames(dfm)]

parties <- unique(party_var[!is.na(party_var)])

keyness_by_party <- lapply(setNames(parties, parties), function(p) {
  target_docs <- which(party_var == p)
  textstat_keyness(dfm, target = target_docs)
})

saveRDS(keyness_by_party, "data/keyness_by_party3.rds")
