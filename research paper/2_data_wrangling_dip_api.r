# Script to wrangle output from Claude script bundestag_speeches_pipeline
# Author: Robert Bowien


bt_speeches_dat <- read.csv("data/bundestag_20wp_speeches.csv")

bt_speeches <- bt_speeches_dat[bt_speeches_dat$speech_text != "", ]


bt_speeches <- bt_speeches |> subset(select=c(aktivitaet_id, datum, speaker_name, party, topic, pdf_url, speech_text))

colnames(bt_speeches) <- c("aktivitaet_id", "Date", "Name", "Party", "topic", "pdf_url", "Speech")
bt_speeches$speech_id <- seq_len(nrow(bt_speeches))

bt_speeches$Date <- as.Date(bt_speeches$Date)
nrow(bt_speeches)

head(bt_speeches$Speech)
# saveRDS(bt_speeches, "data/bt_speeches_dpi_api.rds")
write.csv(bt_speeches, "data/bt_speeches_dpi_api.csv")
