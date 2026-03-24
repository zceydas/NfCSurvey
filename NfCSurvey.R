###Code from Jupyter
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(broom)

library(dplyr)

# Finished, Consent_agree and Eligibility_agree need to == 1, Psychedelics_EverUse need to be 1 to continue analysis


df <- read.csv("/Users/eweisma1/Downloads/NFC.csv", check.names = FALSE)
names(df) <- trimws(names(df))
names(df) <- gsub("\\.$", "", names(df))
df

df_clean <- df %>%
  filter(
    Finished == 1,
    Consent_agree == 1,
    Eligibility_agree == 1,
    Psychedelics_EverUse == 1
  )

dim(df_clean)

# now we will compare user response to attention_chk_1 correct being 4; attention_chk_2 correct being 3; attention_chk_3 correct being 4, if the mean accuracy across all three checks is below 66%, then do not include that participant so another cleaning level
df_clean <- df_clean %>%
  mutate(
    # convert to numeric
    attention_chk_1 = as.numeric(attention_chk_1),
    attention_chk_2 = as.numeric(attention_chk_2),
    attention_chk_3 = as.numeric(attention_chk_3),
    
    # count wrong answers, ignoring NA
    attention_wrong = rowSums(
      cbind(
        attention_chk_1 != 4,
        attention_chk_2 != 3,
        attention_chk_3 != 4
      ),
      na.rm = TRUE
    )
  )
df_clean_attention <- df_clean %>% filter(attention_wrong <= 1)
df_clean_attention <- cbind(ID = seq_len(nrow(df_clean_attention)), df_clean_attention)
df_clean_attention
data_clean <- df_clean_attention

#calculate MEQ Scores
source("QA_Library.R")
meq_cols <- paste0("MEQ_", 1:30)
data_clean[meq_cols] <- lapply(data_clean[meq_cols], as.numeric)
meq_output <- meq.scoring(data_clean)

#calculate CEQ Scores
ceq_cols <- paste0("CEQ_", 1:26)
data_clean[ceq_cols] <- lapply(data_clean[ceq_cols], as.numeric)
ceq_output <- ceq.scoring(data_clean)

#calculate Psychological Insight (PIQ) Scores
piq_cols <- paste0("PsychInsight_", 1:23)
data_clean[piq_cols] <- lapply(data_clean[piq_cols], as.numeric)
piq_output <- psychinsight.scoring(data_clean)

#calculate AAQ pre and post scores
aaqpre_cols <- paste0("AAQ_pre_", 1:7)
data_clean[aaqpre_cols] <- lapply(data_clean[aaqpre_cols], as.numeric)
aaqpre_output <- aaq.scoring(data_clean[, c("ID", aaqpre_cols)])
aaqpost_cols <- paste0("AAQ_post_", 1:7)
data_clean[aaqpost_cols] <- lapply(data_clean[aaqpost_cols], as.numeric)
aaqpost_output <- aaq.scoring(data_clean[, c("ID", aaqpost_cols)])
aaq_combined <- merge(aaqpre_output, aaqpost_output, by = "keeps", suffixes = c("_pre", "_post"))
aaq_combined$aaq_delta <- aaq_combined$aaq.total_post - aaq_combined$aaq.total_pre
names(aaq_combined)[names(aaq_combined) == "keeps"] <- "ID"

#calculate NFC pre and post scores
source("NFC_functions.R")
nfcpre_cols <- paste0("NfC_Pre_", 1:18)
data_clean[nfcpre_cols] <- lapply(data_clean[nfcpre_cols], as.numeric)
nfcpre_output <- nfc.scoring(data_clean[, c("ID", nfcpre_cols)])
nfcpost_cols <- paste0("NfC_Post_", 1:18)
data_clean[nfcpost_cols] <- lapply(data_clean[nfcpost_cols], as.numeric)
nfcpost_output <- nfc.scoring(data_clean[, c("ID", nfcpost_cols)])
nfc_combined <- merge(nfcpre_output, nfcpost_output, by = "keeps", suffixes = c("_pre", "_post"))
nfc_combined$nfc_delta <- nfc_combined$nfc.total_post - nfc_combined$nfc.total_pre
names(nfc_combined)[names(nfc_combined) == "keeps"] <- "ID"

#calculate CFS pre and post scores
cfspre_cols <- paste0("CFS_Pre_", 1:12)
data_clean[cfspre_cols] <- lapply(data_clean[cfspre_cols], as.numeric)
cfspre_output <- cfs.scoring(data_clean[, c("ID", cfspre_cols)])
cfspost_cols <- paste0("CFS_Post_", 1:12)
data_clean[cfspost_cols] <- lapply(data_clean[cfspost_cols], as.numeric)
cfspost_output <- cfs.scoring(data_clean[, c("ID", cfspost_cols)])
cfs_combined <- merge(cfspre_output, cfspost_output, by = "keeps", suffixes = c("_pre", "_post"))
cfs_combined$cfs_delta <- cfs_combined$cfs.total_post - cfs_combined$cfs.total_pre
names(cfs_combined)[names(cfs_combined) == "keeps"] <- "ID"

#calculate WMQ pre and post scores
wmqpre_cols <- paste0("WMQ_Pre_", 1:30)
data_clean[wmqpre_cols] <- lapply(data_clean[wmqpre_cols], as.numeric)
wmqpre_output <- wmq.scoring(data_clean[, c("ID", wmqpre_cols)])
wmqpost_cols <- paste0("WMQ_Post_", 1:30)
data_clean[wmqpost_cols] <- lapply(data_clean[wmqpost_cols], as.numeric)
wmqpost_output <- wmq.scoring(data_clean[, c("ID", wmqpost_cols)])
wmq_combined <- merge(wmqpre_output, wmqpost_output, by = "keeps", suffixes = c("_pre", "_post"))
wmq_combined$wmq_totaldelta <- wmq_combined$wmq.total_post - wmq_combined$wmq.total_pre
wmq_combined$wmq_storagedelta <- wmq_combined$wmq.storage_post - wmq_combined$wmq.storage_pre
wmq_combined$wmq_attentiondelta <- wmq_combined$wmq.attention_post - wmq_combined$wmq.attention_pre
wmq_combined$wmq_executivedelta <- wmq_combined$wmq.executive_post - wmq_combined$wmq.executive_pre
names(wmq_combined)[names(wmq_combined) == "keeps"] <- "ID"

#calculate DASS pre and post scores
dasspre_cols <- paste0("DASS_pre_", 1:21)
data_clean[dasspre_cols] <- lapply(data_clean[dasspre_cols], as.numeric)
dasspre_output <- dassprepost.scoring(data_clean[, c("ID", dasspre_cols)])
dasspost_cols <- paste0("DASS_post_", 1:21)
data_clean[dasspost_cols] <- lapply(data_clean[dasspost_cols], as.numeric)
dasspost_output <- dassprepost.scoring(data_clean[, c("ID", dasspost_cols)])
dass_combined <- merge(dasspre_output, dasspost_output, by = "keeps", suffixes = c("_pre", "_post"))
dass_combined$dass_depressiondelta <- dass_combined$dass.depression_post - dass_combined$dass.depression_pre
dass_combined$dass_anxietydelta <- dass_combined$dass.anxiety_post - dass_combined$dass.anxiety_pre #negative indicates improvement in symptoms
dass_combined$dass_stressdelta <- dass_combined$dass.stress_post - dass_combined$dass.stress_pre
names(dass_combined)[names(dass_combined) == "keeps"] <- "ID"

#Make bar graphs for all change scores as a function of psychedelics_when
aaq_combined <- left_join(aaq_combined, data_clean[, c("ID", "Psychedelics_when")], by = "ID")
nfc_combined <- left_join(nfc_combined, data_clean[, c("ID", "Psychedelics_when")], by = "ID")
cfs_combined <- left_join(cfs_combined, data_clean[, c("ID", "Psychedelics_when")], by = "ID")
wmq_combined <- left_join(wmq_combined, data_clean[, c("ID", "Psychedelics_when")], by = "ID")
dass_combined <- left_join(dass_combined, data_clean[, c("ID", "Psychedelics_when")], by = "ID")
label_levels <- c("<1mo", "3-6mo", "6-12mo", "1-3yr", ">3yr")

for(df in list(aaq_combined, nfc_combined, cfs_combined, wmq_combined, dass_combined)){
  df$Psychedelics_when <- factor(df$Psychedelics_when, levels = 1:5, labels = label_levels)
}
plot_delta_when(nfc_combined, "nfc_delta", "NFC Change")
plot_delta_when(aaq_combined, "aaq_delta", "AAQ Change")
plot_delta_when(cfs_combined, "cfs_delta", "CFS Change")
plot_delta_when(wmq_combined, "wmq_totaldelta", "WMQ Total Change")
plot_delta_when(wmq_combined, "wmq_storagedelta", "WMQ Storage Change")
plot_delta_when(wmq_combined, "wmq_attentiondelta", "WMQ Attention Change")
plot_delta_when(wmq_combined, "wmq_executivedelta", "WMQ Executive Change")
plot_delta_when(dass_combined, "dass_depressiondelta", "DASS Depression Change")
plot_delta_when(dass_combined, "dass_anxietydelta", "DASS Anxiety Change")
plot_delta_when(dass_combined, "dass_stressdelta", "DASS Stress Change")

#Mixed Effects of NFC
nfc_long <- nfc_combined %>%
  select(ID, nfc.total_pre, nfc.total_post) %>%
  pivot_longer(
    cols = c(nfc.total_pre, nfc.total_post),
    names_to = "time",
    values_to = "nfc_total"
  ) %>%
  mutate(
    time = factor(time, levels = c("nfc.total_pre", "nfc.total_post"), labels = c("pre", "post"))
  )
nfc_long <- nfc_long %>%
  left_join(
    data_clean %>% select(ID, Psychedelics_when),
    by = "ID"
  )
nfc_long$Psychedelics_when <- factor(
  nfc_long$Psychedelics_when,
  levels = 1:5,
  labels = c("<1mo", "3-6mo", "6-12mo", "1-3yr", ">3yr")
)
# Combine groups 2,3,4 into a single "3-36mo" group
nfc_long <- nfc_long %>%
  mutate(
    recent_user = case_when(
      Psychedelics_when == "<1mo" ~ "<1mo",
      Psychedelics_when %in% c("3-6mo", "6-12mo", "1-3yr") ~ "3-36mo",
      Psychedelics_when == ">3yr" ~ ">3yr"
    ),
    recent_user = factor(recent_user, levels = c("<1mo", "3-36mo", ">3yr"))
  )
# Random intercept for each participant
nfc_model <- lmer(nfc_total ~ time * Psychedelics_when + (1|ID), data = nfc_long)
summary(nfc_model)
# Check pre vs post NFC change within each first-use group
nfc_emm <- emmeans(nfc_model, pairwise ~ time | Psychedelics_when)
nfc_emm$contrasts  # pairwise comparisons

nfc_model_recent <- lmer(nfc_total ~ time * recent_user + (1|ID), data = nfc_long)
summary(nfc_model_recent)

nfc_emm_recent <- emmeans(nfc_model_recent, pairwise ~ time | recent_user)
nfc_emm_recent$contrasts
