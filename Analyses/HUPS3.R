# Setup -------------------------------------------------------------------

# Dependencies
if (!require(needs)) {install.packages("needs"); library(needs)}
if (!require(goallabr)) {
  if (!require(devtools)) {install.packages("devtools"); library(devtools)}
  install_github("chrisharrisUU/goallabr")
}
needs(BayesFactor, dplyr, ggplot2, ggsci, goallabr,
      gridExtra, here, magrittr, tidyr, papaja, purrr)
prioritize(dplyr)

source(here("Auxiliary/HUPS3_init.R"))

# Color palette for graphs
pal <- ggsci::pal_uchicago()(5)[c(3, 5, 1)]


# Functions ---------------------------------------------------------------

# Import functions from GitHub
# https://stackoverflow.com/a/35720824/10357426
source_https <- function(u, unlink.tmp.certs = FALSE) {
  # load package
  if (!require(RCurl)) {install.packages("RCurl"); library(RCurl)}
  
  # read script lines from website using a security certificate
  if (!file.exists(here("Auxiliary/cacert.pem"))) {
    download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = here("Auxiliary/cacert.pem"))
  }
  script <- getURL(u, followlocation = TRUE, cainfo = here("Auxiliary/cacert.pem"))
  if (unlink.tmp.certs) {unlink(here("Auxiliary/cacert.pem"))}
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir = .GlobalEnv)
}
# My functions for outputting inference tests in APAish style
source_https("https://raw.githubusercontent.com/chrisharrisUU/testoutputs/master/functions.R")


# Canonical sorting -------------------------------------------------------

df3 <- df3_raw %>%
  # Recode primacy --> frequent option is right
  mutate(pref1_1 = ifelse(primacy == "right frequent", pref1_raw, 100 - pref1_raw),
         pref2_1 = ifelse(primacy == "right frequent", pref2_raw, 100 - pref2_raw),
         empl_pos = ifelse(primacy == "right frequent", empl1_raw, (-1) * empl1_raw),
         empl_neg = ifelse(primacy == "right frequent", empl2_raw, (-1) * empl2_raw),
         like_pos = ifelse(primacy == "right frequent", like1_raw, (-1) * like1_raw),
         like_neg = ifelse(primacy == "right frequent", like2_raw, (-1) * like2_raw)) %>%
  # Merge impression variable (only asked once)
  mutate(impr_l_raw = ifelse(order == "positive first", impr1_l_raw, impr2_l_raw),
         impr_r_raw = ifelse(order == "positive first", impr1_r_raw, impr2_r_raw)) %>%
  # Recode primacy --> frequent option is right for difference measures
  mutate(impr_dp = ifelse(primacy == "left frequent", impr_l_raw - impr_r_raw, impr_r_raw - impr_l_raw) / 100,
         cond_pos_dp = ifelse(primacy == "left frequent", cond1_l_raw - cond1_r_raw, cond1_r_raw - cond1_l_raw) / 100,
         cond_neg_dp = ifelse(primacy == "left frequent", cond2_l_raw - cond2_r_raw, cond2_r_raw - cond2_l_raw) / 100,
         conf_pos_dp = ifelse(primacy == "left frequent", conf1_l_raw - conf1_r_raw, conf1_r_raw - conf1_l_raw) / 100,
         conf_neg_dp = ifelse(primacy == "left frequent", conf2_l_raw - conf2_r_raw, conf2_r_raw - conf2_l_raw) / 100) %>%
  # Rescale
  mutate(pref_pos = pref1_1 - 50,
         pref_neg = pref2_1 - 50)

### Note to self: not recoded for condition (hups2 is!). Expectation: above .5 in smiles, below .5 in frowns

# Demographic data --------------------------------------------------------

# Summary of most relevant demographic data
df3 %>%
  summarise(N = n(),
            female = length(which(gender == "female")),
            age_mean = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            Psychstudents_percent = length(which(psych == "Yes")) / n() * 100)

# Level of education
df3 %>%
  group_by(edu) %>%
  summarise(count = length(edu))

# Percentage degree
length(which(as.integer(df3$edu) > 2)) / nrow(df3)


# Prolific ----------------------------------------------------------------

# # Create CSV of all accepted IDs
# df3 %>%
#   select(participant.id) %>%
#   distinct(participant.id) %>%
#   write.csv(file = "Output/Prolific/hups3_Prolific_accepted.csv", row.names = FALSE)

# Be around -----------------------------------------------------------
df3 %>%
  mutate(like_cond_pos = case_when(condition == "smiles" & like_pos == 1 ~ "Smiles\nexpected\nbias",
                                   condition == "smiles" & like_pos == -1 ~ "Smiles\nunexpected\nbias",
                                   condition == "frowns" & like_pos == -1 ~ "Frowns\nexpected\nbias",
                                   condition == "frowns" & like_pos == 1 ~ "Frowns\nunexpected\nbias"),
         like_cond_neg = case_when(condition == "smiles" & like_neg == -1 ~ "Smiles\nexpected\nbias",
                                   condition == "smiles" & like_neg == 1 ~ "Smiles\nunexpected\nbias",
                                   condition == "frowns" & like_neg == 1 ~ "Frowns\nexpected\nbias",
                                   condition == "frowns" & like_neg == -1 ~ "Frowns\nunexpected\nbias")) %>%
  pivot_longer(cols = c("like_cond_pos", "like_cond_neg"),
               names_to = "posneg",
               values_to = "like_cond") %>%
  mutate(like_cond = factor(like_cond, levels = c("Smiles\nexpected\nbias",
                                                  "Smiles\nunexpected\nbias",
                                                  "Frowns\nexpected\nbias",
                                                  "Frowns\nunexpected\nbias"))) %>%
  mutate(posneg = factor(posneg, levels = c("like_cond_pos", "like_cond_neg"), labels = c("positive_framing", "negative_framing"))) %>%
  ggplot(aes(x = like_cond)) +
  geom_bar() +
  theme_apa() +
  labs(x = "Conditions",
       y = "Employability") +
  theme(legend.position = "none") +
  facet_wrap(~posneg)

# Distribution
distr_like <- df3 %>%
  pivot_longer(cols = c("like_pos", "like_neg"),
               names_to = "posneg",
               values_to = "like_cond") %>%
  mutate(like_cond = ifelse(like_cond == 1, 1, 0)) %$%
  table(like_cond, posneg, condition)
distr_like


# Preference --------------------------------------------------------------

# Graph
df3 %>%
  pivot_longer(cols = c("pref_pos", "pref_neg"),
               names_to = "posneg",
               values_to = "pref") %>%
  mutate(posneg = factor(posneg, levels = c("pref_pos", "pref_neg"), labels = c("positive_framing", "negative_framing"))) %>%
  ggplot(aes(x = condition,
             y = pref,
             fill = condition)) +
  geom_violin() + 
  stat_summary(fun.data = "mean_sdl", geom = "crossbar", width = 0.2, fun.args = list(mult = 1)) +
  geom_jitter(size = 0.5, width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_fill_manual(values = pal) +
  theme_apa() +
  labs(x = "Condition",
       y = "Relative contingency estimates") +
  theme(legend.position = "none") +
  facet_wrap(~posneg)
# ggsave(filename = "Output/Graphs/Exp2_pref.svg", device = "svg", dpi = 320, width = 9.08, height = 5.72)

### Descriptives
df3 %>%
  pivot_longer(cols = c("pref_pos", "pref_neg"),
               names_to = "posneg",
               values_to = "pref") %>%
  mutate(posneg = factor(posneg, levels = c("pref_pos", "pref_neg"), labels = c("positive_framing", "negative_framing"))) %>%
  group_by(condition, posneg) %>%
  summarize(M = printnum(mean(pref)),
            SD = printnum(sd(pref)))

# Employability -----------------------------------------------------------
df3 %>%
  mutate(like_empl_pos = case_when(condition == "smiles" & empl_pos == 1 ~ "Smiles\nexpected\nbias",
                                   condition == "smiles" & empl_pos == -1 ~ "Smiles\nunexpected\nbias",
                                   condition == "frowns" & empl_pos == -1 ~ "Frowns\nexpected\nbias",
                                   condition == "frowns" & empl_pos == 1 ~ "Frowns\nunexpected\nbias"),
         like_empl_neg = case_when(condition == "smiles" & empl_neg == -1 ~ "Smiles\nexpected\nbias",
                                   condition == "smiles" & empl_neg == 1 ~ "Smiles\nunexpected\nbias",
                                   condition == "frowns" & empl_neg == 1 ~ "Frowns\nexpected\nbias",
                                   condition == "frowns" & empl_neg == -1 ~ "Frowns\nunexpected\nbias")) %>%
  pivot_longer(cols = c("like_empl_pos", "like_empl_neg"),
               names_to = "posneg",
               values_to = "empl_cond") %>%
  mutate(empl_cond = factor(empl_cond, levels = c("Smiles\nexpected\nbias",
                                                  "Smiles\nunexpected\nbias",
                                                  "Frowns\nexpected\nbias",
                                                  "Frowns\nunexpected\nbias"))) %>%
  mutate(posneg = factor(posneg, levels = c("like_empl_pos", "like_empl_neg"), labels = c("positive_framing", "negative_framing"))) %>%
  ggplot(aes(x = empl_cond)) +
  geom_bar() +
  theme_apa() +
  labs(x = "Conditions",
       y = "Employability") +
  theme(legend.position = "none") +
  facet_wrap(~posneg)

# # Distribution
# distr_empl <- df3 %>%
#   mutate(empl = ifelse(empl == 1, 1, 0)) %$%
#   table(empl, condition)

# Impression --------------------------------------------------------------
df3 %>%
  ggplot(aes(x = condition,
             y = impr_dp,
             fill = condition)) +
  geom_violin() + 
  stat_summary(fun.data = "mean_sdl", geom = "crossbar", width = 0.2, fun.args = list(mult = 1)) +
  geom_jitter(size = 0.5, width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_fill_manual(values = pal) +
  theme_apa() +
  labs(x = "Condition",
       y = "Impression estimates") +
  theme(legend.position = "none") +
  facet_wrap(~order)
# ggsave(filename = "Output/Graphs/Exp2_impr.svg", device = "svg", dpi = 320, width = 9.08, height = 5.72)

### Descriptives
df3 %>%
  group_by(condition, order) %>%
  summarize(M = printnum(mean(impr_dp)),
            SD = printnum(sd(impr_dp)))

# Conditional estimates ---------------------------------------------------
df3 %>%
  pivot_longer(cols = c("cond_pos_dp", "cond_neg_dp"),
               names_to = "posneg",
               values_to = "cond_dp") %>%
  mutate(posneg = factor(posneg, levels = c("cond_pos_dp", "cond_neg_dp"), labels = c("positive_framing", "negative_framing"))) %>%
  ggplot(aes(x = condition,
             y = cond_dp,
             fill = condition)) +
  geom_violin() + 
  stat_summary(fun.data = "mean_sdl", geom = "crossbar", width = 0.2, fun.args = list(mult = 1)) +
  geom_jitter(size = 0.5, width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_fill_manual(values = pal) +
  theme_apa() +
  labs(x = "Condition",
       y = "Conditional probability estimates") +
  theme(legend.position = "none") +
  facet_wrap(~posneg)
# ggsave(filename = "Output/Graphs/Exp2_cond.svg", device = "svg", dpi = 320, width = 9.08, height = 5.72)

### Descriptives
df3 %>%
  pivot_longer(cols = c("cond_pos_dp", "cond_neg_dp"),
               names_to = "posneg",
               values_to = "cond_dp") %>%
  mutate(posneg = factor(posneg, levels = c("cond_pos_dp", "cond_neg_dp"), labels = c("positive_framing", "negative_framing"))) %>%
  group_by(condition, posneg) %>%
  summarize(M = printnum(mean(cond_dp)),
            SD = printnum(sd(cond_dp)))

# Confidence --------------------------------------------------------------

df3 %>%
  pivot_longer(cols = c("conf_pos_dp", "conf_neg_dp"),
               names_to = "posneg",
               values_to = "conf_dp") %>%
  mutate(posneg = factor(posneg, levels = c("conf_pos_dp", "conf_neg_dp"), labels = c("positive_framing", "negative_framing"))) %>%
  ggplot(aes(x = condition,
             y = conf_dp,
             fill = condition)) +
  geom_violin() + 
  stat_summary(fun.data = "mean_sdl", geom = "crossbar", width = 0.2, fun.args = list(mult = 1)) +
  geom_jitter(size = 0.5, width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_fill_manual(values = pal) +
  theme_apa() +
  labs(x = "Condition",
       y = "Confidence estimates") +
  theme(legend.position = "none") +
  facet_wrap(~posneg)
# ggsave(filename = "Output/Graphs/Exp2_conf.svg", device = "svg", dpi = 320, width = 9.08, height = 5.72)

### Descriptives
df3 %>%
  pivot_longer(cols = c("conf_pos_dp", "conf_neg_dp"),
               names_to = "posneg",
               values_to = "conf_dp") %>%
  group_by(condition, posneg) %>%
  summarize(M = printnum(mean(conf_dp)),
            SD = printnum(sd(conf_dp)))

# ### Test
# df3 %>%
#   as.data.frame() %>%
#   ttestBF(formula = conf_dp ~ condition,
#           nullInterval = c(-Inf, Inf),
#           data = .) %>%
#   printBFt()
# ttest(data = df3,
#       y = conf_dp,
#       x = condition,
#       dir = "two.sided")
