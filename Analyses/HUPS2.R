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

source(here("Auxiliary/HUPS2_init.R"))

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

df2 <- df2_raw %>%
  # Recode primacy --> frequent option is right
  mutate(pref1 = ifelse(primacy == "right frequent", pref_raw, 100 - pref_raw),
         empl1 = ifelse(primacy == "right frequent", empl_raw, (-1) * empl_raw),
         like1 = ifelse(primacy == "right frequent", like_raw, (-1) * like_raw)) %>%
  # Recode condition
  mutate(pref2 = ifelse(condition == "smiles", pref1, 100 - pref1),
         empl = ifelse(condition == "smiles", empl1, (-1) * empl1),
         like = ifelse(condition == "smiles", like1, (-1) * like1)) %>%
  mutate(impr_dp = case_when(condition == "smiles" & primacy == "left frequent"  ~ (impr_l_raw - impr_r_raw) / 100,
                             condition == "smiles" & primacy == "right frequent" ~ (impr_r_raw - impr_l_raw) / 100,
                             condition == "frowns" & primacy == "left frequent"  ~ (impr_r_raw - impr_l_raw) / 100,
                             condition == "frowns" & primacy == "right frequent" ~ (impr_l_raw - impr_r_raw) / 100),
         cond_dp = case_when(condition == "smiles" & primacy == "left frequent"  ~ (cond_l_raw - cond_r_raw) / 100,
                             condition == "smiles" & primacy == "right frequent" ~ (cond_r_raw - cond_l_raw) / 100,
                             condition == "frowns" & primacy == "left frequent"  ~ (cond_r_raw - cond_l_raw) / 100,
                             condition == "frowns" & primacy == "right frequent" ~ (cond_l_raw - cond_r_raw) / 100),
         conf_dp = case_when(condition == "smiles" & primacy == "left frequent"  ~ (conf_l_raw - conf_r_raw) / 100,
                             condition == "smiles" & primacy == "right frequent" ~ (conf_r_raw - conf_l_raw) / 100,
                             condition == "frowns" & primacy == "left frequent"  ~ (conf_r_raw - conf_l_raw) / 100,
                             condition == "frowns" & primacy == "right frequent" ~ (conf_l_raw - conf_r_raw) / 100)) %>%
  # Rescale
  mutate(pref = pref2 - 50)

# Demographic data --------------------------------------------------------

# Summary of most relevant demographic data
df2 %>%
  summarise(N = n(),
            female = length(which(gender == "female")),
            age_mean = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            Psychstudents_percent = length(which(psych == "Yes")) / n() * 100)

# Level of education
df2 %>%
  group_by(edu) %>%
  summarise(count = length(edu))

# Percentage degree
length(which(as.integer(df2$edu) > 2)) / nrow(df2)


# Prolific ----------------------------------------------------------------

# Create CSV of all accepted IDs
# df2 %>%
#   select(participant.id) %>%
#   distinct(participant.id) %>%
#   write.csv(file = "Output/Prolific/hups2_Prolific_accepted.csv", row.names = FALSE)

# Be around -----------------------------------------------------------
df2 %>%
  mutate(like_cond = case_when(condition == "smiles" & like == 1 ~ "Smiles\nexpected\nbias",
                               condition == "smiles" & like == -1 ~ "Smiles\nunexpected\nbias",
                               condition == "frowns" & like == 1 ~ "Frowns\nexpected\nbias",
                               condition == "frowns" & like == -1 ~ "Frowns\nunexpected\nbias")) %>%
  mutate(like_cond = factor(like_cond, levels = c("Smiles\nexpected\nbias",
                                                  "Smiles\nunexpected\nbias",
                                                  "Frowns\nexpected\nbias",
                                                  "Frowns\nunexpected\nbias"))) %>%
  ggplot(aes(x = like_cond)) +
  geom_bar() +
  theme_apa() +
  labs(x = "Conditions",
       y = "Employability") +
  theme(legend.position = "none")

# Distribution
distr_like <- df2 %>%
  mutate(like = ifelse(like == 1, 1, 0)) %$%
  table(like, condition)
distr_like

# Tests
# Smiles
proportionBF(y = distr_like["1", "smiles"],
             N = distr_like["1", "smiles"] + distr_like["0", "smiles"],
             p = .5,
             rscale = "ultrawide",
             nullInterval = c(.5, 1)) %>% printBFb()
binom.test(x = distr_like["1", "smiles"],
           n = distr_like["1", "smiles"] + distr_like["0", "smiles"],
           p = .5,
           alternative = "greater")

# Frowns
proportionBF(y = distr_like["1", "frowns"],
             N = distr_like["1", "frowns"] + distr_like["0", "frowns"],
             p = .5,
             rscale = "ultrawide",
             nullInterval = c(.5, 1)) %>% printBFb()
binom.test(x = distr_like["1", "frowns"],
           n = distr_like["1", "frowns"] + distr_like["0", "frowns"],
           p = .5,
           alternative = "greater")

# Preference --------------------------------------------------------------

# Graph
df2 %>%
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
  theme(legend.position = "none")
# ggsave(filename = "Output/Graphs/Exp2_pref.svg", device = "svg", dpi = 320, width = 9.08, height = 5.72)

### Descriptives
df2 %>%
  group_by(condition) %>%
  summarize(M = printnum(mean(pref)),
            SD = printnum(sd(pref)))

### Tests

# Global
df2 %>%
  mutate(condition = relevel(condition, ref = "frowns")) %>%
  lm(pref ~ condition, data = .) %>%
  summary()

# Rich
df2 %>%
  filter(condition == "smiles") %>%
  as.data.frame() %$%
  ttestBF(pref,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = pref,
      x = condition,
      mu = 0,
      sub = "smiles",
      dir = "greater")

# Impoverished
df2 %>%
  filter(condition == "frowns") %>%
  as.data.frame() %$%
  ttestBF(pref,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = pref,
      x = condition,
      mu = 0,
      sub = "frowns",
      dir = "greater")

# Strength
df2 %>%
  as.data.frame() %>%
  ttestBF(formula = pref ~ condition,
          nullInterval = c(-Inf, Inf),
          data = .) %>%
  printBFt()
ttest(data = df2,
      y = pref,
      x = condition,
      dir = "two.sided")

## Overall
df2 %>%
  as.data.frame() %$%
  ttestBF(pref,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = pref,
      x = condition,
      mu = 0,
      dir = "greater")

# Employability -----------------------------------------------------------
df2 %>%
  mutate(empl_cond = case_when(condition == "smiles" & like == 1 ~ "Smiles\nexpected\nbias",
                               condition == "smiles" & like == -1 ~ "Smiles\nunexpected\nbias",
                               condition == "frowns" & like == 1 ~ "Frowns\nexpected\nbias",
                               condition == "frowns" & like == -1 ~ "Frowns\nunexpected\nbias")) %>%
  mutate(empl_cond = factor(empl_cond, levels = c("Smiles\nexpected\nbias",
                                                  "Smiles\nunexpected\nbias",
                                                  "Frowns\nexpected\nbias",
                                                  "Frowns\nunexpected\nbias"))) %>%
  ggplot(aes(x = empl_cond)) +
  geom_bar() +
  theme_apa() +
  labs(x = "Conditions",
       y = "Employability") +
  theme(legend.position = "none")

# Distribution
distr_empl <- df2 %>%
  mutate(empl = ifelse(empl == 1, 1, 0)) %$%
  table(empl, condition)

# Tests
# Smiles
proportionBF(y = distr_empl["1", "smiles"],
             N = distr_empl["1", "smiles"] + distr_empl["0", "smiles"],
             p = .5,
             rscale = "ultrawide",
             nullInterval = c(.5, 1)) %>% printBFb()
binom.test(x = distr_empl["1", "smiles"],
           n = distr_empl["1", "smiles"] + distr_empl["0", "smiles"],
           p = .5,
           alternative = "greater")

# Frowns
proportionBF(y = distr_empl["1", "frowns"],
             N = distr_empl["1", "frowns"] + distr_empl["0", "frowns"],
             p = .5,
             rscale = "ultrawide",
             nullInterval = c(.5, 1)) %>% printBFb()
binom.test(x = distr_empl["1", "frowns"],
           n = distr_empl["1", "frowns"] + distr_empl["0", "frowns"],
           p = .5,
           alternative = "greater")

# Impression --------------------------------------------------------------
df2 %>%
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
  theme(legend.position = "none")
# ggsave(filename = "Output/Graphs/Exp2_impr.svg", device = "svg", dpi = 320, width = 9.08, height = 5.72)

### Descriptives
df2 %>%
  group_by(condition) %>%
  summarize(M = printnum(mean(impr_dp)),
            SD = printnum(sd(impr_dp)))

### Tests

# Global
df2 %>%
  mutate(condition = relevel(condition, ref = "frowns")) %>%
  lm(impr_dp ~ condition, data = .) %>%
  summary()

# Rich
df2 %>%
  filter(condition == "smiles") %>%
  as.data.frame() %$%
  ttestBF(impr_dp,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = impr_dp,
      x = condition,
      mu = 0,
      sub = "smiles",
      dir = "greater")

# Impoverished
df2 %>%
  filter(condition == "frowns") %>%
  as.data.frame() %$%
  ttestBF(impr_dp,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = impr_dp,
      x = condition,
      mu = 0,
      sub = "frowns",
      dir = "greater")

# Strength
df2 %>%
  as.data.frame() %>%
  ttestBF(formula = impr_dp ~ condition,
          nullInterval = c(-Inf, Inf),
          data = .) %>%
  printBFt()
ttest(data = df2,
      y = impr_dp,
      x = condition,
      dir = "two.sided")

## Overall
df2 %>%
  as.data.frame() %$%
  ttestBF(impr_dp,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = impr_dp,
      x = condition,
      mu = 0,
      dir = "greater")

# Conditional estimates ---------------------------------------------------
df2 %>%
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
  theme(legend.position = "none")
# ggsave(filename = "Output/Graphs/Exp2_cond.svg", device = "svg", dpi = 320, width = 9.08, height = 5.72)

### Descriptives
df2 %>%
  group_by(condition) %>%
  summarize(M = printnum(mean(cond_dp)),
            SD = printnum(sd(cond_dp)))

### Tests

# Global
df2 %>%
  mutate(condition = relevel(condition, ref = "frowns")) %>%
  lm(cond_dp ~ condition, data = .) %>%
  summary()

# Rich
df2 %>%
  filter(condition == "smiles") %>%
  as.data.frame() %$%
  ttestBF(cond_dp,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = cond_dp,
      x = condition,
      mu = 0,
      sub = "smiles",
      dir = "greater")

# Impoverished
df2 %>%
  filter(condition == "frowns") %>%
  as.data.frame() %$%
  ttestBF(cond_dp,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = cond_dp,
      x = condition,
      mu = 0,
      sub = "frowns",
      dir = "greater")

# Strength
df2 %>%
  as.data.frame() %>%
  ttestBF(formula = cond_dp ~ condition,
          nullInterval = c(-Inf, Inf),
          data = .) %>%
  printBFt()
ttest(data = df2,
      y = cond_dp,
      x = condition,
      dir = "two.sided")

## Overall
df2 %>%
  as.data.frame() %$%
  ttestBF(cond_dp,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()
ttest(data = df2,
      y = cond_dp,
      x = condition,
      mu = 0,
      dir = "greater")

# Confidence --------------------------------------------------------------

df2 %>%
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
  theme(legend.position = "none")
# ggsave(filename = "Output/Graphs/Exp2_conf.svg", device = "svg", dpi = 320, width = 9.08, height = 5.72)

### Descriptives
df2 %>%
  group_by(condition) %>%
  summarize(M = printnum(mean(conf_dp)),
            SD = printnum(sd(conf_dp)))

### Test
df2 %>%
  as.data.frame() %>%
  ttestBF(formula = conf_dp ~ condition,
          nullInterval = c(-Inf, Inf),
          data = .) %>%
  printBFt()
ttest(data = df2,
      y = conf_dp,
      x = condition,
      dir = "two.sided")
