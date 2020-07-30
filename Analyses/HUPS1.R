### Setup----
# Dependencies
if (!require(needs)) {install.packages("needs"); library(needs)}
if (!require(goallabr)) {
  if (!require(devtools)) {install.packages("devtools"); library(devtools)}
  install_github("chrisharrisUU/goallabr")
}
needs(BayesFactor, dplyr, ggplot2, goallabr, gridExtra,
      here, magrittr, tidyr, papaja, purrr)
prioritize(dplyr)

source(here("Auxiliary/HUPS1_init.R"))

### Functions----
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

### Canonical sorting----
df1 <- do_counterbalance(df1)


### Demographic data----
# Summary of most relevant demographic data
df1 %>%
  summarise(N = n(),
            female = length(which(gender == "female")),
            age_mean = mean(age, na.rm = TRUE),
            age_sd = sd(age, na.rm = TRUE),
            Psychstudents_percent = length(which(psych == "Yes")) / n() * 100)

# Level of education
df1 %>%
  group_by(edu) %>%
  summarise(count = length(edu))

# Percentage degree
length(which(as.integer(df1$edu) > 2)) / nrow(df1)


### Preference----
pref_long <- df1 %>%
  select(pref_rich, pref_imp) %>%
  gather(key = "cond",
         value = "pref",
         c(pref_rich, pref_imp)) %>%
  mutate(cond = ifelse(cond == "pref_rich", "rich", "imp")) %>%
  mutate(cond = factor(cond, levels = c("rich", "imp")))

pref_long %>%
  group_by(cond) %>%
  summarize(avg = mean(pref))

pref_long %>%
  group_by(cond, pref) %>%
  summarise(n())

pref_long %>%
  ggplot() +
  geom_count(aes(x = cond,
                   y = pref,
                   fill = cond)) +
  scale_fill_brewer(palette = "Dark2") +
  theme_apa()

pref_long %>%
  as.data.frame() %>%
  ttestBF(formula = pref ~ cond, nullInterval = c(0, Inf), data = .) %>%
  printBFt()
ttest(pref_long,
      x = cond,
      y = pref,
      dir = "greater")

### Frequency----
freq_long <- df1 %>%
  select(freq_rich, freq_imp, participant.id) %>%
  gather(key = "cond",
         value = "freq",
         c(freq_rich, freq_imp)) %>%
  mutate(cond = ifelse(cond == "freq_rich", "rich", "imp")) %>%
  mutate(cond = factor(cond, levels = c("rich", "imp"))) %>%
  mutate(freq = ifelse(freq == 1, freq, 0))

freq_long %>%
  group_by(cond) %>%
  summarize(avg = mean(freq))

freq_long %>%
  ggplot() +
  geom_count(aes(x = cond,
                 y = freq,
                 fill = cond)) +
  scale_fill_brewer(palette = "Dark2") +
  theme_apa()

freq_long %>%
  as.data.frame() %>%
  ttestBF(formula = freq ~ cond, data = .) %>%
  printBFt()
ttest(freq_long,
      x = cond,
      y = freq,
      dir = "two.sided")

# Difference in proportion correct?
distr <- table(freq_long$cond, freq_long$freq)

testBF <- proportionBF(y = distr["rich", "1"],
                       N = distr["rich", "1"] + distr["rich", "0"],
                       p = distr["imp", "1"] / (distr["imp", "1"] + distr["imp", "0"]),
                       rscale = "ultrawide",
                       nullInterval = c(0, 1)) %>%
  printBFb()
testFR <- binom.test(x = distr["rich", "1"],
                     n = distr["rich", "1"] + distr["rich", "0"],
                     p = distr["imp", "1"] / (distr["imp", "1"] + distr["imp", "0"]),
                     alternative = "two.sided")

paste0(testBF, ", p = ", printnum(testFR$p.value))

### Conditionals----
cond_long <- rbind(
  df1 %>%
    select(cond_rich_fr, cond_rich_in) %>%
    gather(key = "frequency",
           value = "value",
           c(cond_rich_fr, cond_rich_in)) %>%
    mutate(frequency = ifelse(frequency == "cond_rich_fr", "freq", "infr")) %>%
    mutate(frequency = factor(frequency, levels = c("freq", "infr")),
           cond = "rich"),
  df1 %>%
    select(cond_imp_fr, cond_imp_in) %>%
    gather(key = "frequency",
           value = "value",
           c(cond_imp_fr, cond_imp_in)) %>%
    mutate(frequency = ifelse(frequency == "cond_imp_fr", "freq", "infr")) %>%
    mutate(frequency = factor(frequency, levels = c("freq", "infr")),
           cond = "imp")
) %>%
  mutate(cond = factor(cond, levels = c("rich", "imp")))

cond_long %>%
  group_by(cond, frequency) %>%
  summarize(avg = mean(value))

df1 %>%
  mutate(rich = (cond_rich_fr - cond_rich_in) / 100,
         imp  = (cond_imp_fr - cond_imp_in) / 100) %>%
  summarize(rich = mean(rich),
            imp  = mean(imp))

# Differ?
df1 %>%
  mutate(rich_dp = (cond_rich_fr - cond_rich_in) / 100,
         imp_dp  = (cond_imp_fr - cond_imp_in) / 100) %$%
  ttestBF(x = rich_dp,
          y = imp_dp,
          nullInterval = c(-Inf, Inf),
          paired = TRUE) %>%
  printBFt()

# From 0
df1 %>%
  mutate(rich_dp = (cond_rich_fr - cond_rich_in) / 100,
         imp_dp  = (cond_imp_fr - cond_imp_in) / 100) %>%
  select(rich_dp, imp_dp) %>%
  gather(key = "cond",
         value = "dp") %$%
  ttestBF(dp,
          mu = 0,
          nullInterval = c(0, Inf)) %>%
  printBFt()

cond_long %>%
  ggplot(aes(x = cond,
             y = value,
             fill = frequency)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  theme_apa()


cond_long %>%
  ggplot(aes(x = cond,
             y = value,
             fill = frequency)) +
  geom_violin() + 
  geom_boxplot(width = 0.2) +
  geom_jitter(size = 0.5, width = 0.2) +
  geom_hline(yintercept = 50, linetype = 2) +
  scale_fill_brewer(palette = "Dark2") +
  # scale_fill_grey(start = .5) +
  theme_apa() +
  labs(x = "Condition",
       y = "Value") +
  theme(legend.position = "none")
