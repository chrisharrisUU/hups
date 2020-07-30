### Data import
source(here("Auxiliary/HUPS3_import.r"))
rm(as.data.frame.avector, `[.avector`)
# Remove not completed cases
data3 %<>%
  filter(LASTPAGE == 18)

# Rename and create dataframe
column.rename <- function(.data) {
  # ID
  participant.id <- transmute(.data, participant.id  = IV01_RV1)
  
  # Full info
  
  # Many smiles vs frowns
  # Left or right frequent
  # Females or males
  # Pair 1 or pair 2
  
  levelnames <- expand.grid(
    c("smile", "frown"), # Condition
    c("left", "right"), # Primacy
    c("female", "male"), # gender
    c("ab", "ba", "cd", "dc"), # Frequent person
    c("pos", "neg")) %>% # Order DVs
    transmute(urn = paste(Var1, Var2, Var3, Var4, Var5, sep = "-")) %>%
    unlist %>%
    unname()
  counterbalancing <- factor(.data$UR01, levels = 1:64, labels = levelnames[1:64]) %>%
    as_tibble() %>%
    rename(counterbalancing = value) %>%
    mutate(condition = ifelse(grepl("smile-", counterbalancing), "smiles", "frowns"),
           primacy = ifelse(grepl("-left-", counterbalancing), "left frequent", "right frequent"),
           gender = ifelse(grepl("-male-", counterbalancing), "men", "women"),
           order = ifelse(grepl("-pos", counterbalancing), "positive first", "negative first")) %>%
    mutate(pairs_aux = substr(counterbalancing,
                              unlist(map(gregexpr("male-", counterbalancing), 1)) + 5,
                              unlist(map(gregexpr("male-", counterbalancing), 1)) + 6)) %>%
    mutate(pairs = case_when(gender == "women" & pairs_aux == "ab" ~ "1-3",
                             gender == "women" & pairs_aux == "ba" ~ "3-1",
                             gender == "women" & pairs_aux == "cd" ~ "2-5",
                             gender == "women" & pairs_aux == "dc" ~ "5-2",
                             gender == "men" & pairs_aux == "ab" ~ "2-3",
                             gender == "men" & pairs_aux == "ba" ~ "3-2",
                             gender == "men" & pairs_aux == "cd" ~ "6-8",
                             TRUE ~ "8-6")) %>%
    select(-pairs_aux) %>%
    mutate(across(c(primacy, gender), factor)) %>%
    mutate(condition = factor(condition, levels = c("smiles", "frowns")),
           order = factor(order, levels = c("positive first", "negative first")))
  
  # Likeability
  like <- transmute(.data,
                    like1_raw = ifelse(DV01_01 == "%left%", -1, 1),
                    like2_raw = ifelse(DV02_01 == "%left%", -1, 1))
  
  # Preference
  pref <- transmute(.data,
                    pref1_raw = DV03_01 - 1,
                    pref2_raw = DV04_01 - 1)
  
  # Employability
  empl <- transmute(.data,
                    empl1_raw = ifelse(DV05_01 == "%left%", -1, 1),
                    empl2_raw = ifelse(DV06_01 == "%left%", -1, 1))
  
  # Impression
  impr <- transmute(.data,
                    impr1_l_raw = DV07_01 - 1,
                    impr1_r_raw = DV07_02 - 1,
                    impr2_l_raw = DV08_01 - 1,
                    impr2_r_raw = DV08_02 - 1)
  
  # Conditionals
  cond <- transmute(.data,
                    cond1_l_raw = DV09_01 - 1,
                    cond1_r_raw = DV09_02 - 1,
                    cond2_l_raw = DV10_01 - 1,
                    cond2_r_raw = DV10_02 - 1)
  
  # Confidence
  conf <- transmute(.data,
                    conf1_l_raw = DV11_01 - 1,
                    conf1_r_raw = DV11_02 - 1,
                    conf2_l_raw = DV12_01 - 1,
                    conf2_r_raw = DV12_02 - 1)
  
  # Demographics
  age <- .data$DM02 %>% as.character() %>% as.numeric()
  gender <- .data$DM03
  psych <- .data$DM04
  edu <- .data$DM05
  nat <- .data$DM06
  lang <- .data$DM07
  attcheck <- .data$DM08
  
  # Return dataframe
  data.frame(participant.id, counterbalancing,
             like, pref, empl, impr, cond, conf,
             age, gender, psych, edu, nat, lang, attcheck) %>%
    as_tibble() %>%
    return
}
df3_raw <- column.rename(data3)
rm(column.rename)