### Data import
source(here("Auxiliary/HUPS2_import.r"))
rm(as.data.frame.avector, `[.avector`)
# Remove not completed cases
data2 %<>%
  filter(LASTPAGE == 14)

# Rename and create dataframe
column.rename <- function(.data) {
  # ID
  participant.id <- .data %>%
    transmute(participant.id  = IV01_RV1)
  
  # Condition
  # 1 -> rich, 75% wins
  # 2 -> impoverished, 75% losses
  condition <- .data %>%
    transmute(condition = ifelse(UR01 %in% c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31), 1, 2)) %>%
    mutate(condition = factor(condition, levels = c(1,2), labels = c("smiles", "frowns")))
  # Primacy
  primacy <- .data %>%
    transmute(primacy = ifelse(UR01 %in% c(1, 2, 5, 6, 9, 10, 13, 14, 17, 18, 21, 22, 25, 26, 29, 30), -1, 1)) %>%
    mutate(primacy = factor(primacy, levels = c(-1,1), labels = c("left frequent", "right frequent")))
  # Gender
  gender <- .data %>%
    transmute(gender = ifelse(UR01 %in% c(1, 2, 3, 4, 9, 10, 11, 12, 17, 18, 19, 20, 25, 26, 27, 28), -1, 1)) %>%
    mutate(gender = factor(gender, levels = c(-1,1), labels = c("women", "men")))
  # Pairs
  pairs <- .data %>%
    mutate(gender = ifelse(UR01 %in% c(1, 2, 3, 4, 9, 10, 11, 12, 17, 18, 19, 20, 25, 26, 27, 28), -1, 1)) %>%
    mutate(gender = factor(gender, levels = c(-1,1), labels = c("women", "men"))) %>%
    mutate(pairs_aux = case_when(UR01 %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,
                                 UR01 %in% c(9, 10, 11, 12, 13, 14, 15, 16) ~ 2,
                                 UR01 %in% c(17, 18, 19, 20, 21, 22, 23, 24) ~ 3,
                                 TRUE ~ 4)) %>%
    transmute(pairs = case_when(gender == "women" & pairs_aux == 1 ~ "1-3",
                                gender == "women" & pairs_aux == 2 ~ "3-1",
                                gender == "women" & pairs_aux == 3 ~ "2-5",
                                gender == "women" & pairs_aux == 4 ~ "5-2",
                                gender == "men" & pairs_aux == 1 ~ "2-3",
                                gender == "men" & pairs_aux == 2 ~ "3-2",
                                gender == "men" & pairs_aux == 3 ~ "6-8",
                                TRUE ~ "8-6"))
  # Full info
  
  # Many smiles vs frowns
  # Left or right frequent
  # Females or males
  # Pair 1 or pair 2
  
  levelnames <- c("smile-left-female-ab", "frown-left-female-ab", "smile-right-female-ab",
                  "frown-right-female-ab", "smile-left-male-ab", "frown-left-male-ab",
                  "smile-right-male-ab", "frown-right-male-ab", "smile-left-female-ba",
                  "frown-left-female-ba", "smile-right-female-ba", "frown-right-female-ba",
                  "smile-left-male-ba", "frown-left-male-ba", "smile-right-male-ba",
                  "frown-right-male-ba", "smile-left-female-cd", "frown-left-female-cd",
                  "smile-right-female-cd", "frown-right-female-cd", "smile-left-male-cd",
                  "frown-left-male-cd", "smile-right-male-cd", "frown-right-male-cd",
                  "smile-left-female-dc", "frown-left-female-dc", "smile-right-female-dc",
                  "frown-right-female-dc", "smile-left-male-dc", "frown-left-male-dc",
                  "smile-right-male-dc", "frown-right-male-dc")
  counterbalancing <- .data$UR01 %>%
    factor(levels = 1:32, labels = levelnames[1:32])
  
  # Likeability
  like <- .data %>%
    transmute(like_raw = ifelse(DV09_01 == "%left%", -1, 1))
  
  # Preference
  pref <- transmute(.data, pref_raw = DV05_01 - 1)
  
  # Employability
  empl <- .data %>%
    transmute(empl_raw = ifelse(DV09_01 == "%left%", -1, 1))
  
  # Impression
  impr <- .data %>%
    mutate(impr_l_raw = DV06_01 - 1,
           impr_r_raw = DV06_02 - 1)
  
  # Conditionals
  cond <- .data %>%
    transmute(cond_l_raw = DV01_01 - 1,
              cond_r_raw = DV07_02 - 1)
  
  # Confidence
  conf <- .data %>%
    transmute(conf_l_raw = DV02_01 - 1,
              conf_r_raw = DV08_02 - 1)
  
  # Demographics
  age <- .data$DM02 %>% as.character() %>% as.numeric()
  gender <- .data$DM03
  psych <- .data$DM04
  edu <- .data$DM05
  nat <- .data$DM06
  lang <- .data$DM07
  
  # Return dataframe
  data.frame(participant.id, 
             counterbalancing, condition, primacy, gender, pairs,
             like, pref, empl, impr, cond, conf,
             age, gender, psych, edu, nat, lang) %>%
    as_tibble() %>%
    return
}
df2_raw <- column.rename(data2)
rm(column.rename)