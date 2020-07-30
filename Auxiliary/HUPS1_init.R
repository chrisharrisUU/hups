### Data import
source(here("Auxiliary/HUPS1_import.r"))
rm(as.data.frame.avector, `[.avector`)
data1 <- data
rm(data)
# Remove not completed cases
data1 %<>%
  filter(LASTPAGE == 20)

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
    mutate(condition = factor(condition, levels = c(1,2), labels = c("rich", "impoverished")))
  # Primacy
  primacy <- .data %>%
    transmute(primacy = ifelse(UR01 %in% c(1, 2, 5, 6, 9, 10, 13, 14, 17, 18, 21, 22, 25, 26, 29, 30), -1, 1)) %>%
    mutate(primacy = factor(primacy, levels = c(-1,1), labels = c("left frequent", "right frequent")))
  # Gender
  gender <- .data %>%
    transmute(winning = ifelse(UR01 %in% c(1, 2, 3, 4, 9, 10, 11, 12, 17, 18, 19, 20, 25, 26, 27, 28), -1, 1)) %>%
    mutate(winning = factor(winning, levels = c(-1,1), labels = c("women first", "men first")))
  # Order women
  order_w <- .data %>%
    transmute(order_w = ifelse(UR01 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 17, 18, 19, 20, 21, 22, 23, 24), -1, 1)) %>%
    mutate(order_w = factor(order_w, levels = c(-1,1), labels = c("w1", "w2")))
  # Order men
  order_m <- .data %>%
    transmute(order_m = ifelse(UR01 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), -1, 1)) %>%
    mutate(order_m = factor(order_m, levels = c(-1,1), labels = c("m1", "m2")))
  # Full info
  levelnames <- c("rich-left-women-w1-m1", "imp-left-women-w1-m1", "rich-right-women-w1-m1", "imp-right-women-w1-m1",
                  "rich-left-men-w1-m1", "imp-left-men-w1-m1", "rich-right-men-w1-m1", "imp-right-men-w1-m1",
                  "rich-left-women-w2-m1", "imp-left-women-w2-m1", "rich-right-women-w2-m1", "imp-right-women-w2-m1",
                  "rich-left-men-w2-m1", "imp-left-men-w2-m1", "rich-right-men-w2-m1", "imp-right-men-w2-m1",
                  "rich-left-women-w1-m2", "imp-left-women-w1-m2", "rich-right-women-w1-m2", "imp-right-women-w1-m2",
                  "rich-left-men-w1-m2", "imp-left-men-w1-m2", "rich-right-men-w1-m2", "imp-right-men-w1-m2",
                  "rich-left-women-w2-m2", "imp-left-women-w2-m2", "rich-right-women-w2-m2", "imp-right-women-w2-m2",
                  "rich-left-men-w2-m2", "imp-left-men-w2-m2", "rich-right-men-w2-m2", "imp-right-men-w2-m2")
  counterbalancing <- .data$UR01 %>%
    factor(levels = 1:32, labels = levelnames[1:32])
  
  # Preference
  pref1 <- .data %>%
    transmute(pref1_raw = ifelse(DV01_01 == "%left1%", -1, 1))
  pref2 <- .data %>%
    transmute(pref2_raw = ifelse(DV02_01 == "%left2%", -1, 1))
  
  # Frequency
  freq1 <- .data %>%
    transmute(freq1_raw = ifelse(DV03_01 == "%left1%", -1, 1))
  freq2 <- .data %>%
    transmute(freq2_raw = ifelse(DV04_01 == "%left2%", -1, 1))
  
  # Conditional estimates
  cond <- .data %>%
    transmute(cond1_raw = DV05_01 - 1,
              cond2_raw = DV05_02 - 1,
              cond3_raw = DV05_03 - 1,
              cond4_raw = DV05_04 - 1)
  
  # Demographics
  age <- .data$DM02 %>% as.character() %>% as.numeric()
  gender <- .data$DM03
  psych <- .data$DM04
  edu <- .data$DM05
  
  # Return dataframe
  data.frame(participant.id, 
             counterbalancing, condition, primacy, gender, order_w, order_m,
             pref1, pref2, freq1, freq2, cond,
             age, gender, psych, edu) %>%
    as_tibble() %>%
    return
}
df1 <- column.rename(data1)
rm(column.rename)

# Counterbalancing

# condition (rich vs impoverished first)
# primacy (A vs B)
# winning color (yellow vs blue)
# direction of question (yellow vs blue)
# A |-------| B

do_counterbalance <- function(df) {
  df %>%
    # Ignore Order-Women and Order-Men. There should be no difference
    #  2(condition: rich-imp vs. imp-rich, within)
    # x2(primacy: left-right vs. right-left frequent)
    # Preference
    # 1: Frequent = 1, infrequent = -1
    mutate(pref1_raw2 = ifelse(primacy == "left frequent", pref1_raw * (-1), pref1_raw),
           freq1_raw2 = ifelse(primacy == "left frequent", freq1_raw * (-1), freq1_raw),
           
           pref2_raw2 = ifelse(primacy == "left frequent", pref2_raw,        pref2_raw * (-1)),
           freq2_raw2 = ifelse(primacy == "left frequent", freq2_raw,        freq2_raw * (-1))) %>%
    # 2: assign condition
    mutate(pref_rich = ifelse(condition == "rich", pref1_raw2, pref2_raw2),
           pref_imp  = ifelse(condition == "rich", pref2_raw2, pref1_raw2),
           
           freq_rich = ifelse(condition == "rich", freq1_raw2, freq2_raw2),
           freq_imp  = ifelse(condition == "rich", freq2_raw2, freq1_raw2)) %>%
    # Conditionals
    # 1: frequently vs. infrequently shown
    mutate(cond_first_freq = ifelse(primacy == "left frequent", cond1_raw, cond2_raw),
           cond_first_infr = ifelse(primacy == "left frequent", cond2_raw, cond1_raw),
           
           cond_secon_freq = ifelse(primacy == "left frequent", cond4_raw, cond3_raw),
           cond_secon_infr = ifelse(primacy == "left frequent", cond3_raw, cond4_raw)) %>%
    # 2: assign condition
    mutate(cond_rich_fr = ifelse(condition == "rich", cond_first_freq, cond_secon_freq),
           cond_rich_in = ifelse(condition == "rich", cond_first_infr, cond_secon_infr),
           
           cond_imp_fr  = ifelse(condition == "rich", cond_secon_freq, cond_first_freq),
           cond_imp_in  = ifelse(condition == "rich", cond_secon_infr, cond_first_infr))
}