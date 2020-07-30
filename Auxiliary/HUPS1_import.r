# This script reads a CSV file in GNU R.
# While reading this file, comments will be created for all variables.
# The comments for values will be stored as attributes (attr) as well.

# data_file = file.choose()
# setwd("./")
data_file = here("Data/HUPS1_data.csv")

data = read.table(
  file=data_file, encoding="UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", row.names = "CASE",
  col.names = c(
    "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","UR01_CP","UR01","IV01_RV1",
    "IV02_01","TR01_01","TR01_01a","TR01_02","TR01_02a","TR01_03","TR01_03a",
    "TR01_04","TR01_04a","TR01_05","TR01_05a","TR01_06","TR01_06a","TR01_07",
    "TR01_07a","TR01_08","TR01_08a","TR01_09","TR01_09a","TR01_10","TR01_10a",
    "TR01_11","TR01_11a","TR01_12","TR01_12a","TR01_13","TR01_13a","TR01_14",
    "TR01_14a","TR01_15","TR01_15a","TR01_16","TR01_16a","TR02_01","TR02_01a",
    "TR02_02","TR02_02a","TR02_03","TR02_03a","TR02_04","TR02_04a","TR02_05",
    "TR02_05a","TR02_06","TR02_06a","TR02_07","TR02_07a","TR02_08","TR02_08a",
    "TR02_09","TR02_09a","TR02_10","TR02_10a","TR02_11","TR02_11a","TR02_12",
    "TR02_12a","TR02_13","TR02_13a","TR02_14","TR02_14a","TR02_15","TR02_15a",
    "TR02_16","TR02_16a","DV01_01","DV01_01a","DV03_01","DV03_01a","DV02_01",
    "DV02_01a","DV04_01","DV04_01a","DV05_01","DV05_02","DV05_03","DV05_04",
    "DM01_01","DM02","DM03","DM04","DM05","TIME001","TIME002","TIME003","TIME004",
    "TIME005","TIME006","TIME007","TIME008","TIME009","TIME010","TIME011","TIME012",
    "TIME013","TIME014","TIME015","TIME016","TIME017","TIME018","TIME019","TIME020",
    "TIME_SUM","MAILSENT","LASTDATA","FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE",
    "MISSING","MISSREL","TIME_RSI","DEG_TIME"
  ),
  as.is = TRUE,
  colClasses = c(
    CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
    MODE="character", STARTED="POSIXct", UR01_CP="numeric", UR01="numeric",
    IV01_RV1="character", IV02_01="character", TR01_01="numeric",
    TR01_01a="numeric", TR01_02="numeric", TR01_02a="numeric",
    TR01_03="numeric", TR01_03a="numeric", TR01_04="numeric",
    TR01_04a="numeric", TR01_05="numeric", TR01_05a="numeric",
    TR01_06="numeric", TR01_06a="numeric", TR01_07="numeric",
    TR01_07a="numeric", TR01_08="numeric", TR01_08a="numeric",
    TR01_09="numeric", TR01_09a="numeric", TR01_10="numeric",
    TR01_10a="numeric", TR01_11="numeric", TR01_11a="numeric",
    TR01_12="numeric", TR01_12a="numeric", TR01_13="numeric",
    TR01_13a="numeric", TR01_14="numeric", TR01_14a="numeric",
    TR01_15="numeric", TR01_15a="numeric", TR01_16="numeric",
    TR01_16a="numeric", TR02_01="numeric", TR02_01a="numeric",
    TR02_02="numeric", TR02_02a="numeric", TR02_03="numeric",
    TR02_03a="numeric", TR02_04="numeric", TR02_04a="numeric",
    TR02_05="numeric", TR02_05a="numeric", TR02_06="numeric",
    TR02_06a="numeric", TR02_07="numeric", TR02_07a="numeric",
    TR02_08="numeric", TR02_08a="numeric", TR02_09="numeric",
    TR02_09a="numeric", TR02_10="numeric", TR02_10a="numeric",
    TR02_11="numeric", TR02_11a="numeric", TR02_12="numeric",
    TR02_12a="numeric", TR02_13="numeric", TR02_13a="numeric",
    TR02_14="numeric", TR02_14a="numeric", TR02_15="numeric",
    TR02_15a="numeric", TR02_16="numeric", TR02_16a="numeric",
    DV01_01="numeric", DV01_01a="numeric", DV03_01="numeric",
    DV03_01a="numeric", DV02_01="numeric", DV02_01a="numeric",
    DV04_01="numeric", DV04_01a="numeric", DV05_01="numeric", DV05_02="numeric",
    DV05_03="numeric", DV05_04="numeric", DM01_01="character", DM02="numeric",
    DM03="numeric", DM04="numeric", DM05="numeric", TIME001="integer",
    TIME002="integer", TIME003="integer", TIME004="integer", TIME005="integer",
    TIME006="integer", TIME007="integer", TIME008="integer", TIME009="integer",
    TIME010="integer", TIME011="integer", TIME012="integer", TIME013="integer",
    TIME014="integer", TIME015="integer", TIME016="integer", TIME017="integer",
    TIME018="integer", TIME019="integer", TIME020="integer", TIME_SUM="integer",
    MAILSENT="POSIXct", LASTDATA="POSIXct", FINISHED="logical",
    Q_VIEWER="logical", LASTPAGE="numeric", MAXPAGE="numeric",
    MISSING="numeric", MISSREL="numeric", TIME_RSI="numeric", DEG_TIME="numeric"
  ),
  skip = 1,
  check.names = TRUE, fill = TRUE,
  strip.white = FALSE, blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
)

rm(data_file)

attr(data, "project") = "hups1"
attr(data, "description") = "hups1"
attr(data, "date") = "2019-05-08 13:52:43"
attr(data, "server") = "https://www.soscisurvey.de"

# Variable und Value Labels
data$TR01_01 = factor(data$TR01_01, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_02 = factor(data$TR01_02, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_03 = factor(data$TR01_03, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_04 = factor(data$TR01_04, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_05 = factor(data$TR01_05, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_06 = factor(data$TR01_06, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_07 = factor(data$TR01_07, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_08 = factor(data$TR01_08, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_09 = factor(data$TR01_09, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_10 = factor(data$TR01_10, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_11 = factor(data$TR01_11, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_12 = factor(data$TR01_12, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_13 = factor(data$TR01_13, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_14 = factor(data$TR01_14, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_15 = factor(data$TR01_15, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR01_16 = factor(data$TR01_16, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_01 = factor(data$TR02_01, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_02 = factor(data$TR02_02, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_03 = factor(data$TR02_03, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_04 = factor(data$TR02_04, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_05 = factor(data$TR02_05, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_06 = factor(data$TR02_06, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_07 = factor(data$TR02_07, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_08 = factor(data$TR02_08, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_09 = factor(data$TR02_09, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_10 = factor(data$TR02_10, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_11 = factor(data$TR02_11, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_12 = factor(data$TR02_12, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_13 = factor(data$TR02_13, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_14 = factor(data$TR02_14, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_15 = factor(data$TR02_15, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$TR02_16 = factor(data$TR02_16, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data$DV01_01 = factor(data$DV01_01, levels=c("1","2"), labels=c("%left1%","%right1%"), ordered=FALSE)
data$DV03_01 = factor(data$DV03_01, levels=c("1","2"), labels=c("%left1%","%right1%"), ordered=FALSE)
data$DV02_01 = factor(data$DV02_01, levels=c("1","2"), labels=c("%left2%","%right2%"), ordered=FALSE)
data$DV04_01 = factor(data$DV04_01, levels=c("1","2"), labels=c("%left2%","%right2%"), ordered=FALSE)
data$DM02 = factor(data$DM02, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","-9"), labels=c("18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","[NA] Not answered"), ordered=FALSE)
data$DM03 = factor(data$DM03, levels=c("1","2","3","-9"), labels=c("male","female","other","[NA] Not answered"), ordered=FALSE)
data$DM04 = factor(data$DM04, levels=c("1","2","-9"), labels=c("Yes","No","[NA] Not answered"), ordered=FALSE)
data$DM05 = factor(data$DM05, levels=c("1","2","3","4","5","6","-9"), labels=c("No formal education","Secondary school/GCSE","College/A levels","Undergraduate degree (BA, BSc or comparable)","Graduate degree (MA, MSc or comparable)","Doctorate degree (PhD, MD, or comparable)","[NA] Not answered"), ordered=FALSE)
attr(data$UR01,"1") = "rich-left-women-w1-m1"
attr(data$UR01,"2") = "imp-left-women-w1-m1"
attr(data$UR01,"3") = "rich-right-women-w1-m1"
attr(data$UR01,"4") = "imp-right-women-w1-m1"
attr(data$UR01,"5") = "rich-left-men-w1-m1"
attr(data$UR01,"6") = "imp-left-men-w1-m1"
attr(data$UR01,"7") = "rich-right-men-w1-m1"
attr(data$UR01,"8") = "imp-right-men-w1-m1"
attr(data$UR01,"9") = "rich-left-women-w2-m1"
attr(data$UR01,"10") = "imp-left-women-w2-m1"
attr(data$UR01,"11") = "rich-right-women-w2-m1"
attr(data$UR01,"12") = "imp-right-women-w2-m1"
attr(data$UR01,"13") = "rich-left-men-w2-m1"
attr(data$UR01,"14") = "imp-left-men-w2-m1"
attr(data$UR01,"15") = "rich-right-men-w2-m1"
attr(data$UR01,"16") = "imp-right-men-w2-m1"
attr(data$UR01,"17") = "rich-left-women-w1-m2"
attr(data$UR01,"18") = "imp-left-women-w1-m2"
attr(data$UR01,"19") = "rich-right-women-w1-m2"
attr(data$UR01,"20") = "imp-right-women-w1-m2"
attr(data$UR01,"21") = "rich-left-men-w1-m2"
attr(data$UR01,"22") = "imp-left-men-w1-m2"
attr(data$UR01,"23") = "rich-right-men-w1-m2"
attr(data$UR01,"24") = "imp-right-men-w1-m2"
attr(data$UR01,"25") = "rich-left-women-w2-m2"
attr(data$UR01,"26") = "imp-left-women-w2-m2"
attr(data$UR01,"27") = "rich-right-women-w2-m2"
attr(data$UR01,"28") = "imp-right-women-w2-m2"
attr(data$UR01,"29") = "rich-left-men-w2-m2"
attr(data$UR01,"30") = "imp-left-men-w2-m2"
attr(data$UR01,"31") = "rich-right-men-w2-m2"
attr(data$UR01,"32") = "imp-right-men-w2-m2"
attr(data$TR01_01a,"-1") = "Mensuration impossible"
attr(data$TR01_02a,"-1") = "Mensuration impossible"
attr(data$TR01_03a,"-1") = "Mensuration impossible"
attr(data$TR01_04a,"-1") = "Mensuration impossible"
attr(data$TR01_05a,"-1") = "Mensuration impossible"
attr(data$TR01_06a,"-1") = "Mensuration impossible"
attr(data$TR01_07a,"-1") = "Mensuration impossible"
attr(data$TR01_08a,"-1") = "Mensuration impossible"
attr(data$TR01_09a,"-1") = "Mensuration impossible"
attr(data$TR01_10a,"-1") = "Mensuration impossible"
attr(data$TR01_11a,"-1") = "Mensuration impossible"
attr(data$TR01_12a,"-1") = "Mensuration impossible"
attr(data$TR01_13a,"-1") = "Mensuration impossible"
attr(data$TR01_14a,"-1") = "Mensuration impossible"
attr(data$TR01_15a,"-1") = "Mensuration impossible"
attr(data$TR01_16a,"-1") = "Mensuration impossible"
attr(data$TR02_01a,"-1") = "Mensuration impossible"
attr(data$TR02_02a,"-1") = "Mensuration impossible"
attr(data$TR02_03a,"-1") = "Mensuration impossible"
attr(data$TR02_04a,"-1") = "Mensuration impossible"
attr(data$TR02_05a,"-1") = "Mensuration impossible"
attr(data$TR02_06a,"-1") = "Mensuration impossible"
attr(data$TR02_07a,"-1") = "Mensuration impossible"
attr(data$TR02_08a,"-1") = "Mensuration impossible"
attr(data$TR02_09a,"-1") = "Mensuration impossible"
attr(data$TR02_10a,"-1") = "Mensuration impossible"
attr(data$TR02_11a,"-1") = "Mensuration impossible"
attr(data$TR02_12a,"-1") = "Mensuration impossible"
attr(data$TR02_13a,"-1") = "Mensuration impossible"
attr(data$TR02_14a,"-1") = "Mensuration impossible"
attr(data$TR02_15a,"-1") = "Mensuration impossible"
attr(data$TR02_16a,"-1") = "Mensuration impossible"
attr(data$DV01_01a,"-1") = "Mensuration impossible"
attr(data$DV03_01a,"-1") = "Mensuration impossible"
attr(data$DV02_01a,"-1") = "Mensuration impossible"
attr(data$DV04_01a,"-1") = "Mensuration impossible"
attr(data$DV05_01,"1") = "Always frowned"
attr(data$DV05_01,"101") = "Always smiled"
attr(data$DV05_02,"1") = "Always frowned"
attr(data$DV05_02,"101") = "Always smiled"
attr(data$DV05_03,"1") = "Always frowned"
attr(data$DV05_03,"101") = "Always smiled"
attr(data$DV05_04,"1") = "Always frowned"
attr(data$DV05_04,"101") = "Always smiled"
attr(data$FINISHED,"F") = "Canceled"
attr(data$FINISHED,"T") = "Finished"
attr(data$Q_VIEWER,"F") = "Respondent"
attr(data$Q_VIEWER,"T") = "Spectator"
comment(data$SERIAL) = "Serial number (if provided)"
comment(data$REF) = "Reference (if provided in link)"
comment(data$QUESTNNR) = "Questionnaire that has been used in the interview"
comment(data$MODE) = "Interview mode"
comment(data$STARTED) = "Time the interview has started (Europe/Berlin)"
comment(data$UR01_CP) = "Counterbalancing: Complete clearances of the ballot, yet"
comment(data$UR01) = "Counterbalancing: Code drawn"
comment(data$IV01_RV1) = "POST/GET Variable: PROLIFIC_PID"
comment(data$IV02_01) = "Page order: Page order"
comment(data$TR01_01) = "Forced: %forced%"
comment(data$TR01_01a) = "Forced: %forced% response time [ms]"
comment(data$TR01_02) = "Forced: %forced%"
comment(data$TR01_02a) = "Forced: %forced% response time [ms]"
comment(data$TR01_03) = "Forced: %forced%"
comment(data$TR01_03a) = "Forced: %forced% response time [ms]"
comment(data$TR01_04) = "Forced: %forced%"
comment(data$TR01_04a) = "Forced: %forced% response time [ms]"
comment(data$TR01_05) = "Forced: %forced%"
comment(data$TR01_05a) = "Forced: %forced% response time [ms]"
comment(data$TR01_06) = "Forced: %forced%"
comment(data$TR01_06a) = "Forced: %forced% response time [ms]"
comment(data$TR01_07) = "Forced: %forced%"
comment(data$TR01_07a) = "Forced: %forced% response time [ms]"
comment(data$TR01_08) = "Forced: %forced%"
comment(data$TR01_08a) = "Forced: %forced% response time [ms]"
comment(data$TR01_09) = "Forced: %forced%"
comment(data$TR01_09a) = "Forced: %forced% response time [ms]"
comment(data$TR01_10) = "Forced: %forced%"
comment(data$TR01_10a) = "Forced: %forced% response time [ms]"
comment(data$TR01_11) = "Forced: %forced%"
comment(data$TR01_11a) = "Forced: %forced% response time [ms]"
comment(data$TR01_12) = "Forced: %forced%"
comment(data$TR01_12a) = "Forced: %forced% response time [ms]"
comment(data$TR01_13) = "Forced: %forced%"
comment(data$TR01_13a) = "Forced: %forced% response time [ms]"
comment(data$TR01_14) = "Forced: %forced%"
comment(data$TR01_14a) = "Forced: %forced% response time [ms]"
comment(data$TR01_15) = "Forced: %forced%"
comment(data$TR01_15a) = "Forced: %forced% response time [ms]"
comment(data$TR01_16) = "Forced: %forced%"
comment(data$TR01_16a) = "Forced: %forced% response time [ms]"
comment(data$TR02_01) = "Forced: %forced%"
comment(data$TR02_01a) = "Forced: %forced% response time [ms]"
comment(data$TR02_02) = "Forced: %forced%"
comment(data$TR02_02a) = "Forced: %forced% response time [ms]"
comment(data$TR02_03) = "Forced: %forced%"
comment(data$TR02_03a) = "Forced: %forced% response time [ms]"
comment(data$TR02_04) = "Forced: %forced%"
comment(data$TR02_04a) = "Forced: %forced% response time [ms]"
comment(data$TR02_05) = "Forced: %forced%"
comment(data$TR02_05a) = "Forced: %forced% response time [ms]"
comment(data$TR02_06) = "Forced: %forced%"
comment(data$TR02_06a) = "Forced: %forced% response time [ms]"
comment(data$TR02_07) = "Forced: %forced%"
comment(data$TR02_07a) = "Forced: %forced% response time [ms]"
comment(data$TR02_08) = "Forced: %forced%"
comment(data$TR02_08a) = "Forced: %forced% response time [ms]"
comment(data$TR02_09) = "Forced: %forced%"
comment(data$TR02_09a) = "Forced: %forced% response time [ms]"
comment(data$TR02_10) = "Forced: %forced%"
comment(data$TR02_10a) = "Forced: %forced% response time [ms]"
comment(data$TR02_11) = "Forced: %forced%"
comment(data$TR02_11a) = "Forced: %forced% response time [ms]"
comment(data$TR02_12) = "Forced: %forced%"
comment(data$TR02_12a) = "Forced: %forced% response time [ms]"
comment(data$TR02_13) = "Forced: %forced%"
comment(data$TR02_13a) = "Forced: %forced% response time [ms]"
comment(data$TR02_14) = "Forced: %forced%"
comment(data$TR02_14a) = "Forced: %forced% response time [ms]"
comment(data$TR02_15) = "Forced: %forced%"
comment(data$TR02_15a) = "Forced: %forced% response time [ms]"
comment(data$TR02_16) = "Forced: %forced%"
comment(data$TR02_16a) = "Forced: %forced% response time [ms]"
comment(data$DV01_01) = "pref1: "
comment(data$DV01_01a) = "pref1:  response time [ms]"
comment(data$DV03_01) = "freq1: "
comment(data$DV03_01a) = "freq1:  response time [ms]"
comment(data$DV02_01) = "pref2: "
comment(data$DV02_01a) = "pref2:  response time [ms]"
comment(data$DV04_01) = "freq2: "
comment(data$DV04_01a) = "freq2:  response time [ms]"
comment(data$DV05_01) = "conditionals: %left1%"
comment(data$DV05_02) = "conditionals: %right1%"
comment(data$DV05_03) = "conditionals: %left2%"
comment(data$DV05_04) = "conditionals: %right2%"
comment(data$DM01_01) = "ID: [01]"
comment(data$DM02) = "Age"
comment(data$DM03) = "Gender"
comment(data$DM04) = "Psychstudent"
comment(data$DM05) = "Education"
comment(data$TIME001) = "Time spent on page 1"
comment(data$TIME002) = "Time spent on page 2"
comment(data$TIME003) = "Time spent on page 3"
comment(data$TIME004) = "Time spent on page 4"
comment(data$TIME005) = "Time spent on page 5"
comment(data$TIME006) = "Time spent on page 6"
comment(data$TIME007) = "Time spent on page 7"
comment(data$TIME008) = "Time spent on page 8"
comment(data$TIME009) = "Time spent on page 9"
comment(data$TIME010) = "Time spent on page 10"
comment(data$TIME011) = "Time spent on page 11"
comment(data$TIME012) = "Time spent on page 12"
comment(data$TIME013) = "Time spent on page 13"
comment(data$TIME014) = "Time spent on page 14"
comment(data$TIME015) = "Time spent on page 15"
comment(data$TIME016) = "Time spent on page 16"
comment(data$TIME017) = "Time spent on page 17"
comment(data$TIME018) = "Time spent on page 18"
comment(data$TIME019) = "Time spent on page 19"
comment(data$TIME020) = "Time spent on page 20"
comment(data$TIME_SUM) = "Time spent overall (except outliers)"
comment(data$MAILSENT) = "Time when the invitation mailing was sent (personally identifiable recipients, only)"
comment(data$LASTDATA) = "Time when the data was most recently updated"
comment(data$FINISHED) = "Has the interview been finished (reached last page)?"
comment(data$Q_VIEWER) = "Did the respondent only view the questionnaire, omitting mandatory questions?"
comment(data$LASTPAGE) = "Last page that the participant has handled in the questionnaire"
comment(data$MAXPAGE) = "Hindmost page handled by the participant"
comment(data$MISSING) = "Missing answers in percent"
comment(data$MISSREL) = "Missing answers (weighted by relevance)"
comment(data$TIME_RSI) = "Degradation points for being very fast"
comment(data$DEG_TIME) = "Degradation points for being very fast"



# Assure that the comments are retained in subsets
as.data.frame.avector = as.data.frame.vector
`[.avector` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}
data_tmp = data.frame(
  lapply(data, function(x) {
    structure( x, class = c("avector", class(x) ) )
  } )
)
mostattributes(data_tmp) = attributes(data)
data = data_tmp
rm(data_tmp)

