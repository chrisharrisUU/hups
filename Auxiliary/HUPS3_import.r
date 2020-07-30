# This script reads a CSV file in GNU R.
# While reading this file, comments will be created for all variables.
# The comments for values will be stored as attributes (attr) as well.

# data3_file = file.choose()
# setwd("./")
data3_file = here("Data/HUPS3_data.csv")

data3 = read.table(
  file=data3_file, encoding="UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", row.names = "CASE",
  col.names = c(
    "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","UR01_CP","UR01","IV01_RV1",
    "IV02_01","TR01_01","TR01_01a","TR01_02","TR01_02a","TR01_03","TR01_03a",
    "TR01_04","TR01_04a","TR01_05","TR01_05a","TR01_06","TR01_06a","TR01_07",
    "TR01_07a","TR01_08","TR01_08a","TR01_09","TR01_09a","TR01_10","TR01_10a",
    "TR01_11","TR01_11a","TR01_12","TR01_12a","TR01_13","TR01_13a","TR01_14",
    "TR01_14a","TR01_15","TR01_15a","TR01_16","TR01_16a","DV01_01","DV01_01a",
    "DV02_01","DV02_01a","DV03_01","DV04_01","DV05_01","DV05_01a","DV06_01",
    "DV06_01a","DV07_01","DV07_02","DV08_01","DV08_02","DV09_01","DV09_02",
    "DV10_01","DV10_02","DV11_01","DV11_02","DV12_01","DV12_02","DM01_01","DM02",
    "DM03","DM04","DM05","DM06_01","DM07_01","DM08","TIME001","TIME002","TIME003",
    "TIME004","TIME005","TIME006","TIME008","TIME009","TIME010","TIME011","TIME012",
    "TIME013","TIME014","TIME015","TIME016","TIME017","TIME018","TIME_SUM",
    "MAILSENT","LASTDATA","FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE","MISSING",
    "MISSREL","TIME_RSI","DEG_TIME"
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
    TR01_16a="numeric", DV01_01="numeric", DV01_01a="numeric",
    DV02_01="numeric", DV02_01a="numeric", DV03_01="numeric", DV04_01="numeric",
    DV05_01="numeric", DV05_01a="numeric", DV06_01="numeric",
    DV06_01a="numeric", DV07_01="numeric", DV07_02="numeric", DV08_01="numeric",
    DV08_02="numeric", DV09_01="numeric", DV09_02="numeric", DV10_01="numeric",
    DV10_02="numeric", DV11_01="numeric", DV11_02="numeric", DV12_01="numeric",
    DV12_02="numeric", DM01_01="character", DM02="numeric", DM03="numeric",
    DM04="numeric", DM05="numeric", DM06_01="character", DM07_01="character",
    DM08="numeric", TIME001="integer", TIME002="integer", TIME003="integer",
    TIME004="integer", TIME005="integer", TIME006="integer", TIME008="integer",
    TIME009="integer", TIME010="integer", TIME011="integer", TIME012="integer",
    TIME013="integer", TIME014="integer", TIME015="integer", TIME016="integer",
    TIME017="integer", TIME018="integer", TIME_SUM="integer",
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

rm(data3_file)

attr(data3, "project") = "hups3"
attr(data3, "description") = "hups3"
attr(data3, "date") = "2020-07-27 16:51:32"
attr(data3, "server") = "https://www.soscisurvey.de"

# Variable und Value Labels
data3$TR01_01 = factor(data3$TR01_01, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_02 = factor(data3$TR01_02, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_03 = factor(data3$TR01_03, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_04 = factor(data3$TR01_04, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_05 = factor(data3$TR01_05, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_06 = factor(data3$TR01_06, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_07 = factor(data3$TR01_07, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_08 = factor(data3$TR01_08, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_09 = factor(data3$TR01_09, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_10 = factor(data3$TR01_10, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_11 = factor(data3$TR01_11, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_12 = factor(data3$TR01_12, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_13 = factor(data3$TR01_13, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_14 = factor(data3$TR01_14, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_15 = factor(data3$TR01_15, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$TR01_16 = factor(data3$TR01_16, levels=c("1"), labels=c("%forced%"), ordered=FALSE)
data3$DV01_01 = factor(data3$DV01_01, levels=c("1","2"), labels=c("%left%","%right%"), ordered=FALSE)
data3$DV02_01 = factor(data3$DV02_01, levels=c("1","2"), labels=c("%left%","%right%"), ordered=FALSE)
data3$DV05_01 = factor(data3$DV05_01, levels=c("1","2"), labels=c("%left%","%right%"), ordered=FALSE)
data3$DV06_01 = factor(data3$DV06_01, levels=c("1","2"), labels=c("%left%","%right%"), ordered=FALSE)
data3$DM02 = factor(data3$DM02, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","-9"), labels=c("18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","[NA] Not answered"), ordered=FALSE)
data3$DM03 = factor(data3$DM03, levels=c("1","2","3","-9"), labels=c("male","female","other","[NA] Not answered"), ordered=FALSE)
data3$DM04 = factor(data3$DM04, levels=c("1","2","-9"), labels=c("Yes","No","[NA] Not answered"), ordered=FALSE)
data3$DM05 = factor(data3$DM05, levels=c("1","2","3","4","5","6","-9"), labels=c("No formal education","Secondary school/GCSE","College/A levels","Undergraduate degree (BA, BSc or comparable)","Graduate degree (MA, MSc or comparable)","Doctorate degree (PhD, MD, or comparable)","[NA] Not answered"), ordered=FALSE)
data3$DM08 = factor(data3$DM08, levels=c("1","2","-9"), labels=c("I agree","I disagree","[NA] Not answered"), ordered=FALSE)
attr(data3$UR01,"1") = "smile-left-female-ab-pos"
attr(data3$UR01,"2") = "frown-left-female-ab-pos"
attr(data3$UR01,"3") = "smile-right-female-ab-pos"
attr(data3$UR01,"4") = "frown-right-female-ab-pos"
attr(data3$UR01,"5") = "smile-left-male-ab-pos"
attr(data3$UR01,"6") = "frown-left-male-ab-pos"
attr(data3$UR01,"7") = "smile-right-male-ab-pos"
attr(data3$UR01,"8") = "frown-right-male-ab-pos"
attr(data3$UR01,"9") = "smile-left-female-ba-pos"
attr(data3$UR01,"10") = "frown-left-female-ba-pos"
attr(data3$UR01,"11") = "smile-right-female-ba-pos"
attr(data3$UR01,"12") = "frown-right-female-ba-pos"
attr(data3$UR01,"13") = "smile-left-male-ba-pos"
attr(data3$UR01,"14") = "frown-left-male-ba-pos"
attr(data3$UR01,"15") = "smile-right-male-ba-pos"
attr(data3$UR01,"16") = "frown-right-male-ba-pos"
attr(data3$UR01,"17") = "smile-left-female-cd-pos"
attr(data3$UR01,"18") = "frown-left-female-cd-pos"
attr(data3$UR01,"19") = "smile-right-female-cd-pos"
attr(data3$UR01,"20") = "frown-right-female-cd-pos"
attr(data3$UR01,"21") = "smile-left-male-cd-pos"
attr(data3$UR01,"22") = "frown-left-male-cd-pos"
attr(data3$UR01,"23") = "smile-right-male-cd-pos"
attr(data3$UR01,"24") = "frown-right-male-cd-pos"
attr(data3$UR01,"25") = "smile-left-female-dc-pos"
attr(data3$UR01,"26") = "frown-left-female-dc-pos"
attr(data3$UR01,"27") = "smile-right-female-dc-pos"
attr(data3$UR01,"28") = "frown-right-female-dc-pos"
attr(data3$UR01,"29") = "smile-left-male-dc-pos"
attr(data3$UR01,"30") = "frown-left-male-dc-pos"
attr(data3$UR01,"31") = "smile-right-male-dc-pos"
attr(data3$UR01,"32") = "frown-right-male-dc-pos"
attr(data3$UR01,"33") = "smile-left-female-ab-neg"
attr(data3$UR01,"34") = "frown-left-female-ab-neg"
attr(data3$UR01,"35") = "smile-right-female-ab-neg"
attr(data3$UR01,"36") = "frown-right-female-ab-neg"
attr(data3$UR01,"37") = "smile-left-male-ab-neg"
attr(data3$UR01,"38") = "frown-left-male-ab-neg"
attr(data3$UR01,"39") = "smile-right-male-ab-neg"
attr(data3$UR01,"40") = "frown-right-male-ab-neg"
attr(data3$UR01,"41") = "smile-left-female-ba-neg"
attr(data3$UR01,"42") = "frown-left-female-ba-neg"
attr(data3$UR01,"43") = "smile-right-female-ba-neg"
attr(data3$UR01,"44") = "frown-right-female-ba-neg"
attr(data3$UR01,"45") = "smile-left-male-ba-neg"
attr(data3$UR01,"46") = "frown-left-male-ba-neg"
attr(data3$UR01,"47") = "smile-right-male-ba-neg"
attr(data3$UR01,"48") = "frown-right-male-ba-neg"
attr(data3$UR01,"49") = "smile-left-female-cd-neg"
attr(data3$UR01,"50") = "frown-left-female-cd-neg"
attr(data3$UR01,"51") = "smile-right-female-cd-neg"
attr(data3$UR01,"52") = "frown-right-female-cd-neg"
attr(data3$UR01,"53") = "smile-left-male-cd-neg"
attr(data3$UR01,"54") = "frown-left-male-cd-neg"
attr(data3$UR01,"55") = "smile-right-male-cd-neg"
attr(data3$UR01,"56") = "frown-right-male-cd-neg"
attr(data3$UR01,"57") = "smile-left-female-dc-neg"
attr(data3$UR01,"58") = "frown-left-female-dc-neg"
attr(data3$UR01,"59") = "smile-right-female-dc-neg"
attr(data3$UR01,"60") = "frown-right-female-dc-neg"
attr(data3$UR01,"61") = "smile-left-male-dc-neg"
attr(data3$UR01,"62") = "frown-left-male-dc-neg"
attr(data3$UR01,"63") = "smile-right-male-dc-neg"
attr(data3$UR01,"64") = "frown-right-male-dc-neg"
attr(data3$TR01_01a,"-1") = "Mensuration impossible"
attr(data3$TR01_02a,"-1") = "Mensuration impossible"
attr(data3$TR01_03a,"-1") = "Mensuration impossible"
attr(data3$TR01_04a,"-1") = "Mensuration impossible"
attr(data3$TR01_05a,"-1") = "Mensuration impossible"
attr(data3$TR01_06a,"-1") = "Mensuration impossible"
attr(data3$TR01_07a,"-1") = "Mensuration impossible"
attr(data3$TR01_08a,"-1") = "Mensuration impossible"
attr(data3$TR01_09a,"-1") = "Mensuration impossible"
attr(data3$TR01_10a,"-1") = "Mensuration impossible"
attr(data3$TR01_11a,"-1") = "Mensuration impossible"
attr(data3$TR01_12a,"-1") = "Mensuration impossible"
attr(data3$TR01_13a,"-1") = "Mensuration impossible"
attr(data3$TR01_14a,"-1") = "Mensuration impossible"
attr(data3$TR01_15a,"-1") = "Mensuration impossible"
attr(data3$TR01_16a,"-1") = "Mensuration impossible"
attr(data3$DV01_01a,"-1") = "Mensuration impossible"
attr(data3$DV02_01a,"-1") = "Mensuration impossible"
attr(data3$DV03_01,"1") = "%left%"
attr(data3$DV03_01,"101") = "%right%"
attr(data3$DV04_01,"1") = "%left%"
attr(data3$DV04_01,"101") = "%right%"
attr(data3$DV05_01a,"-1") = "Mensuration impossible"
attr(data3$DV06_01a,"-1") = "Mensuration impossible"
attr(data3$DV07_01,"1") = "Grumpy"
attr(data3$DV07_01,"101") = "Cheerful"
attr(data3$DV07_02,"1") = "Grumpy"
attr(data3$DV07_02,"101") = "Cheerful"
attr(data3$DV08_01,"1") = "Cheerful"
attr(data3$DV08_01,"101") = "Grumpy"
attr(data3$DV08_02,"1") = "Cheerful"
attr(data3$DV08_02,"101") = "Grumpy"
attr(data3$DV09_01,"1") = "0 %"
attr(data3$DV09_01,"101") = "100 %"
attr(data3$DV09_02,"1") = "0 %"
attr(data3$DV09_02,"101") = "100 %"
attr(data3$DV10_01,"1") = "0 %"
attr(data3$DV10_01,"101") = "100 %"
attr(data3$DV10_02,"1") = "0 %"
attr(data3$DV10_02,"101") = "100 %"
attr(data3$DV11_01,"1") = "0 %"
attr(data3$DV11_01,"101") = "100 %"
attr(data3$DV11_02,"1") = "0 %"
attr(data3$DV11_02,"101") = "100 %"
attr(data3$DV12_01,"1") = "0 %"
attr(data3$DV12_01,"101") = "100 %"
attr(data3$DV12_02,"1") = "0 %"
attr(data3$DV12_02,"101") = "100 %"
attr(data3$FINISHED,"F") = "Canceled"
attr(data3$FINISHED,"T") = "Finished"
attr(data3$Q_VIEWER,"F") = "Respondent"
attr(data3$Q_VIEWER,"T") = "Spectator"
comment(data3$SERIAL) = "Serial number (if provided)"
comment(data3$REF) = "Reference (if provided in link)"
comment(data3$QUESTNNR) = "Questionnaire that has been used in the interview"
comment(data3$MODE) = "Interview mode"
comment(data3$STARTED) = "Time the interview has started (Europe/Berlin)"
comment(data3$UR01_CP) = "Counterbalancing: Complete clearances of the ballot, yet"
comment(data3$UR01) = "Counterbalancing: Code drawn"
comment(data3$IV01_RV1) = "POST/GET Variable: PROLIFIC_PID"
comment(data3$IV02_01) = "Page order: Page order"
comment(data3$TR01_01) = "Forced: %forced%"
comment(data3$TR01_01a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_02) = "Forced: %forced%"
comment(data3$TR01_02a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_03) = "Forced: %forced%"
comment(data3$TR01_03a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_04) = "Forced: %forced%"
comment(data3$TR01_04a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_05) = "Forced: %forced%"
comment(data3$TR01_05a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_06) = "Forced: %forced%"
comment(data3$TR01_06a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_07) = "Forced: %forced%"
comment(data3$TR01_07a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_08) = "Forced: %forced%"
comment(data3$TR01_08a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_09) = "Forced: %forced%"
comment(data3$TR01_09a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_10) = "Forced: %forced%"
comment(data3$TR01_10a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_11) = "Forced: %forced%"
comment(data3$TR01_11a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_12) = "Forced: %forced%"
comment(data3$TR01_12a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_13) = "Forced: %forced%"
comment(data3$TR01_13a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_14) = "Forced: %forced%"
comment(data3$TR01_14a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_15) = "Forced: %forced%"
comment(data3$TR01_15a) = "Forced: %forced% response time [ms]"
comment(data3$TR01_16) = "Forced: %forced%"
comment(data3$TR01_16a) = "Forced: %forced% response time [ms]"
comment(data3$DV01_01) = "Likeability: %left%"
comment(data3$DV01_01a) = "Likeability: %left% response time [ms]"
comment(data3$DV02_01) = "Likeability2: %left%"
comment(data3$DV02_01a) = "Likeability2: %left% response time [ms]"
comment(data3$DV03_01) = "Preference: %left%/%right%"
comment(data3$DV04_01) = "Preference2: %left%/%right%"
comment(data3$DV05_01) = "Employment: %left%"
comment(data3$DV05_01a) = "Employment: %left% response time [ms]"
comment(data3$DV06_01) = "Employment2: %left%"
comment(data3$DV06_01a) = "Employment2: %left% response time [ms]"
comment(data3$DV07_01) = "Impression: %left%"
comment(data3$DV07_02) = "Impression: %right%"
comment(data3$DV08_01) = "Impression2: %left%"
comment(data3$DV08_02) = "Impression2: %right%"
comment(data3$DV09_01) = "Conditionals: ... %name_left%, they smiled?"
comment(data3$DV09_02) = "Conditionals: ... %name_right%, they smiled?"
comment(data3$DV10_01) = "Conditionals2: ... %name_left%, they frowned?"
comment(data3$DV10_02) = "Conditionals2: ... %name_right%, they frowned?"
comment(data3$DV11_01) = "Confidence: How confident (in %) are you that you can make a reasonable estimate?"
comment(data3$DV11_02) = "Confidence: How confident (in %) are you that you can make a reasonable estimate?"
comment(data3$DV12_01) = "Confidence: How confident (in %) are you that you can make a reasonable estimate?"
comment(data3$DV12_02) = "Confidence: How confident (in %) are you that you can make a reasonable estimate?"
comment(data3$DM01_01) = "ID: [01]"
comment(data3$DM02) = "Age"
comment(data3$DM03) = "Gender"
comment(data3$DM04) = "Psychstudent"
comment(data3$DM05) = "Education"
comment(data3$DM06_01) = "Nationality: [01]"
comment(data3$DM07_01) = "Mothertongue: [01]"
comment(data3$DM08) = "AttCheck"
comment(data3$TIME001) = "Time spent on page 1"
comment(data3$TIME002) = "Time spent on page 2"
comment(data3$TIME003) = "Time spent on page 3"
comment(data3$TIME004) = "Time spent on page 4"
comment(data3$TIME005) = "Time spent on page 5"
comment(data3$TIME006) = "Time spent on page 6"
comment(data3$TIME008) = "Time spent on page 8"
comment(data3$TIME009) = "Time spent on page 9"
comment(data3$TIME010) = "Time spent on page 10"
comment(data3$TIME011) = "Time spent on page 11"
comment(data3$TIME012) = "Time spent on page 12"
comment(data3$TIME013) = "Time spent on page 13"
comment(data3$TIME014) = "Time spent on page 14"
comment(data3$TIME015) = "Time spent on page 15"
comment(data3$TIME016) = "Time spent on page 16"
comment(data3$TIME017) = "Time spent on page 17"
comment(data3$TIME018) = "Time spent on page 18"
comment(data3$TIME_SUM) = "Time spent overall (except outliers)"
comment(data3$MAILSENT) = "Time when the invitation mailing was sent (personally identifiable recipients, only)"
comment(data3$LASTDATA) = "Time when the data was most recently updated"
comment(data3$FINISHED) = "Has the interview been finished (reached last page)?"
comment(data3$Q_VIEWER) = "Did the respondent only view the questionnaire, omitting mandatory questions?"
comment(data3$LASTPAGE) = "Last page that the participant has handled in the questionnaire"
comment(data3$MAXPAGE) = "Hindmost page handled by the participant"
comment(data3$MISSING) = "Missing answers in percent"
comment(data3$MISSREL) = "Missing answers (weighted by relevance)"
comment(data3$TIME_RSI) = "Degradation points for being very fast"
comment(data3$DEG_TIME) = "Degradation points for being very fast"



# Assure that the comments are retained in subsets
as.data.frame.avector = as.data.frame.vector
`[.avector` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}
data3_tmp = data.frame(
  lapply(data3, function(x) {
    structure( x, class = c("avector", class(x) ) )
  } )
)
mostattributes(data3_tmp) = attributes(data3)
data3 = data3_tmp
rm(data3_tmp)

