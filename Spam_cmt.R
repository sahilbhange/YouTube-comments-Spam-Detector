
ytdspam_psy <- read.csv(file.choose(), header = TRUE, sep = ",")
ytdspam_kattyparry <- read.csv(file.choose(), header = TRUE, sep = ",")
ytdspam_LMFAO <- read.csv(file.choose(), header = TRUE, sep = ",")
ytdspam_Emnm <- read.csv(file.choose(), header = TRUE, sep = ",")


rm(YTDSPAM_TRN)

#Merging the data to create single dataset
YTDSPAM_TRN = rbind(ytdspam_psy,ytdspam_kattyparry,ytdspam_LMFAO,ytdspam_Emnm)

dim(YTDSPAM_TRN)

names(YTDSPAM_TRN)
View(YTDSPAM_TRN)

is.factor(YTDSPAM_TRN$CLASS)

YTDSPAM_TRN$CLASS <- as.factor(YTDSPAM_TRN$CLASS)

#removing the special characters (﻿) in the content column
library(stringr)
YTDSPAM_TRN$CONTENT <- str_replace(YTDSPAM_TRN$CONTENT, "﻿", "")

# toMatch is the dictionary of few SPAM words 

View(YTDSPAM_TRN)

SpmMatch <- c("check my video", "Follow me", "watch my videos","subscribe","Please share","Check out","my channel","my page","giftcard","promos","sex","channel","new track","ATTENTION","HTTP","subs","check","like them","new album","Hack","VOTE","please listen","join me","help me","help","youtube","gay","share","fuck","make money","visit","Donate","trailer","free","channel","instagram","facebook","soundcloud","support","website")

library(stringi)
#GE the count of SPAM words in the CONTENT COLUMN
YTDSPAM_TRN$spm_cnt <- stri_count_regex(toupper(YTDSPAM_TRN$CONTENT), toupper(paste(SpmMatch, collapse="|")))

#To check if the comment contains strings "http","www" or ".com" string which represent promotions and could be SPAM and set IS_HTTP=1 else 0

YTDSPAM_TRN$is_http = grepl(toupper("http"),toupper(YTDSPAM_TRN$CONTENT)) | grepl(toupper("www"),toupper(YTDSPAM_TRN$CONTENT))| grepl(toupper("/.com"),toupper(YTDSPAM_TRN$CONTENT)) | grepl(toupper("subscribe"),toupper(YTDSPAM_TRN$CONTENT))

#throwing out 'stop words' that have little discriminative power (e.g. the, a, in).

stopwords <- c( "a","i","me","my","we",	"our"	, "ours","ourselves", "you", "your","yourself","yourselves", "he","him", "his", "himself","her","hers", "herself","it", "its", "itself","them","their", "theirs","themselves", "what", "which","whom","for","this", "that","these", "those", "am","are","was", "were","be", "been", "being","has","had", "having","do", "does", "did","would","should", "could","ought", "i'm", "you're","she's","it's", "we're","they're", "i've", "you've","they've","i'd", "you'd","he'd", "she'd", "we'd","i'll","you'll", "he'll","she'll", "we'll", "they'll","aren't","wasn't", "weren't","hasn't", "haven't", "hadn't","don't","didn't", "won't","wouldn't", "shan't", "shouldn't","cannot","couldn't", "mustn't","let's", "that's", "who's","here's","there's", "when's","where's", "why's", "how's","an","the", "and","but", "if", "or","as","until", "while","of", "at", "by","with","about", "against","between", "into", "through","before","after", "above","below", "to", "from","down","in", "out","on", "off", "over","again","further", "then","once", "here", "there","where","why", "how","all", "any", "both","few","more", "most","other", "some", "such","nor","not", "only","own", "same", "so","too","very")


library (tm)

#creating the new filtered content column by removing stop words in content column
#as stop words unnecessorily increase the comment length
YTDSPAM_TRN$CONTENT_FLTR  =  removeWords(toupper(YTDSPAM_TRN$CONTENT),toupper(stopwords))     #Remove stopwords


# Get the number of words in the comment to compare it with the number of spam words in that comment
YTDSPAM_TRN$fltrwordcnt <- sapply(gregexpr("[[:alpha:]]+", YTDSPAM_TRN$CONTENT_FLTR), function(x) sum(x > 0))


# get the ratio of SPAM words to the number of words in the comment

YTDSPAM_TRN$spmtowrd= (YTDSPAM_TRN$spm_cnt/YTDSPAM_TRN$fltrwordcnt)

head(YTDSPAM_TRN$fltrwordcnt)


# If the length of comment is suspiciously large (length > 50) then it is mostly a SPAM comment
YTDSPAM_TRN$maxlen = ifelse(YTDSPAM_TRN$fltrwordcnt>50, 1, 0)

table(YTDSPAM_TRN$CLASS)

View(YTDSPAM_TRN[,c(4,5,9,11)])


attach(YTDSPAM_TRN)
names(YTDSPAM_TRN)

library("e1071")
library("caret")

#convert required fields to factors

YTDSPAM_TRN$spm_cnt <- as.factor(YTDSPAM_TRN$spm_cnt)
YTDSPAM_TRN$is_http <- as.factor(YTDSPAM_TRN$is_http)
YTDSPAM_TRN$CONTENT_FLTR <- as.factor(YTDSPAM_TRN$CONTENT_FLTR)
YTDSPAM_TRN$spmtowrd <- as.factor(YTDSPAM_TRN$spmtowrd)
YTDSPAM_TRN$fltrwordcnt <- as.factor(YTDSPAM_TRN$fltrwordcnt)
YTDSPAM_TRN$fltrwordcnt <- as.factor(YTDSPAM_TRN$fltrwordcnt)

is.factor(YTDSPAM_TRN$is_http)

is.factor(YTDSPAM_TRN$CLASS)
is.factor(YTDSPAM_TRN$spm_cnt)
is.factor(YTDSPAM_TRN$is_http)
is.factor(YTDSPAM_TRN$CONTENT_FLTR)
is.factor(YTDSPAM_TRN$fltrwordcnt)
is.factor(YTDSPAM_TRN$spmtowrd)

View(YTDSPAM_TRN)

# Run Naive Bayes on newly extraced field fromm the Yootube data

SpmModel <- naiveBayes(CLASS~ spm_cnt+is_http+fltrwordcnt+spmtowrd+maxlen, data = YTDSPAM_TRN)

#SpmModel <- naiveBayes(CLASS~ spm_cnt+is_http+fltrwordcnt+spmtowrd, data = YTDSPAM_TRN)

print(SpmModel)


# Create the test data from the Yootube Shakira file

ytdspam_shakira <- read.csv(file.choose(), header = TRUE, sep = ",")

is.factor(ytdspam_shakira$COMMENT_ID)
is.factor(ytdspam_shakira$AUTHOR)
is.factor(ytdspam_shakira$DATE)
is.factor(ytdspam_shakira$CONTENT)
is.factor(ytdspam_shakira$CLASS)

ytdspam_shakira$CLASS = as.factor(ytdspam_shakira$CLASS)

View(ytdspam_shakira)

ytdspam_shakira$CONTENT <- str_replace(ytdspam_shakira$CONTENT, "﻿", "")

# toMatch is the dictionary of few words which fall under the SPAM Category

SpmMatch <- c("check my video", "Follow me", "watch my videos","subscribe","Please share","Check out","my channel","my page","giftcard","promos","sex","channel","new track","ATTENTION","HTTP","subs","check","like them","new album","Hack","VOTE","please listen","join me","help me","help","youtube","gay","share","fuck","make money","visit","Donate","trailer","free","channel","instagram","facebook","soundcloud","support","website")

library(stringi)
#GE the count of SPAM words in the CONTENT COLUMN
ytdspam_shakira$spm_cnt <- stri_count_regex(toupper(ytdspam_shakira$CONTENT), toupper(paste(SpmMatch, collapse="|")))


#To check if the comment contains strings "http","www" or ".com" string which represent promotions and could be SPAM and set IS_HTTP=1 else 0

ytdspam_shakira$is_http = grepl(toupper("http"),toupper(ytdspam_shakira$CONTENT)) | grepl(toupper("www"),toupper(ytdspam_shakira$CONTENT))| grepl(toupper("/.com"),toupper(ytdspam_shakira$CONTENT)) | grepl(toupper("subscribe"),toupper(ytdspam_shakira$CONTENT))

#throwing out 'stop words' that have little discriminative power (e.g. the, a, in).

stopwords <- c( "a","i","me","my","we",	"our"	, "ours","ourselves", "you", "your","yourself","yourselves", "he","him", "his", "himself","her","hers", "herself","it", "its", "itself","them","their", "theirs","themselves", "what", "which","whom","for","this", "that","these", "those", "am","are","was", "were","be", "been", "being","has","had", "having","do", "does", "did","would","should", "could","ought", "i'm", "you're","she's","it's", "we're","they're", "i've", "you've","they've","i'd", "you'd","he'd", "she'd", "we'd","i'll","you'll", "he'll","she'll", "we'll", "they'll","aren't","wasn't", "weren't","hasn't", "haven't", "hadn't","don't","didn't", "won't","wouldn't", "shan't", "shouldn't","cannot","couldn't", "mustn't","let's", "that's", "who's","here's","there's", "when's","where's", "why's", "how's","an","the", "and","but", "if", "or","as","until", "while","of", "at", "by","with","about", "against","between", "into", "through","before","after", "above","below", "to", "from","down","in", "out","on", "off", "over","again","further", "then","once", "here", "there","where","why", "how","all", "any", "both","few","more", "most","other", "some", "such","nor","not", "only","own", "same", "so","too","very")


#creating the new filtered content column by removing stop words in content column
#as stop words unnecessorily increase the comment length
ytdspam_shakira$CONTENT_FLTR  =  removeWords(toupper(ytdspam_shakira$CONTENT),toupper(stopwords))     #Remove stopwords


# Get the number of words in the comment to compare it with the number of spam words in that comment
ytdspam_shakira$fltrwordcnt <- sapply(gregexpr("[[:alpha:]]+", ytdspam_shakira$CONTENT_FLTR), function(x) sum(x > 0))


# get the ratio of SPAM words to the number of words in the comment

ytdspam_shakira$spmtowrd= (ytdspam_shakira$spm_cnt/ytdspam_shakira$fltrwordcnt)

# If the length of comment is suspiciously large (length > 50) then it is mostly a SPAM comment
ytdspam_shakira$maxlen = ifelse(ytdspam_shakira$fltrwordcnt>50, 1, 0)

is.factor(ytdspam_shakira$spm_cnt)
is.factor(ytdspam_shakira$is_http)
is.factor(ytdspam_shakira$CONTENT_FLTR)
is.factor(ytdspam_shakira$fltrwordcnt)
is.factor(ytdspam_shakira$spmtowrd)

ytdspam_shakira$spm_cnt <- as.factor(ytdspam_shakira$spm_cnt)
ytdspam_shakira$is_http <- as.factor(ytdspam_shakira$is_http)
ytdspam_shakira$CONTENT_FLTR <- as.factor(ytdspam_shakira$CONTENT_FLTR)
ytdspam_shakira$fltrwordcnt <- as.factor(ytdspam_shakira$fltrwordcnt)
ytdspam_shakira$spmtowrd <- as.factor(ytdspam_shakira$spmtowrd)


View(ytdspam_shakira)

#prediction <- predict(SpmModel, newdata = ytdspam_shakira$spm_cnt+ytdspam_shakira$is_http+ytdspam_shakira$fltrwordcnt+ytdspam_shakira$spmtowrd+ytdspam_shakira$maxlen)

prediction <- predict(SpmModel, newdata = ytdspam_shakira)

print(prediction)
confusionMatrix(prediction, ytdspam_shakira$CLASS)


View(SpmModel)
