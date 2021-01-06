#finds text within a document between two patterns that identify the text -- MUST BE LOADED INTO LOCAL ENVIRONMENT
findValue <- function(full, startS, endS, change = 0, startSS = startS, clean_quote = T, clean_comma = T)
{
  loc = grep(startS,full) + change
  if(length(loc) > 0)
  {
    myresult = list()
    for(i in 1:length(loc)){
      start = gregexpr(startSS,full[loc[i]])
      for(j in 1:length(start[[1]]))
      {
        index <- length(myresult)+1
        half = substr(full[loc[i]], start[[1]][j],nchar(full[loc[i]]))
        end = regexpr(endS,half)[1]
        myresult[index] = gsub(" "," ",substr(half, 1 + nchar(startSS), end - 1))
        #Clean up some errant characters that will make trouble later
        if(clean_quote)
          myresult[index] = gsub('"',"",myresult[index])
        if(clean_comma)
          myresult[index] <- gsub(',','',myresult[index])
      }
    }
  } else myresult = character()
  return(myresult)
} 

#returns generic name or list of generic names of the drug name found in the medications text data
getGenericName <- function(drugS) {
  drugSpaced <- gsub(" ", "%20", drugS)
  for(x in 1:10){
    foo = NULL
    library(httr)
    try(
       foo <- GET(paste("https://rxnav.nlm.nih.gov/REST/drugs.xml?name=", drugSpaced, "&allsrc=1&tty=IN", sep = ""))
       )
       if(!is.null(foo)) break
    }
     
  if(is.null(foo)) stop("You don't have internet or 'https://rxnav.nlm.nih.gov/REST/' is down")
  
  #only proceed if there's a good signal from API
  drug_list <- character()
  if (foo$status_code == 200) {
     bar <- rawToChar(foo$content)
     foobar <- (unlist(findValue(bar, '<name>','</name',0,'<name>', clean_quote = F, clean_comma = F)))[-1]
     if (length(foobar) > 0) {
        #the shortest name description under "concept properties" basically always contains the drug name(s) of only the drug we're searching for
        drug_list <- foobar[[which.min(sapply(foobar, function (x) nchar(x)))]]
        if (length(grep(paste(drugS, " ", sep=""), drug_list, ignore.case = TRUE)) == 1) {
            drug_list <- drugS #if the drug name is already in it's generic form, then it's name is always listed in the <name> section. This is better than extracting names with gsub.
        }
        else {
          drug_list <- unlist(strsplit(drug_list, " / ")) #each drug name is separated with a "/" if a line contains multiple ingredients (i.e. a brand name of a drug with two active ingredient)
          drug_list <- gsub(" [0-9].*$|,.*$", "", drug_list)
          drug_list <- gsub("[N0-9].*[RLT] ", "", drug_list)
        }
     }
     else {
       assign('tmpflag', TRUE, envir = .GlobalEnv) #Row corresponding to input string flagged if RxNorm API's "Drugs" Feature failed to find a drug name (could lead to potential error in detecting drug names)
       return(spell_check(drugS))
     }
  }
  return(drug_list)
}

#if the drug name cannot be found from the RxNorm Drugs API, then RxNorm API's Spelling Suggestions Feature can be used to detect a spell check
spell_check <- function(drugSpc) {
  drugSp <- gsub(" ", "%20", drugSpc)
  foo2 <- NULL
  for(x in 1:10){
    library(httr)
    try(
      foo2 <- GET(paste("https://rxnav.nlm.nih.gov/REST/spellingsuggestions.xml?name=", drugSp, "&allsrc=1&tty=IN", sep = ""))
    )
    if(!is.null(foo2)) break
  }
  
  if(is.null(foo2)) stop("You don't have internet or 'https://rxnav.nlm.nih.gov/REST/' is down")
  
  #Only proceed if you get good connection from API
  if(foo2$status_code == 200) {
    bar2 <- rawToChar(foo2$content)
    foobar2 <- (unlist(findValue(bar2, '<suggestion>','</suggestion',0,'<suggestion>', clean_quote = F, clean_comma = F)))
    #my interesting choice of design you may like or not: returned a corrected spelling if the length of the drug name was long enough, and the list of suggested drug names was short enough, making it more likely that the first spellcheck suggestion is correct
    #This choice was made based on my observations on how accurate spelling suggestions was for misspelled drug names of varying lengths and patterns
    if (length(foobar2) > 0 & (nchar(drugSpc) > 7 | length(foobar2) < 4)) {
      return(foobar2[1])
    }
    else {
      return(character())
    }
  }
  return(character())
}


#main code

meddata <- read.csv("C:/Users/brend/Desktop/summer 2020/VUMC Internship/CCC19-all-drugs-06-17.csv", header = T, stringsAsFactors = F)
tmp <- meddata$meds_other

tmp <- gsub("vit ", "Vitamin ", tmp, ignore.case = TRUE) #replacing "vit" abbreviation since api doesn't pick up on this abbreviation
tmp <- gsub(", C;", ", Vitamin C;", tmp) #for record 792

#tried to remove "common words" from the text, see use of rm_stopwords in later code
medical_stopwords <- c("prn", "instructions", "initiated", "weekday abbreviations", 
                       "oral", "daily", "day", "tid", "use", "for", "take", "was", 
                       "tablet", "constipation", "chewable", "a-d", "xl", "-xl", 
                       "-xp", "td", "inhaler", "inhalers", "disp", "rfl", "meals", 
                       "die", "po", "bid", "supplement", "ointment", "epo", "pr", 
                       "by mouth", "as needed", "every", "mild pain", "fever", "sat", "sun")

#splitting strings by all possible delimiters
meddata$meds_other_list <- strsplit(tmp, ', |; |\t|  |and |& |,|/|: |\\. | of | with | on | \\+ | - ')

#flag rows that potentially failed to detect drug name, or contain info that is too generic or info other than medication names (users did not follow directions)
#flagged row if the getGenericName function (for any string in the split list) resulted in an RxNORM API extraction that did not contain any drug names (invalid drug name or misspelling)
meddata$meds_other_flag <- FALSE

for (x in 1:nrow(meddata)) {
  temp <- unlist(meddata$meds_other_list[x])
  temp <- gsub(" [(].*$", "", temp)
  #removing parts of string that begin with numbers, since these parts of the strings do not contain drug name
  temp <- gsub(" [0-9].*$", "", temp)
  temp <- gsub("[0-9].*[g\\.] ", "", temp)
  #now remove any string that begins with a number
  temp <- gsub("^[0-9].*", "", temp)
  #remove phrases like q24hr, etc.
  temp <- gsub("q.* ", "", temp)
  temp <- trimws(temp, which = "both")
  #remove common medical words ("medical_stopwords")
  temp <- unlist(rm_stopwords(temp, medical_stopwords, separate = FALSE, ignore.case = TRUE))
  #removing strings too short to be drug names
  temp <- temp[nchar(temp) >= 2]
  tmpflag <- FALSE
  temp2 <- unname(unlist(sapply(temp, getGenericName)))
  meddata$meds_other_flag[x] <- tmpflag
  meddata$meds_other_list[x] <- list(temp2)
}

#Web scraping method
# rxsearch2 <- "zosyn"
# 
# for(x in 1:10){
#   foo = NULL
#   library(httr)
#   try(
#     foo <- GET(paste("https://rxnav.nlm.nih.gov/REST/rxcui.xml?name=",
#                      rxsearch2,
#                      "&allsrc=1&tty=IN",
#                      sep = ""))
#   )
#   if(!is.null(foo)) break
# }
# 
# if(is.null(foo)) stop("You don't have internet or 'https://rxnav.nlm.nih.gov/REST/' is down")
# 
# #Only proceed if there's a good signal from the RxNorm api
# 
# if(foo$status_code == 200)
# {
#   bar <- rawToChar(foo$content)
#   foobar <- unlist(findValue(bar, '<rxnormId>','</rxnormId',0,'<rxnormId>', clean_quote = F, clean_comma = F))
#   htmlcode <- readLines(paste0("http://purl.bioontology.org/ontology/RXNORM/", foobar[1]))
#   trade_name_index <- grep("Tradename of", htmlcode)
#   drugnames <- NULL
#   if (length(grep("Tradename of", htmlcode)) > 0) {
#     s <- htmlcode[trade_name_index[1] + 2]
#     id_list <- unlist(findValue(s, 'RXNORM/','</a>',0,'RXNORM/', clean_quote = F, clean_comma = F))
#     id_list <- id_list[nchar(id_list) < 10]
#     for (x in 1:length(id_list)) {
#       htmldrugname <- readLines(paste0("http://purl.bioontology.org/ontology/RXNORM/", id_list[x]))
#       drugname_code <- htmldrugname[(grep("Preferred Name", htmldrugname)) + 2]
#       drugnames[x]  <- unlist(findValue(drugname_code, "<span class='prefLabel'>",'</span>',0,"<span class='prefLabel'>", clean_quote = F, clean_comma = F))
#     }
#   }
#   else {
#     drugname_code <- htmlcode[(grep("Preferred Name", htmlcode))+2]
#     drugnames  <- unlist(findValue(drugname_code, "<span class='prefLabel'>",'</span>',0,"<span class='prefLabel'>", clean_quote = F, clean_comma = F))
#   }
# }
