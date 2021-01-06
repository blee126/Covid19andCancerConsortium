#WBC value transformation

newdat <- read.csv("C:/Users/brend/Desktop/summer 2020/VUMC Internship/CCC19_labs_DATA_2020-06-01_2202.csv", header = T, stringsAsFactors = F)

#creation of flag variable to identify WBC counts for which I could not identity the correct WBC count units, given the row of newdat
newdat$wbc_flag <- FALSE

#creating the transformed row
newdat$wbc_transformed <- newdat$wbc_numeric

#I could determine the units (cells/uL or cells*10^9/L) for any newdat with a decimal value, greater than 500, or less than 100 (proper units for this variable is cells*10^9/L, as stated on REDCAP variable description)

newdat[which(newdat[,"wbc_transformed"] > 500), "wbc_transformed"] <- newdat[which(newdat[,"wbc_transformed"] > 500), "wbc_transformed"]/1000

#I looked at other columns (values between 100 and 500: "gray area") and individually saw whether I could determine the correct units for them. I could not determine the units for three of the rows

newdat[which(newdat[,"wbc_transformed"] == 105), "wbc_flag"] <- TRUE
newdat[which(newdat[,"wbc_transformed"] == 200), "wbc_flag"] <- TRUE
newdat[which(newdat[,"wbc_transformed"] == 400), "wbc_flag"] <- TRUE


#ALC value transformation (started out with alc_flag set to all NA values since this variable was slightly more complicated to deal with)

#creation of flag and alc_transformed
newdat$alc_flag <- NA
newdat[which(newdat[,"alc"] < 10), "alc_flag"] <- FALSE
newdat[which(newdat[,"alc"] > 120), "alc_flag"] <- FALSE
newdat$alc_transformed <- newdat$alc
#values between 10-120 I am unsure about units (cells/uL or cells*10^9/L)

#transforming newdat: multiply by 1000 when needed (cells/uL is proper units)
newdat[which(newdat[,"alc"] < 10), "alc_transformed"] <- newdat[which(newdat[,"alc"] < 10), "alc_transformed"]*1000
newdat[which(newdat$alc_transformed != floor(newdat$alc_transformed)), "alc_flag"] <- FALSE
newdat[which(newdat$alc_transformed != floor(newdat$alc_transformed)), "alc_transformed"] <- newdat[which(newdat$alc_transformed != floor(newdat$alc_transformed)), "alc_transformed"] * 1000

#used total wbc count to determine the units of alc
newdat[which(newdat[,"wbc_transformed"] < 30), "alc_flag"] <- FALSE

#for this patient, I found out (at meeting with Jeremy) that this patient 1064 had CLL, indicating high alc
newdat[which(newdat[,"record_id"] == 1064), "alc_flag"] <- FALSE
newdat[which(newdat[,"record_id"] == 1064), "alc_transformed"] <- newdat[which(newdat[,"record_id"] == 1064), "alc_transformed"]*1000
#same deal for patient #133386
newdat[which(newdat[,"record_id"] == 133386), "alc_flag"] <- FALSE
newdat[which(newdat[,"record_id"] == 133386), "alc_transformed"] <- newdat[which(newdat[,"record_id"] == 133386), "alc_transformed"]*1000

#If there was no total wbc count for alc values between 10-120, I could not easily infer the alc units
newdat[which(newdat[, "alc_transformed"] < 120, is.na(newdat$alc_flag)), "alc_flag"] <- TRUE

#most of the remaining newdat with alc between 10-120, had a total wbc count to determine correct alc units
newdat[which(newdat[, "wbc_transformed"] < 80), "alc_flag"] <- FALSE
newdat[which(newdat[, "alc_transformed"] == 0), "alc_flag"] <- FALSE

#for remaining alc newdat, I determined their units individually, or flagged them true if I couldn't determined their units
newdat[which(newdat[, "record_id"] == 1023), "alc_flag"] <- FALSE
newdat[which(newdat[, "record_id"] == 133419), "alc_flag"] <- FALSE
newdat[which(newdat[, "record_id"] == 1398), "alc_flag"] <- FALSE
newdat[which(newdat[, "alc"] == 0.1), "alc_flag"] <- FALSE
newdat[which(newdat[, "record_id"] == 462), "alc_flag"] <- TRUE


#ANC value transformation, similar to ALC

newdat$anc_flag <- FALSE
newdat$anc_transformed <- newdat$anc

#individual newdat where there was no wbc count to determine correct units (in gray area at anc = 100)
newdat[which(newdat[, "anc_transformed"] == 100), "anc_flag"] <- TRUE

#rest of newdat with anc < 100 was determined to be in units of cells*10^9/L, multiplied by 1000 to get cells/uL
newdat[which(newdat[, "anc_transformed"] < 100), "anc_transformed"] <- newdat[which(newdat[, "anc_transformed"] < 100), "anc_transformed"]*1000

#individual record for which I could not determine anc value units
newdat[which(newdat[, "record_id"] == 133398), "anc_flag"] <- TRUE

#only anc value less than 100 where I could conclude that it was in cells/uL (fixed this newdat point after multiplying anc values less than 100)
newdat[which(newdat[, "record_id"] == 773), "anc_transformed"] <- 20.0

#AEC value calculation

newdat$aec_flag <- FALSE
newdat$aec_transformed <- newdat$aec

#similar to other WBC counts, multiplied all values with a non-zero digit after the decimal point by 1000, to get values in cells/uL
newdat[which(newdat[,"aec_transformed"] %% 1 >0), "aec_transformed"] <- newdat[which(newdat[,"aec_transformed"] %% 1 >0), "aec_transformed"] * 1000

#determined if aec values could be transformed into correct units 
newdat[which(newdat[, "record_id"] == 335), "aec_flag"] <- TRUE
newdat[which(newdat[, "record_id"] == 1485), "aec_flag"] <- TRUE
newdat[which(newdat[, "aec_transformed"] == 11), "aec_flag"] <- TRUE
newdat[which(newdat[, "record_id"] == 824), "aec_flag"] <- TRUE
newdat[which(newdat[, "record_id"] == 133193), "aec_flag"] <- TRUE
newdat[which(newdat[,"aec_transformed"] < 10), "aec_transformed"] <- newdat[which(newdat[,"aec_transformed"] < 10), "aec_transformed"] * 1000
newdat[which(newdat[, "record_id"] == 801), "aec_flag"] <- TRUE

#HGB value transformation (only thing I needed to do was transform a few values that were reported in g/L instead of g/dL)

newdat$hgb_transformed <- newdat$hgb
newdat[which(newdat[,"hgb_transformed"] > 100), "hgb_transformed"] <- newdat[which(newdat[,"hgb_transformed"] > 100), "hgb_transformed"] / 10

#PLT value transformation (only needed to transform a few values that were off by 1000)

newdat$plt_transformed <- newdat$plt
newdat[which(newdat[,"plt_transformed"] > 10000), "plt_transformed"] <- newdat[which(newdat[,"plt_transformed"] > 10000), "plt_transformed"] / 1000

#creat_numeric transformation

newdat$creat_flag <- FALSE
newdat$creat_transformed <- newdat$creat_numeric
newdat[which(newdat[,"creat_numeric"] > 30), "creat_transformed"] <- newdat[which(newdat[,"creat_numeric"] > 30, arr.ind=TRUE), "creat_transformed"] / 88.4
newdat$creat_transformed <- round(newdat$creat_transformed, digits = 2)

#couldn't determine units for 2 rows
newdat[which(newdat[,"creat_numeric"] == 42), "creat_flag"] <- TRUE
newdat[which(newdat[,"creat_numeric"] == 14), "creat_flag"] <- TRUE

#tbili transformation

tbili <- gsub("[^0-9.]", "", newdat$tbili_numeric)
tbili <- as.double(tbili)
newdat$tbili_transformed <- tbili

#AST-SGOT

ast <- gsub("[^0-9.]", "", newdat$ast_numeric)
ast <- as.double(ast)
newdat$ast_transformed <- ast

#ALT-SGOT

alt <- gsub("[^0-9.]", "", newdat$alt_numeric)
alt <- as.double(alt)
newdat$alt_transformed <- alt

#PT 

pt <- gsub("[^0-9.]", "", newdat$pt_numeric)
pt <- as.double(pt)
newdat$pt_transformed <- pt

#APTT

aptt <- gsub("[^0-9.]", "", newdat$aptt_numeric)
aptt <- as.double(aptt)
newdat$aptt_transformed <- aptt

#Fibrinogen (not sure about rows with 3 smallest values since both high and low fibrinogen levels are possible)

newdat$fibrinogen_flag <- FALSE
fib <- gsub("[^0-9. ]", "", newdat$fibrinogen_numeric)
fib <- substring(fib, 1, 4)
fib <- as.double(fib)
newdat$fibrinogen_transformed <- fib
newdat[which(newdat[,"fibrinogen_transformed"] < 50), "fibrinogen_flag"] <- TRUE

#D-dimer

newdat$ddimer_flag <- FALSE

#column for values
ddimer <- gsub("[^0-9. ]", "", newdat$ddimer_numeric)
ddimer <- trimws(ddimer, "l")
ddimer <- gsub( " .*$", "", ddimer)
ddimer <- as.double(ddimer)
newdat$ddimer_value <- ddimer

#column for units
newdat$ddimer_units <- gsub("[0-9.()<>,]", "", newdat$ddimer_numeric)
newdat$ddimer_units <- trimws(newdat$ddimer_units, "l")
newdat$ddimer_units <- gsub("superior than ", "", newdat$ddimer_units)
newdat$ddimer_units <- gsub("Superior than ", "", newdat$ddimer_units)
newdat[which(newdat[, "record_id"] == 134298), "ddimer_units"] <- "ug/mL"

#fixed ddimer value accidentally entered as "8,74
newdat[which(newdat[, "record_id"] == 133887), "ddimer_value"] <- 8.74

#if units but no value, or value but no units, then flagged true
newdat[which(!(is.na(newdat$ddimer_value)) & newdat$ddimer_units == ""), "ddimer_flag"] <- TRUE
newdat[which(!(newdat$ddimer_units == "") & is.na(newdat$ddimer_value)), "ddimer_flag"] <- TRUE

#LDH

newdat$ldh_flag <- FALSE

#column for values
ldh <- gsub("[^0-9. ]", "", newdat$ldh_numeric)
ldh <- trimws(ldh, "l")
ldh <- gsub( " .*$", "", ldh)
ldh <- as.double(ldh)
newdat$ldh_value <- ldh

#column for units
newdat$ldh_units <- gsub("[0-9.()<>,-]", "", newdat$ldh_numeric)
newdat$ldh_units <- gsub("normal", "", newdat$ldh_units)
newdat$ldh_units <- trimws(newdat$ldh_units, "l")

#if units but no value, or value but no units, then flagged true
newdat[which(!(is.na(newdat$ldh_value)) & newdat$ldh_units == ""), "ldh_flag"] <- TRUE
newdat[which(!(newdat$ldh_units == "") & is.na(newdat$ldh_value)), "ldh_flag"] <- TRUE

#TnI

newdat$tni_flag <- FALSE
#Since the survey asks for values greater than or equal to 0.05
newdat[which(newdat[,"tni_numeric"] < 0.05), "tni_flag"] <- TRUE

#not sure if TnI can be greater than 500 ng/mL, or if these columns are in wrong units
newdat[which(newdat[,"tni_numeric"] > 500), "tni_flag"] <- TRUE

newdat$tni_transformed <- newdat$tni_numeric

#HS_trop

newdat$hs_trop_flag <- FALSE

newdat$hs_trop_transformed <- newdat$hs_trop_numeric
#looks like many users may have entered value in pg/L instead of pg/mL, hence the values that are in the thousands
#newdat[which(newdat[,"hs_trop_transformed"] > 1000), "hs_trop_transformed"] <- newdat[which(newdat[,"hs_trop_transformed"] > 1000), "hs_trop_transformed"]/1000
#huge gray area here (between 100-1000)
newdat[which(newdat[,"hs_trop_transformed"] >= 100), "hs_trop_flag"] <- TRUE
#newdat[which(newdat[,"hs_trop_numeric"] >= 100000), "hs_trop_flag"] <- FALSE

#BNP

newdat$bnp_flag <- FALSE
newdat$bnp_transformed <- newdat$bnp_numeric
#BNP is reported between 5-5000 pg/mL, not sure what units values greater than 5000 are in
newdat[which(newdat[,"bnp_transformed"] > 5000), "bnp_flag"] <- TRUE

#CRP

newdat$crp_flag <- FALSE

#column for values
crp <- gsub("[^0-9. ]", "", newdat$crp_numeric)
crp <- trimws(crp, "l")
crp <- gsub( " .*$", "", crp)
#fixing one row with faulty input
crp[crp == "42.2."] <- "42.2"
crp <- as.double(crp)
newdat$crp_value <- crp

#column for units
newdat$crp_units <- gsub("[0-9.()<>,-]", "", newdat$crp_numeric)
newdat$crp_units <- gsub("high", "", newdat$crp_units)
newdat$crp_units <- trimws(newdat$crp_units, "l")

#if units but no value, or value but no units, then flagged true
newdat[which(!(is.na(newdat$crp_value)) & newdat$crp_units == ""), "crp_flag"] <- TRUE
newdat[which(!(newdat$crp_units == "") & is.na(newdat$crp_value)), "crp_flag"] <- TRUE

#IL6, looks like values can range greatly, but still not sure if there is an upper limit

newdat$il6_transformed <- newdat$il6_numeric


#HIV_VL and HIV_CD4

newdat$hiv_vl_transformed <- newdat$hiv_vl
newdat$hiv_cd4_transformed <- newdat$hiv_cd4