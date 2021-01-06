
load("C:/Users/brend/Desktop/summer 2020/VUMC Internship/htwt.RData")

temp.ref <- which(is.na(ccc19x$bmi) & ccc19x$height != '' & ccc19x$weight != '' & 
                    ccc19x$significant_comorbidities___238136002 == 0 &
                    ccc19x$significant_comorbidities___414916001 == 0)

#removing rows that are missing height or weight
temp <- ccc19x[temp.ref,c('height','weight')]
  
#fixing transposed data
temp[grepl("kg", temp$height, fixed = TRUE), c("height", "weight")] <- temp[grepl("kg", temp$height, fixed = TRUE), c("weight", "height")]
temp[grepl("lb", temp$height, fixed = TRUE), c("height", "weight")] <- temp[grepl("lb", temp$height, fixed = TRUE), c("weight", "height")]

#converting all heights to meters

# #first removing all units from each string in height column
# temp[, "height"] <- gsub("cm","",temp$height)
# temp[, "height"] <- gsub("m","",temp$height)
# temp[, "height"] <- gsub("M","",temp$height)
# temp[, "height"] <- gsub("inches","",temp$height)

#fixing data in the format ft'in" (e.g 5'11"), could also be used in a similar way to fix data in format "x feet y inches"
x <- temp[grepl("'", temp$height, fixed = TRUE), "height"]
x <- gsub("'", "", x)
x <- gsub("\"", "", x)
x <- gsub(" ", "", x)
y <- strtoi(substr(x, 1, 1))
z <- strtoi(substr(x, 2, 3))
x <- y * 12 + z
x <- toString(x)
x <- strsplit(x, ", ")
x <- paste(x[[1]], 'inches')
temp[grepl("'", temp$height, fixed = TRUE), "height"] <- x

#fixed the sole height entry in the format "x foot y inches"
temp[grepl("foot", temp$height, fixed = TRUE), "height"] <- "61 inches"

#converted height strings into double values and put them in a new column
temp$mheight <- temp$height
temp$mheight <- gsub(temp$mheight, pattern = 'cm|[mM]| |in|inches', replacement = '')
temp$mheight <- as.numeric(temp$mheight)

#converting each height in the mheight double value column into height in meters (values greater than 100 are assumed to be in centimeters)
temp.ref <- grep(temp$height, pattern = 'cm')
temp$mheight[temp.ref] <- temp$mheight[temp.ref]/100

temp.ref <- grep(temp$height, pattern = 'in')
temp$mheight[temp.ref] <- temp$mheight[temp.ref]*0.0254

temp.ref <- which(temp$mheight > 100)
temp$mheight[temp.ref] <- temp$mheight[temp.ref]/100

# temp[temp$mheight > 100, "mheight"] <- temp[temp$mheight > 100, "mheight"]/100
# temp[temp$mheight > 10, "mheight"] <- temp[temp$mheight > 10, "mheight"]*0.0254

#fixing weight units
# temp[, "weight"] <- gsub("kg","",temp$weight)
# temp[, "weight"] <- gsub("Kg","",temp$weight)
# temp[, "weight"] <- gsub("KG","",temp$weight)

#fixing data entered in units of pounds
#(this command failed, but it attempted to fix weight data in pounds in both formats "x lbs" and "x pounds" simultaneously)
#x <- temp[grepl("lb", temp$weight, fixed = TRUE) || grepl("lbs", temp$weight, fixed = TRUE) || grepl("pounds", temp$weight, fixed = TRUE), "weight"]

#fixing the data entered in pounds, converting it to kg and re-entering the data into the weight column
# x <- temp[grepl("lb", temp$weight, fixed = TRUE), "weight"]
# x <- gsub("[^0-9.]", "", x)
# x <- as.double(x)
# x <- x*0.454
# x <- toString(x)
# x <- strsplit(x, ", ")
# temp[grepl("lb", temp$weight, fixed = TRUE), "weight"] <- x[1]
# 
# #fixing the sole data entry with "x pounds" format
# temp[grepl("pounds", temp$weight, fixed = TRUE), "weight"] <- "64.014"

#converting weight value strings to double values and entering them into a new column
temp$kgweight <- temp$weight
temp$kgweight <- gsub(temp$kgweight, pattern = 'lbs|lb|pounds|kg| |', replacement = '', ignore.case = T)
temp$kgweight <- as.double(temp$kgweight)

#Convert lbs into kg (assume that any value without a unit is already in kg)
temp.ref <- grep(temp$weight, pattern = 'lb|pound')
temp$kgweight[temp.ref] <- temp$kgweight[temp.ref]*0.454

#calculating bmi and storing final result into a new copy
temp$bmi <- temp$kgweight / (temp$mheight)^2
