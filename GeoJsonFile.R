library(rgdal)
setwd("/Users/annieulichney/Desktop")
library(stringr)
library(geojson)

#read data
counties_old <- readOGR(dsn = "/Users/annieulichney/Desktop/nycounties.GeoJSON")
counties <- readOGR(dsn = "/Users/annieulichney/Desktop/annie2.json")
#countypop <- read.csv("NYcountypop.csv")
census <- read.csv("census_data.csv",as.is=T)

#census data processing 
census$NAME <- gsub(' County, New York', '', census$NAME)
census$DP05_0001E <- as.numeric(census$DP05_0001E)
totalPop <- sum(census$DP05_0001E[2:63], na.rm = TRUE)
census$NAME[64] <- 'New York State Total' #avoid confusing NY county with NY state numbers

#Read in under custody data
uc <- read.csv("UCgeo.csv")
uc$crimeCounty <- str_to_title(uc$crimeCounty)

#add county for each prison
uc$prisonCounty[uc$prison == 'fishkill'] <- "Dutchess"
uc$prisonCounty[uc$prison == 'sing sing'] <- "Westchester"
uc$prisonCounty[uc$prison == 'washington'] <- "Washington"
uc$prisonCounty[uc$prison == 'upstate'] <- "Franklin"
uc$prisonCounty[uc$prison == 'woodbourne'] <- "Sullivan"
uc$prisonCounty[uc$prison == 'greene'] <- "Greene"
uc$prisonCounty[uc$prison == 'shawangunk'] <- "Orange"
uc$prisonCounty[uc$prison == 'clinton'] <- "Clinton"
uc$prisonCounty[uc$prison == 'eastern'] <- "Ulster"
uc$prisonCounty[uc$prison == 'great meadow'] <- "Washington"
uc$prisonCounty[uc$prison == 'wende'] <- "Erie"
uc$prisonCounty[uc$prison == 'elmira'] <- "Chemung"
uc$prisonCounty[uc$prison == 'sullivan'] <- "Sullivan"
uc$prisonCounty[uc$prison == 'attica'] <- "Wyoming"
uc$prisonCounty[uc$prison == 'coxsackie'] <- "Greene"
uc$prisonCounty[uc$prison == 'green haven'] <- "Dutchess"
uc$prisonCounty[uc$prison == 'edgecombe'] <- "New York"
uc$prisonCounty[uc$prison == 'mohawk'] <- "Oneida"
uc$prisonCounty[uc$prison == 'five points'] <- "Seneca"
uc$prisonCounty[uc$prison == 'cayuga'] <- "Cayuga"
uc$prisonCounty[uc$prison == 'marcy'] <- "Oneida"
uc$prisonCounty[uc$prison == 'otisville'] <- "Orange"
uc$prisonCounty[uc$prison == 'auburn'] <- "Cayuga"
uc$prisonCounty[uc$prison == 'downstate'] <- "Dutchess"
uc$prisonCounty[uc$prison == 'collins'] <- "Erie"
uc$prisonCounty[uc$prison == 'adirondack'] <- "Essex"
uc$prisonCounty[uc$prison == 'cape vincent'] <- "Jefferson"
uc$prisonCounty[uc$prison == 'franklin'] <- "Franklin"
uc$prisonCounty[uc$prison == 'midstate'] <- "Oneida"
uc$prisonCounty[uc$prison == 'wyoming'] <- "Wyoming"
uc$prisonCounty[uc$prison == 'bare hill'] <- "Franklin"
uc$prisonCounty[uc$prison == 'groveland'] <- "Livingston"
uc$prisonCounty[uc$prison == 'gouverneur'] <- "St. Lawrence"
uc$prisonCounty[uc$prison == 'southport'] <- "Chemung"
uc$prisonCounty[uc$prison == 'ulster'] <- "Ulster"
uc$prisonCounty[uc$prison == 'wallkill'] <- "Ulster"
uc$prisonCounty[uc$prison == 'gowanda'] <- "Erie"
uc$prisonCounty[uc$prison == 'riverview'] <- "St. Lawrence"
uc$prisonCounty[uc$prison == 'orleans'] <- "Orleans"
uc$prisonCounty[uc$prison == 'bedford hills'] <- "Westchester"
uc$prisonCounty[uc$prison == 'albion'] <- "Orleans"
uc$prisonCounty[uc$prison == 'taconic'] <- "Westchester"
uc$prisonCounty[uc$prison == 'hudson'] <- "Columbia"
uc$prisonCounty[uc$prison == 'lakeview'] <- "Chautauqua"
uc$prisonCounty[uc$prison == 'willard'] <- "Seneca"
uc$prisonCounty[uc$prison == 'watertown'] <- "Jefferson"
uc$prisonCounty[uc$prison == 'altona'] <- "Clinton"
uc$prisonCounty[uc$prison == 'ogdensburg'] <- "St. Lawrence"
uc$prisonCounty[uc$prison == 'queensboro'] <- "Queens"
uc$prisonCounty[uc$prison == 'hale creek'] <- "Fulton"
uc$prisonCounty[uc$prison == 'rochester'] <- "Monroe"
uc$prisonCounty[uc$prison == 'moriah'] <- "Essex"

##Cleaning
#one person has na for crimeCounty, fill in with Kings
uc$crimeCounty[is.na(uc$crimeCounty)] <- 'New York'
uc$crimeCounty[uc$crimeCounty == 'St. Lawrence'] <- 'St. Lawrence'
uc$prisonCounty[uc$prisonCounty == 'St. Lawrence'] <- 'St. Lawrence'
census$NAME[census$NAME == 'St. Lawrence'] <- 'St. Lawrence'

#get rid of county after each county name
counties@data$name <- gsub(' County', '', counties@data$name)
counties@data$name[counties@data$name == 'St Lawrence'] <- 'St. Lawrence'
 

#store values of interest in spatial df
for(i in 1:length(counties@data$name)){
  
 #number crimes committed in each county
  counties@data$numCrimeCommitted[i] <- dim(uc[uc$crimeCounty == counties@data$name[i], ])[1]
  
 #male number crimes committed each county
  counties@data$numCrimeCommittedMale[i] <- length(uc$crimeCounty[uc$crimeCounty == counties@data$name[i] & uc$sex == 'M'])
  
 #female number crimes committed each county
  counties@data$numCrimeCommittedFemale[i] <- length(uc$crimeCounty[uc$crimeCounty == counties@data$name[i] & uc$sex == 'F'])
  
 #total number in prison in each county
 counties@data$numIncarcerated[i] <- length(uc$prisonCounty[uc$prisonCounty == counties@data$name[i]])
 
 #total males in prison in each county
 counties@data$numIncarceratedMale[i] <- length(uc$prisonCounty[uc$prisonCounty == counties@data$name[i] & uc$sex == 'M'])
 
 #total females in prison in each county
 counties@data$numIncarceratedFemale[i] <- length(uc$prisonCounty[uc$prisonCounty == counties@data$name[i] & uc$sex == 'F'])
  
 #population for each county
 counties@data$population[i] <- census$DP05_0001E[census$NAME == counties@data$name[i]]
 
 #male population for each county
 counties@data$populationMale[i] <- as.integer(census$DP05_0002E[census$NAME == counties@data$name[i]])
 
 #female population for each county
 counties@data$populationFemale[i] <- as.integer(census$DP05_0003E[census$NAME == counties@data$name[i]])
 
 #incarcerated per 100,000
 counties@data$incarcerationRate[i] <- counties@data$numCrimeCommitted[i]/counties@data$population[i] * 100000
 
 #male incarcerated per 100,000
 counties@data$incarcerationRateMale[i] <- counties@data$numCrimeCommittedMale[i]/counties@data$populationMale[i] * 100000
 
 #female incarcerated per 100,000
 counties@data$incarcerationRateFemale[i] <- counties@data$numCrimeCommittedFemale[i]/counties@data$populationFemale[i] * 100000
 
 #number incarcerated in the county/number crimes committed in the county
 counties@data$incarceratedOverCommitted[i] <-counties@data$numIncarcerated[i] / counties@data$numCrimeCommitted[i]
 
 #Hispanic population for each county
 counties@data$countyHispanic[i] <- as.integer(census$DP05_0071E[census$NAME == counties@data$name[i]])
 
 #Hispanic percent for each county
 counties@data$countyHispanicPct[i] <- as.numeric(census$DP05_0071PE[census$NAME == counties@data$name[i]])
 
 #NH White population for each county
 counties@data$countyNHWhite[i] <- as.integer(census$DP05_0077E[census$NAME == counties@data$name[i]])
 
 #NH White percent for each county
 counties@data$countyNHWhitePct[i] <- as.numeric(census$DP05_0077PE[census$NAME == counties@data$name[i]])
 
 #NH Black population for each county
 counties@data$countyNHBlack[i] <- as.integer(census$DP05_0078E[census$NAME == counties@data$name[i]])
 
 #NH Black percent for each county
 counties@data$countyNHBlackPct[i] <- as.numeric(census$DP05_0078PE[census$NAME == counties@data$name[i]])
 
 #NH Other population for each county
 counties@data$countyNHOther[i] <- as.integer(census$DP05_0079E[census$NAME == counties@data$name[i]]) + as.integer(census$DP05_0080E[census$NAME == counties@data$name[i]]) + as.integer(census$DP05_0081E[census$NAME == counties@data$name[i]]) + as.integer(census$DP05_0082E[census$NAME == counties@data$name[i]]) + as.integer(census$DP05_0083E[census$NAME == counties@data$name[i]]) 
 
 #NH Other percent for each county
 counties@data$countyNHOtherPct[i] <- as.numeric(census$DP05_0079PE[census$NAME == counties@data$name[i]]) + as.numeric(census$DP05_0080PE[census$NAME == counties@data$name[i]]) + as.numeric(census$DP05_0081PE[census$NAME == counties@data$name[i]]) + as.numeric(census$DP05_0082PE[census$NAME == counties@data$name[i]]) + as.numeric(census$DP05_0083PE[census$NAME == counties@data$name[i]]) 
 
 #Hispanic population for prison
 counties@data$prisonHispanic[i] <- length(uc$crimeCounty[uc$crimeCounty == counties@data$name[i] & uc$modEthRace == 'Hispanic'])
 
 #Hispanic percent for prison
 counties@data$prisonHispanicPct[i] <- counties@data$prisonHispanic[i]/counties@data$population[i]
 
 #NH White population for prison
 counties@data$prisonNHWhite[i] <- length(uc$crimeCounty[uc$crimeCounty == counties@data$name[i] & uc$modEthRace == 'NH-White'])
 
 #NH White percent for prison
 counties@data$prisonNHWhitePct[i] <- counties@data$prisonNHWhite[i]/counties@data$population[i]
 
 #NH Black population for prison
 counties@data$prisonNHBlack[i] <- length(uc$crimeCounty[uc$crimeCounty == counties@data$name[i] & uc$modEthRace == 'NH-Black'])
 
 #NH Black percent for prison
 counties@data$prisonNHBlackPct[i] <- counties@data$prisonNHBlack[i]/counties@data$population[i]
 
 #Other race/ethn population for prison
 counties@data$prisonNHOther[i] <- length(uc$crimeCounty[uc$crimeCounty == counties@data$name[i] & uc$modEthRace == 'Other'])
 
 #Other race/ethn percent for prison
 counties@data$prisonNHOtherPct[i] <- counties@data$prisonNHOther[i]/counties@data$population[i]
 
 #Hispanic incarcerated per 100,000
 counties@data$incarcerationRateHispanic[i] <- counties@data$prisonHispanic[i]/counties@data$countyHispanic[i] * 100000
 
 #NH White incarcerated per 100,000
 counties@data$incarcerationRateNHWhite[i] <- counties@data$prisonNHWhite[i]/counties@data$countyNHWhite[i] * 100000
 
 #NH Black incarcerated per 100,000
 counties@data$incarcerationRateNHBlack[i] <- counties@data$prisonNHBlack[i]/counties@data$countyNHBlack[i] * 100000
 
 #NH Other incarcerated per 100,000
 counties@data$incarcerationRateNHOther[i] <- counties@data$prisonNHOther[i]/counties@data$countyNHOther[i] * 100000
 
}


counties2 <- as.geojson(counties)


outputfile <- file("output.geojson")
writeLines(counties2, outputfile)
close(outputfile)