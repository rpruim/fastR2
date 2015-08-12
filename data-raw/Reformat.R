
require(dplyr)

reCode <- function(new, old = tolower(new), transform.names = c("tolower", "camelCase")) {
  transform.names <- match.arg(transform.names)
  cat(
    paste0(new, " <- ", old, "\n",
         "names(", new, ") <- ", transform.names, "(names(", new, "))", "\n",
         "devtools::use_data(", new, ", overwrite = TRUE)"
  )
  )
}

camelCase <- function(x) {
  sapply(x, function(x) { substring(x, 1, 1) <- tolower(substr(x,1,1)); x } )
}

files <- dir("old-format", full.names = TRUE)
for (f in files)
  load(f)

ACTgpa <- actgpa
devtools::use_data(ACTgpa, overwrite = TRUE)

AirlineArrival <- airlineArrival
names(AirlineArrival) <- tolower(names(AirlineArrival))
devtools::use_data(AirlineArrival, overwrite = TRUE)

AirPollution <- airpollution
names(AirPollution) <- tolower(names(AirPollution))
devtools::use_data(AirPollution, overwrite = TRUE)

BallDrop <- balldrop
names(BallDrop) <- tolower(names(BallDrop))
devtools::use_data(BallDrop, overwrite = TRUE)

Batting <- batting
devtools::use_data(Batting, overwrite = TRUE)

Buckthorn <- buckthorn
devtools::use_data(Buckthorn, overwrite = TRUE)

Bugs <- bugs
devtools::use_data(Bugs, overwrite = TRUE)

Concrete28 <- concrete28
devtools::use_data(Concrete28, overwrite = TRUE)

ConcreteAll <- concreteAll
devtools::use_data(ConcreteAll, overwrite = TRUE)

Corn <- corn
devtools::use_data(Corn, overwrite = TRUE)

Cuckoo <- cuckoo
devtools::use_data(Cuckoo, overwrite = TRUE)

DeathPenalty <- deathPen %>% select(-Penalty)
names(DeathPenalty) <- tolower(names(DeathPenalty))
devtools::use_data(DeathPenalty, overwrite = TRUE)

Drag <- drag
devtools::use_data(drag, overwrite = TRUE)

Endurance <- endurance
names(Endurance) <- tolower(names(Endurance))
devtools::use_data(Endurance, overwrite = TRUE)

FamilySmoking <- familySmoking
names(FamilySmoking) <- tolower(names(FamilySmoking))
devtools::use_data(FamilySmoking, overwrite = TRUE)

Fumbles <- fumbles
devtools::use_data(Fumbles, overwrite = TRUE)

FUSION1 <- fusion1
FUSION2 <- fusion2
devtools::use_data(FUSION1, overwrite = TRUE)
devtools::use_data(FUSION2, overwrite = TRUE)

GPA <- gpa
devtools::use_data(GPA, overwrite = TRUE)

HeliumFootballs <- heliumFootballs
names(HeliumFootballs) <- tolower(names(HeliumFootballs))
devtools::use_data(HeliumFootballs, overwrite = TRUE)

Ice <- ice
names(Ice) <- tolower(names(Ice))
devtools::use_data(Ice, overwrite = TRUE)

Inflation <- inflation
devtools::use_data(Inflation, overwrite = TRUE)

names(Jordan8687) <- tolower(names(Jordan8687))
devtools::use_data(Jordan8687, overwrite = TRUE)

Kids <- kids
names(Kids) <- tolower(names(Kids))
devtools::use_data(Kids, overwrite = TRUE)

LittleSurvey <- littleSurvey
names(LittleSurvey) <- tolower(names(LittleSurvey))
devtools::use_data(LittleSurvey, overwrite = TRUE)

MathNoise <- mathnoise
devtools::use_data(MathNoise, overwrite = TRUE)

# lower case here or not?
MIAA05 <- miaa05
names(MIAA05)[1:2] <- tolower(names(MIAA05))[1:2]
devtools::use_data(MIAA05, overwrite = TRUE)

MLB2004 <- mlb2004
names(MLB2004)[1:2] <- tolower(names(MLB2004))[1:2]
devtools::use_data(MLB2004, overwrite = TRUE)

NCAA2008 <- ncaa2008
names(NCAA2008) <- tolower(names(NCAA2008))
devtools::use_data(NCAA2008, overwrite = TRUE)

NCAA2009 <- ncaa2009
# names(NCAA2009) <- tolower(names(NCAA2009))
devtools::use_data(NCAA2009, overwrite = TRUE)

NCAA2010 <- ncaa2010
# names(NCAA2010) <- tolower(names(NCAA2010))
devtools::use_data(NCAA2010, overwrite = TRUE)

NFL2007 <- nfl2007
names(NFL2007) <- camelCase(names(NFL2007))
devtools::use_data(NFL2007, overwrite = TRUE)

Noise <- noise
devtools::use_data(Noise, overwrite = TRUE)

Pallets <- palettes
names(Pallets) <- c("pallets", "employee", "day")
devtools::use_data(Pallets, overwrite = TRUE)

PaperPlanes <- paperplanes
devtools::use_data(PaperPlanes, overwrite = TRUE)

Pendulum <- pendulum
devtools::use_data(Pendulum, overwrite = TRUE)

PetStress <- petstress
names(PetStress) <- tolower(names(PetStress))
devtools::use_data(PetStress, overwrite = TRUE)

Pheno <- pheno
devtools::use_data(Pheno, overwrite = TRUE)

Pigs <- pigs
names(Pigs) <- c("roll", "blackScore", "pinkScore", "score", "height", "start", "black", "pink")
devtools::use_data(Pigs, overwrite = TRUE)
# no need for pigConfig

Pitching2005 <- pitching2005
devtools::use_data(Pitching2005, overwrite = TRUE)

Poison <- poison
names(Poison) <- tolower(names(Poison))
devtools::use_data(Poison, overwrite = TRUE)

Poison <- poison
names(Poison) <- tolower(names(Poison))
devtools::use_data(Poison, overwrite = TRUE)

Punting <- punting
devtools::use_data(Punting, overwrite = TRUE)

RatPoison <- ratpoison
devtools::use_data(RatPoison, overwrite = TRUE)

devtools::use_data(rgolfballs, overwrite = TRUE)

RubberBand <- rubberband
names(RubberBand) <- tolower(names(Rubberband))
devtools::use_data(RubberBand, overwrite = TRUE)

Scent <- scent
devtools::use_data(Scent, overwrite = TRUE)

Soap <- soap
names(Soap) <- tolower(names(Soap))
devtools::use_data(Soap, overwrite = TRUE)

Spheres <- spheres
devtools::use_data(Spheres, overwrite = TRUE)

Step <- step
devtools::use_data(Step, overwrite = TRUE)

Stereogram <- stereogram
names(Stereogram) <- tolower(names(Stereogram))
devtools::use_data(Stereogram, overwrite = TRUE)

Students <- students
names(Students) <- c("ACT", "SAT", "grad", "gradGPA", "hsGPA", "cohort")
devtools::use_data(Students, overwrite = TRUE)

Taste1 <- taste1
devtools::use_data(Taste1, overwrite = TRUE)

TasteTest <- tastetest
devtools::use_data(TasteTest, overwrite = TRUE)

TireWear <- tirewear
names(TireWear) <- tolower(names(TireWear))
devtools::use_data(TireWear, overwrite = TRUE)

Traffic <- traffic
devtools::use_data(Traffic, overwrite = TRUE)

Trebuchet <- trebuchet
devtools::use_data(Trebuchet, overwrite = TRUE)

Trebuchet1 <- trebuchet %>% filter(counterWt == 1)
devtools::use_data(Trebuchet1, overwrite = TRUE)

Trebuchet2 <- trebuchet %>% filter(counterWt == 2)
devtools::use_data(Trebuchet2, overwrite = TRUE)

# next two are in mosaicData 
# Utilities <- utilities
# devtools::use_data(Utilities, overwrite = TRUE)

# reCode("Utilities2")
# devtools::use_data(Utilities2, overwrite = TRUE)

WorkingWomen <- workingWomen
devtools::use_data(WorkingWomen, overwrite = TRUE)

