
# substitution to correct auto-generated file markup s:list("\([^)]*\)"):\1:g   

#' Michael Jordan personal scoring
#' 
#' The number of points scored by Michael Jordan in each game of the 1986-87
#' regular season.
#' 
#' 
#' @name Jordan8687
#' @rdname Jordan8687
#' @docType data
#' @format A data frame with 82 observations on the following 2 variables.
#' \describe{ 
#'   \item{game }{a numeric vector} 
#'   \item{points }{a numeric vector} }
#' @keywords datasets
#' @examples
#'
#' data(Jordan8687)
#' gf_qq(~ points, data = Jordan8687)
#' 
NULL





#' ACT scores and GPA
#' 
#' ACT scores and college GPA for a small sample of college students.
#' 
#' 
#' @name ACTgpa
#' @rdname ACTgpa
#' @docType data
#' @format A data frame with 26 observations on the following 2 variables.
#' \describe{ 
#' \item{ACT }{ACT score} 
#' \item{GPA }{GPA} }
#' @keywords datasets
#' @examples
#'
#' gf_point(GPA ~ ACT, data = ACTgpa)
#' 
NULL





#' Airline On-Time Arrival Data
#' 
#' Flights categorized by destination city, airline, and whether or not the
#' flight was on time.
#' 
#' 
#' @name AirlineArrival
#' @rdname AirlineArrival
#' @docType data
#' @format A data frame with 11000 observations on the following 3 variables.
#' \describe{ 
#'   \item{airport}{ a factor with levels \code{LosAngeles},
#'     \code{Phoenix}, \code{SanDiego}, \code{SanFrancisco}, \code{Seattle}}
#'   \item{result}{ a factor with levels \code{Delayed}, \code{OnTime}}
#'   \item{airline}{ a factor with levels \code{Alaska}, \code{AmericaWest}} }
#' @references These and similar data appear in many text books under the topic
#' of Simpson's paradox.
#' @source Barnett, Arnold. 1994. ``How numbers can trick you.''
#' \emph{Technology Review}, vol. 97, no. 7, pp. 38--45.
#' @keywords datasets
#' @examples
#' 
#' tally(
#'   airline ~ result, data = AirlineArrival, 
#'   format = "perc", margins = TRUE)
#' tally(
#'   result ~ airline + airport, 
#'   data = AirlineArrival, format = "perc", margins = TRUE)
#' AirlineArrival2 <- 
#'   AirlineArrival %>% 
#'   group_by(airport, airline, result) %>% 
#'   summarise(count = n()) %>%
#'   group_by(airport, airline) %>%
#'   mutate(total = sum(count), percent = count/total * 100) %>% 
#'   filter(result == "Delayed") 
#' AirlineArrival3 <- 
#'   AirlineArrival %>% 
#'   group_by(airline, result) %>% 
#'   summarise(count = n()) %>%
#'   group_by(airline) %>%
#'   mutate(total = sum(count), percent = count/total * 100) %>% 
#'   filter(result == "Delayed") 
#'   gf_line(percent ~ airport, color = ~ airline, group = ~ airline, 
#'           data = AirlineArrival2) %>%
#'     gf_point(percent ~ airport, color = ~ airline, size = ~total, 
#'              data = AirlineArrival2) %>%
#'     gf_hline(yintercept = ~ percent, color = ~airline, 
#'              data = AirlineArrival3, linetype = "dashed") %>%
#'     gf_labs(y = "percent delayed") 
 
NULL





#' Air pollution measurements
#' 
#' Air pollution measurements at three locations.
#' 
#' 
#' @name AirPollution
#' @rdname AirPollution
#' @docType data
#' @format A data frame with 6 observations on the following 2 variables.
#' \describe{ 
#'   \item{pollution}{ a numeric vector}
#'   \item{location}{ a factor with levels \code{Hill Suburb}, 
#'         \code{Plains Suburb}, \code{Urban City}} }
#' @source David J. Saville and Graham R. Wood, \emph{Statistical methods: A
#' geometric primer}, Springer, 1996.
#' @keywords datasets
#' @examples
#' 
#' data(AirPollution)
#' summary(lm(pollution ~ location, data = AirPollution))
#' 
NULL





#' Ball dropping data
#' 
#' Undergraduate students in a physics lab recorded the height from which a
#' ball was dropped and the time it took to reach the floor.
#' 
#' 
#' @name BallDrop
#' @rdname BallDrop
#' @docType data
#' @format A data frame with 30 observations on the following 2 variables.
#' \describe{ 
#' \item{height}{ height in meters} 
#' \item{time}{ time in seconds} }
#' @source Steve Plath, Calvin College Physics Department
#' @keywords datasets
#' @examples
#' 
#' gf_point(time ~ height, data = BallDrop)
#' 
NULL





#' Major League Batting 2000-2005
#' 
#' Major League batting data for the seasons from 2000-2005.
#' 
#' 
#' @name Batting
#' @rdname Batting
#' @docType data
#' @format A data frame with 8062 observations on the following 22 variables.
#' \describe{ 
#' \item{player}{ unique identifier for each player}
#' \item{year}{ year} 
#' \item{stint}{ for players who were traded
#' mid-season, indicates which portion of the season the data cover}
#' \item{team}{ three-letter code for team} 
#' \item{league}{ a
#' factor with levels \code{AA} \code{AL} \code{NL}} 
#' \item{G}{ games}
#' \item{AB}{ at bats} 
#' \item{R}{ runs} 
#' \item{H}{ hits}
#' \item{H2B}{ doubles} 
#' \item{H3B}{ triples}
#' \item{HR}{ home runs} 
#' \item{RBI}{ runs batted in}
#' \item{SB}{ stolen bases} 
#' \item{CS}{ caught stealing}
#' \item{BB}{ bases on balls (walks)} 
#' \item{SO}{ strike outs}
#' \item{IBB}{ intentional base on balls} 
#' \item{HBP}{ hit by
#' pitch} \item{SH}{ a numeric vector} 
#' \item{SF}{ sacrifice fly}
#' \item{GIDP}{ grounded into double play} }
#' @keywords datasets
#' @examples
#' 
#' data(Batting)
#' gf_histogram( ~ HR, data = Batting)
#' 
NULL


#' Buckthorn
#' 
#' Data from an experiment to determine the efficacy of various methods of
#' eradicating buckthorn, an invasive woody shrub.  Buckthorn plants were
#' chopped down and the stumps treated with various concentrations of
#' glyphosate.  The next season, researchers returned to see whether the plant
#' had regrown.
#' 
#' 
#' @name Buckthorn
#' @rdname Buckthorn
#' @docType data
#' @format A data frame with 165 observations on the following 3 variables.
#' \describe{ 
#' \item{shoots}{ number of new shoots coming from stump}
#' \item{conc}{ concentration of glyphosate applied}
#' \item{dead}{ weather the stump was considered dead} }
#' @source David Dornbos, Calvin College
#' @keywords datasets
#' @examples
#' 
#' data(Buckthorn)
#' 
NULL





#' Bugs
#' 
#' This data frame contains data from an experiment to see if insects are more
#' attracted to some colors than to others. The researchers prepared colored
#' cards with a sticky substance so that insects that landed on them could not
#' escape. The cards were placed in a field of oats in July. Later the
#' researchers returned, collected the cards, and counted the number of cereal
#' leaf beetles trapped on each card.
#' 
#' 
#' @name Bugs
#' @rdname Bugs
#' @docType data
#' @format A data frame with 24 observations on the following 2 variables.
#' \describe{ 
#' \item{color}{ color of card; one of \code{B}(lue)
#' \code{G}(reen) \code{W}(hite) \code{Y}(ellow)} 
#' \item{trapped}{ number
#' of insects trapped on the card} }
#' @source M. C. Wilson and R. E. Shade, Relative attractiveness of various
#' luminescent colors to the cereal leaf beetle and the meadow spittlebug,
#' \emph{Journal of Economic Entomology} 60 (1967), 578--580.
#'
#' @keywords datasets
#'
#' @examples
#' 
#' data(Bugs)
#' favstats(trapped ~ color, data = Bugs)
#' 
NULL





#' Concrete Compressive Strength Data
#' 
#' These data were collected by I-Cheng Yeh to determine how the compressive
#' strength of concrete is related to its ingredients (cement, blast furnace
#' slag, fly ash, water, superplasticizer, coarse aggregate, and fine
#' aggregate) and age.
#' 
#' @name Concrete
#' @rdname Concrete
#' @aliases Concrete
#' @docType data
#' 
#' @format \code{Concrete} is a data frame with the following 
#' variables.
#' \describe{ 
#' \item{limestone}{ percentage of limestone}
#' \item{water}{ water-cement ratio}
#' \item{strength}{ compressive strength (MPa) after 28 days}
#' }
#' 
#' @references Appeared in Devore's "Probability and Statsistics for Engineers and 
#' the Sciences (6th ed).  The variables have been renamed.
#' 
NULL
 
#' #' Concrete Compressive Strength Data
#' 
#' These data were collected by I-Cheng Yeh to determine how the compressive
#' strength of concrete is related to its ingredients (cement, blast furnace
#' slag, fly ash, water, superplasticizer, coarse aggregate, and fine
#' aggregate) and age.
#' 
#' @name ConcreteAll
#' @rdname ConcreteAll
#' @aliases ConcreteAll 
#' @docType data
#' @format \code{concreteAll} is a data frame with the following 9 variables.
#' \describe{ 
#' \item{cement}{ amount of cement (kg/m^3)}
#' \item{slag}{ amount of blast furnace slag (kg/m^3)}
#' \item{ash}{ amount of fly ash(kg/m^3)} 
#' \item{water}{ amount of water (kg/m^3)} 
#' \item{superP}{ amount of superplasticizer (kg/m^3)}
#' \item{coarseAg}{ amount of coarse aggregate (kg/m^3)}
#' \item{fineAg}{ amount of fine aggregate (kg/m^3)}
#' \item{age}{ age of concrete in days }
#' \item{strength}{ compressive strength measured in MPa} }
#' \code{Concrete} is a subset of \code{ConcreteAll}.
#' @references I-Cheng Yeh (1998), "Modeling of strength of high performance
#' concrete using artificial neural networks," \cite{Cement and Concrete
#' Research}, Vol. 28, No. 12, pp. 1797-1808.
#' @source Data were obtained from the Machine Learning Repository
#' (\url{https://archive.ics.uci.edu/ml/}) where they were deposited by I-Cheng
#' Yeh (\email{icyeh@chu.edu.tw}) who retains the copyright for these data.
#' @keywords datasets
#' @examples
#' 
#' data(Concrete)
#' 
NULL


#' Cooling Water
#' 
#' Temperature of a mug of water as it cools.
#'  
#' @name CoolingWater
#' @aliases CoolingWater1 CoolingWater2 CoolingWater3 CoolingWater4
#' @usage data(CoolingWater1)
#' @usage data(CoolingWater2)
#' @usage data(CoolingWater3)
#' @usage data(CoolingWater4)
#' @docType data
#' @format 
#'   A data frame with the following variables.
#'   \describe{
#'       \item{\code{time}}{ time in seconds}
#'       \item{\code{temp}}{ temperature in Celsius (\code{CoolingWater1}, \code{CoolingWater2}) 
#'       or Fahrenheit (\code{CoolingWater3}, \code{CoolingWater4})
#'       }
#'   }
#' @source
#' These data were collected by Stan Wagon and his students at Macelester
#' College to explore Newton's Law of Cooling and the ways that the law 
#' fails to capture all of the physics involved in cooling water.
#' \code{CoolingWater1} and \code{CoolingWater2} appeared in a plot in Wagon (2013) 
#' and were (approximatley) extracted from the plot.
#' \code{CoolingWater3} and \code{CoolingWater4} appeared in a plot in Wagon (2005).
#' The data in 
#' \code{CoolingWater2} and \code{CoolingWater4} were collected with a film of oil on
#' the surface of the water to minimize evaporation.
#' 
#' @references
#' \itemize{
#' \item
#' R. Portmann and S. Wagon. "How quickly does hot water cool?" 
#' \emph{Mathematica in Education and Research}, 
#' 10(3):1-9, July 2005.
#' \item
#' R. Israel, P. Saltzman, and S. Wagon. 
#' "Cooling coffee without solving differential equations". 
#' \emph{Mathematics Magazine},  86(3):204-210, 2013.
#' }
#' @examples 
#' data(CoolingWater1)
#' data(CoolingWater2)
#' data(CoolingWater3)
#' data(CoolingWater4)
#' if (require(ggformula)) {
#'   gf_line(
#'     temp ~ time, color = ~ condition, 
#'     data = rbind(CoolingWater1, CoolingWater2))
#' }
#' if (require(ggformula)) {
#'   gf_line(
#'     temp ~ time, color = ~ condition, 
#'     data = rbind(CoolingWater3, CoolingWater4))
#' }
#' 
#' @keywords datasets
NULL




#' Corn Yield
#' 
#' William Gosset analyzed data from an experiment comparing the yield of
#' regular and kiln-dried corn.
#' 
#' Gosset (Student) reported on the results of seeding plots with two different
#' kinds of seed. Each type of seed (regular and kiln-dried) was planted in
#' adjacent plots, accounting for 11 pairs of "split" plots.
#' 
#' @name Corn
#' @rdname Corn
#' @docType data
#' @format A data frame with 11 observations on the following 2 variables.
#' \describe{ 
#' \item{reg}{ yield of regular corn (lbs/acre)}
#' \item{kiln}{ yield of kiln-dried corn (lbs/acre)} }
#' @references W.S. Gosset, "The Probable Error of a Mean," Biometrika, 6
#' (1908), pp 1-25.
#' @source These data are also available at DASL, the data and story library
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' Corn2 <- stack(Corn)
#' names(Corn2) <- c('yield','treatment')
#' lm(yield ~ treatment, data = Corn2)
#' t.test(yield ~ treatment, data = Corn2)
#' t.test(Corn$reg, Corn$kiln)
#' 
NULL





#' Cuckoo eggs in other birds' nests
#' 
#' Cuckoos are knows to lay their eggs in the nests of other (host) birds.  The
#' eggs are then adopted and hatched by the host birds. These data were
#' originally collected by O. M. Latter in 1902 to see how the size of a cuckoo
#' egg is related to the species of the host bird.
#' 
#' 
#' @name Cuckoo
#' @rdname Cuckoo
#' @docType data
#' @format A data frame with 120 observations on the following 2 variables.
#' \describe{ 
#' \item{length}{ length of egg (mm)}
#' \item{species}{ a factor with levels \code{hedge sparrow}
#' \code{meadow pipet} \code{pied wagtail} \code{robin} \code{tree pipet}
#' \code{wren}} }
#' @references These data are also available from DASL, the data and story
#' library 
#' (\url{https://dasl.datadescription.com/}).
#' @source L.H.C. Tippett, \emph{The Methods of Statistics}, 4th Edition, John
#' Wiley and Sons, Inc., 1952, p. 176.
#' @keywords datasets
#' @examples
#' 
#' data(Cuckoo)
#' gf_boxplot(length ~ species, data = Cuckoo)
#' 
NULL





#' Death Penalty and Race
#' 
#' A famous example of Simpson's paradox.
#' 
#' 
#' @name DeathPenalty
#' @rdname DeathPenalty
#' @aliases DeathPenalty 
#' @docType data
#' @format A data frame with 326 observations.  
# The factors are coded more
# succinctly in \code{deathPen}, but otherwise the data are the same.
#' \describe{ 
#' \item{id}{ a subject id}
#' \item{victim}{ a factor with levels \code{Bl} \code{Wh})} 
#' \item{defendant}{ a factor with levels \code{Bl}, \code{Wh} } 
#' \item{death}{ a factor with levels \code{Yes}, \code{No}} 
#' \item{penalty}{ a factor with levels \code{death} \code{other}} 
#' }
#' @source Radelet, M. (1981). Racial characteristics and imposition of the
#' death penalty. \emph{American Sociological Review}, 46:918--927.
#' @keywords datasets
#' @examples
#' 
#' tally(penalty ~ defendant, data = DeathPenalty)
#' tally(penalty ~ defendant + victim, data = DeathPenalty)
#' 
NULL





#' Drag force experiment
#' 
#' The data come from an experiment to determine how terminal velocity depends
#' on the mass of the falling object.  A helium balloon was rigged with a small
#' basket and just the ballast to make it neutrally buoyant.  Mass was then
#' added and the terminal velocity is calculated by measuring the time it took
#' to fall between two sensors once terminal velocity has been reached.  Larger
#' masses were drop from higher heights and used sensors more widely spaced.
#' 
#' 
#' @name Drag
#' @rdname Drag
#' @docType data
#' @format A data frame with 42 observations on the following 5 variables.
#' \describe{ 
#' \item{time}{ time (in seconds) to travel between two sensors} 
#' \item{mass}{ net mass (in kg) of falling object}
#' \item{height}{ distance (in meters) between two sensors}
#' \item{velocity}{ average velocity (in m/s) computed from \code{time}
#' and \code{height}} 
#' \item{force.drag}{ calculated drag force (in N, 
#' \code{force.drag = mass * 9.8}) using the fact that at terminal velocity,
#' the drag force is equal to the force of gravity} }
#' @source Calvin College physics students under the supervision of Professor
#' Steve Plath.
#' @keywords datasets
#' @examples
#' 
#' data(Drag)
#' with(Drag, force.drag / mass)
#' gf_point(velocity ~ mass, data = Drag)
#' 
NULL




#' Endurance and vitamin C
#' 
#' The effect of a single 600 mg dose of ascorbic acid versus a sugar placebo
#' on the muscular endurance (as measured by repetitive grip strength trials)
#' of fifteen male volunteers (19-23 years old).
#' 
#' Three initial maximal contractions were performed for each subject, with the
#' greatest value indicating maximal grip strength. Muscular endurance was
#' measured by having the subjects squeeze the dynamometer, hold the
#' contraction for three seconds, and repeat continuously until a value of 50%
#' maximum grip strength was achieved for three consecutive contractions.
#' Endurance was defined as the number of repetitions required to go from
#' maximum grip strength to the initial 50% value. Subjects were given frequent
#' positive verbal encouragement in an effort to have them complete as many
#' repetitions as possible.
#' 
#' The study was conducted in a double-blind manner with crossover.
#' 
#' @name Endurance
#' @rdname Endurance
#' @docType data
#' @format A data frame with 15 observations on the following 5 variables.
#' \describe{ 
#' \item{vitamin}{ number of repetitions until reaching 50%
#' maximal grip after taking viatimin} 
#' \item{first}{ which treatment was
#' done first, a factor with levels \code{Placebo} \code{Vitamin}}
#' \item{placebo}{ number of repetitions until reaching 50% maximal grip
#' strength after taking placebo} }
#' @references Keith, R. E., and Merrill, E. (1983).  The effects of vitamin C
#' on maximum grip strength and muscular endurance.  \emph{Journal of Sports
#' Medicine and Physical Fitness}, 23, 253-256.
#' @source These data are available from OzDASL, the Australasian data and
#' story library (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(Endurance)
#' t.test(Endurance$vitamin, Endurance$placebo, paired = TRUE)
#' t.test(log(Endurance$vitamin), log(Endurance$placebo), paired = TRUE)
#' t.test(1/Endurance$vitamin, 1/Endurance$placebo, paired = TRUE)
#' gf_qq( ~ vitamin - placebo, data = Endurance)
#' gf_qq( ~ log(vitamin) - log(placebo), data = Endurance)
#' gf_qq( ~ 1/vitamin - 1/placebo, data = Endurance)
#' 
NULL





#' Family smoking
#' 
#' A cross-tabulation of whether a student smokes and how many of his or her
#' parents smoke from a study conducted in the 1960's.
#' 
#' 
#' @name FamilySmoking
#' @rdname FamilySmoking
#' @docType data
#' @format A data frame with 5375 observations on the following 2 variables.
#' \describe{ 
#' \item{student}{ a factor with levels \code{DoesNotSmoke} \code{Smokes}} 
#' \item{parents}{ a factor with levels
#' \code{NeitherSmokes} \code{OneSmokes}} \code{BothSmoke} }
#' @references The data also appear in
#' 
#' Brigitte Baldi and David S. Moore, \emph{The Practice of Statistics in the
#' Life Sciences}, Freeman, 2009.
#' @source S. V. Zagona (ed.), \emph{Studies and issues in smoking behavior},
#' University of Arizona Press, 1967.
#' @keywords datasets
#' @examples
#' 
#' data(FamilySmoking)
#' xchisq.test( tally(parents ~ student, data = FamilySmoking) )
#' 
NULL










#' NCAA football fumbles
#' 
#' This data frame gives the number of fumbles by each NCAA FBS team for the
#' first three weeks in November, 2010.
#' 
#' The fumble counts listed here are total fumbles, not fumbles lost.  Some of
#' these fumbles were recovered by the team that fumbled.
#' 
#' @name Fumbles
#' @rdname Fumbles
#' @docType data
#' @format A data frame with 120 observations on the following 7 variables.
#' \describe{ 
#' \item{team}{ NCAA football team} 
#' \item{rank}{ rank based on fumbles per game through games on November 26, 2010}
#' \item{W}{ number of wins through games on November 26, 2010}
#' \item{L}{ number of losses through games on November 26, 2010}
#' \item{week1}{ number of fumbles on November 6, 2010}
#' \item{week2}{ number of fumbles on November 13, 2010}
#' \item{week3}{ number of fumbles on November 20, 2010} }
#' @source
#' \url{https://www.teamrankings.com/college-football/stat/fumbles-per-game}
#' @keywords datasets
#' @examples
#' 
#' data(Fumbles)
#' m <- max(Fumbles$week1)
#' table(factor(Fumbles$week1,levels = 0:m))
#' favstats( ~ week1, data = Fumbles)
#' # compare with Poisson distribution
#' cbind(
#' 		  fumbles = 0:m,
#' 		  observedCount = table(factor(Fumbles$week1,levels = 0:m)),
#' 		  modelCount= 120* dpois(0:m,mean(Fumbles$week1)),
#' 		  observedPct = table(factor(Fumbles$week1,levels = 0:m))/120,
#' 		  modelPct= dpois(0:m,mean(Fumbles$week1))
#' 	) %>% signif(3)
#' showFumbles <- function(x, lambda = mean(x),...) {
#'   result <-
#'     gf_dhistogram( ~ week1, data = Fumbles, binwidth = 1, alpha = 0.3) %>%
#'     gf_dist("pois", lambda = mean( ~ week1, data = Fumbles) )
#'   print(result)
#'   return(result)
#' }
#' showFumbles(Fumbles$week1)
#' showFumbles(Fumbles$week2)
#' showFumbles(Fumbles$week3)
#' 
NULL





#' FUSION type 2 diabetes study
#' 
#' Phenotype and genotype data from the Finland United States Investigation of
#' NIDDM (type 2) Diabetes (FUSION) study.
#' 
#' 
#' @name Pheno
#' @rdname FUSION 
#' @aliases Pheno FUSION1 FUSION2
#' @docType data
#' @format Data frames with the following variables.  
#' \describe{
#' \item{id}{ subject ID number for matching between data sets}
#' \item{t2d}{ a factor with levels \code{case} \code{control}}
#' \item{bmi}{ body mass index} 
#' \item{sex}{ a factor with levels
#' \code{F} \code{M}} 
#' \item{age}{ age of subject at time phenotypes were
#' colelcted} 
#' \item{smoker}{ a factor with levels \code{former}
#' \code{never} \code{occasional} \code{regular}} 
#' \item{chol}{ total cholesterol} 
#' \item{waist}{ waist circumference (cm)}
#' \item{weight}{ weight (kg) } 
#' \item{height}{ height (cm) }
#' \item{whr}{ waist hip ratio } 
#' \item{sbp}{ systolic blood pressure} 
#' \item{dbp}{ diastolic blood pressure}
#' \item{marker}{ RS name of SNP} 
#' \item{markerID}{ numeric ID for SNP} 
#' \item{allele1}{ first allele coded as 1 = A, 2 = C, 3 = G, 4 = T}
#' \item{allele2}{ second allele coded as 1 = A, 2 = C, 3 = G, 4 = T}
#' \item{genotype}{ both alleles coded as a factor}
#' \item{Adose}{ number of A alleles} 
#' \item{Cdose}{ number of C alleles} 
#' \item{Gdose}{ number of G alleles}
#' \item{Tdose}{ number of T alleles} }
#' @source Similar to the data presented in
#' 
#' Laura J. Scott, Karen L. Mohlke, Lori L. Bonnycastle, Cristen J. Willer, Yun
#' Li, William L. Duren, Michael R. Erdos, Heather M. Stringham, Pe- ter S.
#' Chines, Anne U. Jackson, Ludmila Prokunina-Olsson, Chia-Jen J. Ding, Amy J.
#' Swift, Narisu Narisu, Tianle Hu, Randall Pruim, Rui Xiao, Xiao- Yi Y. Li,
#' Karen N. Conneely, Nancy L. Riebow, Andrew G. Sprau, Maurine Tong, Peggy P.
#' White, Kurt N. Hetrick, Michael W. Barnhart, Craig W. Bark, Janet L.
#' Goldstein, Lee Watkins, Fang Xiang, Jouko Saramies, Thomas A.  Buchanan,
#' Richard M. Watanabe, Timo T. Valle, Leena Kinnunen, Goncalo R.  Abecasis,
#' Elizabeth W. Pugh, Kimberly F. Doheny, Richard N. Bergman, Jaakko
#' Tuomilehto, Francis S. Collins, and Michael Boehnke, A genome-wide
#' association study of type 2 diabetes in Finns detects multiple
#' susceptibility vari- ants, \emph{Science} (2007).
#' @keywords datasets
#' @examples
#' 
#' data(Pheno); data(FUSION1); data(FUSION2)
#' FUSION1m <- merge(FUSION1, Pheno, by = "id", all.x = FALSE, all.y = FALSE) 
#' xtabs( ~ t2d + genotype, data = FUSION1m) 
#' xtabs( ~ t2d + Gdose, data = FUSION1m) 
#' chisq.test( xtabs( ~ t2d + genotype, data = FUSION1m ) )
#' f1.glm <- glm( factor(t2d) ~ Gdose, data = FUSION1m, family = binomial) 
#' summary(f1.glm)
#' 
NULL





#' Golf ball numbers
#' 
#' Allan Rossman used to live on a golf course in a spot where dozens of balls
#' would come into his yard every week.  He collected the balls and eventually
#' tallied up the numbers on the first 5000 golf balls he collected. Of these
#' 486 bore the number 1, 2, 3, or 4.  The remaining 14 golf balls were omitted
#' from the data.
#' 
#' 
#' @name golfballs
#' @rdname golfballs
#' @docType data
#' @format The format is: num [1:4] 137 138 107 104
#' @source Data collected by Allan Rossman in Carlisle, PA.
#' @keywords datasets
#' @examples
#' 
#' data(golfballs)
#' golfballs/sum(golfballs)
#' chisq.test(golfballs, p = rep(.25,4))
#' 
NULL

# Goose permit data found at 
# http://www.math.umt.edu/patterson/ProfileLikelihoodCI.pdf

#' Goose permits
#' 
#' In a 1979 study by Bishop and Heberlein, 237 hunters were each offered one of 
#' 11 cash amounts (bids) ranging from $1 to $200 in return for their hunting permits.
#' The data records how many hunters offered each bid kept or sold their permit.
#' 
#' @name GooosePermits
#' @rdname GoosePermits
#' @docType data
#' @format A data frame with 11 rows and 5 columns.  
#' Each row corresponds to a bid (in US dollars)
#' offered for a goose permit.  The colums \code{keep} and \code{sell} indicate 
#' how many hunters offered that bid kept or sold their permit, respectively.
#' \code{n} is the sum of \code{keep} and \code{sell} and \code{prop_sell}
#' is the proportion that sold.
#' 
#' @references
#' Bishop and Heberlein (Amer. J. Agr. Econ. 61, 1979).
#'  
#' @examples
#' goose.mod <- glm( cbind(sell, keep) ~ log(bid), data = GoosePermits, family = binomial())
#' gf_point(0 ~ bid, size = ~keep, color = "gray50", data = GoosePermits) %>%
#'   gf_point(1 ~ bid, size = ~ sell, color = "navy") %>%
#'   gf_function(fun = makeFun(goose.mod)) %>%
#'   gf_refine(guides(size = "none"))
#'   
#' ggplot(data = GoosePermits) +
#'   geom_point( aes(x = bid, y = 0, size = keep), colour = "gray50") +
#'   geom_point( aes(x = bid, y = 1, size = sell), colour = "navy") +
#'   stat_function(fun = makeFun(goose.mod)) +
#'   guides( size = "none")
#'   
#' gf_point( (sell / (sell + keep)) ~ bid, data = GoosePermits,
#'     size = ~ sell + keep, color = "navy") %>%
#'   gf_function(fun = makeFun(goose.mod))  %>%
#'   gf_text(label = ~ as.character(sell + keep), colour = "white", size = 3) %>%
#'   gf_refine(scale_size_area()) %>% 
#'   gf_labs(y = "probabity of selling")
#'   
#' ggplot(data = GoosePermits) +
#'   stat_function(fun = makeFun(goose.mod)) +
#'   geom_point( aes(x = bid, y = sell / (sell + keep), size = sell + keep), colour = "navy") +
#'   geom_text( aes(x = bid, y = sell / (sell + keep), label = as.character(sell + keep)), 
#'     colour = "white", size = 3) +
#'   scale_size_area() + 
#'   labs(y = "probabity of selling")
#'   

NULL

#' GPA, ACT, and SAT scores
#' 
#' GPA, ACT, and SAT scores for a sample of students.
#' 
#' 
#' @name GPA
#' @rdname GPA
#' @docType data
#' @format A data frame with 271 observations on the following 4 variables.
#' \describe{ 
#' \item{act}{ ACT score}
#' \item{gpa}{ college grade point average} 
#' \item{satm}{ SAT mathematics score}
#' \item{satv}{ SAT verbal score} 
#' }
#' @keywords datasets
#' @examples
#' 
#' data(GPA)
#' splom(GPA)
#' 
NULL

#' Punting helium- and air-filled footballs
#' 
#' Two identical footballs, one air-filled and one helium-filled, were used
#' outdoors on a windless day at The Ohio State University's athletic complex.
#' Each football was kicked 39 times and the two footballs were alternated with
#' each kick. The experimenter recorded the distance traveled by each ball.
#' 
#' 
#' @name HeliumFootballs
#' @rdname HeliumFootballs
#' @docType data
#' @format A data frame with 39 observations on the following 3 variables.
#' \describe{ 
#' \item{trial}{ trial number} 
#' \item{air}{ distance traveled by air-filled football (yards)} 
#' \item{helium}{ distance traveled by helium-filled football (yards)} }
#' @references Lafferty, M. B. (1993), "OSU scientists get a kick out of sports
#' controversy", \emph{The Columbus Dispatch} (November, 21, 1993), B7.
#' @source These data are available from DASL, the data and story library
# (\url{ftp://sunsite.univie.ac.at/mirrors/lib.stat.cmu.edu/DASL/.index.html}).
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(HeliumFootballs)
#' gf_point(helium ~ air, data = HeliumFootballs)
#' gf_dhistogram( 
#'   ~ (helium - air), data = HeliumFootballs, 
#'   fill = ~ (helium > air),  bins = 15, boundary = 0 
#' )
#' 
NULL





#' Cooling muscles with ice
#' 
#' This data set contains the results of an experiment comparing the efficacy
#' of different forms of dry ice application in reducing the temperature of the
#' calf muscle.
#' 
#' The 12 subjects in this study came three times, at least four days apart,
#' and received one of three ice treatments (cubed ice, crushed ice, or ice
#' mixed with water). In each case, the ice was prepared in a plastic bag and
#' applied dry to the subjects calf muscle.  The temperature measurements were
#' taken on the skin surface and inside the calf muscle (via a 4 cm long probe)
#' every 30 seconds for 20 minutes prior to icing, for 20 minutes during icing,
#' and for 2 hours after the ice had been removed.  The temperature
#' measurements are stored in variables that begin with \code{b} (baseline),
#' \code{t} (treatment), or \code{r} (recovery) followed by a numerical code
#' for the elapsed time formed by concatenating the number of minutes and
#' seconds. For example, \code{R1230} contains the temperatures 12 minutes and
#' 30 seconds after the ice had been removed.
#' 
#' Variables include 
#' \describe{ 
#' \item{Subject}{ identification number}
#' \item{sex}{ a factor with levels \code{female} \code{male}}
#' \item{weight}{ weight of subject (kg)} 
#' \item{Height}{ height of subject (cm)} 
#' \item{Skinfold}{ skinfold thickness}
#' \item{calf}{ calf diameter (cm)} 
#' \item{Age}{ age of subject}
#' \item{location}{ a factor with levels \code{intramuscular}
#' \code{surface}} 
#' \item{Treatment}{ a factor with levels \code{crushed}
#' \code{cubed} \code{wet}} 
#' \item{B0}{ baseline temperature at time 0}
#' \item{b30}{ baseline temperature 30 seconds after start}
#' \item{b100}{ baseline temperature 1 minute after start}
#' \item{b1930}{ baseline temperature 19 minutes 30 seconds start}
#' \item{t0}{ treatment temperature at beginning of treatment}
#' \item{t30}{ treatment temperature 30 seconds after start of
#' treatment} 
#' \item{t100}{ treatment temperature 1 minute after start of
#' treatment} 
#' \item{t1930}{ treatment temperature 19 minutes 30 seconds
#' after start of treatment} 
#' \item{R0}{ recovery temperature at start of
#' recovery} 
#' \item{r30}{ recovery temperature 30 seconds after start of
#' recovery} 
#' \item{r100}{ recovery temperature 1 minute after start of
#' recovery} 
#' \item{r12000}{ recovery temperature 120 minutes after start
#' of recovery} }
#' 
#' @name Ice
#' @rdname Ice
#' @docType data
#' @source Dykstra, J. H., Hill, H. M., Miller, M. G., Michael T. J., Cheatham,
#' C. C., and Baker, R.J., Comparisons of cubed ice, crushed ice, and wetted
#' ice on intramuscular and surface temperature changes, \emph{Journal of
#' Athletic Training} 44 (2009), no. 2, 136--141.
#' @keywords datasets
#' @examples
#' 
#' data(Ice)
#' gf_point(weight ~ skinfold, color = ~ sex, data = Ice)
#' if (require(readr) && require(tidyr)) {
#'   Ice2 <- Ice %>% 
#'   gather("key", "temp", b0:r12000) %>% 
#'   separate(key, c("phase", "time"), sep = 1) %>% 
#'   mutate(time = parse_number(time), subject = as.character(subject))  
#'   
#'   gf_line( temp ~ time, data = Ice2 %>% filter(phase == "t"), 
#'            color = ~ sex,  group = ~subject, alpha = 0.6) %>%
#'     gf_facet_grid( treatment ~ location)
#' }
#' 
NULL





#' Inflation data
#' 
#' The article developed four measures of central bank independence and
#' explored their relation to inflation outcomes in developed and developing
#' countries. This datafile deals with two of these measures in 23 nations.
#' 
#' 
#' @name Inflation
#' @rdname Inflation
#' @docType data
#' @format A data frame with 23 observations on the following 5 variables.
#' \describe{ 
#' \item{country}{ country where data were collected}
#' \item{ques}{ questionnaire index of independence}
#' \item{inf}{ annual inflation rate, 1980-1989 (percent)}
#' \item{legal}{ legal index of independence}
#' \item{dev}{ developed (1) or developing (2) nation} }
#' @references A. Cukierman, S.B. Webb, and B. Negapi, "Measuring the
#' Independence of Central Banks and Its Effect on Policy Outcomes," World Bank
#' Economic Review, Vol. 6 No. 3 (Sept 1992), 353-398.
#' @source These data are available from OzDASL, the Australasian Data and
#' Story Library (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(Inflation)
#' 
NULL





#' Goals and popularity factors for school kids
#' 
#' Subjects were students in grades 4-6 from three school districts in
#' Michigan. Students were selected from urban, suburban, and rural school
#' districts with approximately 1/3 of their sample coming from each district.
#' Students indicated whether good grades, athletic ability, or popularity was
#' most important to them.  They also ranked four factors: grades, sports,
#' looks, and money, in order of their importance for popularity.  The
#' questionnaire also asked for gender, grade level, and other demographic
#' information.
#' 
#' 
#' @name Kids
#' @rdname Kids
#' @docType data
#' @format A data frame with 478 observations on the following 11 variables.
#' \describe{ 
#' \item{gender}{ a factor with levels \code{boy}
#' \code{girl}} 
#' \item{grade}{ grade in school}
#' \item{age}{ student age} 
#' \item{race}{ a factor with levels
#' \code{other} \code{White}} 
#' \item{urban.rural}{ a factor with levels
#' \code{Rural} \code{Suburban} \code{Urban}} 
#' \item{school}{ a factor
#' with levels \code{Brentwood Elementary} \code{Brentwood Middle} \code{Brown
#' Middle} \code{Elm} \code{Main} \code{Portage} \code{Ridge} \code{Sand}
#' \code{Westdale Middle}} 
#' \item{goals}{ a factor with levels
#' \code{Grades} \code{Popular} \code{Sports}} 
#' \item{grades}{ rank of
#' `make good grades' (1 = most important for popularity; 4 = least important)}
#' \item{sports}{ rank of `beging good at sports' (1 = most important for
#' popularity; 4 = least important)} 
#' \item{looks}{ rank of `beging
#' handsome or pretty' (1 = most important for popularity; 4 = least important)}
#' \item{money}{ rank of `having lots of money' (1 = most important for
#' popularity; 4 = least important)} }
#' @references Chase, M. A., and Dummer, G. M. (1992), "The Role of Sports as a
#' Social Determinant for Children," Research Quarterly for Exercise and Sport,
#' 63, 418-424.
#' @source These data are available at DASL, the data and story library
# (\url{ftp://sunsite.univie.ac.at/mirrors/lib.stat.cmu.edu/DASL/.index.html}).
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(Kids)
#' tally(goals ~ urban.rural, data = Kids)
#' chisq.test(tally(~ goals + urban.rural, data = Kids))
#' 
NULL





#' Results from a little survey
#' 
#' These data are from a little survey given to a number of students in
#' introductory statistics courses. Several of the items were prepared in
#' multiple versions and distributed randomly to the students.
#' 
#' 
#' @name LittleSurvey
#' @rdname LittleSurvey
#' @docType data
#' @format A data frame with 279 observations on the following 20 variables.
#' \describe{ 
#' \item{number}{ a number between 1 and 30}
#' \item{colorver}{ which version of the 'favorite color' question was
#' on the survey. A factor with levels \code{v1} \code{v2}}
#' \item{color}{ favorite color if among predefined choices.  A factor
#' with levels \code{} \code{black} \code{green} \code{other} \code{purple}
#' \code{red}} 
#' \item{othercolor}{ favorite color if not among choices
#' above.} 
#' \item{animalver}{ which version of the 'favorite color'
#' question was on the survey. A factor with levels \code{v1} \code{v2}}
#' \item{animal}{ favorite animal if among predefined choices.  A factor
#' with levels \code{} \code{elephant} \code{giraffe} \code{lion}
#' \code{other}.} 
#' \item{otheranimal}{ favorite animal if not among the
#' predefined choices.} 
#' \item{pulsever}{ which version of the 'pulse'
#' question was on the survey} 
#' \item{pulse}{ self-reported pulse}
#' \item{tvver}{ which of three versions of the TV question was on the
#' survey} 
#' \item{tvbox}{ a factor with levels \code{<1} \code{>4}
#' \code{>8} \code{1-2} \code{2-4} \code{4-8} \code{none} \code{other}}
#' \item{tvhours}{ a numeric vector} 
#' \item{surprisever}{ which of
#' two versions of the 'surprise' question was on the survey}
#' \item{surprise}{ a factor with levels \code{no} \code{yes}}
#' \item{playver}{ which of two versions of the 'play' question was on
#' the survey} 
#' \item{play}{ a factor with levels \code{no} \code{yes}}
#' \item{diseasever}{ which of two versions of the 'play' question was
#' on the survey} 
#' \item{disease}{ a factor with levels \code{A}
#' \code{B}} 
#' \item{homeworkver}{ which of two versions of the 'homework'
#' question was on the survey} 
#' \item{homework}{ a factor with levels
#' \code{A} \code{B}} }
#' @keywords datasets
#' 
#' @section Question Wording:
#' 
#' 1.1. Write down any number between 1 and 30 (inclusive).
#' 
#' 2.1. What is your favorite color? Choices: black red; green; purple; other
#' 
#' 2.2. What is your favorite color?
#' 
#' 3.1. What is your favorite zoo animal? Choices: giraffe; lion; elephant; other
#' 
#' 3.2. What is your favorite zoo animal?
#' 
#' 4.1. Measure and record your pulse.
#' 
#' 5.1. How much time have you spent watching TV in the last week?
#' 
#' 5.2. How much time have you spent watching TV in the last week? 
#' Choises: none; under 1; hour 1-2 hours; 2-4 hours; more than 4 hours
#' 
#' 5.3. How much time have you spent watching TV in the last week? 
#' Choises: under 1 hour; 1-2 hours; 2-4 hours; 4-8 hours;  more than 8 hours
#' 
#' 6.1. Social science researchers have conducted extensive empirical studies
#'   and concluded that the expression "absence makes the heart grow fonder" is
#'   generally true. Do you find this result surprising or not surprising?
#'   
#' 6.2. Social science researchers have conducted extensive empirical studies
#'    and concluded that the expression "out of sight out of mind" is generally
#'    true. Do you find this result surprising or not surprising?
#'    
#' 7.1. Suppose that you have decided to see a play for which the admission charge is $20 per ticket. 
#'     As you prepare to purchase the ticket, you discover that you have lost a $20 bill. 
#'     Would you still pay $20 for a ticket to see the play?
#'     
#' 7.2. Suppose that you have decided to see a play for which the admission charge is $20 per ticket. 
#'     As you prepare to enter the theater, you discover that you have lost your ticket. 
#'     Would you pay $20 to buy a new ticket to see the play?
#'     
#' 8.1. suppose that the United States is preparing for the outbreak of an unusual Asian disease that is 
#'     expected to kill 600 people. Two alternative programs to combat the disease have been proposed. 
#'     Assume that the exact scientific estimates of the consequences of the programs are as follows:
#'     If program A is adopted, 200 people will be saved.
#'     If program B is adopted, there is a 1/3 probability that 600 people will be saved 
#'     and a 2/3 probability that nobody will be saved.
#'     Which of the two programs would you favor? 
#'     
#' 8.2. Suppose that the United States is preparing for the outbreak of an unusual Asian disease that is 
#'     expected to kill 600 people. two alternative programs to combat the disease have been proposed. 
#'     Assume that the exact scientific estimates of the consequences of the programs are as follows:
#'     
#'     If program A is adopted, 400 people will die.
#'     If program B is adopted, there is a 1/3 probability that no one will die and a 2/3 probability
#'     that all 600 people will die.
#'     Which of the two programs would you favor? A or B
#' 
#' 9.1. A national survey of college students revealed that professors at this college assign 
#' "significantly more homework that the nationwide average for an institution of its type."
#' How does this finding compare with your experience?  
#'     Choises: a. That sounds about right to me; b that doesn't sound right to me.
#'     
#' 9.2. A national survey of college students revealed that professors at this college assign 
#' an amount of homework that "is fairly typical for institutions of its type."
#' How does this finding compare with your experience?
#' Choices: A that sounds about right to me; b that doesn't sound right to me.
#' 
#' @examples
#' data(LittleSurvey)
#' tally(surprise ~ surprisever, data = LittleSurvey)
#' tally(disease ~ diseasever, data = LittleSurvey)
#' 
NULL

#' Test performance and noise
#' 
#' In this experiment, hyperactive and control students were given a
#' mathematics test in either a quiet or loud testing environment.
#' 
#' 
#' @name MathNoise
#' @rdname MathNoise
#' @docType data
#' @format A data frame with 40 observations on the following 3 variables.
#' \describe{ 
#' \item{score}{ score on a mathematics test}
#' \item{noise}{ a factor with levels \code{hi} \code{lo}}
#' \item{group}{ a factor with levels \code{control} \code{hyper}} }
#' @source Sydney S. Zentall and Jandira H. Shaw, Effects of classroom noise on
#' perfor- mance and activity of second-grade hyperactive and control children,
#' \emph{Journal of Educational Psychology} 72 (1980), no. 6, 830.
#' @keywords datasets
#' @examples
#' 
#' data(MathNoise)
#' xyplot (score ~ noise, data = MathNoise, group = group, type = 'a', 
#' 		auto.key = list(columns = 2, lines = TRUE, points = FALSE))
#' 
#' gf_jitter(score ~ noise, data = MathNoise, color = ~ group, alpha = 0.4, 
#'           width = 0.1, height = 0) %>%
#'   gf_line(score ~ noise, data = MathNoise, color = ~ group, group = ~ group,
#'         stat = "summary")
NULL





#' MIAA basketball 2004-2005 season
#' 
#' Individual player statistics for the 2004-2005 Michigan Intercollegiate
#' Athletic Association basketball season.
#' 
#' 
#' @name MIAA05
#' @rdname MIAA05
#' @docType data
#' @format A data frame with 134 observations on the following 27 variables.
#' \describe{ 
#' \item{number}{ jersey number}
#' \item{player}{ player's name} 
#' \item{GP}{ games played}
#' \item{GS}{ games started} 
#' \item{Min}{ minutes played}
#' \item{AvgMin}{ average minutes played per game}
#' \item{FG}{ field goals made} 
#' \item{FGA}{ field goals attempted} 
#' \item{FGPct}{ field goal percentage}
#' \item{FG3}{ 3-point field goals made} 
#' \item{FG3A}{ 3-point field goals attempted} 
#' \item{FG3Pct}{ 3-point field goal percentage}
#' \item{FT}{ free throws made} 
#' \item{FTA}{ free throws attempted} 
#' \item{FTPct}{ free throw percentage}
#' \item{Off}{ offensive rebounds} 
#' \item{Def}{ defensive rebounds} 
#' \item{Tot}{ total rebounds } 
#' \item{RBG}{ rebounds per game} 
#' \item{PF}{ personal fouls} 
#' \item{FO}{ games fouled out} 
#' \item{A}{ assists} 
#' \item{TO}{ turn overs}
#' \item{Blk}{ blocked shots} 
#' \item{Stl}{ steals}
#' \item{Pts}{ points scored} 
#' \item{PTSG}{ points per game} }
#' @source MIAA sports archives (\url{https://miaa.org/})
#' @keywords datasets
#' @examples
#' 
#' data(MIAA05)
#' gf_histogram(~ FTPct, data = MIAA05)
#' 
NULL





#' Major League Baseball 2004 team data
#' 
#' Team batting statistics, runs allowed, and runs scored for the 2004 Major
#' League Baseball season.
#' 
#' 
#' @name MLB2004
#' @rdname MLB2004
#' @docType data
#' @format A data frame with 30 observations on the following 20 variables.
#' \describe{ 
#' \item{team}{ team city, a factor}
#' \item{league}{ League, a factor with levels \code{AL} \code{NL}}
#' \item{W}{ number of wins} 
#' \item{L}{ number of losses}
#' \item{G}{ number of games} 
#' \item{R}{ number of runs scored}
#' \item{OR}{ oppnents' runs -- number of runs allowed}
#' \item{Rdiff}{ run difference -- \code{R - OR}}
#' \item{AB}{ number of at bats} 
#' \item{H}{ number of hits}
#' \item{DBL}{ number of doubles} 
#' \item{TPL}{ number of triples}
#' \item{HR}{ number of home runs} 
#' \item{BB}{ number of walks
#' (bases on balls)} 
#' \item{SO}{ number of strike outs}
#' \item{SB}{ number of stolen bases} 
#' \item{CS}{ number of times
#' caught stealing} 
#' \item{BA}{ batting average}
#' \item{SLG}{ slugging percentage} 
#' \item{OBA}{ on base average}
#' }
#' @keywords datasets
#' @examples
#' 
#' data(MLB2004)
#' gf_point(W ~ Rdiff, data = MLB2004)
#' 
NULL





#' NCAA Division I Basketball Results
#' 
#' Results of NCAA basketball games
#' 
#' 
#' @name NCAAbb
#' @rdname NCAAbb
#' @docType data
#' @format Nine variables describing NCAA Division I basketball games.
#' \describe{ 
#' \item{date}{ date on which game was played}
#' \item{away}{ visiting team} 
#' \item{ascore}{ visiting team's score} 
#' \item{home}{ home team} 
#' \item{hscore}{ home team's score} 
#' \item{notes}{ code indicting games played at neutral sites (n or N) or 
#' in tournaments (T)} 
#' \item{location}{ where game was played}
#' \item{season}{ a character indicating which season the game belonged to}
#' \item{postseason}{ a logical indicating whether the game is a postseason game}
#' }
#' @source \url{https://kenpom.com}
#' @keywords datasets
#' @examples
#' 
#' data(NCAAbb)
#' # select one year and add some additional variables to the data frame
#' NCAA2010 <-
#'   NCAAbb %>% 
#'   filter(season == "2009-10") %>%
#'   mutate(
#'     dscore = hscore - ascore,
#'     homeTeamWon = dscore > 0,
#'     numHomeTeamWon <- -1 + 2 * as.numeric(homeTeamWon),
#'     winner = ifelse(homeTeamWon, home, away),
#'     loser  = ifelse(homeTeamWon, away, home),
#'     wscore = ifelse(homeTeamWon, hscore, ascore),
#'     lscore = ifelse(homeTeamWon, ascore, hscore)
#'   )
#' NCAA2010 %>% select(date, winner, loser, wscore, lscore, dscore, homeTeamWon) %>% head()
NULL



####### edits still needed above here  ############

#' NFL 2007 season
#' 
#' Results of National Football League games (2007 season, including playoffs)
#' 
#' 
#' @name NFL2007
#' @rdname NFL2007
#' @docType data
#' @format A data frame with 267 observations on the following 7 variables.
#' \describe{ 
#' \item{date}{ date on which game was played}
#' \item{visitor}{ visiting team} 
#' \item{visitorScore}{ score for visiting team} 
#' \item{home}{ home team} 
#' \item{homeScore}{ score for home team} 
#' \item{line}{ `betting line'}
#' \item{totalLine}{ 'over/under' line (for combined score of both teams)} }
#' @keywords datasets
#' @examples
#' 
#' data(NFL2007) 
#' NFL <- NFL2007 
#' NFL$dscore <- NFL$homeScore - NFL$visitorScore 
#' w <- which(NFL$dscore > 0) 
#' NFL$winner <- NFL$visitor; NFL$winner[w] <- NFL$home[w] 
#' NFL$loser <- NFL$home; NFL$loser[w] <- NFL$visitor[w] 
#' # did the home team win? 
#' NFL$homeTeamWon <- NFL$dscore > 0 
#' table(NFL$homeTeamWon)
#' table(NFL$dscore > NFL$line)
#' 
NULL





#' Noise 
#' 
#' In order to test the effect of room noise, subjects were given a test under
#' 5 different sets of conditions: 1) no noise, 2) intermittent low volume, 3)
#' intermittent high volume, 4) continuous low volume, and 5) continuous high
#' volume.
#' 
#' 
#' @name Noise
#' @rdname Noise
#' @docType data
#' @format A data frame with 50 observations on the following 5 variables.
#' \describe{ 
#' \item{id}{ subject identifier} 
#' \item{score}{ score
#' on the test} 
#' \item{condition}{ numeric code for condition}
#' \item{volume}{ a factor with levels \code{high} \code{low}
#' \code{none}} 
#' \item{frequency}{ a factor with levels \code{continuous}
#' \code{intermittent} \code{none}} }
#' @keywords datasets
#' @examples
#' 
#' data(Noise)
#' Noise2 <- Noise %>% filter(volume != 'none')
#' model <- lm(score ~ volume * frequency, data = Noise2) 
#' anova(model)
#' gf_jitter(score ~ volume, data = Noise2, color = ~ frequency, 
#'           alpha = 0.4, width = 0.1, height = 0) %>%
#'   gf_line(score ~ volume, data = Noise2, group = ~frequency, color = ~ frequency,
#'           stat = "summary")
#'         
#' gf_jitter(score ~ frequency, data = Noise2, color = ~ volume, 
#'           alpha = 0.4, width = 0.1, height = 0) %>%
#'   gf_line(score ~ frequency, data = Noise2, group = ~ volume, color = ~ volume,
#'           stat = "summary")
#' 
NULL





#' Pallet repair data
#' 
#' The paletts data set contains data from a firm that recycles paletts.
#' Paletts from warehouses are bought, repaired, and resold. (Repairing a
#' palette typically involves replacing one or two boards.) The company has
#' four employees who do the repairs. The employer sampled five days for each
#' employee and recorded the number of pallets repaired.
#' 
#' 
#' @name Pallets
#' @rdname Pallets
#' @docType data
#' @format A data frame with 20 observations on the following 3 variables.
#' \describe{ 
#' \item{pallets}{ number of pallets repaired}
#' \item{employee}{ a factor with levels \code{A} \code{B} \code{C}
#' \code{D}} 
#' \item{day}{ a factor with levels \code{day1} \code{day2}
#' \code{day3} \code{day4} \code{day5}} }
#' @source Michael Stob, Calvin College
#' @keywords datasets
#' @examples
#' 
#' data(Pallets)
#' # Do the employees differ in the rate at which they repair pallets?
#' pal.lm1 <- lm(pallets ~ employee, data = Pallets) 
#' anova(pal.lm1)
#' # Now using day as a blocking variable
#' pal.lm2 <- lm(pallets ~ employee + day, data = Pallets) 
#' anova(pal.lm2)
#' gf_line(pallets ~ day, data = Pallets,
#' 		group = ~employee,
#' 		color = ~employee) %>%
#'   gf_point() %>%
#'   gf_labs(title = "Productivity by day and employee")
#' 
NULL





#' Paper airplanes
#' 
#' Student-collected data from an experiment investigating the design of paper
#' airplanes.
#' 
#' These data were collected by Stewart Fischer and David Tippetts, statistics
#' students at the Queensland University of Technology in a subject taught by
#' Dr. Margaret Mackisack. Here is their description of the data and its
#' collection:
#' 
#' The experiment decided upon was to see if by using two different designs of
#' paper aeroplane, how far the plane would travel. In considering this, the
#' question arose, whether different types of paper and different angles of
#' release would have any effect on the distance travelled. Knowing that paper
#' aeroplanes are greatly influenced by wind, we had to find a way to eliminate
#' this factor. We decided to perform the experiment in a hallway of the
#' University, where the effects of wind can be controlled to some extent by
#' closing doors.
#' 
#' In order to make the experimental units as homogeneous as possible we
#' allocated one person to a task, so person 1 folded and threw all planes,
#' person 2 calculated the random order assignment, measured all the distances,
#' checked that the angles of flight were right, and checked that the plane
#' release was the same each time.
#' 
#' The factors that we considered each had two levels as follows:
#' 
#' Paper: A4 size, 80g and 50g
#' 
#' Design: High Performance Dual Glider, and Incredibly Simple Glider (patterns
#' attached to original report)
#' 
#' Angle of release: Horizontal, or 45 degrees upward.
#' 
#' The random order assignment was calculated using the random number function
#' of a calculator. Each combination of factors was assigned a number from one
#' to eight, the random numbers were generated and accordingly the order of the
#' experiment was found.
#' 
#' @name PaperPlanes
#' @rdname PaperPlanes
#' @docType data
#' @format A data frame with 16 observations on the following 5 variables.
#' \describe{ 
#' \item{distance}{ distance plane traveled (cm)}
#' \item{paper}{ type of paper used} 
#' \item{angle}{ a numeric
#' vector} 
#' \item{design}{ design of plane (\code{hi performance} or
#' \code{simple})} 
#' \item{order}{ order in which planes were thrown} }
#' @references Mackisack, M. S. (1994). What is the use of experiments
#' conducted by statistics students? \emph{Journal of Statistics Education}, 2,
#' no 1.
#' @source These data are also available at OzDASL, the Australasian Data and
#' Story Library (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(PaperPlanes)
#' 
NULL





#' Pendulum data
#' 
#' Period and pendulum length for a number of string and mass pendulums
#' constructed by physics students.  The same mass was used throughout, but the
#' length of the string was varied from 10cm to 16 m.
#' 
#' 
#' @name Pendulum
#' @rdname Pendulum
#' @docType data
#' @format A data frame with 27 observations on the following 3 variables.
#' \describe{ 
#' \item{length}{ length of the pendulum (in meters)}
#' \item{period}{ average time of period (in seconds) over several
#' swings of the pendulum} 
#' \item{delta.length}{ an estimate of the
#' accuracy of the length measurement} }
#' @source Calvin College physics students under the direction of Professor
#' Steve Plath.
#' @keywords datasets
#' @examples
#' 
#' data(Pendulum)
#' gf_point(period ~ length, data = Pendulum)
#' 
NULL





#' Pets and stress
#' 
#' Does having a pet or a friend cause more stress?
#' 
#' Fourty-five women, all self-proclaimed dog-lovers, were randomly divided
#' into three groups of subjects. Each performed a stressful task either alone,
#' with a friend present, or with their dog present.  The average heart rate
#' during the task was used as a measure of stress.
#' 
#' @name PetStress
#' @rdname PetStress
#' @docType data
#' @format A data frame with 45 observations on the following 2 variables.
#' \describe{ 
#' \item{group}{ a factor with levels \code{C}ontrol,
#' \code{F}riend, or \code{P}et} 
#' \item{rate}{ average heart rate while
#' performing a stressful task} }
#' @references These data also appear in
#' 
#' Brigitte Baldi and David S. Moore, \emph{The Practice of Statistics in the
#' Life Sciences}, Freeman, 2009.
#' @source K. M. Allen, J. Blascovich, J. Tomaka, and R. M. Kelsey, Presence of
#' human friends and pet dogs as moderators of autonomic responses to stress in
#' women, \emph{Journal of Personality and Social Psychology} 61 (1991), no. 4,
#' 582--589.
#' @keywords datasets
#' @examples
#' 
#' data(PetStress)
#' xyplot(rate ~ group, data = PetStress, jitter.x = TRUE, type = c('p', 'a'))
#' gf_jitter(rate ~ group, data = PetStress, width = 0.1, height = 0) %>%
#'   gf_line(group = 1, stat = "summary", color = "red")
#' 
NULL





#' Pass the Pigs
#' 
#' This data set contains information collected from rolling the pair of pigs
#' (found in the game "Pass the Pigs") 6000 times.
#' 

#' 
#' In "Pass the Pigs", players roll two pig-shaped rubber dice and earn or lose
#' points depending on the configuration of the rolled pigs. Players compete
#' individually to earn 100 points.  On each turn, a player rolls he or she
#' decides to stop or until "pigging out" or
#' 
#' The pig configurations and their associated scores are
#' 
#' 1 = Dot Up (0)
#' 
#' 2 = Dot Down (0)
#' 
#' 3 = Trotter (5)
#' 
#' 4 = Razorback (5)
#' 
#' 5 = Snouter (10)
#' 
#' 6 = Leaning Jowler (15)
#' 
#' 7 = Pigs are touching one another (-1; lose all points)
#' 
#' One pig Dot Up and one Dot Down ends the turn (a "pig out") and results in 0
#' points for the turn.  If the pigs touch, the turn is ended and all points
#' for the game must be forfeited. Two pigs in the Dot Up or Dot Down
#' configuration score 1 point.  Otherwise, The scores of the two pigs in
#' different configurations are added together. The score is doubled if both
#' both pigs have the same configuration, so, for example, two Snouters are
#' worth 40 rather than 20.
#' 
#' @name Pigs
#' @rdname Pigs
#' @aliases Pigs 
#' @docType data
#' @format A data frame with 6000 observations on the following 6 variables.
#' \describe{ 
#' \item{roll}{ roll number (1-6000)}
#' \item{blackScore}{ numerical code for position of black pig}
#' \item{black}{ position of black pig coded as a factor}
#' \item{pinkScore}{ numerical code for position of pink pig}
#' \item{pink}{ position of pink pig coded as a factor}
#' \item{score}{ score of the roll} 
#' \item{height}{ height from
#' which pigs were rolled (5 or 8 inches)} 
#' \item{start}{ starting
#' position of the pigs (0 = both pigs backwards, 1 = one bacwards one
#' forwards, 2 = both forwards)} }
#' @source John C. Kern II, Duquesne University (\email{kern@mathcs.duq.edu})
#' @keywords datasets
#' @examples
#' 
#' data(Pigs)
#' tally( ~ black, data = Pigs )
#' if (require(tidyr)) {
#'   Pigs %>% 
#'   select(roll, black, pink) %>%
#'   gather(pig, state, black, pink) %>%
#'   tally( state ~ pig, data = ., format = "prop", margins = TRUE)
#' }
#'   
#' 
NULL





#' Major League Baseball 2005 pitching
#' 
#' Major League Baseball pitching statistics for the 2005 season.
#' 
#' 
#' @name Pitching2005
#' @rdname Pitching2005
#' @docType data
#' @format A data frame with 653 observations on the following 26 variables.
#' \describe{ 
#' \item{playerID}{ unique identifier for each player}
#' \item{yearID}{ year} 
#' \item{stint}{ for players who played with
#' multiple teams in the same season, \code{stint} is increased by one each
#' time the player joins a new team} 
#' \item{teamID}{ three-letter identifier for team} 
#' \item{lgID}{ league team plays in, coded as \code{AL} or \code{NL}} 
#' \item{W}{ wins} 
#' \item{L}{ losses}
#' \item{G}{ games played in} 
#' \item{GS}{ games started}
#' \item{CG}{ complete games} 
#' \item{SHO}{ shut outs}
#' \item{SV}{ saves recorded} 
#' \item{IPouts}{ outs recorded (innings pitched, measured in outs rather than innings)}
#' \item{H}{ hits allowed} 
#' \item{ER}{ earned runs allowed}
#' \item{HR}{ home runs allowed} 
#' \item{BB}{ walks (bases on balls) allowed} 
#' \item{SO}{ strike outs} 
## \item{BAOpp}{ opposing hitters' batting average} 
#' \item{ERA}{ earned run average}
#' \item{IBB}{ intentional walks} 
#' \item{WP}{ wild pitches}
#' \item{HBP}{ number of batters hit by pitch} 
#' \item{BK}{ balks}
#' \item{BFP}{ batters faced pitching} 
#' \item{GF}{ ratio of ground balls to fly balls} 
#' \item{R}{ runs allowed} }
#' @keywords datasets
#' @examples
#' 
#' data(Pitching2005)
#' gf_point(IPouts/3 ~ W, data = Pitching2005, ylab = "innings pitched", xlab = "wins")
#' 
NULL





#' Poison data
#' 
#' The data give the survival times (in hours) in a 3 x 4 factorial experiment,
#' the factors being (a) three poisons and (b) four treatments. Each
#' combination of the two factors is used for four animals. The allocation to
#' animals is completely randomized.
#' 
#' 
#' @name Poison
#' @rdname Poison
#' @docType data
#' @format A data frame with 48 observations on the following 3 variables.
#' \describe{ 
#' \item{poison}{ type of poison (1, 2, or 3)}
#' \item{treatment}{ manner of treatment (1, 2, 3, or 4)}
#' \item{time}{ time until death (hours)} }
#' @references Box, G. E. P., and Cox, D. R. (1964). An analysis of
#' transformations (with Discussion). J. R. Statist. Soc. B, 26, 211-252.
#' 
#' Aitkin, M. (1987). Modelling variance heterogeneity in normal regression
#' using GLIM. Appl. Statist., 36, 332-339.
#' 
#' Smyth, G. K., and Verbyla, A. P. (1999). Adjusted likelihood methods for
#' modelling dispersion in generalized linear models. Environmetrics 10,
#' 696-709. \url{http://www.statsci.org/smyth/pubs/ties98tr.html}.
#' @source These data are also available from OzDASL, the Australian Data and
#' Story Library (\url{https://dasl.datadescription.com/}).  (Note: The time measurements
#' of the data at OzDASL are in units of tens of hours.)
#' @keywords datasets
#' @examples
#' 
#' data(poison)
#' poison.lm <- lm(time~factor(poison) * factor(treatment), data = Poison) 
#' plot(poison.lm,w = c(4,2))
#' anova(poison.lm)
#' # improved fit using a transformation
#' poison.lm2 <- lm(1/time ~ factor(poison) * factor(treatment), data = Poison) 
#' plot(poison.lm2,w = c(4,2))
#' anova(poison.lm)
#' 
NULL





#' American football punting
#' 
#' Investigators studied physical characteristics and ability in 13 football
#' punters. Each volunteer punted a football ten times. The investigators
#' recorded the average distance for the ten punts, in feet. They also recorded
#' the average hang time (time the ball is in the air before the receiver
#' catches it), and a number of measures of leg strength and flexibility.
#' 
#' 
#' @name Punting
#' @rdname Punting
#' @docType data
#' @format A data frame with 13 observations on the following 7 variables.
#' \describe{ 
#' \item{distance}{ mean distance for 10 punts (feet) }
#' \item{hang}{ mean hang time (seconds) }
#' \item{rStrength}{ right leg strength (pounds)}
#' \item{lStrength}{ left leg strength (pounds)}
#' \item{rFlexibility}{ right leg flexibility (degrees)}
#' \item{lFlexibility}{ left leg flexibility (degrees)}
#' \item{oStrength}{ overall leg strength (foot-pounds)} }
#' @references "The relationship between selected physical performance
#' variables and football punting ability" by the Department of Health,
#' Physical Education and Recreation at the Virginia Polytechnic Institute and
#' State University, 1983.
#' @source These data are also available at OzDASL
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(Punting)
#' gf_point(hang ~ distance, data = Punting)
#' 
NULL





#' Rat poison -- unfinished documentation
#' 
#' Data from an experiment to see whether flavor and location of rat poison
#' influence the consumption by rats.
#' 
#' 
#' @name RatPoison
#' @rdname RatPoison
#' @docType data
#' @format A data frame with 20 observations on the following 3 variables.
#' \describe{ 
#' \item{consumption}{ a numeric vector}
#' \item{flavor}{ a factor with levels \code{bread}
#' \code{butter-vanilla} \code{plain} \code{roast beef}}
#' \item{location}{ a factor with levels \code{A} \code{B} \code{C}
#' \code{D} \code{E}} }
#' @keywords datasets
#' @examples
#' 
#' data(RatPoison)
#' gf_line(consumption ~ flavor, group = ~ location, color = ~ location, data = RatPoison) %>%
#'   gf_point()
NULL





#' Simulated golf ball data
#' 
#' A matrix of random golf ball numbers simulated using
#' \code{rmultinom(n = 10000,size = 486,prob = rep(0.25,4))}.
#' 
#' 
#' @name rgolfballs
#' @rdname rgolfballs
#' @docType data
#' @keywords datasets
#' @examples
#' 
#' data(rgolfballs)
#' 
NULL





#' Rubber band launching -- unfinished documentation
#' 
#' Results of an experiment comparing a rubber band travels to the amount it
#' was stretched prior to launch.
#' 
#' 
#' @name RubberBand
#' @rdname RubberBand
#' @docType data
#' @format A data frame with 16 observations on the following 2 variables.
#' \describe{ 
#' \item{stretch}{ amount rubber band was stretched before
#' launch} 
#' \item{distance}{ distance rubber band traveled } }
#' @keywords datasets
#' @examples
#' 
#' data(RubberBand)
#' gf_point(distance ~ stretch, data = RubberBand) %>%
#'   gf_lm(interval = "confidence")
#' 
NULL





#' Maze tracing and scents
#' 
#' Subjects were asked to to complete a pencil and paper maze when they were
#' smelling a floral scent and when they were not.
#' 
#' 
#' @name Scent
#' @rdname Scent
#' @docType data
#' @format A data frame with 21 observations on the following 12 variables.
#' \describe{ 
#' \item{id}{ ID number} 
#' \item{sex}{ a factor with
#' levels \code{F} and\code{M}} 
#' \item{smoker}{ a factor with levels
#' \code{N}, \code{Y}} 
#' \item{opinion}{ opinion of the odor
#' (\code{indiff}, \code{neg}, or \code{pos}}) 
#' \item{age}{ age of
#' subject (in years)} 
#' \item{first}{ which treatment was first,
#' \code{scented} or \code{unscented}} 
#' \item{u1}{ time (in seconds) in
#' first unscented trial} 
#' \item{u2}{ time (in seconds) in second
#' unscented trial} 
#' \item{u3}{ time (in seconds) in third unscented
#' trial} 
#' \item{s1}{ time (in seconds) in first scented trial}
#' \item{s2}{ time (in seconds) in second scented trial}
#' \item{s3}{ time (in seconds) in third scented trial} }
#' @references Hirsch, A. R., and Johnston, L. H. "Odors and Learning," Smell
#' & Taste Treatment and Research Foundation, Chicago.
#' @source These data are also available at DASL, the data and story library
# (\url{ftp://sunsite.univie.ac.at/mirrors/lib.stat.cmu.edu/DASL/.index.html}).
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(Scent)
#' summary(Scent)
#' 
NULL





#' Dwindling soap
#' 
#' A bar of soap was weighed after showering to see how much soap was used each
#' shower.
#' 
#' According to Rex Boggs:
#' 
#' I had a hypothesis that the daily weight of my bar of soap [in grams] in my
#' shower wasn't a linear function, the reason being that the tiny little bar
#' of soap at the end of its life seemed to hang around for just about ever. I
#' wanted to throw it out, but I felt I shouldn't do so until it became
#' unusable. And that seemed to take weeks.
#' 
#' Also I had recently bought some digital kitchen scales and felt I needed to
#' use them to justify the cost. I hypothesized that the daily weight of a bar
#' of soap might be dependent upon surface area, and hence would be a quadratic
#' function \dots{} .
#' 
#' The data ends at day 22. On day 23 the soap broke into two pieces and one
#' piece went down the plughole.
#' 
#' @name Soap
#' @rdname Soap
#' @docType data
#' @format A data frame with 15 observations on the following 3 variables.
#' \describe{ 
#' \item{date}{ } 
#' \item{day}{ days since start of soap
#' usage and data collection} 
#' \item{weight}{ weight of bar of soap (in
#' grams) } }
#' @source Data collected by Rex Boggs and available from OzDASL
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(Soap)
#' gf_point(weight ~ day, data = Soap)
#' 
NULL





#' Measuring spheres
#' 
#' Measurements of the diameter (in meters) and mass (in kilograms) of a set of
#' steel ball bearings.
#' 
#' 
#' @name Spheres
#' @rdname Spheres
#' @docType data
#' @format A data frame with 12 observations on the following 2 variables.
#' \describe{ 
#' \item{diameter}{ diameter of bearing (m)}
#' \item{mass}{ mass of the bearing (kg) } }
#' @source These data were collected by Calvin College physics students under
#' the direction of Steve Plath.
#' @keywords datasets
#' @examples
#' data(Spheres)
#' gf_point(mass ~ diameter, data = Spheres)
#' gf_point(mass ~ diameter, data = Spheres) %>%
#'   gf_refine(scale_x_log10(), scale_y_log10())
NULL





#' Stepping experiment
#' 
#' An experiment was conducted by students at The Ohio State University in the
#' fall of 1993 to explore the nature of the relationship between a person's
#' heart rate and the frequency at which that person stepped up and down on
#' steps of various heights.
#' 
#' An experiment was conducted by students at The Ohio State University in the
#' fall of 1993 to explore the nature of the relationship between a person's
#' heart rate and the frequency at which that person stepped up and down on
#' steps of various heights. The response variable, heart rate, was measured in
#' beats per minute. There were two different step heights: 5.75 inches (coded
#' as \code{lo}), and 11.5 inches (coded as \code{hi}). There were three rates
#' of stepping: 14 steps/min. (coded as \code{slow}), 21 steps/min. (coded as
#' \code{medium}), and 28 steps/min. (coded as \code{fast}). This resulted in
#' six possible height/frequency combinations. Each subject performed the
#' activity for three minutes. Subjects were kept on pace by the beat of an
#' electric metronome. One experimenter counted the subject's pulse for 20
#' seconds before and after each trial. The subject always rested between
#' trials until her or his heart rate returned to close to the beginning rate.
#' Another experimenter kept track of the time spent stepping. Each subject was
#' always measured and timed by the same pair of experimenters to reduce
#' variability in the experiment. Each pair of experimenters was treated as a
#' block.
#' 
#' @name Step
#' @rdname Step
#' @docType data
#' @format A data frame with 30 observations on the following 7 variables.
#' \describe{ 
#' \item{order}{ performance order}
#' \item{block}{ number of experimenter block}
#' \item{restHR}{ resting heart rate (beats per minute)}
#' \item{HR}{ final heart rate} 
#' \item{height}{ height of step
#' (\code{hi} or \code{lo})} 
#' \item{freq}{ whether subject stepped
#' \code{fast}, \code{medium}, or \code{slow}} }
#' @source These data are available at DASL, the data and story library
# (\url{ftp://sunsite.univie.ac.at/mirrors/lib.stat.cmu.edu/DASL/.index.html}).
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(Step)
#' gf_jitter(HR-restHR ~ freq, color = ~height, data = Step, group = ~height,
#'           height = 0, width = 0.1) %>%
#'   gf_line(stat = "summary", group = ~height)
#' gf_jitter(HR-restHR ~ height, color = ~freq, data = Step, group = ~freq,
#'           height = 0, width = 0.1) %>%
#'   gf_line(stat = "summary", group = ~freq)
NULL





#' Stereogram fusion
#' 
#' Results of an experiment on the effect of prior information on the time to
#' fuse random dot steregrams. One group (NV) was given either no information
#' or just verbal information about the shape of the embedded object. A second
#' group (group VV) received both verbal information and visual information
#' (e.g., a drawing of the object).
#' 
#' 
#' @name Stereogram
#' @rdname Stereogram
#' @docType data
#' @format A data frame with 78 observations on the following 2 variables.
#' \describe{ 
#' \item{time}{time until subject was able to fuse a random
#' dot stereogram} 
#' \item{group}{treatment group: \code{NV}(no visual
#' instructions) \code{VV} (visual instructions)} }
#' @references Frisby, J. P.  and Clatworthy, J. L., "Learning to see complex
#' random-dot stereograms," \emph{Perception}, 4, (1975), pp. 173-178.
#' 
#' Cleveland, W. S. \emph{Visualizing Data}. 1993.
#' @source These data are available at DASL, the data and story library
# (\url{ftp://sunsite.univie.ac.at/mirrors/lib.stat.cmu.edu/DASL/.index.html}.
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(Stereogram)
#' favstats(time ~ group, data = Stereogram)
#' gf_violin(time ~ group, data = Stereogram, alpha = 0.2, fill = "skyblue") %>%
#' gf_jitter(time ~ group, data = Stereogram, height = 0, width = 0.25)
#' 
NULL





#' Standardized test scores and GPAs
#' 
#' Standardized test scores and GPAs for 1000 students.
#' 
#' 
#' @name Students
#' @rdname Students
#' @docType data
#' @format A data frame with 1000 observations on the following 6 variables.
#' \describe{ 
#' \item{ACT}{ ACT score} 
#' \item{SAT}{ SAT score}
#' \item{grad}{ has the student graduated from college?}
#' \item{gradGPA}{ college GPA at graduation} 
#' \item{hsGPA}{ high
#' school GPA} 
#' \item{cohort}{ year of graduation or expected graduation}
#' }
#' @keywords datasets
#' @examples
#' 
#' data(Students)
#' gf_point(ACT ~ SAT, data = Students)
#' gf_point(gradGPA ~ hsGPA, data = Students)
#' 
NULL





#' Taste test data
#' 
#' The results from a study comparing different preparation methods for taste
#' test samples.
#' 
#' The samples were prepared for tasting using either a coarse screen or a fine
#' screen, and with either a high or low liquid content. A total taste score is
#' recorded for each of 16 groups of 50 testers each. Each group had 25 men and
#' 25 women, each of whom scored the samples on a scale from -3 (terrible) to 3
#' (excellent).  The sum of these individual scores is the overall taste score
#' for the group.
#' 
#' @name TasteTest
#' @rdname TasteTest
#' @aliases TasteTest Taste1
#' @docType data
#' @format A data frame with 16 observations on 2 (\code{taste1}) or 4
#' (\code{tastetest}) variables.  
#' \describe{ 
#' \item{score}{ taste score
#' from a group of 50 testers} 
#' \item{scr}{ a factor with levels
#' \code{coarse} \code{fine}} 
#' \item{liq}{ a factor with levels \code{hi}
#' \code{lo}} 
#' \item{type}{ a factor with levels \code{A} \code{B}
#' \code{C} \code{D}} }
#' @source E. Street and M. G. Carroll, \emph{Preliminary evaluation of a food
#' product}, Statistics: A Guide to the Unknown (Judith M. Tanur et al., eds.),
#' Holden-Day, 1972, pp. 220-238.
#' @keywords datasets
#' @examples
#' 
#' data(TasteTest)
#' data(Taste1)
#' gf_jitter(score ~ scr, data = TasteTest, color = ~liq, width = 0.2, height =0) %>%
#'   gf_line(stat = "summary", group = ~liq)
#' df_stats(score ~ scr | liq, data = TasteTest)
#' 
NULL





#' Estimating tire wear
#' 
#' Tread wear is estimated by two methods: weight loss and groove wear.
#' 
#' @name TireWear
#' @rdname TireWear
#' @docType data
#' @format A data frame with 16 observations on the following 2 variables.
#' \describe{ 
#' \item{weight}{ estimated wear (1000's of miles) base on
#' weight loss} 
#' \item{groove}{ estimated wear (1000's of miles) based on
#' groove wear} }
#' @references R. D. Stichler, G. G. Richey, and J. Mandel, "Measurement of
#' Treadware of Commercial Tires", \emph{Rubber Age}, 73:2 (May 1953).
#' @source These data are available at DASL, the Data and Story Library
# (\url{ftp://sunsite.univie.ac.at/mirrors/lib.stat.cmu.edu/DASL/.index.html}).
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(TireWear)
#' gf_point(weight ~ groove, data = TireWear)
#' 
NULL





#' New England traffic fatalities (1951-1959)
#' 
#' Used by Tufte as an example of the importance of context, these data show
#' the traffic fatality rates in New England in the 1950s.  Connecticut
#' increased enforcement of speed limits in 1956.  In their full context, it is
#' difficult to say if the decline in Connecticut traffic fatalities from 1955
#' to 1956 can be attributed to the stricter enforcement.
#' 
#' 
#' @name Traffic
#' @rdname Traffic
#' @docType data
#' @format A data frame with 9 observations on the following 6 variables.
#' \describe{ 
#' \item{year}{ a year from 1951 to 1959}
#' \item{cn.deaths}{ number of traffic deaths in Connecticut}
#' \item{ny}{ deaths per 100,000 in New York} 
#' \item{cn}{ deaths per 100,000 in Connecticut} 
#' \item{ma}{ deaths per 100,000 in  Massachusetts} 
#' \item{ri}{ deaths per 100,000 in in Rhode Island} }
#' @references Donald T. Campbell and H. Laurence Ross. "The Connecticut
#' Crackdown on Speeding: Time-Series Data in Quasi-Experimental Analysis",
#' \emph{Law & Society Review} Vol. 3, No. 1 (Aug., 1968), pp. 33-54.
#' 
#' Gene V. Glass. "Analysis of Data on the Connecticut Speeding Crackdown as a
#' Time-Series Quasi-Experiment" \emph{Law & Society Review}, Vol. 3, No. 1
#' (Aug., 1968), pp. 55-76.
#' @source Tufte, E. R.  \emph{The Visual Display of Quantitative Information},
#' 2nd ed. Graphics Press, 2001.
#' @keywords datasets
#' @examples
#' 
#' data(Traffic)
#' gf_line(cn.deaths ~ year, data = Traffic)
#' if (require(tidyr)) {
#'   TrafficLong <- 
#'     Traffic %>% 
#'     select(-2) %>%
#'     gather(state, fatality.rate, ny:ri)
#'    gf_line(fatality.rate ~ year, group = ~state, color = ~state, data = TrafficLong) %>%
#'      gf_point(fatality.rate ~ year, group = ~state, color = ~state, data = TrafficLong) %>%
#' 		  gf_lims(y = c(0, NA))
#' }
#' 
NULL





#' Trebuchet data
#' 
#' Measurements from an experiment that involved firing projectiles with a
#' small trebuchet under different conditions.
#' 
#' 
#' @name Trebuchet
#' @rdname Trebuchet
#' @aliases Trebuchet Trebuchet1 Trebuchet2
#' @docType data
#' @format Data frames with the following variables.  
#' \describe{
#' \item{object}{ the object serving as projectile\code{bean} \code{big
#' washerb} \code{bigWash} \code{BWB} \code{foose} \code{golf} \code{MWB}
#' \code{SWB} \code{tennis ball} \code{wood}}
#' \item{projectileWt}{ weight of projectile (in grams)}
#' \item{counterWt}{ weight of counter weight (in kg)}
#' \item{distance}{ distance projectile traveled (in cm)}
#' \item{form}{ a factor with levels \code{a} \code{b} \code{B} \code{c}
#' describing the configuration of the trebuchet.} }
#' @details
#' \code{Trebuchet1} and \code{Trebuchet2} are subsets of \code{Trebuchet} restricted
#' to a single value of \code{counterWt}
#' @source Data collected by Andrew Pruim as part of a Science Olympiad
#' competition.
#' @keywords datasets
#' @examples
#' 
#' data(Trebuchet); data(Trebuchet1); data(Trebuchet2)
#' gf_point(distance ~ projectileWt, data = Trebuchet1)
#' gf_point(distance ~ projectileWt, data = Trebuchet2)
#' gf_point(distance ~ projectileWt, color = ~ factor(counterWt), data = Trebuchet) %>%
#'   gf_smooth(alpha = 0.2, fill = ~factor(counterWt))
#' 
NULL


#' Unemployment data
#' 
#' Unemployment data
#' 
#' @docType data
#' @name Unemployment
#' @usage data(Unemployment)
#' @format  A data.frame with 10 observations on the following 4 variables.
#' \describe{
#'    \item{\code{unemp }}{Millions of unemployed people}
#'    \item{\code{production }}{Federal Reserve Board index of industrial production}
#'    \item{\code{year }}{}
#'    \item{\code{iyear }}{indexed year}
#' }
#' 
#' @source 
#' Paul F. Velleman and Roy E. Welsch. 
#' "Efficient Computing of Regression Diagnostics",
#' The American Statistician, Vol. 35, No. 4 (Nov., 1981), pp. 234-242.
#' (https://www.jstor.org/stable/2683296)
#' @examples
#' data(Unemployment)

NULL




#' Women in the workforce
#' 
#' The labor force participation rate of women in each of 19 U.S. cities in
#' each of two years. # Reference: United States Department of Labor Statistics
#' # # Authorization: free use # # Description: # # Variable Names: # # 1.
#' City: City in the United States # 2. labor72: Labor Force Participation rate
#' of women in 1972 # 3. labor68: Labor Force Participation rate of women in
#' 1968 # # The Data: #
#' 
#' @name WorkingWomen
#' @rdname WorkingWomen
#' @docType data
#' @format A data frame with 19 observations on the following 3 variables.
#' \describe{ 
#' \item{city}{ name of a U.S. city (coded as a factor with
#' 19 levels)} 
#' \item{labor72}{ percent of women in labor force in 1972}
#' \item{labor68}{ percent of women in labor force in 1968} }
#' @source These data are from the United States Department of Labor Statistics
#' and are also available at DASL, the Data and Story Library
# (\url{ftp://sunsite.univie.ac.at/mirrors/lib.stat.cmu.edu/DASL/.index.html}).
#' (\url{https://dasl.datadescription.com/}).
#' @keywords datasets
#' @examples
#' 
#' data(WorkingWomen)
#' gf_point(labor72 ~ labor68, data = WorkingWomen)
#' 
NULL



