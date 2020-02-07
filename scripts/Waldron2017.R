#Waldron 2017 Data
library(mgcv)
library(MuMIn)
library(devtools)
P <- rprojroot::find_rstudio_root_file

#Read in text file
steward<-read.delim("data/Waldron_data.txt", sep="\t") 
party<-steward[steward$CBDParty==1,]

write.csv(data, P("data/Waldron.csv"), append = FALSE)

# NEW STUFF NEEDED: % reduction per million dollars and cost of 50% reduction for Indonesia and DRC. 
# Newdata: Inertia88 needs to be for 0408 (birdRLC040817) and inertia for 0812 (minus Brazil!!) (birdRLC0812_nofuturecast or birdRLC081217). 
# (1) reduction per million dollars non-linear and context-dependent. Try adding and subtracting 1m and 5m to Indonesia and DRC in past and using 9708 data 
# (marginal impact on percentage should be different four ways: past and now (owing to context change) and up and down (owing to non-linearity).
# State what percentage of the budget this is. (2) 50% reduction may be an ask too far.  Could simply carry out same exercise as (1) 
# but looking for the dollars to meet 50% reduction then and now (i.e. up to 2008 data). 
# **********************************************************************************

# *****************************************************
#    FLAGS AND SETTABLE PARAMETERS
# *****************************************************

flagbinom <-0 # SET TO  1 if running the binomial analysis, 0 if running the continuous analysis.
cutoff<-5 # chooses up to what point you want to analyse: 5 is all data, 4 omits rich nations, 3 omits upper middle income nations
flag9200<-0 # set to 1 if want this period regressed, else does 1992-2003
usespenddiff<-0 # experiment setting this to 1 if want to try to estimate the difference in spending. Very rough because don’t really know for sure what 80s spending was, have to assume it was 50% of government spending for 1990 estimate and then add aid difference. Not used in the end, not fully developed.
downweightbiggie <-1 # NO LONGER USED, this flag is only operative if NOT using the propensity score weighting and currently, all analyses do use that weighting. SET TO ZERO IF RUNNING BINOMIAL. Set first flag to 1 if want to give a low regression weight to the influential high-species-changing countries, so they don’t distort the smoother. 
respfraction<-0.17 # threshold for minimum range fraction that triggers responsibility.
olderEurope<-0 # Test if including the questionable older data on Sweden, Norway and Iceland affects the outcomes (it does, slightly) 
FUKUS<-0 # set to 1 to include US (mainland only) , 0 to exclude it. NB ALL THREE OF THESE FLAGS NEED TO BE ZERO TO CORRECTLY MODEL GLOBAL BDS, DUE TO THE PROBLEM OF MULTIPLE TERROTORIES WITHOUT DISAGGREGATED SPENDING AND WITH VERY DIFFERENT SOCIOECONOMIC CONDITIONS
FUK<- 0# set to 1 to include UK, 0 to exclude it (NullRLC and the weighted sum of threatened species fractions would need to be calculated sub-nationally for all of these countries and that is not part of the country-level study. Same true of US but error much smaller)
FF<-0 # set to 0 to exclude France. Same problem of multiple territories 
if (flagbinom == 1) FF<-1
useFAOforest<-0 # 1 if using FAO forest cover change data for 1990-2005, 0 if using Hansen loss of pixels with >75% forest cover (which proxies loss of primary forest rather than simple tree cover, having a 0.71 r2 on loss of primary forest in countries where this is robust in FAO data)
BIRD<-1 # set to 1 if including birds 
MAMMAL<-1 # ditto for mammals
omitSolomons<-0
if (flagbinom == 1) omitSolomons<-1 # THIS IS FOR BINOMIAL. see line for Senegal. Solomons also has out-of-the-park governance growth, but try keeping it in.
single_parameter_anomalies_in<-1 # set to 0 to exclude the three countries which have atypical responses to a single parameter: Mongolia and Kazakstan totally reverse population growth effects, while Chile has an atypical response for governance (and pop density, but that’s not used any more). Set to 2 to include Chile but exclude the population growth distorters. Set to 3 to exclude Chile only. Note that a fourth country, Botswana, could be in this group, but gets excluded for having zero declines and fewer than 25 fractional spp. SET TO 1 TO INCLUDE ALL ANOMALOUS COUNTRIES.
nobiggy<-1 # set to 1 to include big decliners (the top five BDS countries), except India and China. This is related to concerns that high-BDS countries may be driving the regression relationships.
noIndia<-1 # set to 1 to exclude India, since spending data is questionable
noChina<-0 # set to 1 to exclude China, since spending data is incomplete by being only central (tho Brazil has a similar problem, with provincial spending being underreported)
omitPNG<-0; omitVietnam<-1; omitSeychelles <-1; omitSenegal<-0
# Viet Nam has barely credible government spending figures, known to be largely directed at parks near the capital, and is a big outlier on agricultural growth too. Seychelles has a very complex spending landscape not easily captured by our main approaches. 
adjustsmallNulls<- 0 # not used now. Was exploring the problem that RLC has a minimum value but NullRLC doesn’t so for very small null (Bangladesh 1.96 and Senegal 3.23), the apparent decline will be disproportionately large. This is a big cheat, though, because it artificially doubles the species richness of those two countries. Alternative is to omit Senegal and even Bangladesh (Senegal lost 30% of its NullRLC, Bangladesh 12%, maximum loss for any country with NullRLC>10 is 10%. Both can be omitted by setting stew4o to exclude NullRLC<3.3 of <5 (removes also tiny ex-Soviet Kyrgyz, Uzbek and Georgian) at bottom.
minNullRLC<-0 # as previous comment – can set to higher values (e.g. 5) to avoid analyzing countries where very small species richness values mean that any decline at all will appear to be a large percentage loss of the total biota, which can create clearly exaggerated values in some forms of analysis e.g. if you implicitly analyze decline as a proportion of total species richness
useAustronesia<-0 # set to 1 if using a Austronesia region for Indonesia, Malaysia, PNG, East Timor, Micronesia, Fiji, Brunei, Philippines, Singapore, Fiji, Solomon Islands, Tuvalu, Tonga. Not used any more.
Adjust1994_2000<-1.0 # this is the proportion by which you reduce the 1994-2000 bird declines; can be 0.33, 1 or 0. Set to 1 to ignore all 1994-2000 birds, 0 to include them all, 0.33 to include two thirds of the 94-00 declines.
zeroPsithreshold<-25 # defines threshold for the “informative zeroes”, on the basis of the minimum species richness (fractional) allowable when analyzing a case of zero species declines in a 13 year period. Generally, 15-35 works well, otherwise get a zero-heavy pattern in the residuals.
runfor_forecast <- 1 # set to 0 and will get model with 101 countries including wealthy non-OECD, but this hinders forecasts, so can exclude wealthy non-OECD (by setting to 1) to run forecasts.
europeaction<-2 # set to 1 if excluding EU, 2 if including it
dodgyfinanceaction<-1 # set to 1 if excluding poor finance info, 2 if including all points
cutoffgap<-3   # defines World Bank income group ranking below which aid data used to estimate missing government spending values. 
resguardo <- 0 # unclear whether to include Colombia’s resguardo funding; set this to 5,000,000 if yes, 0 if no
Chinaprovince<-1.35 # estimated multiplier of central spending to account for provincial spending in China
steward$poorcurrentfinanceinfo [steward$COUNTRY=="China"]<-0
   
# flags and parameters to check whether varying the harder-to-define regions makes a difference. 
# China’s region and the way SE Asia was divided up are tested. Leave these as they are now.

steward$Regionbasic2[steward$COUNTRY=="China"]<-"asia" # if you want to vary China’s region; can also be casia. Original is asia (eastern asia) but in the continuous analysis, China then becomes the only country in this region


Austronesia <- c(which(steward$COUNTRY=="Indonesia"), which (steward$COUNTRY=="Malaysia"), 
                 which (steward$COUNTRY=="Papua_New_Guinea"), which(steward$COUNTRY=="Federated_States_of_Micronesia"), 
                 which (steward$COUNTRY=="East_Timor"), which(steward$COUNTRY=="Philippines"), which(steward$COUNTRY=="Fiji"), 
                 which(steward$COUNTRY=="Brunei"), which(steward$COUNTRY=="Solomon_Islands"), which(steward$COUNTRY=="Tonga"), 
                 which(steward$COUNTRY=="Tuvalu"), which(steward$COUNTRY=="Singapore"))

steward$Region<-as.character(steward$Regionbasic2)
steward$Region[Austronesia]<- "Austronesia"
if (useAustronesia == 1) steward$Regionbasic2<-steward$Region

#####################################################################################################################
# DATA CHOICES: DEFINES GOVERNANCE METRIC BEING USED AND PERIODS BEING USED FOR REGRESSION (THE OLDEST PERIOD),     # 
# FOR IMMINENT DECLINE ANALYSIS (THE MIDDLE PERIOD), AND FOR CURRENT FUNDING NEEDS ANALYSIS (THE MOST RECENT PERIOD)#
#####################################################################################################################



