install.packages('dplyr')
install.packages('knitr')
library(readr)
library(dplyr)
library(ggplot2)
jpnwomen <- read_csv("~/Desktop/Thesis/IPU/Japan_women_in_parl.csv")


#years
year_jpnx = c('1969', '1972', '1976', '1980', '1983', '1986', '1990', '1993', '1996', '2000', '2003', '2005', '2009', '2012', '2014', '2017', '2021')
women_jpny = c('1.65','1.43', '1.17', '2.15', '1.57', '1.37', '2.34', '2.74', '4.6', '7.29', '7.08', '8.96', '11.25', '7.92', '9.47', '10.11', '9.68')

plot(year_jpnx, women_jpny, type = "o",
     xlab = "Time",
     ylab = "Percentage of Women in Parliament",
     main = "Women in Japan's Parliament Over Time")

#ROK Women in Parliament
rokwomen <- read_csv("~/Desktop/Thesis/IPU/Korea_women_in_par.csv")
year_rokX = c('1971', '1973', '1978', '1981', '1985', '1988', '1992', '1996', '2000', '2004','2008', '2012')
women_roky = c('1.96', '4.57', '3.59', '2.9', '2.90', '2.00', '1.0', '3.01', '5.86', '13.04', '13.71', '15.67')
plot(year_rokX, women_roky, type = "o",
     xlab = "Time",
     ylab = "Percentage of Women in Parliament",
     main = "Women In ROK's Parliament Over Time")

#Taiwan Women in Parliament
taiwanwomen <- read_csv("~/Desktop/Data Collection - Honors Thesis - Taiwan WIG.csv")
year_taiwanX = c('1996',	'1999', '2002',	'2005',	'2008',	'2012',	'2016',	'2020')
women_taiwanY = c('14.30',	'19.80',	'22.10',	'21.20',	'30.50',	'36.60',	'38.90',	'40.50')

#Japan, Taiwan, ROK WIP over time graph

year_jpnroktwn = c('1996',	'1969',	'1972',	'1972',	'1973',	'1976',	'1978',	'1980',	'1981',	'1983',	'1985',	'1986',	'1988',	'1990',	'1992',	'1993',	'1996',	'1999',	'2000',	'2002',	'2003',	'2004',	'2005',	'2008',	'2009',	'2012',	'2014',	'2016',	'2017',	'2020',	'2021')
plot(year_jpnx, women_jpny, type = "o", col="blue", pch="o", xlim = c(1969,2021), ylim = c(0,45), xlab = "Time", ylab = "Percentage of Women in Parliament",
     main = "Women In Parliaments Over Time", lty=1)
points(year_taiwanX, women_taiwanY, col="red", pch="*")
lines(year_taiwanX, women_taiwanY, col="red", lty=2)
points(year_rokX, women_roky, col="green", pch="X")
lines(year_rokX, women_roky, col="green", lty=3)
legend(1970,40, legend = c("Japan","Taiwan","South Korea"), col = c("blue", "red", "green"), pch = c("o", "*", "X"), lty = c(1,2,3), ncol = 3)

#WVS work:
d1981 <- read_csv("~/Desktop/Thesis/WorldValuesSurvey/WVSData/WVS 1981 CSV.csv")
d1990 <- read_csv("~/Desktop/Thesis/WorldValuesSurvey/WVSData/WVS 1990 CSV.csv")

#Hypothesis 1: views on womenʻs role, national pride, support for the LDP and its relationship to political apathy index
#1981 clean data for Hypothesis #1
h1_1981 <- select(d1981, 
                  77, 58,
                  28,
                  71,
                  62,
                  115,
                  116, 28, 72, 73, 74, 37, 118, 36)
h1_1981$LackofPolitical_Engagement <- 0
h1_1981[h1_1981 == -2] <- 0
h1_1981$`V214: Sex`[h1_1981$`V214: Sex` == 1] <- 0
h1_1981$`V214: Sex`[h1_1981$`V214: Sex` == 2] <- 1
h1_1981$LackofPolitical_Engagement <- (h1_1981$`V118: Political action: signing a petition`+h1_1981$`V119: Political action: joining in boycotts`+h1_1981$`V120: Political action: attending lawful/peaceful demonstrations`)
h1_1981$`V106: Aims of respondent: first choice`[h1_1981$`V106: Aims of respondent: first choice` == 2]<- 0
h1_1981$`V106: Aims of respondent: first choice`[h1_1981$`V106: Aims of respondent: first choice` == 3]<- 0
h1_1981$`V106: Aims of respondent: first choice`[h1_1981$`V106: Aims of respondent: first choice` == 4]<- 0
h1_1981$`V37: How often discusses political matters with friends`[h1_1981$`V37: How often discusses political matters with friends` == 0] <- NA
h1_1981$year <- 1981
h1981.test <- rename(h1_1981,
                     sex = `V214: Sex`,
                     national_pride = `V205: How proud of nationality`,
                     life_satisfaction = `V65: Satisfaction with your life`,
                     disinterest_in_politics = `V117: Interest in politics`,
                     lackofpolitical_discourse = `V37: How often discusses political matters with friends`,
                     maintaining_order = `V106: Aims of respondent: first choice`,
                     age = `V216: Age`,
                     political_inaction = `LackofPolitical_Engagement`,
                     sign_petition = `V118: Political action: signing a petition`,
                     year = `year`,
                     boycott = `V119: Political action: joining in boycotts`,
                     attend_demonstration = `V120: Political action: attending lawful/peaceful demonstrations`,
                     financial_satisfaction = `V64: Satisfaction with financial situation of household`,
                     political_spectrum = `V123: Self positioning in political scale`,
                     women_should_have_children = `V93: A woman has to have children to be fulfilled`)

#1990 clean data for Hypothesis #1
h1_1990 <- select(d1990,
                  11, 237, 244, 211, 255, 313, 341, 344, 238, 239, 240, 97, 346, 128)
h1_1990$LackofPolitical_Engagement <- 0
h1_1990[h1_1990 == -1] <- 0
h1_1990$`V353: Sex`[h1_1990$`V353: Sex` ==1] <- 0
h1_1990$`V353: Sex`[h1_1990$`V353: Sex` ==2] <- 1
h1_1990$`V351: Which party would you vote for: first choice`[h1_1990$`V351: Which party would you vote for: first choice` == 392001] <- 1
h1_1990$`V351: Which party would you vote for: first choice`[h1_1990$`V351: Which party would you vote for: first choice` == -2] <- 0
h1_1990$`V351: Which party would you vote for: first choice`[h1_1990$`V351: Which party would you vote for: first choice` == 392015] <- 0
h1_1990$`V351: Which party would you vote for: first choice`[h1_1990$`V351: Which party would you vote for: first choice` == 392014] <- 0
h1_1990$`V351: Which party would you vote for: first choice`[h1_1990$`V351: Which party would you vote for: first choice` == 392004] <- 0
h1_1990$`V351: Which party would you vote for: first choice`[h1_1990$`V351: Which party would you vote for: first choice` == 392003] <- 0
h1_1990$`V351: Which party would you vote for: first choice`[h1_1990$`V351: Which party would you vote for: first choice` == 5] <- 0
h1_1990$`V351: Which party would you vote for: first choice`[h1_1990$`V351: Which party would you vote for: first choice` == 4] <- 0
h1_1990$LackofPolitical_Engagement <- (h1_1990$`V242: Political action: Signing a petition`+h1_1990$`V243: Political action: joining in boycotts`+h1_1990$`V244: Political action: attending lawful/peaceful demonstrations`)
h1_1990$`V259: Aims of respondent: first choice`[h1_1990$`V259: Aims of respondent: first choice` == 2] <- 0
h1_1990$`V259: Aims of respondent: first choice`[h1_1990$`V259: Aims of respondent: first choice` == 3] <- 0
h1_1990$`V259: Aims of respondent: first choice`[h1_1990$`V259: Aims of respondent: first choice` == 4] <- 0
h1_1990$`V10: How often discusses political matters with friends`[h1_1990$`V10: How often discusses political matters with friends`==0] <- NA
h1_1990$year <- 1990
h1990.test <- rename(h1_1990,
                     sex = `V353: Sex`,
                     national_pride = `V322: How proud of nationality`,
                     life_satisfaction = `V96: Satisfaction with your life`,
                     disinterest_in_politics = `V241: Interest in politics`,
                     lackofpolitical_discourse = `V10: How often discusses political matters with friends`,
                     maintaining_order = `V259: Aims of respondent: first choice`,
                     age = `V355: Age`,
                     political_inaction = `LackofPolitical_Engagement`,
                     sign_petition = `V242: Political action: Signing a petition`,
                     boycott = `V243: Political action: joining in boycotts`,
                     attend_demonstration = `V244: Political action: attending lawful/peaceful demonstrations`,
                     year = `year`,
                     financial_satisfaction = `V132: Satisfaction with financial situation of household`,
                     political_spectrum = `V248: Self positioning in political scale`,
                     women_should_have_children = `V215: A woman has to have children to be fulfilled`,
                     support_for_LDP = `V351: Which party would you vote for: first choice`)

#1995 clean data for Hypothesis #1
d1995 <- read_csv("~/Desktop/Thesis/WorldValuesSurvey/WVSData/WVS 1995 CSV.csv")
h1_1995 <- select(d1995,
                  41, 86, 99,116, 110, 195, 198, 200, 111, 112, 113, 59, 202, 58)
h1_1995$LackofPolitical_Engagement <- 0
h1_1995[h1_1995 == -1] <- 0
h1_1995$`V214: Sex`[h1_1995$`V214: Sex` ==1] <- 0
h1_1995$`V214: Sex`[h1_1995$`V214: Sex` ==2] <- 1
h1_1995$`V210: Which party would you vote for: First choice`[h1_1995$`V210: Which party would you vote for: First choice` == 392001] <- 1
h1_1995$`V210: Which party would you vote for: First choice`[h1_1995$`V210: Which party would you vote for: First choice` == 392011] <- 0
h1_1995$`V210: Which party would you vote for: First choice`[h1_1995$`V210: Which party would you vote for: First choice` == 392004] <- 0
h1_1995$`V210: Which party would you vote for: First choice`[h1_1995$`V210: Which party would you vote for: First choice` == 392010] <- 0
h1_1995$`V210: Which party would you vote for: First choice`[h1_1995$`V210: Which party would you vote for: First choice` == 4] <- 0
h1_1995$`V210: Which party would you vote for: First choice`[h1_1995$`V210: Which party would you vote for: First choice` == 392012] <- 0
h1_1995$`V210: Which party would you vote for: First choice`[h1_1995$`V210: Which party would you vote for: First choice` == 5] <- 0
h1_1995$LackofPolitical_Engagement <- (h1_1995$`V118: Political action: Signing a petition`+h1_1995$`V119: Political action: joining in boycotts`+h1_1995$`V120: Political action: attending lawful/peaceful demonstrations`)
h1_1995$`V106: Aims of respondent: first choice`[h1_1995$`V106: Aims of respondent: first choice` ==2] <- 0
h1_1995$`V106: Aims of respondent: first choice`[h1_1995$`V106: Aims of respondent: first choice` ==3] <- 0
h1_1995$`V106: Aims of respondent: first choice`[h1_1995$`V106: Aims of respondent: first choice` ==4] <- 0
h1_1995$`V37: How often discusses political matters with friends`[h1_1995$`V37: How often discusses political matters with friends` == 0] <- NA
h1_1995$year <- 1995
h1995.test <- rename(h1_1995,
                     sex = `V214: Sex`,
                     national_pride = `V205: How proud of nationality`,
                     life_satisfaction = `V65: Satisfaction with your life`,
                     disinterest_in_politics = `V117: Interest in politics`,
                     lackofpolitical_discourse = `V37: How often discusses political matters with friends`,
                     maintaining_order = `V106: Aims of respondent: first choice`,
                     age = `V216: Age`,
                     political_inaction = LackofPolitical_Engagement,
                     sign_petition = `V118: Political action: Signing a petition`,
                     boycott = `V119: Political action: joining in boycotts`,
                     attend_demonstration = `V120: Political action: attending lawful/peaceful demonstrations`,
                     year = `year`,
                     financial_satisfaction = `V64: Satisfaction with financial situation of household`,
                     political_spectrum = `V123: Self positioning in political scale`,
                     women_should_have_children = `V93: A woman has to have children to be fulfilled`,
                     support_for_LDP = `V210: Which party would you vote for: First choice`)

#2000 clean data for Hypothesis #1
d2000 <- read_csv("~/Desktop/Thesis/WorldValuesSurvey/WVSData/WVS 2000 CSV.csv")
h1_2000 <- select(d2000,
                  36, 104, 116, 127, 133, 218, 221, 225, 128, 129, 130, 75, 227, 74)
h1_2000$LackofPolitical_Engagement <- 0
h1_2000[h1_2000 == -1] <- 0
h1_2000$`V223: Sex`[h1_2000$`V223: Sex` ==1] <- 0
h1_2000$`V223: Sex`[h1_2000$`V223: Sex` ==2] <- 1
h1_2000$year <- 2000
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 392001] <- 1
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == -2] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 392009] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 392008] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 392006] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 392004] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 392005] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 392003] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 392002] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 5] <- 0
h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`[h1_2000$`V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)` == 4] <- 0
h1_2000$LackofPolitical_Engagement <- (h1_2000$`V134: Political action: Signing a petition`+ h1_2000$`V135: Political action: joining in boycotts`+ h1_2000$`V136: Political action: attending lawful/peaceful demonstrations`)
h1_2000$`V122: Aims of respondent: first choice`[h1_2000$`V122: Aims of respondent: first choice` == 2] <- 0
h1_2000$`V122: Aims of respondent: first choice`[h1_2000$`V122: Aims of respondent: first choice` == 3] <- 0
h1_2000$`V122: Aims of respondent: first choice`[h1_2000$`V122: Aims of respondent: first choice` == 4] <- 0
h1_2000$`V32: How often discusses political matters with friends`[h1_2000$`V32: How often discusses political matters with friends` ==0] <- NA
h1_2000$year <- 2000
h2000.test <- rename(h1_2000,
                     sex = `V223: Sex`,
                     national_pride = `V216: How proud of nationality`,
                     life_satisfaction = `V81: Satisfaction with your life`,
                     disinterest_in_politics = `V133: Interest in politics`,
                     lackofpolitical_discourse = `V32: How often discusses political matters with friends`,
                     maintaining_order = `V122: Aims of respondent: first choice`,
                     age = `V225: Age`,
                     political_inaction = LackofPolitical_Engagement,
                     sign_petition = `V134: Political action: Signing a petition`,
                     boycott = `V135: Political action: joining in boycotts`,
                     attend_demonstration = `V136: Political action: attending lawful/peaceful demonstrations`,
                     year = `year`,
                     financial_satisfaction = `V80: Satisfaction with financial situation of household`,
                     political_spectrum = `V139: Self positioning in political scale`,
                     women_should_have_children = `V110: A woman has to have children to be fulfilled`,
                     support_for_LDP = `V220: Which party would you vote for if there were a national election tomorrow: first choice (emglish name)`)

#2005 clean data for Hypothesis #1
d2005 <- read_csv("~/Desktop/Thesis/WorldValuesSurvey/WVSData/WVS 2005 CSV.csv")
h1_2005 <- select(d2005,
                  56, 39, 66, 90, 109, 199, 215, 220, 91, 92, 93, 27, 222, 63)
h1_2005$LackofPolitical_Engagement <- 0
h1_2005[h1_2005 == -2] <- 0
h1_2005$`V235: Sex`[h1_2005$`V235: Sex` ==1] <- 0
h1_2005$`V235: Sex`[h1_2005$`V235: Sex`  ==2] <- 1
h1_2005$year <- 2005
h1_2005$`V231: Which party would you vote: first choice`[h1_2005$`V231: Which party would you vote: first choice` == 392001] <- 1
h1_2005$`V231: Which party would you vote: first choice`[h1_2005$`V231: Which party would you vote: first choice` == 392011] <- 0
h1_2005$`V231: Which party would you vote: first choice`[h1_2005$`V231: Which party would you vote: first choice` == 392004] <- 0
h1_2005$`V231: Which party would you vote: first choice`[h1_2005$`V231: Which party would you vote: first choice` == 392003] <- 0
h1_2005$`V231: Which party would you vote: first choice`[h1_2005$`V231: Which party would you vote: first choice` == 392002] <- 0
h1_2005$`V231: Which party would you vote: first choice`[h1_2005$`V231: Which party would you vote: first choice` == 5] <- 0
h1_2005$`V231: Which party would you vote: first choice`[h1_2005$`V231: Which party would you vote: first choice` == 4] <- 0
h1_2005$`V71: Aims of respondent: first choice`[h1_2005$`V71: Aims of respondent: first choice` == 2] <- 0
h1_2005$`V71: Aims of respondent: first choice`[h1_2005$`V71: Aims of respondent: first choice` == 3] <- 0
h1_2005$`V71: Aims of respondent: first choice`[h1_2005$`V71: Aims of respondent: first choice` == 4] <- 0
h1_2005$LackofPolitical_Engagement <- (h1_2005$`V96: Political action: Signing a petition`+h1_2005$`V97: Political action: joining in boycotts`+h1_2005$`V98: Political action: attending lawful/peaceful demonstrations`)
h1_2005$`V44: Jobs scarce: Men should have more right to a job than women`[h1_2005$`V44: Jobs scarce: Men should have more right to a job than women` == 2]<- 0
h1_2005$`V44: Jobs scarce: Men should have more right to a job than women`[h1_2005$`V44: Jobs scarce: Men should have more right to a job than women` == 3]<- 0
h1_2005$`V44: Jobs scarce: Men should have more right to a job than women`[h1_2005$`V44: Jobs scarce: Men should have more right to a job than women` == -1]<- 0
h1_2005$`V61: Men make better political leaders than women do`[h1_2005$`V61: Men make better political leaders than women do` == 3]<- 0
h1_2005$`V61: Men make better political leaders than women do`[h1_2005$`V61: Men make better political leaders than women do` == 4]<- 0
h1_2005$`V61: Men make better political leaders than women do`[h1_2005$`V61: Men make better political leaders than women do` == 1]<- 2
h1_2005$`V61: Men make better political leaders than women do`[h1_2005$`V61: Men make better political leaders than women do` == 2]<- 1
h1_2005$year <- 2005
h2005.test <- rename(h1_2005,
                     sex = `V235: Sex`,
                     national_pride = `V209: How proud of nationality`,
                     life_satisfaction = `V22: Satisfaction with your life`,
                     disinterest_in_politics = `V95: Interest in politics`,
                     womenpoliticalleadersequal = `V61: Men make better political leaders than women do`,
                     maintaining_order = `V71: Aims of respondent: first choice`,
                     age = `V237: Age`,
                     political_inaction = LackofPolitical_Engagement,
                     sign_petition = `V96: Political action: Signing a petition`,
                     boycott = `V97: Political action: joining in boycotts`,
                     attend_demonstration = `V98: Political action: attending lawful/peaceful demonstrations`,
                     year = `year`,
                     financial_satisfaction = `V68: Satisfaction with financial situation of household`,
                     political_spectrum = `V114: Self positioning in political scale`,
                     men_should_work = `V44: Jobs scarce: Men should have more right to a job than women`,
                     support_for_LDP = `V231: Which party would you vote: first choice`)



#2010 clean data for Hypothesis #1
d2010 <- read_csv("~/Desktop/Thesis/WorldValuesSurvey/WVSData/WVS 2010 CSV.csv")
h1_2010 <- select(d2010,
                  48, 54, 65, 88, 99, 216, 233, 246, 89, 90, 91, 26, 248, 62)
h1_2010$LackofPolitical_Engagement <- 0
h1_2010[h1_2010 == -1] <- 0
h1_2010$`V240: Sex`[h1_2010$`V240: Sex` ==1] <- 0
h1_2010$`V240: Sex`[h1_2010$`V240: Sex`  ==2] <- 1
h1_2010$year <- 2010
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392001] <- 1
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392019] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392018] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392017] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392016] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392011] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392004] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392003] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 392002] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 5] <- 0
h1_2010$`V228: Which party would you vote for if there were a national election tomorrow`[h1_2010$`V228: Which party would you vote for if there were a national election tomorrow` == 4] <- 0
h1_2010$`V62: Aims of respondent: first choice`[h1_2010$`V62: Aims of respondent: first choice` == 2] <- 0
h1_2010$`V62: Aims of respondent: first choice`[h1_2010$`V62: Aims of respondent: first choice` == 3] <- 0
h1_2010$`V62: Aims of respondent: first choice`[h1_2010$`V62: Aims of respondent: first choice` == 4] <- 0
h1_2010$LackofPolitical_Engagement <- (h1_2010$`V85: Political action: Signing a petition`+h1_2010$`V86: Political action: joining in boycotts`+h1_2010$`V87: Political action: attending lawful/peaceful demonstrations`)
h1_2010$`V45: Jobs scarce: Men should have more right to a job than women`[h1_2010$`V45: Jobs scarce: Men should have more right to a job than women` == 2]<- 0
h1_2010$`V45: Jobs scarce: Men should have more right to a job than women`[h1_2010$`V45: Jobs scarce: Men should have more right to a job than women` == 3]<- 0
h1_2010$`V45: Jobs scarce: Men should have more right to a job than women`[h1_2010$`V45: Jobs scarce: Men should have more right to a job than women` == -1]<- 0
h1_2010$`V51: Men make better political leaders than women do`[h1_2010$`V51: Men make better political leaders than women do` == 3]<- 0
h1_2010$`V51: Men make better political leaders than women do`[h1_2010$`V51: Men make better political leaders than women do` == 4]<- 0
h1_2010$`V51: Men make better political leaders than women do`[h1_2010$`V51: Men make better political leaders than women do` == 1]<- 2
h1_2010$`V51: Men make better political leaders than women do`[h1_2010$`V51: Men make better political leaders than women do` == 2]<- 1
h1_2010$year <- 2010
h2010.test <- rename(h1_2010,
                     sex = `V240: Sex`,
                     national_pride = `V211: How proud of nationality`,
                     life_satisfaction = `V23: Satisfaction with your life`,
                     disinterest_in_politics = `V84: Interest in politics`,
                     womenpoliticalleadersequal = `V51: Men make better political leaders than women do`,
                     maintaining_order = `V62: Aims of respondent: first choice`,
                     age = `V242: Age`,
                     political_inaction = LackofPolitical_Engagement,
                     sign_petition = `V85: Political action: Signing a petition`,
                     boycott = `V86: Political action: joining in boycotts`,
                     attend_demonstration = `V87: Political action: attending lawful/peaceful demonstrations`,
                     year = `year`,
                     financial_satisfaction = `V59: Satisfaction with financial situation of household`,
                     political_spectrum = `V95: Self positioning in political scale`,
                     men_should_work = `V45: Jobs scarce: Men should have more right to a job than women`,
                     support_for_LDP = `V228: Which party would you vote for if there were a national election tomorrow`)

#2019 clean data for Hypothesis #1
d2019 <- read_csv("~/Desktop/Thesis/WorldValuesSurvey/WVSData/WVS 2019 CSV.csv")
h1_2019 <- select(d2019,
                  61, 65, 190, 235, 236, 279, 293, 299, 245, 246, 247, 259, 84, 301, 85)
h1_2019$LackofPolitical_Engagement <- 0
h1_2019[h1_2019 == -2] <- 0
h1_2019[h1_2019 == -1] <- 0
h1_2019$`Q260: Sex`[h1_2019$`Q260: Sex` ==1] <- 0
h1_2019$`Q260: Sex`[h1_2019$`Q260: Sex`  ==2] <- 1
h1_2019$year <- 2019
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 392001] <- 1
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 392027] <- 0
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 392026] <- 0
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 392025] <- 0
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 392014] <- 0
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 392004] <- 0
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 392003] <- 0
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 5] <- 0
h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow`[h1_2019$`Q223: Which party would you vote for if there were a national election tomorrow` == 4] <- 0
h1_2019$LackofPolitical_Engagement <- (h1_2019$`Q209: Political action: Signing a petition`+ h1_2019$`Q210: Political action: Joining in boycotts`+ h1_2019$`Q211: Political action: Attending lawful/peaceful demonstrations`)
h1_2019$`Q154: Aims of respondent: first choice`[h1_2019$`Q154: Aims of respondent: first choice` == 2]<- 0
h1_2019$`Q154: Aims of respondent: first choice`[h1_2019$`Q154: Aims of respondent: first choice` == 3]<- 0
h1_2019$`Q154: Aims of respondent: first choice`[h1_2019$`Q154: Aims of respondent: first choice` == 4]<- 0
h1_2019$`Q33: Jobs scarce: Men should have more right to a job than women`[h1_2019$`Q33: Jobs scarce: Men should have more right to a job than women` == 2]<- 0
h1_2019$`Q33: Jobs scarce: Men should have more right to a job than women`[h1_2019$`Q33: Jobs scarce: Men should have more right to a job than women` == 3]<- 0
h1_2019$`Q33: Jobs scarce: Men should have more right to a job than women`[h1_2019$`Q33: Jobs scarce: Men should have more right to a job than women` == -1]<- 0
h1_2019$`Q29: Men make better political leaders than women do`[h1_2019$`Q29: Men make better political leaders than women do` == 3]<- 0
h1_2019$`Q29: Men make better political leaders than women do`[h1_2019$`Q29: Men make better political leaders than women do` == 4]<- 0
h1_2019$`Q29: Men make better political leaders than women do`[h1_2019$`Q29: Men make better political leaders than women do` == 1]<- 2
h1_2019$`Q29: Men make better political leaders than women do`[h1_2019$`Q29: Men make better political leaders than women do` == 2]<- 1
h1_2019$`Q200: How often discusses political matters with friends`[h1_2019$`Q200: How often discusses political matters with friends`==0]<- NA
h1_2019$year <- 2019
h2019.test <- rename(h1_2019,
                     sex = `Q260: Sex`,
                     national_pride = `Q254: National pride`,
                     life_satisfaction = `Q49: Satisfaction with your life`,
                     disinterest_in_politics = `Q199: Interest in politics`,
                     womenpoliticalleadersequal = `Q29: Men make better political leaders than women do`,
                     maintaining_order = `Q154: Aims of respondent: first choice`,
                     age = `Q262: Age`,
                     political_inaction = LackofPolitical_Engagement,
                     sign_petition = `Q209: Political action: Signing a petition`,
                     lackofpolitical_discourse = `Q200: How often discusses political matters with friends`,
                     boycott = `Q210: Political action: Joining in boycotts`,
                     attend_demonstration = `Q211: Political action: Attending lawful/peaceful demonstrations`,
                     year = `year`,
                     financial_satisfaction = `Q50: Satisfaction with financial situation of household`,
                     political_spectrum = `Q240: Left-right political scale`,
                     men_should_work = `Q33: Jobs scarce: Men should have more right to a job than women`,
                     support_for_LDP = `Q223: Which party would you vote for if there were a national election tomorrow`)

install.packages('sjmisc')
install.packages('sjPlot')



library(sjPlot)
library(sjmisc)
library(ggplot2)
library(interactions)

#hypothesis 1 --> women who support the LDP, more apathetic
hyp1interest <- lm(disinterest_in_politics ~ support_for_LDP*sex + age, data=h2.all)
summary(hyp1interest)

hinteract1 <- interact_plot(hyp1interest, pred = "support_for_LDP", modx = "sex", data=h2.all)
stargazer(hinteract1, style="APSR")

hyp1action <- lm(political_inaction ~ support_for_LDP*sex + age, data = h2.all)
summary(hyp1action)

interact_plot(hyp1action, pred = "support_for_LDP", modx = "sex", data = h2.all)

hyp1discourse <- lm(lackofpolitical_discourse ~ support_for_LDP*sex + age, data = h2.all)
summary(hyp1discourse)

interact_plot(hyp1discourse, pred = "support_for_LDP", modx = "sex", data = h2.all)

stargazer(hyp1interest, hyp1action, hyp1discourse, style="APSR")

#hypothesis 2 --> women think men make better political leaders, more apathetic
hyp2interest <- lm(disinterest_in_politics ~ womenpoliticalleadersequal*sex, data = h2.all)
summary(hyp2interest)

interact_plot(hyp2interest, pred = "womenpoliticalleadersequal", modx = "sex", data = h2.all)

hyp2action <- lm(political_inaction ~ womenpoliticalleadersequal*sex, data = h2.all)
summary(hyp2action)

interact_plot(hyp2action, pred = "womenpoliticalleadersequal", modx = "sex", data = h2.all)

hyp2discourse <- lm(lackofpolitical_discourse ~ womenpoliticalleadersequal*sex, data = h2.all)
summary(hyp2discourse)

interact_plot(hyp2discourse, pred = "womenpoliticalleadersequal", modx = "sex", data = h2.all)

stargazer(hyp2interest, hyp2action, hyp2discourse, style = "APSR")

#hypothesis 3a --> 
hyp3interest <- lm(h2.all$disinterest_in_politics ~ h2.all$national_pride)
summary(hyp3interest)

stargazer(hyp3interest, style = "APSR")

#hypothesis 3b -->
hyp3LDP <- lm(h2.all$support_for_LDP ~ h2.all$national_pride)
summary(hyp3LDP)

stargazer(hyp3LDP, style = "APSR")

#hypothesis 3c -->
hyp3interestpride <- lm(disinterest_in_politics ~ national_pride*sex, data = h2.all)
summary(hyp3interestpride)

interact_plot(hyp3interestpride, pred = "national_pride", modx = "sex", data = h2.all)

hyp3action <- lm(political_inaction ~ national_pride*sex, data = h2.all)
summary(hyp3action)

interact_plot(hyp3action, pred = "national_pride", modx = "sex", data = h2.all)

hyp3discourse <- lm(lackofpolitical_discourse ~ national_pride*sex, data = h2.all)
summary(hyp3discourse)

interact_plot(hyp3discourse, pred = "national_pride", modx = "sex", data = h2.all)

stargazer(hyp3interestpride, hyp3action, hyp3discourse, style = "APSR")

#hypothesis 4a
hyp4interest <- lm(disinterest_in_politics ~ life_satisfaction*sex + age, data = h2.all)
summary(hyp4interest)

interact_plot(hyp4interest, pred = "life_satisfaction", modx = "sex", data = h2.all)

hyp4action <- lm(political_inaction ~ life_satisfaction*sex + age, data = h2.all)
summary(hyp4action)

interact_plot(hyp4action, pred = "life_satisfaction", modx = "sex", data = h2.all)

hyp4discourse <- lm(lackofpolitical_discourse ~ life_satisfaction*sex + age, data = h2.all)
summary(hyp4discourse)

interact_plot(hyp4discourse, pred = "life_satisfaction", modx = "sex", data = h2.all)

stargazer(hyp4interest, hyp4action, hyp4discourse, style = "APSR")

#hypothesis #4b

hyp4intfinance <- lm(disinterest_in_politics ~ financial_satisfaction*sex + age, data = h2.all)
summary(hyp4intfinance)

interact_plot(hyp4intfinance, pred = "financial_satisfaction", modx = "sex", data = h2.all)

hyp4actfinance <- lm(political_inaction ~ financial_satisfaction*sex + age, data = h2.all)
summary(hyp4actfinance)

interact_plot(hyp4actfinance, pred = "financial_satisfaction", modx = "sex", data = h2.all)

hyp4discfinance <- lm(lackofpolitical_discourse ~ financial_satisfaction*sex + age, data = h2.all)
summary(hyp4discfinance)

interact_plot(hyp4discfinance, pred = "financial_satisfaction", modx = "sex", data = h2.all)

stargazer(hyp4intfinance, hyp4actfinance, hyp4discfinance, style = "APSR")













