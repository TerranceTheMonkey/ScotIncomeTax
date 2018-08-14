# 3. Summary Stats 

# Pre-requisites: 1. Import and Fix
# This code produces three set of tables and one set of graphs:
# I. Tables comparing decomposed tax liability with total (from dataset)
# II. Create Summary of Scottish Data
# III. Analyse RUK data
# IV. Create graphs of Scottish Data

library(tables)

# I. Tables comparing decomposed tax liability with total (from dataset)
#  Create and export tables to csv
FactCheck <- tabular((factor(PF_TOTTAXZZ)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+TOTTAXFACTSCOT)*(sum),data=Scotland)
MarCheck <- tabular(factor(PF_TOTTAXZZ)*(factor(MAR)+1)~(FACTSCOT+TOTTAXXX+TOTTAX+TOTTAXFACTXX+TOTTAXFACT+TOTTAXFACTSCOTXX+TOTTAXFACTSCOT)*(sum),data=Scotland)
write.csv.tabular(FactCheck, file="FactCheck.csv")
write.csv.tabular(MarCheck, file="MarCheck.csv")

# II. Create Summary of Scottish Data
# Create new dataset to run tables
ScotlandTaxpayers <- subset(Scotland,Scotland$TOTTAXFACTSCOTXX != 0)

# Create Tables
# Age Range  (1-Under 25, 2-25:34, 3-35:44, 4-45:54, 5-55:64, 6-65:74, 7-75 and over)
AgeTable <- tabular((factor(AGERANGE)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# Directorship (1-Director (Close Company), 2-Director (Not Close), 3-Not Director, 4-Director (PAYE))
DShipsTable <- tabular((factor(DSHIPS)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# Governemnt Office Region (1-North East, 2-North West, 3-Yorkshire, 4-East Midlands, 5-West Midlands, 6-East
#                            7- London, 8-South East, 9-South West, 10-Wales, 11-Scotland, 12-Northern Ireland
#                           13-Abroad, 14-Unknown)
tabular((factor(GORCODE)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# Industry SIC 2007 A-Agriculture, forestry and fishing, B-Mining and quarrying, C-ManuFACTSCOTuring,
#     D-Electricity, gas, steam and air conditioning supply, E-Water supply; sewerage, waste management and remediation activities
#     F-Construction, G-Wholesale and retail trade; repair of motor vehicles and motor cycles, H-Transport and storage,
#     I-Accommodation and food service activities, J-Information and communication, K-Financial and insurance activities
#     L-Real estate activities, M-Professional, scientific and technical activities, N-Administrative and support service activities,
#     O-Public administration and defence;compulsory social security, P-Education, Q-Human health and social work activities
#     R-Arts, entertainment and recreation, S-Other service activities, 
#     T-Activities of households as employers;undifferentiated goods and services-producing activities of households for own use
#     U-Activities of extraterritorial organisations and bodies, 
#     1500-Other (income from financial investments,property, unemployment benefit, incapacity benefit, and other Social Security benefits)
#     1600-Income from pensions (includes pensioners and persons moving into pensions during the year), 1700-Claimants
#     1900-Individuals with no income from Pay, Pension and unemployment benefit, incapacity benefit and other Social Security benefits
#     Blank Unknown
IndustryTable <- tabular((factor(INDUSTRY07)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# Main Income Source (1-Pay, 2-Pension, 3-Sole Trader, 4-Partnership, 5-Other, 6-Claims)
SourceTable <- tabular((factor(MAINSRCE)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# Marginal Rate of Tax (ST-Standard Income, NS-Non-Standard Income, DV-Dividends, NONTP-Non Taxpayer
#                       BR-Basic Rate, HR-Higher Rate, LR-Additional Rate)
MarTable <- tabular((factor(MAR)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# Self Employed (-1-NA, 0-Not Self Employed, 1-Self Employed)
SelfTable <- tabular((factor(SEINC_NUM)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# Sex  (0-Not attributable, 1-Male, 2-Female)
SexTable <- tabular((factor(SEX)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# State Pension Age Indicator (-1-NA, 0-Non-State Pension Age, 1-State Pension Age)
PenTable <- tabular((factor(SPA)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)
# Taxpayer  (1-Taxpayer, 2-Non-Taxpayer)
tabular((factor(TAXPAYER)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandTaxpayers)

#Export tables to csv
write.csv.tabular(AgeTable, file="AgeTable.csv")
write.csv.tabular(DShipsTable, file="DShipsTable.csv")
write.csv.tabular(IndustryTable, file="IndustryTable.csv")
write.csv.tabular(SourceTable, file="SourceTable.csv")
write.csv.tabular(MarTable, file="MarTable.csv")
write.csv.tabular(SelfTable, file="SelfTable.csv")
write.csv.tabular(SexTable, file="SexTable.csv")
write.csv.tabular(PenTable, file="PenTable.csv")

tabular((factor(SCOTSHARE)+1)~(FACTSCOT+TOTTAXFACTSCOTXX)*(sum),data=Scotland)
tabular((factor(INDUSTRY07)+1)~(FACT+FACTSCOT+TOTTAXFACTXX+TOTTAXFACTSCOTXX)*(sum),data=Scotland)

# III. Analyse RUK data
# Create RUK dataset
RUKTaxpayers <- subset(RUK,RUK$TOTTAXFACTRUKXX != 0)

# Create RUK tables 
# Marginal Rate of Tax (ST-Standard Income, NS-Non-Standard Income, DV-Dividends, NONTP-Non Taxpayer
#                       BR-Basic Rate, HR-Higher Rate, LR-Additional Rate)
tabular((factor(MAR)+1)~(FACT+FACTRUK+TOTTAXFACTXX+TOTTAXFACTRUKXX)*(sum),data=RUKTaxpayers)
# Governemnt Office Region (1-North East, 2-North West, 3-Yorkshire, 4-East Midlands, 5-West Midlands, 6-East
#                            7- London, 8-South East, 9-South West, 10-Wales, 11-Scotland, 12-Northern Ireland
#                           13-Abroad, 14-Unknown)
tabular((factor(GORCODE)+1)~(FACT+FACTRUK+TOTTAXFACTXX+TOTTAXFACTRUKXX)*(sum),data=RUKTaxpayers)

# IV. Create graphs of Scottish Data 
library(ggplot2)
# Main Income Source (1-Pay, 2-Pension, 3-Sole Trader, 4-Partnership, 5-Other, 6-Claims)
ggplot(Scotland,aes(x=factor(MAINSRCE),y=FACTSCOT))+geom_bar(stat="identity")
# Directorship (1-Director (Close Company), 2-Director (Not Close), 3-Not Director, 4-Director (PAYE))
ggplot(Scotland,aes(x=factor(DSHIPS),y=FACTSCOT))+geom_bar(stat="identity")
# Marginal Rate of Tax (ST-Standard Income, NS-Non-Standard Income, DV-Dividends, NONTP-Non Taxpayer
#                       BR-Basic Rate, HR-Higher Rate, LR-Additional Rate)
ggplot(Scotland,aes(x=factor(MAR),y=FACTSCOT))+geom_bar(stat="identity")
# Self Employed (-1-NA, 0-Not Self Employed, 1-Self Employed)
ggplot(Scotland,aes(x=factor(SEINC_NUM),y=FACTSCOT))+geom_bar(stat="identity")
# State Pension Age Indicator (-1-NA, 0-Non-State Pension Age, 1-State Pension Age)
ggplot(Scotland,aes(x=factor(SPA),y=FACTSCOT))+geom_bar(stat="identity")
