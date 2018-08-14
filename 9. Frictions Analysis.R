# 9. Frictions Analysis 

# Pre-requisites: 1. Import and Fix
# This code defines a set of TIEs that vary by income status (B2 and B3), then applies one these regimes.
# However it also applies a friction defiend based on the proportion of taxable income
# It then recalculates tax liabilities and produces a series of tables summarising the result

# In all cases the new variables begin NEWTAX, hence TOTTAXFACTSCOTXX is the original tax
# liability and NEWTAXTOTTAXFACTSCOTXX is the new tax liability.  

# The TIE regime can be varied by changing the value of FrictionTIE, the size of the friction can be varied 
# by changing the value of Friction and the size of the tax rise or cut can be varied by changing the 
# value of TaxChange 

library(Hmisc)
library(tables)

#TIE Values - Regime B2
put1415$TIEB2 <- ifelse(put1415$ADJTAXINC<31865,0.015,
                        ifelse(put1415$ADJTAXINC<80001,0.1,
                               ifelse(put1415$ADJTAXINC<150001,0.2,
                                      ifelse(put1415$ADJTAXINC<300001,0.35,
                                             ifelse(put1415$ADJTAXINC<500001,0.55,0.75)))))
# TIE Values - Regime B3
put1415$TIEB3 <- ifelse(put1415$ADJTAXINC<31865,0.030,
                        ifelse(put1415$ADJTAXINC<80001,0.2,
                               ifelse(put1415$ADJTAXINC<150001,0.4,
                                      ifelse(put1415$ADJTAXINC<300001,0.7,
                                             ifelse(put1415$ADJTAXINC<500001,1.1,1.5)))))
# Variables for analysis
FrictionTIE <- put1415$TIEB2
Friction <- 0.03
TaxChange <- 0.01

# New Taxable Income
put1415$NEWTAXBASICEARINC <-	      ifelse(put1415$TAXEARINC>31865,put1415$BASICEARINC,
                                         ifelse(put1415$TOTTAXX==0,put1415$BASICEARINC+(put1415$BASICEARINC*((-TaxChange/(1-0.2))*FrictionTIE)),
                                                ifelse((put1415$BASICEARINC*TaxChange)/put1415$TOTTAXXX<Friction,put1415$BASICEARINC,put1415$BASICEARINC+(put1415$BASICEARINC*((-TaxChange/(1-0.2))*FrictionTIE)))))
put1415$NEWTAXHIGHEREARINC <-	    ifelse(put1415$TAXEARINC>150000,put1415$HIGHEREARINC,
                                        ifelse(put1415$TOTTAXX==0,put1415$HIGHEREARINC+(put1415$HIGHEREARINC*((-TaxChange/(1-0.2))*FrictionTIE)),
                                                ifelse((put1415$HIGHEREARINC*TaxChange)/put1415$TOTTAXXX<Friction,put1415$HIGHEREARINC,
                                                        ifelse(put1415$HIGHEREARINC+(put1415$HIGHEREARINC*((-TaxChange/(1-0.4))*FrictionTIE))>=0,
                                                            put1415$HIGHEREARINC+(put1415$HIGHEREARINC*((-TaxChange/(1-0.4))*FrictionTIE)),
                                                            put1415$HIGHEREARINC==0 & put1415$HIGHEREARINC==31865-(put1415$HIGHEREARINC+(put1415$HIGHEREARINC*((-TaxChange/(1-0.4))*FrictionTIE)))))))
put1415$NEWTAXADDITIONALEARINC <-  ifelse(put1415$TOTTAXX==0,put1415$ADDITIONALEARINC+(put1415$ADDITIONALEARINC*((-TaxChange/(1-0.45))*FrictionTIE)),
                                          ifelse((put1415$ADDITIONALEARINC*TaxChange)/put1415$TOTTAXXX<Friction,put1415$ADDITIONALEARINC,
                                                ifelse(put1415$ADDITIONALEARINC+(put1415$ADDITIONALEARINC*((-TaxChange/(1-0.45))*FrictionTIE))>=0,
                                                        put1415$ADDITIONALEARINC+(put1415$ADDITIONALEARINC*((-TaxChange/(1-0.45))*FrictionTIE)),
                                                        put1415$ADDITIONALEARINC==0 & put1415$HIGHEREARINC==31865-(put1415$ADDITIONALEARINC+(put1415$ADDITIONALEARINC*((-TaxChange/(1-0.4))*FrictionTIE))))))
put1415$NEWTAXBASICINVINC <-	put1415$BASICINVINC
put1415$NEWTAXHIGHERINVINC <-	put1415$HIGHERINVINC
put1415$NEWTAXADDITIONALINVINC <-	put1415$ADDITIONALINVINC

# New Tax Liability (no factor)
put1415$NEWTAXBASICEAR <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,(0.2+TaxChange)*put1415$NEWTAXBASICEARINC)
put1415$NEWTAXHIGHEREAR <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,(0.4+TaxChange)*put1415$NEWTAXHIGHEREARINC)
put1415$NEWTAXADDITIONALEAR <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,(0.45+TaxChange)*put1415$NEWTAXADDITIONALEARINC)
put1415$NEWTAXBASICINV <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.10*put1415$NEWTAXBASICINVINC)
put1415$NEWTAXHIGHERINV <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.325*put1415$NEWTAXHIGHERINVINC)
put1415$NEWTAXADDITIONALINV <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.375*put1415$NEWTAXADDITIONALINVINC)
put1415$NEWTAXTOTTAXXX <-	ifelse(put1415$NEWTAXBASICEAR + put1415$NEWTAXHIGHEREAR + put1415$NEWTAXADDITIONALEAR + put1415$NEWTAXBASICINV + put1415$NEWTAXHIGHERINV + put1415$NEWTAXADDITIONALINV - put1415$TAX_CREDXX<0,
                                0,mround(put1415$NEWTAXBASICEAR + put1415$NEWTAXHIGHEREAR + put1415$NEWTAXADDITIONALEAR + put1415$NEWTAXBASICINV + put1415$NEWTAXHIGHERINV + put1415$NEWTAXADDITIONALINV - put1415$TAX_CREDXX,5))
put1415$NEWTAXTOTTAXZZ <-	put1415$NEWTAXTOTTAXXX - put1415$TOTTAXXX 

# New Tax Liability (after factor)
put1415$NEWTAXBASICEARFACT <-	(put1415$FACT*put1415$NEWTAXBASICEAR)/1000000
put1415$NEWTAXHIGHEREARFACT <-	(put1415$FACT*put1415$NEWTAXHIGHEREAR)/1000000
put1415$NEWTAXADDITIONALEARFACT <-	(put1415$FACT*put1415$NEWTAXADDITIONALEAR)/1000000
put1415$NEWTAXBASICINVFACT <-	(put1415$FACT*put1415$NEWTAXBASICINV)/1000000
put1415$NEWTAXHIGHERINVFACT <-	(put1415$FACT*put1415$NEWTAXHIGHERINV)/1000000
put1415$NEWTAXADDITIONALINVFACT <-	(put1415$FACT*put1415$NEWTAXADDITIONALINV)/1000000
put1415$NEWTAXTAX_CREDFACTXX <- 	(put1415$FACT*put1415$TAX_CREDXX)/1000000
put1415$NEWTAXTOTTAXFACTXX <-	ifelse(put1415$NEWTAXTOTTAXXX==0,0,put1415$NEWTAXBASICEARFACT + put1415$NEWTAXHIGHEREARFACT + put1415$NEWTAXADDITIONALEARFACT + put1415$NEWTAXBASICINVFACT + put1415$NEWTAXHIGHERINVFACT + put1415$NEWTAXADDITIONALINVFACT - put1415$NEWTAXTAX_CREDFACTXX)

# Scotland Tax Liability (after Composite Record allocation)
put1415$NEWTAXBASICEARFACTSCOT <-	(put1415$SCOTSHARE*put1415$NEWTAXBASICEARFACT)
put1415$NEWTAXHIGHEREARFACTSCOT <-	(put1415$SCOTSHARE*put1415$NEWTAXHIGHEREARFACT)
put1415$NEWTAXADDITIONALEARFACTSCOT <-	(put1415$SCOTSHARE*put1415$NEWTAXADDITIONALEARFACT)
put1415$NEWTAXBASICINVFACTSCOT <-	(put1415$SCOTSHARE*put1415$NEWTAXBASICINVFACT)
put1415$NEWTAXHIGHERINVFACTSCOT <-	(put1415$SCOTSHARE*put1415$NEWTAXHIGHERINVFACT)
put1415$NEWTAXADDITIONALINVFACTSCOT <-	(put1415$SCOTSHARE*put1415$NEWTAXADDITIONALINVFACT)
put1415$NEWTAXTAX_CREDFACTSCOTXX <- 	(put1415$SCOTSHARE*put1415$NEWTAXTAX_CREDFACTXX)
put1415$NEWTAXTOTTAXFACTSCOTXX <-	ifelse(put1415$NEWTAXTOTTAXXX==0,0,put1415$NEWTAXBASICEARFACTSCOT + put1415$NEWTAXHIGHEREARFACTSCOT + put1415$NEWTAXADDITIONALEARFACTSCOT + put1415$NEWTAXBASICINVFACTSCOT + put1415$NEWTAXHIGHERINVFACTSCOT + put1415$NEWTAXADDITIONALINVFACTSCOT - put1415$NEWTAXTAX_CREDFACTSCOTXX)

put1415$NEWTAXEARFACT <-	put1415$NEWTAXBASICEARFACT + put1415$NEWTAXHIGHEREARFACT + put1415$NEWTAXADDITIONALEARFACT 
put1415$NEWTAXINVFACT <-	put1415$NEWTAXBASICINVFACT + put1415$NEWTAXHIGHERINVFACT + put1415$NEWTAXADDITIONALINVFACT

put1415$EARFACT <-	put1415$BASICEARFACT + put1415$HIGHEREARFACT + put1415$ADDITIONALEARFACT 
put1415$INVFACT <-	put1415$BASICINVFACT + put1415$HIGHERINVFACT + put1415$ADDITIONALINVFACT

put1415$EARDIF <- put1415$NEWTAXEARFACT - put1415$EARFACT
put1415$INVDIF <- put1415$NEWTAXINVFACT - put1415$INVFACT
put1415$NEWTAXTOTTAXFACTZZ <- put1415$NEWTAXTOTTAXFACTXX - put1415$TOTTAXFACTXX

ScotlandFriction <- subset(put1415,put1415$GORCODE == 11|put1415$GORCODE == -1)

# Age Range  (1-Under 25, 2-25:34, 3-35:44, 4-45:54, 5-55:64, 6-65:74, 7-75 and over)
FricAgeTable <- tabular((factor(AGERANGE,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# Directorship (1-Director (Close Company), 2-Director (Not Close), 3-Not Director, 4-Director (PAYE))
FricDShipsTable <- tabular((factor(DSHIPS,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# Governemnt Office Region (1-North East, 2-North West, 3-Yorkshire, 4-East Midlands, 5-West Midlands, 6-East
#                            7- London, 8-South East, 9-South West, 10-Wales, 11-Scotland, 12-Northern Ireland
#                           13-Abroad, 14-Unknown)
tabular((factor(GORCODE,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# Income (1-0:31,865,  2-31,865:80,000, 3-80,001:150,000, 4-150,001:300,000, 5-300,001:500,000, 6-500,001-max)
FricIncomeTable <- tabular((factor(TIECategory,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)

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
FricIndustryTable <- tabular((factor(INDUSTRY07,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# Main Income Source (1-Pay, 2-Pension, 3-Sole Trader, 4-Partnership, 5-Other, 6-Claims)
FricSourceTable <- tabular((factor(MAINSRCE,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# Marginal Rate of Tax (ST-Standard Income, NS-Non-Standard Income, DV-Dividends, NONTP-Non Taxpayer
#                       BR-Basic Rate, HR-Higher Rate, LR-Additional Rate)
FricMarTable <- tabular((factor(MAR,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# Self Employed (-1-NA, 0-Not Self Employed, 1-Self Employed)
FricSelfTable <- tabular((factor(SEINC_NUM,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# Sex  (0-Not attributable, 1-Male, 2-Female)
FricSexTable <- tabular((factor(SEX,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# State Pension Age Indicator (-1-NA, 0-Non-State Pension Age, 1-State Pension Age)
FricPenTable <- tabular((factor(SPA,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)
# Taxpayer  (1-Taxpayer, 2-Non-Taxpayer)
tabular((factor(TAXPAYER,exclude=NULL)+1)~(FACTSCOT+TOTTAXFACTSCOTXX+NEWTAXTOTTAXFACTSCOTXX)*(sum),data=ScotlandFriction)

write.csv.tabular(FricAgeTable, file="FricAgeTable.csv")
write.csv.tabular(FricDShipsTable, file="FricDShipsTable.csv")
write.csv.tabular(FricIndustryTable, file="FricIndustryTable.csv")
write.csv.tabular(FricIncomeTable, file="FricIncomeTable.csv")
write.csv.tabular(FricSourceTable, file="FricSourceTable.csv")
write.csv.tabular(FricMarTable, file="FricMarTable.csv")
write.csv.tabular(FricSelfTable, file="FricSelfTable.csv")
write.csv.tabular(FricSexTable, file="FricSexTable.csv")
write.csv.tabular(FricPenTable, file="FricPenTable.csv")
