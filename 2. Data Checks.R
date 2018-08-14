# 2. Summary Stats 

# Pre-requisites: 1. Import and Fix
# This code produces a series of summaries of the checks comparing derived variables with 
# those found in the dataset

library(Hmisc)

# I. Checking UK Data
# Total Investment Income
describe(put1415$PF_TIIZZ)
# Total Earned Income
describe(put1415$PF_TEIZZ)
# Total Income
describe(put1415$PF_TIZZ)
# Taxable Income
describe(put1415$PF_TAXINCZZ)
# Tax Liability
describe(put1415$PF_TOTTAXZZ)

# Main Income Source (1-Pay, 2-Pension, 3-Sole Trader, 4-Partnership, 5-Other, 6-Claims)
table(put1415$PF_TOTTAXZZ,put1415$MAINSRCE)
# Directorship (1-Director (Close Company), 2-Director (Not Close), 3-Not Director, 4-Director (PAYE))
table(put1415$PF_TOTTAXZZ,put1415$DSHIPS)
# Governemnt Office Region (1-North East, 2-North West, 3-Yorkshire, 4-East Midlands, 5-West Midlands, 6-East
#                            7- London, 8-South East, 9-South West, 10-Wales, 11-Scotland, 12-Northern Ireland
#                           13-Abroad, 14-Unknown)
table(put1415$PF_TOTTAXZZ,put1415$GORCODE)
# Marginal Rate of Tax (ST-Standard Income, NS-Non-Standard Income, DV-Dividends, NONTP-Non Taxpayer
#                       BR-Basic Rate, HR-Higher Rate, LR-Additional Rate)
table(put1415$PF_TOTTAXZZ,put1415$MAR)
# Self Employed (-1-NA, 0-Not Self Employed, 1-Self Employed)
table(put1415$PF_TOTTAXZZ,put1415$SEINC_NUM)
# State Pension Age Indicator (-1-NA, 0-Non-State Pension Age, 1-State Pension Age)
table(put1415$PF_TOTTAXZZ,put1415$SPA)

# II. Checking Scotland Data
# Tax Liability
describe(Scotland$PF_TOTTAXZZ)

# Main Income Source (1-Pay, 2-Pension, 3-Sole Trader, 4-Partnership, 5-Other, 6-Claims)
table(Scotland$PF_TOTTAXZZ,Scotland$MAINSRCE)
# Directorship (1-Director (Close Company), 2-Director (Not Close), 3-Not Director, 4-Director (PAYE))
table(Scotland$PF_TOTTAXZZ,Scotland$DSHIPS)
# Governemnt Office Region (1-North East, 2-North West, 3-Yorkshire, 4-East Midlands, 5-West Midlands, 6-East
#                            7- London, 8-South East, 9-South West, 10-Wales, 11-Scotland, 12-Northern Ireland
#                           13-Abroad, 14-Unknown)
table(Scotland$PF_TOTTAXZZ,Scotland$GORCODE)
# Marginal Rate of Tax (ST-Standard Income, NS-Non-Standard Income, DV-Dividends, NONTP-Non Taxpayer
#                       BR-Basic Rate, HR-Higher Rate, LR-Additional Rate)
table(Scotland$PF_TOTTAXZZ,Scotland$MAR)
# Self Employed (-1-NA, 0-Not Self Employed, 1-Self Employed)
table(Scotland$PF_TOTTAXZZ,Scotland$SEINC_NUM)
# State Pension Age Indicator (-1-NA, 0-Non-State Pension Age, 1-State Pension Age)
table(Scotland$PF_TOTTAXZZ,Scotland$SPA)



