# 4. Deductions 

# Pre-requisites: 1. Import and Fix
# This code weights up the indivdual deductions, exemptions and tax credits to be nationally
# representative.  It then produces some summary tables and exports them to csv.

# Deductions and Exemptions (after factor)
Scotland$COVNTSFACT <-	(Scotland$FACT*Scotland$COVNTS)/1000000
Scotland$MOTHDEDFACT <-	(Scotland$FACT*Scotland$MOTHDED)/1000000
Scotland$GIFTAIDFACT <-	(Scotland$FACT*Scotland$GIFTAID)/1000000
Scotland$GIFTINVFACT <-	(Scotland$FACT*Scotland$GIFTINV)/1000000
Scotland$DEFICIENFACT <-	(Scotland$FACT*Scotland$DEFICIEN)/1000000
Scotland$PENSRLFFACT <-	(Scotland$FACT*Scotland$PENSRLF)/1000000
Scotland$PASFACT <-	(Scotland$FACT*Scotland$PAS)/1000000
Scotland$BPADUEFACT <-	(Scotland$FACT*Scotland$BPADUE)/1000000
Scotland$MCASFACT <-	(Scotland$FACT*Scotland$MCAS*0.1)/1000000
Scotland$TAX_CREDFACT <-	(Scotland$FACT*Scotland$TAX_CRED)/1000000
Scotland$PSAV_XSFACT <-	(Scotland$FACT*Scotland$PSAV_XS)/1000000
Scotland$TIFACTXX <-	(Scotland$FACT*Scotland$TIXX)/1000000
Scotland$TAXINCFACTXX <-	(Scotland$FACT*Scotland$TAXINCXX)/1000000

#  Deductions and Exemptions (after Composite Record allocation)
Scotland$COVNTSFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$COVNTSFACT)
Scotland$MOTHDEDFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$MOTHDEDFACT)
Scotland$GIFTAIDFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$GIFTAIDFACT)
Scotland$GIFTINVFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$GIFTINVFACT)
Scotland$DEFICIENFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$DEFICIENFACT)
Scotland$PENSRLFFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$PENSRLFFACT)
Scotland$PASFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$PASFACT)
Scotland$BPADUEFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$BPADUEFACT)
Scotland$MCASFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$MCASFACT)
Scotland$TAX_CREDFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$TAX_CREDFACT)
Scotland$PSAV_XSFACTSCOT <-	(Scotland$SCOTSHARE*Scotland$PSAV_XSFACT)
Scotland$TIFACTSCOTXX <-	(Scotland$SCOTSHARE*Scotland$TIFACTXX)
Scotland$TAXINCFACTSCOTXX <-	(Scotland$SCOTSHARE*Scotland$TAXINCFACTXX)

Scotland$NoCreditsTAXFACTSCOTXX <-	ifelse(Scotland$TOTTAXXX==0,0,Scotland$BASICEARFACTSCOT + Scotland$HIGHEREARFACTSCOT + Scotland$ADDITIONALEARFACTSCOT + Scotland$BASICINVFACTSCOT + Scotland$HIGHERINVFACTSCOT + Scotland$ADDITIONALINVFACTSCOT)

# Create new data sets
SuperRich <- subset(Scotland,Scotland$TIECategory ==6)
ScotlandDeductions <- subset(Scotland,Scotland$TOTTAXFACTSCOTXX != 0)

# Create Tables for Deductions
Deduct <- tabular((factor(TIECategory)+1)~(FACT+FACTSCOT+COVNTSFACTSCOT+MOTHDEDFACTSCOT+GIFTAIDFACTSCOT+GIFTINVFACTSCOT+DEFICIENFACTSCOT+PENSRLFFACTSCOT)*(sum),data=ScotlandDeductions)
Allowance <-tabular((factor(TIECategory)+1)~(FACT+FACTSCOT+PASFACTSCOT+BPADUEFACTSCOT+TIFACTSCOTXX+TAXINCFACTSCOTXX+NoCreditsTAXFACTSCOTXX+TOTTAXFACTSCOTXX)*(sum),data=ScotlandDeductions)
TaxCred <-tabular((factor(TIECategory)+1)~(FACT+FACTSCOT+MCASFACTSCOT+TAX_CREDFACTSCOT+PSAV_XSFACTSCOT)*(sum),data=ScotlandDeductions)

# Output csvs of tables for analysis
write.csv.tabular(Deduct, file="Deduct.csv")
write.csv.tabular(Allowance, file="Allowance.csv")
write.csv.tabular(TaxCred, file="TaxCred.csv")
write.csv(SuperRich, file="SuperRich.csv")