# 1. Import and Fix

# Pre-requisites: None

# This code was written to analyse the 2014/15 Survey of Personal incomes: Public Use Tapes (SPI)
# The data is avalible from the UK Data Service:
# http://doi.org/10.5255/UKDA-SN-8239-1

# This code imports two files related to the SPI, the first is the complete dataset in tabular format
# the second is built from information in Annex B.  This is a csv with three variables:
# SREF, SCOTSHARE, RUKSHARE.  The SCOTSHARE and RUKSHARE variables are the share of each composite record 
# listed in Annex B that is from Scotland and the rest of the UK. This file needs to be created and added to 
# working directory for this code to work

# The rest of this code creates a series of variables required for further analysis

library(utils)

# I. Import original file and Annex B, merge to create main analysis file 'put1415'
put1415raw <- read.delim("~/R/EC929/put1415.tab")
annexb <- read.delim("~/R/EC929/annexb.csv",sep=",")

Comp <- subset(put1415raw,put1415raw$GORCODE == -1)
Single <- subset(put1415raw,put1415raw$GORCODE != -1)

CompComplete <- merge(Comp,annexb, by="SREF")

Single$SCOTSHARE <- ifelse(Single$GORCODE==11,1,0)
Single$RUKSHARE <- ifelse(Single$GORCODE==11,0,1)

put1415 <- rbind(Single,CompComplete)

# II. Create derived variables
# Investment Income
put1415$TIIXX <- put1415$MAINSRCE + put1415$OTHERINV + put1415$DIVIDENDS + put1415$INCPROP + put1415$INCBBS
put1415$TIIZZ <- put1415$TII - put1415$TIIXX
put1415$PF_TIIZZ <-	ifelse(abs(put1415$TIIZZ)>100, "FAIL", "PASS")

# Earned Income
put1415$TEIXX <- ((put1415$PAY + put1415$INCPBEN + put1415$OSSBEN + put1415$TAXTERM + put1415$UBISJA  + 
                     put1415$EPB + put1415$MOTHINC + put1415$OTHERINC) - put1415$EXPS) + 
                  (put1415$SRP + put1415$PENSION) + (put1415$PROFITS - (put1415$CAPALL + put1415$LOSSBF))
put1415$TEIZZ <- put1415$TEI - put1415$TEIXX
put1415$PF_TEIZZ  <-	ifelse(abs(put1415$TEIZZ)>100, "FAIL", "PASS")

# Total Income
put1415$TIXX <- put1415$TIIXX + put1415$TEIXX
put1415$TIZZ <- put1415$TI - put1415$TIXX
put1415$PF_TIZZ <-	ifelse(abs(put1415$TIZZ)>100, "FAIL", "PASS")

# Deductions
put1415$DEDUCT <- put1415$COVNTS + put1415$MOTHDED + put1415$GIFTAID + put1415$GIFTINV + put1415$DEFICIEN + put1415$PENSRLF
put1415$PER <- put1415$PAS + put1415$BPADUE
put1415$TAX_CREDXX <- 0.1*put1415$MCAS + put1415$TAX_CRED

# Taxable Income
put1415$TAXINCXX <- pmax(put1415$TI - put1415$DEDUCT - put1415$PER,0)
put1415$TAXINCZZ <-	put1415$TAXINC - put1415$TAXINCXX
put1415$PF_TAXINCZZ <-	ifelse(abs(put1415$TAXINCZZ)>100, "FAIL", "PASS")

put1415$ADJTAXINC <- put1415$TAXINCXX #+ put1415$PSAV_XS
put1415$TAXEARINC <- pmax(put1415$TII-put1415$DIVIDENDS+put1415$TEI - put1415$DEDUCT - put1415$PER,0)
put1415$TAXINVINC <- put1415$ADJTAXINC-put1415$TAXEARINC

# Income subject to each tax threshold
put1415$BASICEARINC <-	ifelse(put1415$TAXEARINC>31865,31865,put1415$TAXEARINC)
put1415$HIGHEREARINC <-	pmax(ifelse(put1415$TAXEARINC>150000,150000-31865,put1415$TAXEARINC-31865),0)
put1415$ADDITIONALEARINC <-	ifelse(put1415$TAXEARINC>150000,put1415$TAXEARINC-150000,0)
put1415$BASICINVINC <-	ifelse(put1415$BASICEARINC==31865,0,ifelse(31865-put1415$BASICEARINC<put1415$TAXINVINC,31865-put1415$BASICEARINC,put1415$TAXINVINC))
put1415$HIGHERINVINC <- ifelse(put1415$ADJTAXINC>31865,ifelse((put1415$BASICEARINC+put1415$HIGHEREARINC)==150000,0,ifelse(put1415$TAXINVINC>150000-
                   (put1415$BASICEARINC+put1415$HIGHEREARINC+put1415$BASICINVINC),150000-(put1415$BASICEARINC+
                    put1415$HIGHEREARINC+put1415$BASICINVINC),put1415$TAXINVINC-put1415$BASICINVINC)),0)
put1415$ADDITIONALINVINC <-	put1415$ADJTAXINC - put1415$BASICEARINC - put1415$HIGHEREARINC-
  put1415$ADDITIONALEARINC - put1415$BASICINVINC - put1415$HIGHERINVINC

mround <- function(x,base){ 
  base*round(x/base) 
} 

# Tax liability (no factor) - work out the tax liability for each record
put1415$BASICEAR <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.20*put1415$BASICEARINC)
put1415$HIGHEREAR <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.40*put1415$HIGHEREARINC)
put1415$ADDITIONALEAR <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.45*put1415$ADDITIONALEARINC)
put1415$BASICINV <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.10*put1415$BASICINVINC)
put1415$HIGHERINV <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.325*put1415$HIGHERINVINC)
put1415$ADDITIONALINV <-	ifelse(put1415$MAR=="NONTP"|put1415$TAXPAYER==2,0,0.375*put1415$ADDITIONALINVINC)
put1415$TOTTAXXX <-	ifelse(put1415$BASICEAR + put1415$HIGHEREAR + put1415$ADDITIONALEAR + put1415$BASICINV + put1415$HIGHERINV + put1415$ADDITIONALINV - put1415$TAX_CREDXX<0,
                           0,mround(put1415$BASICEAR + put1415$HIGHEREAR + put1415$ADDITIONALEAR + put1415$BASICINV + put1415$HIGHERINV + put1415$ADDITIONALINV - put1415$TAX_CREDXX,5))
put1415$TOTTAXZZ <-	put1415$TOTTAX - put1415$TOTTAXXX
put1415$PF_TOTTAXZZ <-	ifelse(abs(put1415$TOTTAXZZ)>100, "FAIL", "PASS")

# Tax Liability (after factor) - scale up records to be representative of population
put1415$BASICEARFACT <-	(put1415$FACT*put1415$BASICEAR)/1000000
put1415$HIGHEREARFACT <-	(put1415$FACT*put1415$HIGHEREAR)/1000000
put1415$ADDITIONALEARFACT <-	(put1415$FACT*put1415$ADDITIONALEAR)/1000000
put1415$BASICINVFACT <-	(put1415$FACT*put1415$BASICINV)/1000000
put1415$HIGHERINVFACT <-	(put1415$FACT*put1415$HIGHERINV)/1000000
put1415$ADDITIONALINVFACT <-	(put1415$FACT*put1415$ADDITIONALINV)/1000000
put1415$TAX_CREDFACTXX <- (put1415$FACT*put1415$TAX_CREDXX)/1000000
put1415$TOTTAXFACTXX <-	ifelse(put1415$TOTTAXXX==0,0,put1415$BASICEARFACT + put1415$HIGHEREARFACT + put1415$ADDITIONALEARFACT + put1415$BASICINVFACT + put1415$HIGHERINVFACT + put1415$ADDITIONALINVFACT - put1415$TAX_CREDFACTXX)
put1415$TOTTAXFACT <- (put1415$TOTTAX*put1415$FACT)/1000000

# Scotland Tax Liability (after Composite Record allocation) - allocate Scottish share of composite records
put1415$BASICEARFACTSCOT <-	(put1415$SCOTSHARE*put1415$BASICEARFACT)
put1415$HIGHEREARFACTSCOT <-	(put1415$SCOTSHARE*put1415$HIGHEREARFACT)
put1415$ADDITIONALEARFACTSCOT <-	(put1415$SCOTSHARE*put1415$ADDITIONALEARFACT)
put1415$BASICINVFACTSCOT <-	(put1415$SCOTSHARE*put1415$BASICINVFACT)
put1415$HIGHERINVFACTSCOT <-	(put1415$SCOTSHARE*put1415$HIGHERINVFACT)
put1415$ADDITIONALINVFACTSCOT <-	(put1415$SCOTSHARE*put1415$ADDITIONALINVFACT)
put1415$TAX_CREDFACTSCOTXX <-	(put1415$SCOTSHARE*put1415$TAX_CREDFACTXX) 
put1415$TOTTAXFACTSCOTXX <-	ifelse(put1415$TOTTAXXX==0,0,put1415$BASICEARFACTSCOT + put1415$HIGHEREARFACTSCOT + put1415$ADDITIONALEARFACTSCOT + put1415$BASICINVFACTSCOT + put1415$HIGHERINVFACTSCOT + put1415$ADDITIONALINVFACTSCOT - put1415$TAX_CREDFACTSCOTXX)

put1415$FACTSCOT <- 	(put1415$SCOTSHARE*put1415$FACT)
put1415$TOTTAXFACTSCOT <- 	(put1415$SCOTSHARE*put1415$TOTTAXFACT)

# RUK Tax Liability (after Composite Record allocation) - allocate RUK share of composite records
put1415$BASICEARFACTRUK <-	(put1415$RUKSHARE*put1415$BASICEARFACT)
put1415$HIGHEREARFACTRUK <-	(put1415$RUKSHARE*put1415$HIGHEREARFACT)
put1415$ADDITIONALEARFACTRUK <-	(put1415$RUKSHARE*put1415$ADDITIONALEARFACT)
put1415$BASICINVFACTRUK <-	(put1415$RUKSHARE*put1415$BASICINVFACT)
put1415$HIGHERINVFACTRUK <-	(put1415$RUKSHARE*put1415$HIGHERINVFACT)
put1415$ADDITIONALINVFACTRUK <-	(put1415$RUKSHARE*put1415$ADDITIONALINVFACT)
put1415$TAX_CREDFACTRUKXX <-	(put1415$RUKSHARE*put1415$TAX_CREDFACTXX) 
put1415$TOTTAXFACTRUKXX <-	ifelse(put1415$TOTTAXXX==0,0,put1415$BASICEARFACTRUK + put1415$HIGHEREARFACTRUK + put1415$ADDITIONALEARFACTRUK + put1415$BASICINVFACTRUK + put1415$HIGHERINVFACTRUK + put1415$ADDITIONALINVFACTRUK - put1415$TAX_CREDFACTRUKXX)

put1415$FACTRUK <- 	(put1415$RUKSHARE*put1415$FACT)
put1415$TOTTAXFACTRUK <- 	(put1415$RUKSHARE*put1415$TOTTAXFACT)

# Define TIE Income Cateogories (1-0:31,865,  2-31,865:80,000, 3-80,001:150,000, 4-150,001:300,000, 5-300,001:500,000, 6-500,001-max)
put1415$TIECategory <- ifelse(put1415$ADJTAXINC<31865,1,
                              ifelse(put1415$ADJTAXINC<80001,2,
                                     ifelse(put1415$ADJTAXINC<150001,3,
                                            ifelse(put1415$ADJTAXINC<300001,4,
                                                   ifelse(put1415$ADJTAXINC<500001,5,6)))))

# Subset the data for  analysis
Scotland <- subset(put1415,put1415$GORCODE == 11|put1415$GORCODE == -1)
RUK <- subset(put1415,put1415$GORCODE != 11)

# Identify errors in comparison of TOTTAX and TOTTAXXX for Scotland i.e. decompostion of taxable liability
ScotlandErrors <- subset(Scotland,Scotland$PF_TOTTAXZZ == "FAIL")
write.csv(ScotlandErrors,"errors.csv")