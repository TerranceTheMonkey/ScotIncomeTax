# ScotIncomeTax
Analysis of Taxable Income Elasticities in Scotland 

This code stored in this repositry relates to a paper written by Bruce Golding.  The paper is saved here as  "Measuring Behavioural Response to Changes in Income Tax Policy in Scotland"

The analysis contained in this paper uses data from the 2014/15 Survey of Personal incomes: Public Use Tapes (SPI).  The data is avaliable from the UK Data Service: http://doi.org/10.5255/UKDA-SN-8239-1

The code uses the SPI data in tabular format.  It also requires the creation of a file based on Annex B of the SPI.  This is required because the data from some strata had to be aggregated to maintain the privacy of the taxpayers involved.  See the release notes with the data for more on this.  The required Annex B file is a csv with three variables:  SREF, SCOTSHARE, RUKSHARE.  The SCOTSHARE and RUKSHARE variables are the share of each composite record listed in Annex B that is from Scotland and the rest of the UK. This file needs to be created and added to working directory for the first script to work and all the other scripts depend on it.  

There are 9 R scripts in this repositry.  They were orignally run on R version 3.2.1 (2015-06-18) -- "World-Famous Astronaut".  
1. Import and Fix
      Imports data from tabular file and csv (Annex B).  All the following scripts reqiue this to be run first.  
2. Data Checks
      
3. Summary Stats
4. Deductions
5. Naive Analysis
    Results in Section 4.2 of the paper
6. Income Analysis
    Results in Section 4.3 of the paper    
7. Gender Analysis
    Results in Section 4.4 of the paper
8. Director Analysis
    Results in Section 4.5 of the paper
9. Frictions Analysis
    Results in Section 4.6 of the paper

The following conventions apply
