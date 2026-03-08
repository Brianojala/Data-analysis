* NAME: OJALA BRIAN OLOO
* CADRE: CONSULTANT DATA ANALYST
* STATION: MASENO UNIVERSITY
*DATE : 7/3/2026
*==================================================
* Clear stata memory
clear
* Set stata working directory
cd "C:\CDK"
* Open data set
use "C:\CDK\one sided test.dta"
* Plot histogram to study distrion of days
histogram days, bin(20) fcolor(blue) lcolor(white) ytitle(`"Density"') xtitle(`"No of days "') title(`"Number of Days Until MRI for Subjects with MCLandACLT ears"')
* Test the normality Assumption of number of days
swilk days
* Summary statistics of no of days
sum days
* one sided Test
ttesti 17 13.29412 8.8654 15 
* Save data set in stata format dta
save "C:\CDK\one sided test.dta",replace
////////////////////////////////////////////////////////////////////////////////// 
 **********ANALYSIS OF VARIANCE (ANOVA)=====================================
 *=========================================================================
 * CLEAR STATA MEMORY
 clear
 * SET W/D
** Open data set 
use "C:\CDK\ANOVA_1.dta" 
************************************************************  
oneway Selenium Meat_type, bonferroni tabulate
*=====================================================
* Check word document for explaination but note the following
*Bartlett's Test
*1 Verify the assumption of the homogeinity of variances
* Data must follow a normal distribution if not the Levene's test is also preffered
****** Check the Bartlett's test P-value if p value<0.05 conclude that at least two variances are statistically different.

* I explained bonferroni on the word document 
****************************Thanks For your time*********************************
