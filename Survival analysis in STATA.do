****NAME: OJALA BRIAN 
****SURVIVAL ANALYSIS 
************************************************
******CLEAR STATA MEMORY
clear
***********Set wd
cd "C:\CDK"
**************************************
********Load data set
use "C:\CDK\Cancer.dta"
**********Set data to survival data
stset studytime, failure(died) scale(1)
******************************************
stsum, by(status)
*******************************KAPLAN MEIR
sts graph, by(drug) title(`"Follow up of cancer patient over time"')
**********************************************************************