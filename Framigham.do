**NAME: OJALA BRIAN OLOO
**DATA SET USED: Framingham heart study data set
** STATISTICAL DATA MANAGEMENT, ANALYSIS AND PRESENTATION
*** *******************************************************
*** CLEAR STATA MENU
clear
*** SETTING STATA WORKING DIRECTORY
cd "C:\CDK"
*======================================================================
*Import framingham data set
*==============================================================
import excel "C:\CDK\CDK.xlsx", sheet("framingham") firstrow
***=================================================================
** Destring numeric variable by ignoring the characters
***===========================================================
destring education, generate(Education) ignore(`"NA"', illegal)
destring cigsPerDay , generate( CigsPerDay ) ignore(`"NA"', illegal)
destring totChol , generate( TotChol ) ignore(`"NA"', illegal)
destring BPMeds , generate( BPMedS ) ignore(`"NA"', illegal)
destring BMI , generate( bMI ) ignore(`"NA"', illegal)
destring heartRate , generate( HeartRate ) ignore(`"NA"', illegal)
destring glucose , generate( Glucose ) ignore(`"NA"', illegal)
***=======================================================================
* Delete un used variables from the data set
*=========================================================================
drop education cigsPerDay BPMeds totChol BMI heartRate glucose 
*======================================================================
* Rename Male to Gender
codebook male
rename male Gender
*==================================================================
* Explore the  variables 
codebook CigsPerDay Education currentSmoker CigsPerDay BPMedS prevalentStroke prevalentHyp diabetes TotChol sysBP diaBP bMI HeartRate Glucose TenYearCHD 
**====================================================================================================================
* Variable label
label variable Gender "Sex"
label variable age "Patient age(years)"
label variable Education "Education Level"
label variable currentSmoker "Current Smokers"
label variable CigsPerDay "Smoking cigarrete per day"
label variable BPMedS "Medical blood pressure"
label variable prevalentStroke "Prevalent of stroke"
label variable prevalentHyp "Prevalence of hypertension"
label variable diabetes "Prevalence of diabetes"
label variable TotChol "Total cholesterol level"
label variable sysBP "Systolic blood pressure"
label variable diaBP "Diastolic blood pressure"
label variable bMI "Body mass index(Kg/m^2)"
label variable HeartRate "Heart rate"
label variable TenYearCHD "Chronic heart disease"
label variable prevalentHyp "Prevalence of hypertension"  
*==========================================================
* Explore CigsperDay using browse command
browse CigsPerDay
*========================================
* Mutate CigsPerDay to Cigper
gen Cigper=1 if CigsPerDay>=0 & CigsPerDay<=10
replace Cigper=2 if CigsPerDay>=11 & CigsPerDay<=20
replace Cigper=3 if CigsPerDay>=21 & CigsPerDay<=30
replace Cigper=4 if CigsPerDay>=31 & CigsPerDay<=40
replace Cigper=4 if CigsPerDay>=41
tab Cigper
*================================================
* Label Cigper
label variable Cigper "Cigarrete smokers per day"
*=====================================================
* Value label categorical variables from heart data set
label define gender 0"Female" 1"male"
label values Gender gender
label define education 1"Primary" 2"Secondary" 3"College" 4"University"
labe values Education education
label define currentsmoker 0"No" 1"Yes"
label values currentSmoker currentsmoker
label define cigper 1"0-10" 2"11-20" 3"21-30" 4"31-40" 5"41-70"
label values Cigper cigper
label define BPMeds 0"No" 1"Yes"
label values BPMedS BPMeds
label define PrevalentStroke 0"No" 1"Yes"
label values prevalentStroke PrevalentStroke
label define PrevalentHyp 0"No" 1"Yes"
label values prevalentHyp PrevalentHyp
label define Diabetes 0"No" 1"Yes"
label values diabetes Diabetes
label define tenYearCHD 0"No" 1"Yes"
label values TenYearCHD tenYearCHD
*================================================================
* Please note: The data set is full messy with missing values
* So, I follow the following procedures:
* I filter data with only male patients first
keep if Gender==1
* save the data set as framingham_male 
save "C:\CDK\Framigham_male.dta",replace 
* Keeping the columns without missing separate from columns with missing 
keep sysBP diaBP TenYearCHD currentSmoker diabetes prevalentStroke currentSmoker
* Save as Framigham_male_clean
save "C:\CDK\Framigham_male_clean.dta",replace
*===========================================================
*** Study framingham male clean data first
*===========================================================
* Performing some basic tabulation
tab currentSmoker
tab prevalentStroke
tab diabetes
sum sysBP,detail
sum diaBP,detail
tab TenYearCHD
*=============================================================- 
* DATA VIZUALIZATION IN STATA
***************************************************************
* Studying the health conditions associated with heart disease
** CurrentSmoker
** Study prevalence the prevalence of smoker by target chronic heart disease.
*The prevalence of cigarrete smoking is comparitively among
* male patient with chronic heart disease compared to non cases
graph pie, over(currentSmoker) pie(1, color(blue)) pie(2, color(red)) plabel(1 percent) plabel(2 percent) by(, title(`"Prevalence of cigarrete smokers "')) by(TenYearCHD)
//==============================================================
*** prevalentStroke
graph bar, over(prevalentStroke, label(labcolor("purple"))) bar(1, fcolor(purple)) bar(2, fcolor(lavender)) blabel(bar) by(, title(`"Prevalence of stroke among male patients"')) by(TenYearCHD)
***==============================================================
*** Diabetes
graph pie, over(diabetes) pie(1, color(green)) pie(2, color(purple)) plabel(1 percent) plabel(2 percent) by(, title(`"Prevalence of Diabetes among male patients"') caption(`"Framigham heart study"')) by(TenYearCHD) 
**=================================================================
* Distribution parterns of continuous variables
*===================================================================
**Studying the distribution pattern using histogram
** Patients without the diseases their systolic blood pressure skewed to right
histogram sysBP, bin(9) frequency fcolor(blue) lcolor(white) xtitle(`"Systolic blood pressure (mm/Hg)"') by(, title(`"Distribution of systolic blood pressure"')) by(TenYearCHD) 
*=============================================================
* Studying the distribution pattern using box plot in diastolic blood 
*pressure among smokers but stratified by chronic heart disease.
graph box diaBP, over(currentSmoker) box(1, fcolor(maroon)) by(, title(`"Distribution of diastolic blood pressure"')) by(TenYearCHD) 
*================================================================
*****INFERENTIAL STATISTICS*************************
** To reach conclusion that health conditions such as diabetes associated with heart disease, we must have some facts
** Hypothesis Testing
**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* 1: Measures of associations (Chi_Square test,Odds Ratio,Risk Ratio,Risk Difference)
* Ho: No association 
*        vs
* H1: There is association between the exposure and outcome variables 
* In this scenario, chronic heart disease was the independent variable while the remaining variables were the predictors.
***************************************************************
* Chi_Square distribution 
tab currentSmoker TenYearCHD,chi 
* Since P value>0.05, we fail to reject the null hypothesis
* Conclusion,No statistical relationship between Smoking cigrattes with chronic heart diseases
*=========================================================
* As epidemiologist or any other medical practioners we know very well that cigrattes smoking is amojor risk factor of heart disease but in this case there was no statical  relationship between the two variables. This could happened because of other factors like biases e.g Confounder variables (age)
* Now do deal with this, we can control the confouders by using these methods
* 1 Use Mantel-haenzel statistics to compute Common odds ratio but very limited to continuous variables
* 2.Use Strification methods or
* 3.Fit the logistic regression model. 
*================================================================
tab prevalentStroke TenYearCHD,chi 
* P-Value<0.05, reject null hypothesis, therefore we conclude that there was a significant statistical relationsip between stroke and heart diseases.
*=====================================================================
tab diabetes TenYearCHD,chi 
* P-value<0.05 , reject null hypothesis, conclusion: there was also a significant statistical relatioship between diabetes and heart disease.
*========================================================================
*PLEASE NOTE: This is a type of prospective cohort study design therefore we are going to prioritize on Risk ratio
* forming a 2 by 2 table using cigrate smoker and chronic heart disease 
* I used epidemiological calculator from stata to calcute Relative Risk since this was cohort study design
csi 224 882 119 595
***************************************************************************
* INTERPRETATION OF THE RESULTS FROM 2 BY 2 TABLE
****************************************************************************
* The risk of chronic heart disease was 1.09 times as high in current cigarrete smokers compared to nonsmokers over 10 years
*==========================================================================
*N/B Prospective cohort studies|Randomized Control Trials-Deals with incident data-> Risk ratio is the most appropriate.
* Again, Retrospective case-control|Cross sectional-Deals with prevalence data -> Compute Odd Ratios
*******************************************************************************************
* RECAP ON ADVANCED BIOSTATISTICS
*====================================================================================
*Now let's revisits some of the distributions in the Generalized Linear Model (GLM) and how to choose the right distribution to guide in statistical modelling.
* 1 binomial distribution--deals with binary outcome variable
* 2 gaussian (normal)  distribution----deals with continuos scale outcome variables
* 3 Poisson distribution-----used for count or rate data
* 4 Gamma distribution----deals with continuous skewed positive data
* 5 Negative Binomial-----deals with count data with overdispersion (variance greater than the mean)
* 6 Inverse Gaussian distribution------deals with continuous positive skewed data.
************************************************************************************
* In this analysis I use binomial distribution since the outcome variable was a binary variable. Therefore , I fitted the logistic regression model with the covariates as the predictors but remember binomial distribution belongs to exponential family, thus I exponentiated the coefficients of predictor variables in the model to come up with odds ratios.
* Logistic Regression Model
logistic TenYearCHD i.currentSmoker i.prevalentStroke i.diabetes sysBP diaBP
* Interpretation of the outputs
* The odds of developing chronic heart disease was 1.49 times (OR=1.49,95%Cl=1.13 1.90) as high in smokers compared to nonsmokers while patients with stroke were 3.59 times(OR=3.59;95%Cl=0.98, 13.15) more likely to develope this disease.On the same note, patients with diabetes were 2.9times(OR=2.9; 95%Cl=1.6,5.3)more likely to become ill compare to non diabetes patients. 
**************************END>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>





 










