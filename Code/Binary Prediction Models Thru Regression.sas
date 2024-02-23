*0. Read in data;
PROC IMPORT OUT=df
    DATAFILE="C:\Users\alwestb\OneDrive - Emory University\Documents\Conferences\CSP 2024\Presenter stuff\Presentation\Example Data_2.csv"
    DBMS=CSV
    REPLACE;
    GETNAMES=YES;
RUN;

***********************************************************************************
*																				  *
*                            Variable Selection									  *
*																				  *
***********************************************************************************

*1a. Stepwise regression - Forward; 
proc hpgenselect data=df;
   class alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight;
   model mortality(event='Yes') = crp oxygen temp alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight
   	/ dist=Binomial;
   selection method=forward; *default version;
run;

proc hpgenselect data=df;
   class alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight;
   model mortality(event='Yes') = crp oxygen temp alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight
   	/ dist=Binomial;
   selection method=forward (stop=AIC); *stopping by AIC;
run;
	/*Dictionary:
		method=forward -> specifies that we are using the forward selection method
		stop=AIC -> specifies the stop criterion which is AIC in this case. 
			options: SL = by 0.05 significance level, AIC or AICC or BIC = as it sounds*/

*1b. Stepwise regression - Backward; 
proc hpgenselect data=df;
   class alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight;
   model mortality(event='Yes') = crp oxygen temp alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight
   	/ dist=Binomial;
   selection method=backward; 
run;

*1c. Stepwise regression - Bidirectional; 
proc hpgenselect data=df;
   class alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight;
   model mortality(event='Yes') = crp oxygen temp alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight
   	/ dist=Binomial;
   selection method=stepwise; 
run;

*2. Best subset;
proc freq data=df; table insurance; run;
	*data processing since the next step requires dummified variables;
data trial; set df;
if weight="Normal" then weight_Normal=1;
	else weight_Normal=0;
if weight="Over" then weight_Over=1;
	else weight_Over=0;
if weight="Under" then weight_Under=1;
	else weight_Under=0;
if educ="Less than high school" then educ_lthigh=1;
	else educ_lthigh=0;
if educ="High school" then educ_high=1;
	else educ_high=0;
if educ="College or more" then educ_coll=1;
	else educ_coll=0;
if dispo="Transfer" then dispo_transfer=1;
	else dispo_transfer=0;
if dispo="Left AMA" then dispo_ama=1;
	else dispo_ama=0;
if dispo="Discharged" then dispo_disch=1;
	else dispo_disch=0;
if insurance="Private" then insurance_priv=1;
	else insurance_priv=0;
if insurance="Public" then insurance_public=1;
	else insurance_public=0;
if insurance="Self-pay" then insurance_none=1;
	else insurance_none=0;
run;

proc logistic data=trial;
   class alert cancer dist_100 infection insurance para_gt2 prev_admiss sex weight_Over weight_Under educ_lthigh educ_high dispo_transfer dispo_ama insurance_priv insurance_public / param=ref;
   model mortality(event='Yes') = crp oxygen temp alert cancer dist_100 infection para_gt2 prev_admiss sex weight_Over weight_Under educ_lthigh educ_high dispo_transfer dispo_ama insurance_priv insurance_public 
	/ selection=score best=1;
run;
		/*Dictionary:
		selection=score -> specifies that we are using the best subset method
		best=1 -> this means that we only want to see 1 model for each K number of predictor models*/

*3a. Regularization techniques - LASSO;
proc hpgenselect data=df;
   class alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight;
   model mortality(event='Yes') = crp oxygen temp alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight
   	/ dist=Binomial;
   selection method=LASSO(choose=AIC); 
run;

	/* At this time, there are no SAS procedures for ridge and elastic net with a binary outcome. 
	There are procedures for continuous outcomes so hopefully it is expanded to binary outcomes soon.*/

***********************************************************************************
*																				  *
*                            Validation Techniques								  *
*																				  *
***********************************************************************************

*4a. Split validation - Random;
proc hpgenselect data=df;
   class alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight;
   model mortality(event='Yes') = crp oxygen temp alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight
   	/ dist=Binomial;
	partition fraction(test=.30 seed=12345); *this section will do a random split of the data;
	selection method=forward; 
run;
		/*Dictionary:
		partition -> this invokes that we will be splitting the dataset
		fraction(test=.30 seed=12345) -> this means that we are splitting the data so that 30% is in the test dataset and the remaining is in the training dataset. Seed is for consistent results*/

*4b. Split validation - Non-random;
proc hpgenselect data=df;
   class alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight;
   model mortality(event='Yes') = crp oxygen temp alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight
   	/ dist=Binomial;
	partition ROLEVAR=splitting_variable(TEST="No" TRAIN="Yes"); *this section will do a random split of the data;
	selection method=forward; 
run;
		/*Dictionary:
		partition -> this invokes that we will be splitting the dataset
		ROLEVAR=splitting_variable -> this is where you identify the variable that determines who is going into training and testing (your non-random split)
		TEST="No" TRAIN="Yes" -> specifies the values that align with testing and training
			Note: if you randomly assigned your data beforehand through some other function, you could use this method to identify the variable that designates their assignments*/

	/* At this time, there are no SAS procedures for cross-validation and bootstrapping. 
	There are some user made macros but these depend on understanding the macros and arrays used to be able to tweak it to your needs.
	If you have the patience to work with SAS macros/arrays and want to do cross-validation, I recommend you go online and follow other's examples that 
		most closely approximates what you want out of it and then tweak it for yourself.*/

***********************************************************************************
*																				  *
*                            Performance Measures								  *
*																				  *
***********************************************************************************

* ROC and AUC;
proc logistic data=df plots(only)=roc;
   class alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight / param=ref;
   model mortality(event='Yes') = crp oxygen temp alert cancer dispo dist_100 educ infection insurance para_gt2 prev_admiss sex weight ;
	ods output roccurve=ROCdata;
 run;

		/*Dictionary:
		plots(only)=roc -> this produces the roc plot
		ods output roccurve=ROCdata -> this outputs the individuals sensitivities and specificities which we can use for finding optimal thresholds 
 			and for getting diagnostic accuracy measures*/

* Diagnostic accuracy;
proc print data=ROCdata (obs=10); run; *to glimpse the output;

proc freq data=df; table mortality; run; *to get the prevalence of the outcome;

data finding_optimal; set ROCdata;
youdens=_SENSIT_+(1-_1MSPEC_)-1; *to calculate youden's index;
ppv=(_SENSIT_ * 0.1993) / ( (_SENSIT_ * 0.1993) + ((1 - (1-_1MSPEC_)) * (1 - 0.1993)) ); *to calculate positive predictive value - replace 0.1993 with the prevalence of your outcome in your dataset;
npv=((1-_1MSPEC_) * (1 - 0.1993)) / ( ((1-_1MSPEC_) * (1 - 0.1993)) + ((1 - _SENSIT_) * 0.1993) ); *to calculate negative predictive value - replace 0.1993 with the prevalence of your outcome in your dataset;
lr_pos=_SENSIT_/(1-(1-_1MSPEC_)); *to calculate positive likelihood ratio;
lr_neg=(1-_SENSIT_)/(1-_1MSPEC_); *to calculate negative likelihood ratio;
run;

proc sort data=finding_optimal; by descending youdens; run; *we want the ones with the highest youden's index;

proc print data=finding_optimal (obs=5); run;
