libname surv "C:\Users\andre\Desktop\NCSU\AA 502\Survival Analysis\Homework 1";

/* Question 1 A */
/* Percentage of pumps survived */
/* 0.41038961 of 41.04 of pumps survived */
/* 0.58961039 or 58.96 of pumps failed */

/* Question 1 B */
/* Percentage of pumps in each type of failure */
/* Pump - 0		#316	41.04% */
/* Pump - 1		#115	14.94% */
/* Pump - 2		#112	14.55% */
/* Pump - 3		#111	14.42% */
/* Pump - 4		#116	15.06% */

proc freq data=surv.hurricane;
	tables reason reason2;
run;

/* Question 1 C */
/* Average failure time in each type of failure */
/* Pump - 1		#115	mean hour - 26.4434783 */
/* Pump - 2		#112	mean hour - 41.0446429 */
/* Pump - 3		#111	mean hour - 38.8288288 */
/* Pump - 4		#116	mean hour - 21.9396552 */

proc means data=surv.hurricane;
	class reason;
	var hour;
run;

/* Question 1 D */
/* Test for equal variance with Levene's Test */
/* Fail to reject - variances are not equal */
proc glm data=surv.hurricane plots=all;
    class reason;
    model hour=reason;
    means reason / hovtest=levene;
run;
quit;

/* Residuals are not normally distributed */
/* Histogram does not have a normal distribution */
/* All pumps are statistically significant between failure kind and failure time except pump 2 and 3 */
proc glm data=surv.hurricane plots=all;
    class reason;
    model hour=reason;
    lsmeans reason / pdiff=all 
                         adjust=tukey;
run;
quit;

/* Perform non-parametric test because assumptions were not met */
/* Statistically significant difference between failure kind and failure time */
proc npar1way data=surv.hurricane plots=all;
    class reason;
    var hour;
run;
quit;

/******************************************************************************************************/

/* Question 2 A */
/* Survival probablity */
proc lifetest data=surv.hurricane;
	time hour*survive(1);
run;

/* Question 2 B */
/* Survival probablity broken down by failure type */
proc lifetest data=surv.hurricane plots=s(cb=ep);
	time hour*survive(1);
	where reason > 0;
	strata reason;
run;

/* Question 2 C */
/* Conditional failure probabilities */
proc lifetest data=surv.hurricane method=life width=1 plots=hazard;
	time hour*survive(1);
	ods output LifetableEstimates = condprob;
run;

data condprob;
	set condprob;
	retain cum_sum;
	cum_sum + condprobfail;
run;

proc sgplot data = condprob;
	series x = lowertime y = condprobfail;
	xaxis label='Hour';
	title 'Hazard Probability Function';
run;
quit;

proc sgplot data = condprob;
	series x = lowertime y = cum_sum;
	xaxis label='Hour';
	yaxis label='Cumulative Hazard Probability';
	title 'Cumulative Hazard Probability Function';
run;
quit;

/* Question 2 D */
/* Conditional failure probabilities broken down by failure type */
proc lifetest data=surv.hurricane method=life width=1 plots=hazard;
	time hour*survive(1);
	where reason > 0;
	strata reason;
	ods output LifetableEstimates = condprob_reason;
run;

data condprob_reason;
	set condprob_reason;
	retain cum_sum;
	cum_sum + condprobfail;
run;

proc sgplot data = condprob_reason;
	series x = lowertime y = condprobfail;
	xaxis label='Hour';
	title 'Hazard Probability Function';
run;
quit;

proc sgplot data = condprob_reason;
	series x = lowertime y = cum_sum;
	xaxis label='Hour';
	yaxis label='Cumulative Hazard Probability';
	title 'Cumulative Hazard Probability Function';
run;
quit;

/******************************************************************************************************/

/* Question 3 */
data pumps;
	set surv.hurricane;
	if reason = 0 then pump_test = 0;
/* 	water-based failure */
	if reason = 1 then pump_test = 1;
	if reason = 2 then pump_test = 1;
/* 	mechanical failure */
	if reason = 3 then pump_test = 2;
	if reason = 4 then pump_test = 2;
run;

/* Statistical difference between water-based and mechanical failure survival probabilities
for both log-rank and Wilcoxon test  */
proc lifetest data=pumps plots=s(cb=ep);
	time hour*survive(1);
	where pump_test = 1 or pump_test = 2;
	strata pump_test / diff=all;
run;