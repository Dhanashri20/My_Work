/*Imported the data */
data A1; set w1;
proc contents data=A1;
run;
/*Finding the missing for numerical values*/
proc means data=A1 nmiss;
run;
/*Dropping the missing for numeric values*/
data A2(drop= crtcoun adults income numbcars educ1 retdays lor crtcount rmcalls rmmou rmrev tot_ret tot_acpt pre_hnd_price last_swap);
set A1;
run;
proc print data=A2(obs=5);
proc means data=A2 nmiss;
run;

/* Finding the missing values for categorical values */
proc format;
value $missfmt ''='Missing' other='Not Missing';
run;

/*dropping categorical columns*/
data A3;
set A2;
drop ref_qty div_type occu1 ownrent dwlltype mailordr wrkwoman mailresp children infobase cartype HHstatin mailflag solflag dwllsize proptype pcowner;
run;
proc print data=A3(obs=5);
run;
proc means data=A3 nmiss;
run;
proc contents data=A3;run;

PROC SQL;
CREATE TABLE A4 AS
SELECT *
FROM A3
WHERE rev_Mean IS NOT NULL AND mou_Mean IS NOT NULL AND totmrc_Mean IS NOT NULL AND da_Mean IS NOT NULL AND ovrmou_Mean IS NOT NULL AND ovrrev_Mean IS NOT NULL AND vceovr_Mean IS NOT NULL AND datovr_Mean IS NOT NULL AND roam_Mean IS NOT NULL AND rev_Range IS NOT NULL AND mou_Range IS NOT NULL AND totmrc_Range IS NOT NULL AND da_Range IS NOT NULL AND ovrmou_Range IS NOT NULL AND ovrrev_Range
IS NOT NULL AND vceovr_Range 
IS NOT NULL AND datovr_Range 
IS NOT NULL AND roam_Range 
IS NOT NULL AND change_mou 
IS NOT NULL AND change_rev
IS NOT NULL AND avg6mou 
IS NOT NULL AND avg6qty 
IS NOT NULL AND avg6rev 
IS NOT NULL AND hnd_price 
IS NOT NULL AND phones 
IS NOT NULL AND models 
IS NOT NULL AND truck 
IS NOT NULL AND mtrcycle 
IS NOT NULL AND rv 
IS NOT NULL AND age1 
IS NOT NULL AND age2 
IS NOT NULL AND forgntvl 
IS NOT NULL AND eqpdays
IS NOT NULL AND area
IS NOT NULL AND asl_flag
IS NOT NULL AND car_buy
IS NOT NULL AND crclscod
IS NOT NULL AND creditcd
IS NOT NULL AND csa
IS NOT NULL AND ethnic
IS NOT NULL AND hnd_webcap
IS NOT NULL AND kid0_2
IS NOT NULL AND kid11_15
IS NOT NULL AND kid16_17
IS NOT NULL AND kid3_5
IS NOT NULL AND kid6_10
IS NOT NULL AND marital
IS NOT NULL AND new_cell
IS NOT NULL AND prizm_social_one
IS NOT NULL AND refurb_new;
quit; 
proc print data=A4(obs=5);
run;
proc contents data=A4;run;
proc means data=A4 nmiss;run;

/* Encoding the categorical variables*/
proc freq data=A4;
tables area asl_flag car_buy crclscod creditcd csa dualband ethnic hnd_webcap kid0_2 kid11_15 kid16_17 kid3_5 kid6_10 marital new_cell prizm_social_one refurb_new; 
run;

proc format;
value $encode
N=0
Y=1
U=2
T=2
R=1
A=0
B=1
M=3
S=4
NA=0
WC=1
WCMB=2
New=0
UNKNOWN=1;
run;
proc format;
value $encodee
U=0
Y=1;
run;
proc format;
value $encodeee
C=0
R=1
S=2
T=3
U=4;
run;
proc format;
value $encodeeee
B=0
C=1
D=2
F=3
G=4
H=5
I=6
J=7
M=8
N=9
O=10
P=11
R=12
S=13
U=14
X=15
Z=16;
run;
data encoding;
set A4;
encode_asl_flag=put(asl_flag,$encode.);
encode_new_cell=put(new_cell,$encode.);
enocde_dualband=put(dualband,$encode.);
encode_refurb_new=put(refurb_new,$encode.);
encode_creditcd=put(creditcd,$encode.);
encode_marital=put(marital,$encode.);
encode_hnd_webcap=put(hnd_webcap,$encode.);
encode_car_buy=put(car_buy,$encode.);
encode_kid0_2=put(kid0_2,$encodee.);
encode_kid11_15=put(kid11_15,$encodee.);
encode_kid16_17=put(kid16_17,$encodee.);
encode_kid6_10=put(kid6_10,$encodee.);
encode_kid3_5=put(kid3_5,$encodee.);
encode_prizm_social_one=put(prizm_social_one,$encodeee.);
encode_ethnic=put(ethnic,$encodeeee.);
run;
data x(drop=asl_flag new_cell dualband refurb_new creditcd marital hnd_webca car_buy kid0_2 kid11_15 kid16_17 kid6_10 kid3_5 prizm_social_one ethnic
encode_asl_flag encode_new_cell enocde_dualband encode_refurb encode_creditcd encode_marital encode_hnd_webcap encode_car_buy encode_kid0_2
encode_kid11_15 encode_kid16_17 encode_kid6_10 encode_kid3_5 encode_prizm_social_one encode_ethnic encode_refurb_new area crclscod csa);
set encoding;
num_asl_flag= input(encode_asl_flag,8.);
num_new_cell=input(encode_new_cell,8.);
num_dualband=input(encode_dualband,8.);
num_refurb_new=input(encode_refurb_new,8.);
num_creditcd=input(encode_creditcd,8.);
num_marital=input(encode_marital,8.);
num_hnd_webcap=input(encode_hnd_webcap,8.);
num_car_buy=input(encode_car_buy,8.);
num_kid0_2=input(encode_kid0_2,8.);
num_kid11_15=input(encode_kid11_15,8.);
num_kid16_17=input(encode_kid16_17,8.);
num_kid6_10=input(encode_kid6_10,8.);
num_kid3_5=input(encode_kid3_5,8.);
num_prizm_social_one=input(encode_prizm_social_one,8.);
num_ethnic=input(encode_prizm_social_one,8.);
run;
proc contents data=x;run;

/* Grouping according to churned and non churned */
data churned;
set x;
where churn=1;
run;

data not_churned;
set x;
where churn=0;
run;
proc means data= churned mean noprint;
output out=means_churned mean= /autoname;
run;

proc means data=not_churned mean noprint;
output out=means_not_churned mean= /autoname;
run;

data merged;
merge means_churned means_not_churned;
by rev_Mean_Mean;
run;

proc transpose data=merged out=transpose_merged;
run;
proc print data=transpose_merged(obs=10);run;
proc sql;create table percent_diff as select _NAME_,(((col1-col2)/col1)*100) as percent_diff from transpose_merged;quit;

proc sort data= percent_diff;
by descending percent_diff;
run;

data abs_percent_diff;
set percent_diff;
if _NAME_="churn_Mean" then delete;
percent_diff = abs(percent_diff);
proc print data = abs_percent_diff;run;

proc sort data= abs_percent_diff;
by descending percent_diff;
run;

/* Check the group percent*/
proc freq data = churned;
table churn; run;

/*Sampling training data*/
proc surveyselect data = x out = training sampsize = 70000 seed = 13571;run;
proc logistic data = training descending outmodel = log_model;
model churn = iwylis_vce_Mean rev_Mean mou_Mean callfwdv_Mean num_asl_flag drop_dat_Mean 
threeway_Mean custcare_Mean cc_mou_Mean  
recv_sms_Range callwait_Mean 
roam_Mean totmrc_Range;
run;
proc logistic inmodel = log_model ;
score data=training(drop = churn) out = log_predict_train;
run;
data cm_model;
merge training(IN=aa)
log_predict_train(IN=bb);if aa AND bb;
by Customer_ID;
run; 
proc freq data = cm_model;
table I_churn*churn; run;


proc sql;
create table holdout as select * from x except all
select * from training;
quit;
proc logistic inmodel = log_model ;
score data=holdout(drop = churn) out = log_predict;
run;
data predicted_probs;
  set holdout;
  predicted_prob = predict_proba;run;
 data binary_predictions;
  set predicted_probs;
  if predicted_prob >= 0.5 then binary_pred = 1;
  else binary_pred = 0;
run;
proc freq data=binary_predictions;
  tables churn*binary_pred ;run;
