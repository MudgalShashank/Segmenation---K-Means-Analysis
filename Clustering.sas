libname Shashank '/folders/myfolders/sample dataset';

/* PLEASE NOTE THAT THE VARIABLE "PURCHASE_TYPE" WAS INITIALLY CREATED IN THE CC_GENERAL 
EXCEL FILE AND AFTER IMPORTING, THE DUMMY VARIABLES WERE CREATED USING SAS CODES. 
PLEASE REFER TO THE CC_GENERAL EXCEL FILE FOR DETAILED INSIGHT.*/

Proc import datafile= '/folders/myfolders/sample dataset/Final Case Study 1 - Credit Card Segmentation/
CC GENERAL.csv'
out=CC_SEGMENTATION dbms=csv replace;
getnames= yes;
run;

proc contents data=cc_segmentation varnum;
run;

/*Calculating means and STD to identify outliers*/
ods html file='/folders/myfolders/sample dataset/Final Case Study 1 - Credit Card Segmentation/
Outliers.xls';
proc means data=CC_Segmentation N Nmiss min P5 mean std p95 max var;
var
BALANCE	
BALANCE_FREQUENCY	
PURCHASES	
ONEOFF_PURCHASES
INSTALLMENTS_PURCHASES
CASH_ADVANCE
PURCHASES_FREQUENCY
ONEOFF_PURCHASES_FREQUENCY
PURCHASES_INSTALLMENTS_FREQUENCY
CASH_ADVANCE_FREQUENCY
CASH_ADVANCE_TRX
PURCHASES_TRX	
CREDIT_LIMIT
PAYMENTS	
MINIMUM_PAYMENTS
PRC_FULL_PAYMENT
TENURE	;
run;
ods html close;


/*MEAN IMPUTATION */

data CC_SEGMENTATION;
set CC_SEGMENTATION;
if MINIMUM_PAYMENTS = . then MINIMUM_PAYMENTS = 864.2065423;
if CREDIT_LIMIT = . then CREDIT_LIMIT = 4494.45;
run;

/*Capping Outliers*/

data CC_SEGMENTATION;
set CC_SEGMENTATION;

if	BALANCE	>	5911.51	then	BALANCE	=	5911.51	;
if	BALANCE_FREQUENCY	>	1	then	BALANCE_FREQUENCY	=	1	;
if	PURCHASES	>	3999.92	then	PURCHASES	=	3999.92	;
if	ONEOFF_PURCHASES	>	2675	then	ONEOFF_PURCHASES	=	2675	;
if	INSTALLMENTS_PURCHASES	>	1753.08	then	INSTALLMENTS_PURCHASES	=	1753.08	;
if	CASH_ADVANCE	>	4653.69	then	CASH_ADVANCE	=	4653.69	;
if	PURCHASES_FREQUENCY	>	1	then	PURCHASES_FREQUENCY	=	1	;
if	ONEOFF_PURCHASES_FREQUENCY	>	1	then	ONEOFF_PURCHASES_FREQUENCY	=	1	;
if	PURCHASES_INSTALLMENTS_FREQUENCY	>	1	then	PURCHASES_INSTALLMENTS_FREQUENCY	=	1	;
if	CASH_ADVANCE_FREQUENCY	>	0.583333	then	CASH_ADVANCE_FREQUENCY	=	0.583333	;
if	CASH_ADVANCE_TRX	>	15	then	CASH_ADVANCE_TRX	=	15	;
if	PURCHASES_TRX	>	57	then	PURCHASES_TRX	=	57	;
if	CREDIT_LIMIT	>	12000	then	CREDIT_LIMIT	=	12000	;
if	PAYMENTS	>	6083.43	then	PAYMENTS	=	6083.43	;
if	MINIMUM_PAYMENTS	>	2722.22	then	MINIMUM_PAYMENTS	=	2722.22	;
if	PRC_FULL_PAYMENT	>	1	then	PRC_FULL_PAYMENT	=	1	;
if	TENURE	>	12	then	TENURE	=	12	;
if	BALANCE	<	8.80954	then	BALANCE	=	8.80954	;
if	BALANCE_FREQUENCY	<	0.272727	then	BALANCE_FREQUENCY	=	0.272727	;
if	CREDIT_LIMIT	<	1000	then	CREDIT_LIMIT	=	1000	;
if	PAYMENTS	<	89.921689	then	PAYMENTS	=	89.921689	;
if	MINIMUM_PAYMENTS	<	74.625067	then	MINIMUM_PAYMENTS	=	74.625067	;
if	TENURE	<	8	then	TENURE	=	8	;
run;

/*Checking whether the Capping is correctly done or not*/

data test;
set cc_segmentation;
valid_obs_3=1;
if BALANCE	>	5911.51	or
BALANCE_FREQUENCY	>	1	or
PURCHASES	>	3999.92	or
ONEOFF_PURCHASES	>	2675	or
INSTALLMENTS_PURCHASES	>	1753.08	or
CASH_ADVANCE	>	4653.69	or
PURCHASES_FREQUENCY	>	1	or
ONEOFF_PURCHASES_FREQUENCY	>	1	or
PURCHASES_INSTALLMENTS_FREQUENCY	>	1	or
CASH_ADVANCE_FREQUENCY	>	0.583333	or
CASH_ADVANCE_TRX	>	15	or
PURCHASES_TRX	>	57	or
CREDIT_LIMIT	>	12000	or
PAYMENTS	>	6083.43	or
MINIMUM_PAYMENTS	>	2767.05	or
PRC_FULL_PAYMENT	>	1	or
TENURE	>	12	or 
BALANCE	<	8.80954	or
BALANCE_FREQUENCY	<	0.272727	or
CREDIT_LIMIT	<	1000	or
PAYMENTS	<	89.921689	or
MINIMUM_PAYMENTS	<	73.203221	or
TENURE	<	8	or
LIMIT_USAGE	<	0.0029404	or
PAYMENT_TO_MIN_PAYMENT_RATIO	<	0.454491		
then valid_obs_3=0;
run;

proc freq data=test;
table valid_obs_3;
run;

/*RE-Checking the number of missing values*/

proc means data=cc_segmentation nmiss;
run;

/* Creating New KPI's */

data CC_SEGMENTATION;
set CC_SEGMENTATION;
MONTHLY_AVG_PURCHASE = (PURCHASES/12);
MONTHLY_AVG_CASH_ADV = (CASH_ADVANCE/12);
if PURCHASES_TRX = 0 then AVG_AMOUNT_PER_PURCHASE = 0;
else AVG_AMOUNT_PER_PURCHASE = (PURCHASES/PURCHASES_TRX);
if CASH_ADVANCE_TRX = 0 then AVG_AMOUNT_PER_CASH_ADV = 0;
else AVG_AMOUNT_PER_CASH_ADV = (CASH_ADVANCE/CASH_ADVANCE_TRX);
LIMIT_USAGE = (BALANCE/CREDIT_LIMIT);
PAYMENT_TO_MIN_PAYMENT_RATIO = (PAYMENTS/MINIMUM_PAYMENTS);
run;

/*Creating dummy variables from variable - PURCHASE_TYPE*/

data cc_segmentation;
set cc_segmentation;
if Purchase_type="OneOff Purchase"  then OneOff_Purchase= 1; 
else OneOff_Purchase= 0;
if Purchase_type="Installment Purchase"  then Installment_Purchase= 1; 
else Installment_Purchase= 0;
if Purchase_type="Both OneOff & Installment Purchase"  then Both_OneOff_Install_Purchase= 1; 
else Both_OneOff_Install_Purchase = 0;
run;


/* Checking the normality of variables */

proc univariate data= CC_SEGMENTATION;
histogram;
run; 

/*New variables are highly skewed.
Therefore, Transforming them to make them normal*/

data CC_SEGMENTATION;
set CC_SEGMENTATION;
if AVG_AMOUNT_PER_PURCHASE = 0 then ln_AVG_AMOUNT_PER_PURCHASE = 0;
else ln_AVG_AMOUNT_PER_PURCHASE=log(AVG_AMOUNT_PER_PURCHASE);

if AVG_AMOUNT_PER_CASH_ADV = 0 then ln_AVG_AMOUNT_PER_CASH_ADV = 0;
else ln_AVG_AMOUNT_PER_CASH_ADV=log(AVG_AMOUNT_PER_CASH_ADV);

ln_PAYMENT_MIN_PAYMENT_RATIO=log(PAYMENT_TO_MIN_PAYMENT_RATIO);
run;

/*FACTOR ANALYSIS*/

ods html file='/folders/myfolders/sample dataset/Final Case Study 1 - Credit Card Segmentation/
FA_ln.xls';
PROC FACTOR DATA= cc_segmentation
METHOD = PRINCIPAL SCREE MINEIGEN=0 NFACTOR = 7
ROTATE = VARIMAX REORDER OUT= FA;
var	
BALANCE	
BALANCE_FREQUENCY	
PURCHASES	
ONEOFF_PURCHASES	
INSTALLMENTS_PURCHASES	
CASH_ADVANCE	
PURCHASES_FREQUENCY	
ONEOFF_PURCHASES_FREQUENCY	
PURCHASES_INSTALLMENTS_FREQUENCY	
CASH_ADVANCE_FREQUENCY	
CASH_ADVANCE_TRX	
PURCHASES_TRX	
CREDIT_LIMIT		
PAYMENTS
MINIMUM_PAYMENTS
PRC_FULL_PAYMENT	
TENURE		
OneOff_Purchase	
Installment_Purchase	
Both_OneOff_Install_Purchase
LIMIT_USAGE
ln_PAYMENT_MIN_PAYMENT_RATIO
ln_AVG_AMOUNT_PER_PURCHASE
ln_AVG_AMOUNT_PER_CASH_ADV
MONTHLY_AVG_PURCHASE
MONTHLY_AVG_CASH_ADV;
run;
ods html close;

/*Standardizing segmentation variable*/

data cluster;
set cc_segmentation;
Z_MONTHLY_AVG_PURCHASE	=	MONTHLY_AVG_PURCHASE	;
Z_ONEOFF_PURCHASES	=	ONEOFF_PURCHASES	;
Z_ONEOFF_PURCHASES_FREQUENCY	=	ONEOFF_PURCHASES_FREQUENCY	;
Z_Both_OneOff_Install_Purchase	=	Both_OneOff_Install_Purchase	;
Z_INSTALLMENTS_PURCHASES	=	INSTALLMENTS_PURCHASES	;
Z_MONTHLY_AVG_CASH_ADV	=	MONTHLY_AVG_CASH_ADV	;
Z_CASH_ADVANCE_FREQUENCY	=	CASH_ADVANCE_FREQUENCY	;
Z_ln_AVG_AMOUNT_PER_CASH_ADV	=	ln_AVG_AMOUNT_PER_CASH_ADV	;
Z_MINIMUM_PAYMENTS	=	MINIMUM_PAYMENTS	;
Z_BALANCE	=	BALANCE	;
Z_PRC_FULL_PAYMENT	=	PRC_FULL_PAYMENT	;
Z_ln_PAYMENT_MIN_PAYMENT_RATIO	=	ln_PAYMENT_MIN_PAYMENT_RATIO	;
Z_Installment_Purchase	=	Installment_Purchase	;
Z_PURCHASES_INSTALL_FREQUENCY	=	PURCHASES_INSTALLMENTS_FREQUENCY	;
Z_OneOff_Purchase	=	OneOff_Purchase	;
Z_ln_AVG_AMOUNT_PER_PURCHASE	=	ln_AVG_AMOUNT_PER_PURCHASE	;
Z_TENURE	=	TENURE	;
Z_CREDIT_LIMIT	=	CREDIT_LIMIT	;
run;

proc standard data=cluster mean=0 std=1 out=cluster;

var Z_MONTHLY_AVG_PURCHASE
Z_ONEOFF_PURCHASES
Z_ONEOFF_PURCHASES_FREQUENCY
Z_Both_OneOff_Install_Purchase
Z_INSTALLMENTS_PURCHASES
Z_MONTHLY_AVG_CASH_ADV
Z_CASH_ADVANCE_FREQUENCY
Z_ln_AVG_AMOUNT_PER_CASH_ADV
Z_MINIMUM_PAYMENTS
Z_BALANCE
Z_PRC_FULL_PAYMENT
Z_ln_PAYMENT_MIN_PAYMENT_RATIO
Z_Installment_Purchase
Z_PURCHASES_INSTALL_FREQUENCY
Z_OneOff_Purchase
Z_ln_AVG_AMOUNT_PER_PURCHASE
Z_TENURE
Z_CREDIT_LIMIT;
run;


/*Doing K-means segmentation from 3 to 6 cluster*/

proc fastclus data=cluster out=Cluster maxclusters=3 cluster=clus_3 maxiter=100 ;

var Z_MONTHLY_AVG_PURCHASE
Z_ONEOFF_PURCHASES
Z_ONEOFF_PURCHASES_FREQUENCY
Z_Both_OneOff_Install_Purchase
Z_INSTALLMENTS_PURCHASES
Z_MONTHLY_AVG_CASH_ADV
Z_CASH_ADVANCE_FREQUENCY
Z_ln_AVG_AMOUNT_PER_CASH_ADV
Z_MINIMUM_PAYMENTS
Z_BALANCE
Z_PRC_FULL_PAYMENT
Z_ln_PAYMENT_MIN_PAYMENT_RATIO
Z_Installment_Purchase
Z_PURCHASES_INSTALL_FREQUENCY
Z_OneOff_Purchase
Z_ln_AVG_AMOUNT_PER_PURCHASE
Z_TENURE
Z_CREDIT_LIMIT;
run;


proc fastclus data=cluster out=Cluster maxclusters=4 cluster=clus_4 maxiter=100 ;

var Z_MONTHLY_AVG_PURCHASE
Z_ONEOFF_PURCHASES
Z_ONEOFF_PURCHASES_FREQUENCY
Z_Both_OneOff_Install_Purchase
Z_INSTALLMENTS_PURCHASES
Z_MONTHLY_AVG_CASH_ADV
Z_CASH_ADVANCE_FREQUENCY
Z_ln_AVG_AMOUNT_PER_CASH_ADV
Z_MINIMUM_PAYMENTS
Z_BALANCE
Z_PRC_FULL_PAYMENT
Z_ln_PAYMENT_MIN_PAYMENT_RATIO
Z_Installment_Purchase
Z_PURCHASES_INSTALL_FREQUENCY
Z_OneOff_Purchase
Z_ln_AVG_AMOUNT_PER_PURCHASE
Z_TENURE
Z_CREDIT_LIMIT;
run;


proc fastclus data=cluster out=Cluster maxclusters=5 cluster=clus_5 maxiter=100 ;

var Z_MONTHLY_AVG_PURCHASE
Z_ONEOFF_PURCHASES
Z_ONEOFF_PURCHASES_FREQUENCY
Z_Both_OneOff_Install_Purchase
Z_INSTALLMENTS_PURCHASES
Z_MONTHLY_AVG_CASH_ADV
Z_CASH_ADVANCE_FREQUENCY
Z_ln_AVG_AMOUNT_PER_CASH_ADV
Z_MINIMUM_PAYMENTS
Z_BALANCE
Z_PRC_FULL_PAYMENT
Z_ln_PAYMENT_MIN_PAYMENT_RATIO
Z_Installment_Purchase
Z_PURCHASES_INSTALL_FREQUENCY
Z_OneOff_Purchase
Z_ln_AVG_AMOUNT_PER_PURCHASE
Z_TENURE
Z_CREDIT_LIMIT;
run;

proc fastclus data=cluster out=Cluster maxclusters=6 cluster=clus_6 maxiter=100 ;

var Z_MONTHLY_AVG_PURCHASE
Z_ONEOFF_PURCHASES
Z_ONEOFF_PURCHASES_FREQUENCY
Z_Both_OneOff_Install_Purchase
Z_INSTALLMENTS_PURCHASES
Z_MONTHLY_AVG_CASH_ADV
Z_CASH_ADVANCE_FREQUENCY
Z_ln_AVG_AMOUNT_PER_CASH_ADV
Z_MINIMUM_PAYMENTS
Z_BALANCE
Z_PRC_FULL_PAYMENT
Z_ln_PAYMENT_MIN_PAYMENT_RATIO
Z_Installment_Purchase
Z_PURCHASES_INSTALL_FREQUENCY
Z_OneOff_Purchase
Z_ln_AVG_AMOUNT_PER_PURCHASE
Z_TENURE
Z_CREDIT_LIMIT;
run;

/*Checking segment size*/

proc freq data=cluster;
table clus_3 clus_4 clus_5 clus_6;
run;

ods html file='/folders/myfolders/sample dataset/Final Case Study 1 - Credit Card Segmentation/
Profiling.xls';
proc tabulate data=Cluster;
var MONTHLY_AVG_PURCHASE
ONEOFF_PURCHASES
ONEOFF_PURCHASES_FREQUENCY
Both_OneOff_Install_Purchase
INSTALLMENTS_PURCHASES
MONTHLY_AVG_CASH_ADV
CASH_ADVANCE_FREQUENCY
ln_AVG_AMOUNT_PER_CASH_ADV
MINIMUM_PAYMENTS
BALANCE
PRC_FULL_PAYMENT
ln_PAYMENT_MIN_PAYMENT_RATIO
Installment_Purchase
PURCHASES_INSTALLMENTS_FREQUENCY
OneOff_Purchase
ln_AVG_AMOUNT_PER_PURCHASE
TENURE
CREDIT_LIMIT
;
class  clus_3 clus_4 clus_5 clus_6;
table (MONTHLY_AVG_PURCHASE
ONEOFF_PURCHASES
ONEOFF_PURCHASES_FREQUENCY
Both_OneOff_Install_Purchase
INSTALLMENTS_PURCHASES
MONTHLY_AVG_CASH_ADV
CASH_ADVANCE_FREQUENCY
ln_AVG_AMOUNT_PER_CASH_ADV
MINIMUM_PAYMENTS
BALANCE
PRC_FULL_PAYMENT
ln_PAYMENT_MIN_PAYMENT_RATIO
Installment_Purchase
PURCHASES_INSTALLMENTS_FREQUENCY
OneOff_Purchase
ln_AVG_AMOUNT_PER_PURCHASE
TENURE
CREDIT_LIMIT)*mean, clus_3 clus_4 clus_5 clus_6 All;
run;
ods html close;