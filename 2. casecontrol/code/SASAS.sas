/*ALL X*/
proc logistic data=re;
class age2(ref='0') smk2(ref='0') alc2(ref='0') / param=ref;
model case(event='1')=age2 smk2 alc2 smk2*alc2 / risklimits;
run;


/*ALL X*/
proc logistic data=ESO;
class age(ref='1') smk(ref='1') alc(ref='1') / param=ref;
model case(event='1')=age smk alc / risklimits;
run;

/*ALL X*/
proc logistic data=ESO;
class smk(ref='1') alc(ref='1') / param=ref;
model case(event='1')=smk alc / risklimits;
run;

/*AGE*/
proc logistic data=ESO;
class age(ref='1')  / param=ref;
model case(event='1')=age / risklimits;
run;

/*SMK*/
proc logistic data=ESO;
class SMK(ref='1')  / param=ref;
model case(event='1')=SMK / risklimits;
run;

/*ALC*/
proc logistic data=ESO;
class ALC(ref='1')  / param=ref;
model case(event='1')=ALC / risklimits;
run;

/*trend*/
proc freq data=eso;
tables case*age/trend;
run;
proc freq data=eso;
tables case*smk/trend;
run;
proc freq data=eso;
tables case*alc/trend;
run;

/*stratify*/
proc freq data=re;
 tables ALC*age*case/cmh;
run;





/*ALL X*/
proc logistic data=ESO;
class age(ref='1') smk(ref='1') alc(ref='1') / param=ref;
model case(event='1')=age smk alc  / risklimits;
run;

DATA re;
set eso;
if age <= 1 then age2 = 0;
else if age >= 2 then age2 = 1;
if smk <= 1 then smk2 = 0;
else if smk >= 2 then smk2 = 1;
if alc <= 3 then alc2 = 0;
else if alc >= 4 then alc2 = 1;
run;

/*re or*/
proc logistic data=re;
class age2(ref='0')  / param=ref;
model case(event='1')=age2 / risklimits;
run;

proc logistic data=re;
class smk2(ref='0')  / param=ref;
model case(event='1')=smk2 / risklimits;
run;

proc logistic data=re;
class alc2(ref='0')  / param=ref;
model case(event='1')=alc2 / risklimits;
run;


/*interaction*/
proc freq data=re;
 tables age2*smk2*case/cmh;
run;

proc freq data=re;
 tables alc2*age2*case/cmh;
run;

proc freq data=re;
 tables smk2*alc2*case/cmh;
run;

proc freq data=re;
 tables alc2*age2*case/cmh;
run;


/*ALL X*/
proc logistic data=re;
class age2(ref='0') smk2(ref='0') alc2(ref='0') / param=ref;
model case(event='1')=age2 smk2 / risklimits;
run;


proc logistic data=re;
class age2(ref='0') smk2(ref='0') alc2(ref='0') / param=ref;
model case(event='1')=age2 smk2 alc2 age2*alc2/ risklimits;
run;


/**/
data data1; set ESO;
	if smk=1 then do;  smk1=1; smk2=0; smk3=0; smk4=0; end;
	else if smk=2 then do;  smk1=0; smk2=1; smk3=0; smk4=0; end;
	else if smk=3 then do;  smk1=0; smk2=0; smk3=1; smk4=0; end;
	else if smk=4 then do;  smk1=0; smk2=0; smk3=0; smk4=1; end;

	if alc=1 then do;  alc1=1; alc2=0; alc3=0; alc4=0; end;
	else if alc=2 then do;  alc1=0; alc2=1; alc3=0; alc4=0; end;
	else if alc=3 then do;  alc1=0; alc2=0; alc3=1; alc4=0; end;
	else if alc=4 then do;  alc1=0; alc2=0; alc3=0; alc4=1; end;
run;


proc logistic data=data1;
class age(ref='1') smk1(ref='0') smk2(ref='0') smk3(ref='0') smk4(ref='0') alc1(ref='0') alc2(ref='0') alc3(ref='0') alc4(ref='0') / param=ref;
model case(event='1')=age  smk2 smk3 smk4 smk1 alc2 alc3 alc4 alc1  smk4*alc4/ risklimits;
run;
/**/


proc logistic data=ESO;
class age(ref='1') smk(ref='1') alc(ref='1') / param=ref;
model case(event='1')=age smk alc smk*alc/ risklimits;
run;
