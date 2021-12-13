/*trend*/
proc freq data=data;
tables DISEASE*age_g_4/trend;
run;
proc freq data=data;
tables DISEASE*SEX/trend;
run;
proc freq data=data;
tables DISEASE*mut/trend;
run;
PROC FREQ DATA=data;
	TABLE mut*DISEASE / chisq expected fisher; 
RUN; 


proc phreg data=data;
model futime*disease(0)=mut sex age/RL;
run;
DATA data1;
set data;
if futime = "NA" then futime=;
run;

/*stratified analysis*/
proc sort data=data;
by mut;
proc phreg data=data;
model futime*disease(0)=sex age_g_4/RL;
by mut;
run;


proc freq data=data;
 tables mut*age_g_2*DISEASE/cmh;
run;

proc freq data=data;
 tables mut*age_g_2*DISEASE/cmh;
run;


/*COX ph model*/
proc phreg data=data;
class mutation(ref='00');
model futime*disease(0)=eage*mutation/RL;
run;
