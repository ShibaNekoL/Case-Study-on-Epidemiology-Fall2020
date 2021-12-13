/*Data import*/
PROC IMPORT OUT= WORK.DATA		/*OUT = SAS ��ƶ��W��*/
   DATAFILE= ""  /*DATAFILE = ��l����ɦ�m*/
   DBMS=CSV REPLACE;		/*DBMS = ��l����ɮ榡*/
   RUN;

/*Kappa Test*/
proc freq data= data;
tables test1*test2;
test kappa;
run;

/*futime*/
data time;
set data;
interview1=put(interview,6.);
endtime1=put(endtime,6.);
interviewyy=SUBSTR(interview1,1,2)*1;
interviewmm=SUBSTR(interview1,3,2)*1;
endtimeyy=SUBSTR(endtime1,1,2)*1;
endtimemm=SUBSTR(endtime1,3,2)*1;
futime=(endtimeyy-interviewyy)+(endtimemm-interviewmm)/12;
run;


/*Cox's proportional hazards model*/
proc phreg data=time;
model futime*case(0)=test1 age/RL;
run;


/*stratified analysis*/
proc sort data=time;by sex;
proc phreg data=time;
model futime*case(0)=test1 age/RL;
by sex;
run;


/*interaction*/
proc phreg data=time;
model futime*case(0)=test1 age test1*age/RL;
run;

/*interaction �]�s�ܶ�*/
proc phreg data=time;
interaction=test1*age;
model futime*case(0)=test1 age interaction/ RL; 
run;
