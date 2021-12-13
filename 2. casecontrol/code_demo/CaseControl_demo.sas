/*convert character into numeric*/
DATA a;
SET DATA;
risk1=riskA*1;risk2=riskB*1;risk3=riskC*1;
risk4=riskD*1;risk5=riskE*1;case=caco*1;
RUN;

/*linear trend*/
PROC LOGISTIC DESCENDING DATA=a;
MODEL case=risk1;
RUN;

/*interaction*/
PROC LOGISTIC DESCENDING DATA=a;
MODEL case=risk1 risk2  risk1*risk2; 
RUN;

/*dummy_1*/
DATA dummy;
SET a;
risk4a=0; risk4b=0; risk4c=0; risk4d=0; risk4e=0;
if risk4=2 then do;risk4a=1; risk4b=0; risk4c=0; risk4d=0;risk4e=0;end;
else if risk4=3 then do;risk4a=0; risk4b=1; risk4c=0; risk4d=0;risk4e=0;end;
else if risk4=4 then do;risk4a=0; risk4b=0; risk4c=1; risk4d=0;risk4e=0;end;
else if risk4=5 then do;risk4a=0; risk4b=0; risk4c=0; risk4d=1;risk4e=0;end;
else if risk4=6 then do;risk4a=0; risk4b=0; risk4c=0; risk4d=0;risk4e=1;end;
else if risk4=. then do;risk4a=.; risk4b=.; risk4c=.; risk4d=.;risk4e=.;end;
RUN;
PROC LOGISTIC DESCENDING DATA=dummy;
MODEL case=risk4a risk4b risk4c risk4d risk4e;
RUN;

/*dummy_2*/
PROC LOGISTIC DESCENDING DATA=a;
CLASS risk4 (ref='1') /param=ref; 
MODEL case=risk4;
RUN;

/*categorical trend*/
PROC FREQ DATA=a;
TABLES case*risk4/trend;
RUN;

/*dbf data import*/
PROC IMPORT OUT= WORK.DATA		/*OUT = SAS 資料集名稱*/
   DATAFILE= "D:\casecontrol\B1CSC.DBF"		/*DATAFILE = 原始資料檔位置*/
   DBMS=DBF REPLACE;		/*DBMS = 原始資料檔格式*/
   GETDELETED=NO;
   RUN;
/*注意：PROC IMPORT 敘述句共有三行，第三行結束才有分號*/
