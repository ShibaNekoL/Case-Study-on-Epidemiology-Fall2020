/*�פJCSV��*/
proc import out= WORK.aa 
            datafile= "C:\outbreakdemo.csv" /*�����|*/
            dbms=CSV relpace;
run;
/*�˵���J��Ƥ��[*/
proc contents data=aa; run;
/*�˵��e50�����*/
proc print data=aa(obs=50); run;

/***one-way frequency***/
proc freq data=aa ;
	tables Salad Pork_steak Vegetable Cuttlefish;  /*�i��h�����O�ܶ�*/
run;

/***The 2x2 Table***/
proc freq data=aa;
	tables (Salad  Pork_steak Vegetable Cuttlefish)*case;  /*�CX��*/
run;

/***Test statistics***/
proc freq data=aa;
  tables Salad*case/chisq fisher relrisk;
run; 


/**�ɥR     (disease) (nodisease)
(salad)
(no salad)                                    
���ƧǦA���d��                      **/
proc sort data=aa;
by descending case descending Salad;
run;
proc freq data=aa order=data;  /*order=data�O�ھګe���ƧǪ�data(descending)���ǰ��d��*/
  tables Salad*case/chisq fisher relrisk /*expected*/;/*�[expected�i�p��C�ӲӮ檺����ȡA�Ǧ��T�{�O�_�ŦXrule of five*/
run; 

/***Stratified analysis***/
/*���h�ܶ�X�CX��*/
proc freq data=aa;
 	tables  Vegetable*Salad*case/chisq relrisk cmh;
run;


/**�y�榱�u**/
proc freq data=aa;
	tables date/out=freq outcum; /*�p��C�Ӯɶ��I���f�Ҽ�*/
run;
proc print data=freq(obs=50);
run;

title 'test'; 
proc sgplot data=freq;
	xaxis values=(0 to 30 by 1)  label='Date(day)' grid;
	yaxis values=(0 to 6 by 1)  label='Count'  grid;
	series x=date y=count /datalabel=count  lineattrs=(pattern=solid thickness=1px);
run;
