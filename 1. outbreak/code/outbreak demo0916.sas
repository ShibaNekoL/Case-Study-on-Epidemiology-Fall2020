/*匯入CSV檔*/
proc import out= WORK.aa 
            datafile= "C:\outbreakdemo.csv" /*更改路徑*/
            dbms=CSV relpace;
run;
/*檢視輸入資料內涵*/
proc contents data=aa; run;
/*檢視前50筆資料*/
proc print data=aa(obs=50); run;

/***one-way frequency***/
proc freq data=aa ;
	tables Salad Pork_steak Vegetable Cuttlefish;  /*可放多個類別變項*/
run;

/***The 2x2 Table***/
proc freq data=aa;
	tables (Salad  Pork_steak Vegetable Cuttlefish)*case;  /*列X欄*/
run;

/***Test statistics***/
proc freq data=aa;
  tables Salad*case/chisq fisher relrisk;
run; 


/**補充     (disease) (nodisease)
(salad)
(no salad)                                    
先排序再做卡方                      **/
proc sort data=aa;
by descending case descending Salad;
run;
proc freq data=aa order=data;  /*order=data是根據前面排序的data(descending)順序做卡方*/
  tables Salad*case/chisq fisher relrisk /*expected*/;/*加expected可計算每個細格的期望值，藉此確認是否符合rule of five*/
run; 

/***Stratified analysis***/
/*分層變項X列X欄*/
proc freq data=aa;
 	tables  Vegetable*Salad*case/chisq relrisk cmh;
run;


/**流行曲線**/
proc freq data=aa;
	tables date/out=freq outcum; /*計算每個時間點的病例數*/
run;
proc print data=freq(obs=50);
run;

title 'test'; 
proc sgplot data=freq;
	xaxis values=(0 to 30 by 1)  label='Date(day)' grid;
	yaxis values=(0 to 6 by 1)  label='Count'  grid;
	series x=date y=count /datalabel=count  lineattrs=(pattern=solid thickness=1px);
run;
