/***1***/
/***one-way frequency***/
proc freq data=data ;
	tables diarrhea fever stomach_ache vomiting fatigued nausea abdominal_cramp watery_stool mucous_stool bloody_stool;  /*可放多個類別變項*/
run;

DATA data_a;
SET data;
	index_value = diarrhea + fever + stomach_ache +  vomiting +  fatigued + watery_stool ;
	if index_value >= 4 then case = 1;
	else if index_value < 4 then case = 0;
	else case = . ;
RUN;

proc freq data=data_a;
	tables case;  /*可放多個類別變項*/
run;


/***2***/

/**補充     (disease) (nodisease)
(salad)
(no salad)                                    
先排序再做卡方                      **/
proc sort data=data_a;
by descending case descending (drinking gargling handwashing facewashing pork salad vegetable cuttlefish);
run;
proc freq data=data_a order=data;  /*order=data是根據前面排序的data(descending)順序做卡方*/
  tables (drinking gargling handwashing facewashing pork salad vegetable cuttlefish)*case/chisq fisher relrisk /*expected*/;/*加expected可計算每個細格的期望值，藉此確認是否符合rule of five*/
run; 

proc logistic data=data_a;
class drinking(ref='1') gargling(ref='3') handwashing(ref='0') facewashing(ref='0') pork(ref='0') salad(ref='0') vegetable(ref='0') cuttlefish(ref='0') / param=ref;
model case(event='1')= drinking gargling handwashing facewashing pork salad vegetable cuttlefish / risklimits;
run;

proc logistic data=data_a;
class  handwashing(ref='0')/ param=ref;
model case(event='1')= handwashing / risklimits;
run;


/***3***/
proc logistic data=data_a;
class drinking(ref='1') gargling(ref='3') handwashing(ref='0') cuttlefish(ref='0') / param=ref;
model case(event='1')= drinking gargling handwashing cuttlefish drinking*gargling drinking*handwashing drinking*cuttlefish gargling*handwashing gargling*cuttlefish handwashing*cuttlefish/ risklimits;
run;


/*分層變項X列X欄*/ 
proc freq data=data_a;
tables (drinking gargling handwashing facewashing pork salad vegetable cuttlefish)*(drinking gargling handwashing cuttlefish)*case/chisq fisher relrisk;
RUN;

/*分層變項X列X欄*/ 
proc freq data=data_a;
tables (drinking gargling handwashing facewashing pork salad vegetable cuttlefish)*(drinking gargling handwashing cuttlefish)*case/cmh;
RUN;

/***4***/
/**流行曲線**/

DATA data_b;
SET data_a;
	if date >= 7 then mm = 10;
	else if date < 7 then mm = 11;
	else mm = . ;
RUN;

proc freq data=data_b;
	tables date*case /out=freq outcum; /*計算每個時間點的病例數*/
run;

data freq_a;
set freq;
if case = 0 then delete;
run;

title '流行曲線'; 
proc sgplot data=freq_a;
	xaxis values=(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,1,2,3,4,5,6)  label='Date(day)' grid;
	yaxis values=(0 to 35 by 1)  label='Case'  grid;
	series x=date y=count /datalabel=count  lineattrs=(pattern=solid thickness=1px);
run;
