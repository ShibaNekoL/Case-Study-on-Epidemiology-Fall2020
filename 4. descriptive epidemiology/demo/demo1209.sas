
PROC IMPORT OUT= WORK.a 
            DATAFILE= "C:\Users\demo.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="demo$"; 
RUN;
/*死亡變項處理*/
data b;
	set a;
	if death=. then death=0;  /*空白人數補0*/
run;

/*1.粗死亡率 - 死亡人數/總人口數*/
/* ex. 當代(60年 - 89年)粗死亡率*/
proc sort data=b;  /*by指令變項須先排序*/
	by year;
run;
proc means data=b noprint; /*死亡總人數與總人口數計算*/
	var death pop;
	by year;	
	output out=crude sum=deathsum popsum;
run; 
data cruderate; /*粗死亡率計算*/
	set crude;
	rate=deathsum/popsum; /*粗死亡率(當代)*/
run;
/*檢視當代粗死亡率*/
title'當代粗死亡率';
proc print data=cruderate;
	var year deathsum popsum rate;
run;
title;


/*2.特定死亡率*/
/* ex. 當代(60年-89年)年齡別死亡率*/
proc sort data=b;  /*by指令變項須先排序*/
	by year agegp;
run;
proc means data=b noprint; /*死亡總人數與總人口數計算*/
	var death pop;
	by year agegp;
	output out=age_specific sum=deathsum2 popsum2;
run;
data age_specific_rate;                 
	set age_specific;
	rate=deathsum2/popsum2;
run;
/*檢視當代年齡別死亡率*/
title '當代年齡別死亡率';
proc print data=age_specific_rate;
	var year agegp deathsum2 popsum2 rate;
run;
title;



/*3標準化死亡率 (1976年)*/ /*sum(DRi*Pri)*/
/*3-1年齡標準化死亡率*/
data standard; /*直接標準化*/
	set age_specific_rate;
	/*srate=年齡別死亡率X1976年標準人口該年齡層人口百分比*/
	if agegp='Y00_04' then srate=rate*0.120;
	if agegp='Y05_09' then srate=rate*0.100;
	if agegp='Y10_14' then srate=rate*0.090;
	if agegp='Y15_19' then srate=rate*0.090;
	if agegp='Y20_24' then srate=rate*0.080;
	if agegp='Y25_29' then srate=rate*0.080;
	if agegp='Y30_34' then srate=rate*0.060;
	if agegp='Y35_39' then srate=rate*0.060;
	if agegp='Y40_44' then srate=rate*0.060;
	if agegp='Y45_49' then srate=rate*0.060;
	if agegp='Y50_54' then srate=rate*0.050;
	if agegp='Y55_59' then srate=rate*0.040;
	if agegp='Y60_64' then srate=rate*0.040;
	if agegp='Y65_69' then srate=rate*0.030;
	if agegp='Y70_74' then srate=rate*0.020;
	if agegp='Y75_79' then srate=rate*0.010;
	if agegp='Y80_84' then srate=rate*0.005;
	if agegp='Y85ABO' then srate=rate*0.005;
run;

proc sort data=standard;  /*by指令變項須先排序*/
by year;
run;

/**將同年代中的各年齡層加總合併**/
proc means data=standard noprint;
	var srate;
	by year;
	output out=standardrate	sum=standard_rate;
run;
/*檢視當代年齡標準化死亡率*/
proc print data=standardrate;
	var year standard_rate;
run;

/*3-2. SMR*/ /*observed/expected*/ /*sum(Di)/sum(Pi*Ri)*/
/*縣市地區死亡率  vs. 全台死亡率(stander)*/
  /*合併兩個dataset的資料--merge指令*/

/**縣市地區死亡人數(observed)**/
data g;  
	set b;
	year_agegp=compress(year||agegp); /*||: 合併2串文字；compress(): 移除空格*/
run;
proc print data=g(obs=50);
run;

/***全台年齡別死亡率(reference)***/
data ref; 
	set age_specific_rate;
	year_agegp=compress(year||agegp); /*||: 合併2串文字；compress(): 移除空格*/
run;
proc print data=ref(obs=50);
run;

/*合併縣市地區死亡人數(observed),人口數(pop)及全台年齡別死亡率(reference)*/
proc sort data=g; 
	by year_agegp;
proc sort data=ref; 
	by year_agegp;
data i;
	merge g ref; /*合併兩筆資料*/
	by year_agegp; /*依某變項合併; 兩筆資料中皆須有該變項; 合併前該變項須先排序*/
run;

/*計算各年齡層預期死亡人數*/  
data exp;
	set i;
	expected=pop*rate;  
run;
proc print data=exp(obs=50);
	var address year_agegp pop rate expected;
	where year_agegp='60Y10_14';
run;

/***sum(Di)/sum(Pi*Ri)***/     
/*排序各縣市各年齡別死亡人數(observed)及各縣市各年齡別預期死亡人數(expected)*/
proc sort data=exp; 
	by year address; /*依年及縣市別排序*/
run;
/*加總每年各縣市實際死亡人數(death)及預期死亡人數(expected)*/
proc means data=exp noprint;   
	var death expected;
	by year address;
	output out=obsexp sum=observed expected;       
run;
/*計算每年各縣市SMR*/
data smr;
	set obsexp;
	SMR=observed/expected;
run;	
proc print data=smr(obs=50);
	var year address observed expected SMR;
run;




/***當代、世代年齡曲線***/
/*定義年份區間*/
data trend ;
	set b;
	if year in ('60' '61' '62' '63' '64') then yeargp='6064';
	else if year in ('65' '66' '67' '68' '69') then yeargp='6569';
	else if year in ('70' '71' '72' '73' '74') then yeargp='7074';
	else if year in ('75' '76' '77' '78' '79') then yeargp='7579';
	else if year in ('80' '81' '82' '83' '84') then yeargp='8084';
	else if year in ('85' '86' '87' '88' '89') then yeargp='8589';
run;
/*依年份區間及年齡層排序*/
proc sort data=trend;  
	by yeargp agegp; 
run;
/*加總每個區間各年齡層死亡人數及人口數*/
proc means data=trend noprint;
	var death pop;
	by yeargp agegp;
	output out=cu_age_specific sum=sum_death sum_pop;
run;
/*計算每10萬人口死亡率*/
data cu_age_specific_rate;                 
	set cu_age_specific;
	rate10=(sum_death/sum_pop)*100000;
	keep yeargp agegp sum_death sum_pop rate10;
run;
/*將計算結果輸出成Excel檔*/
proc export data= work.cu_age_specific_rate 
             outfile= "C:\Users\curve.xls" /*Excel檔儲存路徑，檔名為"curve"*/
             dbms=excel replace;
     		 sheet="trend"; /*命名Excel工作表為"trend"*/
run;

