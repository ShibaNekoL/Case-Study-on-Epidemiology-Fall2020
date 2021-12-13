/*all*/

/*定義年份區間*/
data trend ;
  set a;
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
/*-計算每10萬人口死亡率*/
data cu_age_specific_rate;                 
  set cu_age_specific;
  rate10=(sum_death/sum_pop)*100000;
  keep yeargp agegp sum_death sum_pop rate10; /*-僅保留所需變項*/
run;


/*將計算結果輸出成Excel檔*/
proc export 
  /*data: 擬輸出至Excel的SAS資料檔*/
  data= work.cu_age_specific_rate 
  /*outfile: Excel檔儲存路徑，檔名為"curve"*/
  outfile= "D:\OneDrive - 國立台灣大學\109-1\流行病學實例討論(no exam)\4. descriptive epidemiology\curve.xls" 
  dbms=excel replace;
  sheet="trend"; /*命名Excel工作表為"trend"*/
run;



/*male*/

/*定義年份區間*/

data trend ;
  set m;
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
/*-計算每10萬人口死亡率*/
data cu_age_specific_rate;                 
  set cu_age_specific;
  rate10=(sum_death/sum_pop)*100000;
  keep yeargp agegp sum_death sum_pop rate10; /*-僅保留所需變項*/
run;


/*將計算結果輸出成Excel檔*/
proc export 
  /*data: 擬輸出至Excel的SAS資料檔*/
  data= work.cu_age_specific_rate 
  /*outfile: Excel檔儲存路徑，檔名為"curve"*/
  outfile= "D:\OneDrive - 國立台灣大學\109-1\流行病學實例討論(no exam)\4. descriptive epidemiology\curve_m.xls" 
  dbms=excel replace;
  sheet="trend"; /*命名Excel工作表為"trend"*/
run;



/*female*/

/*定義年份區間*/

data trend ;
  set f;
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
/*-計算每10萬人口死亡率*/
data cu_age_specific_rate;                 
  set cu_age_specific;
  rate10=(sum_death/sum_pop)*100000;
  keep yeargp agegp sum_death sum_pop rate10; /*-僅保留所需變項*/
run;


/*將計算結果輸出成Excel檔*/
proc export 
  /*data: 擬輸出至Excel的SAS資料檔*/
  data= work.cu_age_specific_rate 
  /*outfile: Excel檔儲存路徑，檔名為"curve"*/
  outfile= "D:\OneDrive - 國立台灣大學\109-1\流行病學實例討論(no exam)\4. descriptive epidemiology\curve_f.xls" 
  dbms=excel replace;
  sheet="trend"; /*命名Excel工作表為"trend"*/
run;
