/*all*/

/*�w�q�~���϶�*/
data trend ;
  set a;
  if year in ('60' '61' '62' '63' '64') then yeargp='6064';
  else if year in ('65' '66' '67' '68' '69') then yeargp='6569';
  else if year in ('70' '71' '72' '73' '74') then yeargp='7074';
  else if year in ('75' '76' '77' '78' '79') then yeargp='7579';
  else if year in ('80' '81' '82' '83' '84') then yeargp='8084';
  else if year in ('85' '86' '87' '88' '89') then yeargp='8589';
run;

/*�̦~���϶��Φ~�ּh�Ƨ�*/
proc sort data=trend;  
  by yeargp agegp; 
run;
/*�[�`�C�Ӱ϶��U�~�ּh���`�H�ƤΤH�f��*/
proc means data=trend noprint;
  var death pop;
  by yeargp agegp;
  output out=cu_age_specific sum=sum_death sum_pop;
run;
/*-�p��C10�U�H�f���`�v*/
data cu_age_specific_rate;                 
  set cu_age_specific;
  rate10=(sum_death/sum_pop)*100000;
  keep yeargp agegp sum_death sum_pop rate10; /*-�ȫO�d�һ��ܶ�*/
run;


/*�N�p�⵲�G��X��Excel��*/
proc export 
  /*data: ����X��Excel��SAS�����*/
  data= work.cu_age_specific_rate 
  /*outfile: Excel���x�s���|�A�ɦW��"curve"*/
  outfile= "D:\OneDrive - ��ߥx�W�j��\109-1\�y��f�ǹ�ҰQ��(no exam)\4. descriptive epidemiology\curve.xls" 
  dbms=excel replace;
  sheet="trend"; /*�R�WExcel�u�@��"trend"*/
run;



/*male*/

/*�w�q�~���϶�*/

data trend ;
  set m;
  if year in ('60' '61' '62' '63' '64') then yeargp='6064';
  else if year in ('65' '66' '67' '68' '69') then yeargp='6569';
  else if year in ('70' '71' '72' '73' '74') then yeargp='7074';
  else if year in ('75' '76' '77' '78' '79') then yeargp='7579';
  else if year in ('80' '81' '82' '83' '84') then yeargp='8084';
  else if year in ('85' '86' '87' '88' '89') then yeargp='8589';
run;

/*�̦~���϶��Φ~�ּh�Ƨ�*/
proc sort data=trend;  
  by yeargp agegp; 
run;
/*�[�`�C�Ӱ϶��U�~�ּh���`�H�ƤΤH�f��*/
proc means data=trend noprint;
  var death pop;
  by yeargp agegp;
  output out=cu_age_specific sum=sum_death sum_pop;
run;
/*-�p��C10�U�H�f���`�v*/
data cu_age_specific_rate;                 
  set cu_age_specific;
  rate10=(sum_death/sum_pop)*100000;
  keep yeargp agegp sum_death sum_pop rate10; /*-�ȫO�d�һ��ܶ�*/
run;


/*�N�p�⵲�G��X��Excel��*/
proc export 
  /*data: ����X��Excel��SAS�����*/
  data= work.cu_age_specific_rate 
  /*outfile: Excel���x�s���|�A�ɦW��"curve"*/
  outfile= "D:\OneDrive - ��ߥx�W�j��\109-1\�y��f�ǹ�ҰQ��(no exam)\4. descriptive epidemiology\curve_m.xls" 
  dbms=excel replace;
  sheet="trend"; /*�R�WExcel�u�@��"trend"*/
run;



/*female*/

/*�w�q�~���϶�*/

data trend ;
  set f;
  if year in ('60' '61' '62' '63' '64') then yeargp='6064';
  else if year in ('65' '66' '67' '68' '69') then yeargp='6569';
  else if year in ('70' '71' '72' '73' '74') then yeargp='7074';
  else if year in ('75' '76' '77' '78' '79') then yeargp='7579';
  else if year in ('80' '81' '82' '83' '84') then yeargp='8084';
  else if year in ('85' '86' '87' '88' '89') then yeargp='8589';
run;

/*�̦~���϶��Φ~�ּh�Ƨ�*/
proc sort data=trend;  
  by yeargp agegp; 
run;
/*�[�`�C�Ӱ϶��U�~�ּh���`�H�ƤΤH�f��*/
proc means data=trend noprint;
  var death pop;
  by yeargp agegp;
  output out=cu_age_specific sum=sum_death sum_pop;
run;
/*-�p��C10�U�H�f���`�v*/
data cu_age_specific_rate;                 
  set cu_age_specific;
  rate10=(sum_death/sum_pop)*100000;
  keep yeargp agegp sum_death sum_pop rate10; /*-�ȫO�d�һ��ܶ�*/
run;


/*�N�p�⵲�G��X��Excel��*/
proc export 
  /*data: ����X��Excel��SAS�����*/
  data= work.cu_age_specific_rate 
  /*outfile: Excel���x�s���|�A�ɦW��"curve"*/
  outfile= "D:\OneDrive - ��ߥx�W�j��\109-1\�y��f�ǹ�ҰQ��(no exam)\4. descriptive epidemiology\curve_f.xls" 
  dbms=excel replace;
  sheet="trend"; /*�R�WExcel�u�@��"trend"*/
run;
