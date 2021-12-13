
PROC IMPORT OUT= WORK.a 
            DATAFILE= "C:\Users\demo.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="demo$"; 
RUN;
/*���`�ܶ��B�z*/
data b;
	set a;
	if death=. then death=0;  /*�ťդH�Ƹ�0*/
run;

/*1.�ʦ��`�v - ���`�H��/�`�H�f��*/
/* ex. ��N(60�~ - 89�~)�ʦ��`�v*/
proc sort data=b;  /*by���O�ܶ������Ƨ�*/
	by year;
run;
proc means data=b noprint; /*���`�`�H�ƻP�`�H�f�ƭp��*/
	var death pop;
	by year;	
	output out=crude sum=deathsum popsum;
run; 
data cruderate; /*�ʦ��`�v�p��*/
	set crude;
	rate=deathsum/popsum; /*�ʦ��`�v(��N)*/
run;
/*�˵���N�ʦ��`�v*/
title'��N�ʦ��`�v';
proc print data=cruderate;
	var year deathsum popsum rate;
run;
title;


/*2.�S�w���`�v*/
/* ex. ��N(60�~-89�~)�~�֧O���`�v*/
proc sort data=b;  /*by���O�ܶ������Ƨ�*/
	by year agegp;
run;
proc means data=b noprint; /*���`�`�H�ƻP�`�H�f�ƭp��*/
	var death pop;
	by year agegp;
	output out=age_specific sum=deathsum2 popsum2;
run;
data age_specific_rate;                 
	set age_specific;
	rate=deathsum2/popsum2;
run;
/*�˵���N�~�֧O���`�v*/
title '��N�~�֧O���`�v';
proc print data=age_specific_rate;
	var year agegp deathsum2 popsum2 rate;
run;
title;



/*3�зǤƦ��`�v (1976�~)*/ /*sum(DRi*Pri)*/
/*3-1�~�ּзǤƦ��`�v*/
data standard; /*�����зǤ�*/
	set age_specific_rate;
	/*srate=�~�֧O���`�vX1976�~�зǤH�f�Ӧ~�ּh�H�f�ʤ���*/
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

proc sort data=standard;  /*by���O�ܶ������Ƨ�*/
by year;
run;

/**�N�P�~�N�����U�~�ּh�[�`�X��**/
proc means data=standard noprint;
	var srate;
	by year;
	output out=standardrate	sum=standard_rate;
run;
/*�˵���N�~�ּзǤƦ��`�v*/
proc print data=standardrate;
	var year standard_rate;
run;

/*3-2. SMR*/ /*observed/expected*/ /*sum(Di)/sum(Pi*Ri)*/
/*�����a�Ϧ��`�v  vs. ���x���`�v(stander)*/
  /*�X�֨��dataset�����--merge���O*/

/**�����a�Ϧ��`�H��(observed)**/
data g;  
	set b;
	year_agegp=compress(year||agegp); /*||: �X��2���r�Fcompress(): �����Ů�*/
run;
proc print data=g(obs=50);
run;

/***���x�~�֧O���`�v(reference)***/
data ref; 
	set age_specific_rate;
	year_agegp=compress(year||agegp); /*||: �X��2���r�Fcompress(): �����Ů�*/
run;
proc print data=ref(obs=50);
run;

/*�X�ֿ����a�Ϧ��`�H��(observed),�H�f��(pop)�Υ��x�~�֧O���`�v(reference)*/
proc sort data=g; 
	by year_agegp;
proc sort data=ref; 
	by year_agegp;
data i;
	merge g ref; /*�X�֨ⵧ���*/
	by year_agegp; /*�̬Y�ܶ��X��; �ⵧ��Ƥ��Ҷ������ܶ�; �X�֫e���ܶ������Ƨ�*/
run;

/*�p��U�~�ּh�w�����`�H��*/  
data exp;
	set i;
	expected=pop*rate;  
run;
proc print data=exp(obs=50);
	var address year_agegp pop rate expected;
	where year_agegp='60Y10_14';
run;

/***sum(Di)/sum(Pi*Ri)***/     
/*�ƧǦU�����U�~�֧O���`�H��(observed)�ΦU�����U�~�֧O�w�����`�H��(expected)*/
proc sort data=exp; 
	by year address; /*�̦~�ο����O�Ƨ�*/
run;
/*�[�`�C�~�U������ڦ��`�H��(death)�ιw�����`�H��(expected)*/
proc means data=exp noprint;   
	var death expected;
	by year address;
	output out=obsexp sum=observed expected;       
run;
/*�p��C�~�U����SMR*/
data smr;
	set obsexp;
	SMR=observed/expected;
run;	
proc print data=smr(obs=50);
	var year address observed expected SMR;
run;




/***��N�B�@�N�~�֦��u***/
/*�w�q�~���϶�*/
data trend ;
	set b;
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
/*�p��C10�U�H�f���`�v*/
data cu_age_specific_rate;                 
	set cu_age_specific;
	rate10=(sum_death/sum_pop)*100000;
	keep yeargp agegp sum_death sum_pop rate10;
run;
/*�N�p�⵲�G��X��Excel��*/
proc export data= work.cu_age_specific_rate 
             outfile= "C:\Users\curve.xls" /*Excel���x�s���|�A�ɦW��"curve"*/
             dbms=excel replace;
     		 sheet="trend"; /*�R�WExcel�u�@��"trend"*/
run;

