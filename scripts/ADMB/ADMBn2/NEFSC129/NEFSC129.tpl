DATA_SECTION
  init_int nobs;
  init_number maxB;
  init_matrix data(1,nobs,1,2)
  vector landings(1,nobs);
  vector biomass(1,nobs);
  int iter;
  !!iter=0;

PARAMETER_SECTION
  init_bounded_number lnK(-10000.,maxB);
  init_number lnMSY;
  // init_bounded_number n(0.1,30.,-2);
  
  objective_function_value f;
  
  number n;
  number z;
  number zMSY;
  number meanP;
  number sig;
  number K;
  number MSY;
  number SSQ;
  number BMSY;
  number FMSY;
  
  vector epsilon(1,nobs-1);
  vector Ppred(1,nobs-1);
  vector Pobs(1,nobs-1);
  
  sdreport_number sd_sig;

PRELIMINARY_CALCS_SECTION
  // get the data out of the columns of the data matrix
  landings=column(data,1);
  biomass=column(data,2);
  
  // calculate observed surplus production (Pobs)
  for(int i=1;i<=nobs-1;i++)
  {
  Pobs(i)=biomass(i+1)-biomass(i)+landings(i);			
  }
  
  // set starting values
  lnMSY=log(0.6*max(Pobs));
  lnK=log(max(biomass));
  n=2;
  
  
PROCEDURE_SECTION
  //*******MAIN PROGRAM********
  		init_states();
  		calc_residuals();
  		calc_ref_points();
  		calc_obj_fun();
  		sd_sig=sig;
  		ofstream ofs("par.mcmc",ios::app);
  		if(mceval_phase()) mcmc_stuff();  		
  //******************************
	

FUNCTION init_states
	K=mfexp(lnK);

	MSY=mfexp(lnMSY);
	

	// calc z, zMSY, and meanP

	z=(1/(n-1))*pow(n,(n/(n-1)));
	zMSY=z*MSY;
    meanP=mean(Pobs);
	
	
FUNCTION calc_residuals
  for(int j=1;j<=nobs-1;j++)
  {
    Ppred(j)=zMSY*((biomass(j)/K) - pow(biomass(j)/K,n));			
  }
  epsilon=Pobs-Ppred;
  sig=sqrt(norm2(epsilon)/(nobs-1));
  SSQ=norm2(epsilon);
 

 
FUNCTION calc_ref_points
  BMSY=pow(n,(1/(1-n)))*K;
  FMSY=MSY/BMSY;
  
  
FUNCTION calc_obj_fun
  f=dnorm(epsilon,sig);
 

GLOBALS_SECTION
	#include <admodel.h>
	

REPORT_SECTION
	report<<BMSY<<"\n"<<n<<endl;
	

FUNCTION mcmc_stuff
	if(iter==0){
		ofstream ofs("par.mcmc");
		ofs<<"K\t"<<"MSY\t"<<"n\t"<<endl;
		cout<<iter<<endl;
	}
	iter++;
	ofstream ofs("par.mcmc",ios::app);	
	ofs<<K<<"\t"<<MSY<<"\t"<<n<<endl;
  