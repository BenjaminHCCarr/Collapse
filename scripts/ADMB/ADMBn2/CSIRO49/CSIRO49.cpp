	#include <admodel.h>
	
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <CSIRO49.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  nobs.allocate("nobs");
  maxB.allocate("maxB");
  data.allocate(1,nobs,1,2,"data");
  landings.allocate(1,nobs);
  biomass.allocate(1,nobs);
iter=0;
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  lnK.allocate(-10000.,maxB,"lnK");
  lnMSY.allocate("lnMSY");
  f.allocate("f");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  n.allocate("n");
  #ifndef NO_AD_INITIALIZE
  n.initialize();
  #endif
  z.allocate("z");
  #ifndef NO_AD_INITIALIZE
  z.initialize();
  #endif
  zMSY.allocate("zMSY");
  #ifndef NO_AD_INITIALIZE
  zMSY.initialize();
  #endif
  meanP.allocate("meanP");
  #ifndef NO_AD_INITIALIZE
  meanP.initialize();
  #endif
  sig.allocate("sig");
  #ifndef NO_AD_INITIALIZE
  sig.initialize();
  #endif
  K.allocate("K");
  #ifndef NO_AD_INITIALIZE
  K.initialize();
  #endif
  MSY.allocate("MSY");
  #ifndef NO_AD_INITIALIZE
  MSY.initialize();
  #endif
  SSQ.allocate("SSQ");
  #ifndef NO_AD_INITIALIZE
  SSQ.initialize();
  #endif
  BMSY.allocate("BMSY");
  #ifndef NO_AD_INITIALIZE
  BMSY.initialize();
  #endif
  FMSY.allocate("FMSY");
  #ifndef NO_AD_INITIALIZE
  FMSY.initialize();
  #endif
  epsilon.allocate(1,nobs-1,"epsilon");
  #ifndef NO_AD_INITIALIZE
    epsilon.initialize();
  #endif
  Ppred.allocate(1,nobs-1,"Ppred");
  #ifndef NO_AD_INITIALIZE
    Ppred.initialize();
  #endif
  Pobs.allocate(1,nobs-1,"Pobs");
  #ifndef NO_AD_INITIALIZE
    Pobs.initialize();
  #endif
  sd_sig.allocate("sd_sig");
}

void model_parameters::preliminary_calculations(void)
{

#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
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
  
  
}

void model_parameters::userfunction(void)
{
  f =0.0;
  //*******MAIN PROGRAM********
  		init_states();
  		calc_residuals();
  		calc_ref_points();
  		calc_obj_fun();
  		sd_sig=sig;
  		ofstream ofs("par.mcmc",ios::app);
  		if(mceval_phase()) mcmc_stuff();  		
  //******************************
	
}

void model_parameters::init_states(void)
{
	K=mfexp(lnK);
	MSY=mfexp(lnMSY);
	// calc z, zMSY, and meanP
	z=(1/(n-1))*pow(n,(n/(n-1)));
	zMSY=z*MSY;
    meanP=mean(Pobs);
	
	
}

void model_parameters::calc_residuals(void)
{
  for(int j=1;j<=nobs-1;j++)
  {
    Ppred(j)=zMSY*((biomass(j)/K) - pow(biomass(j)/K,n));			
  }
  epsilon=Pobs-Ppred;
  sig=sqrt(norm2(epsilon)/(nobs-1));
  SSQ=norm2(epsilon);
 
}

void model_parameters::calc_ref_points(void)
{
  BMSY=pow(n,(1/(1-n)))*K;
  FMSY=MSY/BMSY;
  
  
}

void model_parameters::calc_obj_fun(void)
{
  f=dnorm(epsilon,sig);
 
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
	report<<BMSY<<"\n"<<n<<endl;
	
}

void model_parameters::mcmc_stuff(void)
{
	if(iter==0){
		ofstream ofs("par.mcmc");
		ofs<<"K\t"<<"MSY\t"<<"n\t"<<endl;
		cout<<iter<<endl;
	}
	iter++;
	ofstream ofs("par.mcmc",ios::app);	
	ofs<<K<<"\t"<<MSY<<"\t"<<n<<endl;
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
