%% Basic Model 

%----------------------------------------------------------------
% 0. Housekeeping (close all graphic windows)
%----------------------------------------------------------------

close all;

%----------------------------------------------------------------
% 1. Defining variables
%----------------------------------------------------------------


% Elastic Labor Supply
var qh ql    wnh wnl wsh wsl  p  nnh nnl nsh nsl   lnh lnl lsh lsl ;
parameters psi epsilon r theta ps A  AS alpha  rho eta H  beta kappa_1 pi db de toggle toth totl ;


	% Inelastic Labor Supply
	%var qh ql nnh nnl nsh nsl wnh wnl wsh wsl p ps ;
	%parameters epsilon r theta  A AS alpha  rho eta H pi beta kappa_1 db de toggle lnh lnl lsh lsl;


%parameters epsilon r theta psi A alpha  rho eta H pi beta kappa_1 db de nnh nnl nsh nsl;;

%varexo  eta;

%----------------------------------------------------------------
% 2. Calibration
% Using Stone Geary
%----------------------------------------------------------------

%Elasticity of Labor Supply
epsilon = 0.6;

%Discount Rate
r       = 0.05;

%Skill Premium
theta   = 1.7;

%North and South Productivity
A       = 1.8;
AS	 = 1;

% Non-Labor Share
alpha   = 0.35;

%Elast of Sub between Skill Types
rho     = 0.6;

%Elasticity of Housing
eta     = .4;

% Baseline Housing Req
H       = 0;

% Share of Housing for Income Above Baseline
beta    = 0.94;

%Profits as Normalized for Inelastic Case
pi 	 = H;

%Migration Cost/ Speed Parameter
kappa_1 = .2500;

% Notation Transformations
db      = (beta^beta)*(1-beta)^(1-beta);
de	 = 1+(1/epsilon);
ps      = 1;
psi     = .002;

%Toggle the Elast Labor Supply
toggle  =1;

toth=.25;
totl =.75;

%	lnh     =1;
%	lnl     =1;
%	lsh	 =1;
%	lsl     =1;

%----------------------------------------------------------------
% 3. Model
%----------------------------------------------------------------

model; 

% Wage equations
  wnh = (1-alpha)* theta*A* ( ( ( (theta*lnh*nnh)^rho )+( (lnl*nnl)^rho ) )^((1-alpha-rho)/rho) ) *(theta*lnh*nnh)^(rho-1);
  wnl = (1-alpha)*       A* ( ( ( (theta*lnh*nnh)^rho )+( (lnl*nnl)^rho ) )^((1-alpha-rho)/rho) ) *      (lnl*nnl)^(rho-1);
  wsh = (1-alpha)* theta*AS*( ( ( (theta*lsh*nsh)^rho )+( (lsl*nsl)^rho ) )^((1-alpha-rho)/rho) ) *(theta*lsh*nsh)^(rho-1);
  wsl = (1-alpha)*       AS*( ( ( (theta*lsh*nsh)^rho )+( (lsl*nsl)^rho ) )^((1-alpha-rho)/rho) ) *      (lsl*nsl)^(rho-1);

% Elastic Labor Supply
%  lnh=(1/toggle)^epsilon*( db*         ( (1/p)^(1-beta)  )  *wnh )^epsilon;
%  lnl=(1/toggle)^epsilon*( db*         ( (1/p)^(1-beta)  )  *wnl )^epsilon;
%  lsh=(1/toggle)^epsilon*( db*         ( (1/ps)^(1-beta) )  *wsh )^epsilon;
%  lsl=(1/toggle)^epsilon*( db*         ( (1/ps)^(1-beta) )  *wsl )^epsilon;

 lnh=(1/toggle)^epsilon*(    ( (1/p)^(1-beta)  )  *wnh )^epsilon;
 lnl=(1/toggle)^epsilon*(    ( (1/p)^(1-beta)  )  *wnl )^epsilon;
 lsh=(1/toggle)^epsilon*(    ( (1/ps)^(1-beta) )  *wsh )^epsilon;
 lsl=(1/toggle)^epsilon*(    ( (1/ps)^(1-beta) )  *wsl )^epsilon;




% Population
    nsh+nnh=toth;
    nsl+nnl=totl;

%  Value Functions
      
    qh =  log(   db* (wnh*lnh-H*p+pi)*( (1/p)^(1-beta) ) -toggle*(db/de)*lnh^de   )-log( db* (wsh*lsh-H*ps+pi)*( (1/ps)^(1-beta) )-toggle*(db/de)*lsh^de )  + exp(-r)*qh(+1);
    ql =  log(   db* (wnl*lnl-H*p+pi)*( (1/p)^(1-beta) ) -toggle*(db/de)*lnl^de   )-log( db* (wsl*lsl-H*ps+pi)*( (1/ps)^(1-beta) )-toggle*(db/de)*lsl^de )  + exp(-r)*ql(+1);


% Profits

 % pi =  A*( ( ( (theta*lnh*nnh)^rho )+( (lnl*nnl)^rho ) )^((1-alpha-rho)/rho) ) + AS*( ( ( (theta*lsh*nsh)^rho )+( (lsl*nsl)^rho ) )^((1-alpha-rho)/rho) )  - wnh*lnh*nnh - wnl*lnl*nnl-wsh*lsh*nsh-wsl*lsl*nnl;

% Migration Equations

  log(nnh/nnh(-1)) - log(nsh/nsh(-1))  = psi*(qh) ;
  log(nnl/nnl(-1)) - log(nsl/nsl(-1))  = psi*(ql) ;




% Housing Price
 
   p =  1+(nnh*( H+ (1-beta)*(wnh*lnh/p ) ) + nnl*( H+(1-beta)*(wnl*lnl/p) ) )^1/eta;
   %ps = 1+(nsh*( H+ (1-beta)*(wsh*lsh/ps) ) + nsl*( H+(1-beta)*(wsl*lsl/ps)) )^1/eta;
  	 
end;

%----------------------------------------------------------------
% 4. Computation
%----------------------------------------------------------------

initval;
nnh = 0.69*toth ;
nnl = 0.63*totl;
nsh = (toth -nnh) ;
nsl = (totl-nnl);
%pi = 1;
p =  1;
%ps = 1;
 

lnh=1;
lnl=1;
lsh=1;
lsl=1;

wnh = (1-alpha)* theta*A* (( ((theta*lnh*nnh)^rho)+((lnl*nnl)^rho))^((1-alpha-rho)/rho))*(theta*lnh*nnh)^(rho-1);
wnl = (1-alpha)*       A* (( ((theta*lnh*nnh)^rho)+((lnl*nnl)^rho))^((1-alpha-rho)/rho))*(lnl*nnl)^(rho-1);
wsh = (1-alpha)* theta*AS*(( ((theta*lsh*nsh)^rho)+((lsl*nsl)^rho))^((1-alpha-rho)/rho))*(theta*lsh*nsh)^(rho-1);
wsl = (1-alpha)*       AS*(( ((theta*lsh*nsh)^rho)+((lsl*nsl)^rho))^((1-alpha-rho)/rho))*(lsl*nsl)^(rho-1);


qh = log(wnh/wsh)/(1-exp(-r)) ;
ql = log(wnl/wsl)/(1-exp(-r))  ;

end;




endval;

nnh = toth/(1+(A/AS)^(-1/alpha));
nnl = totl/(1+(A/AS)^(-1/alpha));

nsh = (toth-nnh);
nsl = (totl-nnl);
%pi = 0;
lnh=1;
lnl=1;
lsh=lnh;
lsl=lnl;
 
  wnh = (1-alpha)* theta*A* (( ((theta*lnh*nnh)^rho)+((lnl*nnl)^rho))^((1-alpha-rho)/rho))*(theta*lnh*nnh)^(rho-1);
  wnl = (1-alpha)*       A* (( ((theta*lnh*nnh)^rho)+((lnl*nnl)^rho))^((1-alpha-rho)/rho))*(lnl*nnl)^(rho-1);
  wsh = (1-alpha)* theta*AS*(( ((theta*lsh*nsh)^rho)+((lsl*nsl)^rho))^((1-alpha-rho)/rho))*(theta*lsh*nsh)^(rho-1);
  wsl = (1-alpha)*       AS*(( ((theta*lsh*nsh)^rho)+((lsl*nsl)^rho))^((1-alpha-rho)/rho))*(lsl*nsl)^(rho-1);

p =  1;
%ps = 1;

qh = 0 ;
ql = 0 ;


end;

maxit_=2000; 
steady;

%shocks;
%var eta;
%periods 1:50;
%values 500;
%end;



check;

simul(periods=500);


