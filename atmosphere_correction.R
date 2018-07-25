T_obj <- function(DN, emissivity, T_refl, T_atm, humid, dist){
  #T_refl and T_atm are in K unit
  #dist is in meter unit 
  
  library(pracma)
  #constants
  sigma = 5.67*10^-8 #sigma is the Stefan-Boltzmann constant
  K_atm = 1.9
  h1 = 1.5587
  h2 = 6.939*10^-2
  h3 = -2.7816*10^-4
  h4 = 6.8455*10^-7
  alpha1 = 0.0066
  alpha2 = 0.0126
  beta1 = -0.0023
  beta2 = -0.0067
  epsilon = emissivity #epsilon is the emissivity
  D = dist
  
  #w is a function of relative humidity and atmosphere temperature
  w <- humid*exp(h1+h2*T_atm+h3*T_atm^2+h4*T_atm^3)
  #tau is the transmittance of atmosphere
  tau <- K_atm*exp(-sqrt(D)*(alpha1+beta1*sqrt(w)))+(1-K_atm)*exp(-sqrt(D)*(alpha2+beta2*sqrt(w)))
  #W_tot is the total radiation
  W_tot <- sigma*(0.04*DN)^4
  T_obj <- nthroot(((W_tot-(1-epsilon)*tau*sigma*T_refl^4-(1-tau)*sigma*T_atm^4)/(epsilon*tau*sigma)),4)
  return(T_obj-273.15)
}