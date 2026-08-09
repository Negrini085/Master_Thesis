# This is the implementation of a degree-day based model to evaluate snow water
# equivalent. Total precipitations, as well as minimum and maximum temperature on a 
# daily basis are required as inputs. The model describes separately the two processes 
# that govern the snowpack mass balance at the ground: melt and accumulation.
#
# Snowmelt is computed using a degree-day approach: daily melt is assumed proportional 
# to the excess of temperature above a melting threshold, multiplied by a degree-day factor (DDF). 
# Unlike the classical formulation, which treats the DDF as constant, here the factor is 
# allowed to vary seasonally following the approach proposed by Magnusson, which accounts for 
# seasonal changes in the radiative balance.
#
# Accumulation, i.e. the fraction of precipitation falling as solid precipitation (snow), is 
# instead estimated through a temperature threshold model. For each day i, the minimum (Tn) and 
# maximum (Tx) temperatures are compared a gainst a threshold temperature th: if both exceed the 
# threshold, all precipitation P is considered liquid and the solid component SP is zero; if both 
# are below or equal to the threshold, all precipitation is considered snow (SP = P); in the 
# intermediate case a mixed solid/liquid condition is assumed.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/")