### Green Ampt with Redistribution + Overland Flow - Soil Parameters ###
##By: Michelle Ives ###
## Fall 2023 ###

### Define Soil Paramters for GA/GAR Model
#Paramaters comes from Rawls et al. 1982 

#### Define Soil Parameters/Constants ####
#CURRENT SOIL TEXTURE: Silt Loam
KS<-<-Ks<-0.3 #saturated hydraulic conductivity (cm/hr)
phi<-psi<-50 #Suction/Pressure Head (cm)
FC<-0.187 # field capacity (can also come from SWRC)
wilting_point<-WP<-0.148 # wilting point (can also come from SWRC)
lambda<-0.319 #Pore Size Distribution Index 

#parameters for Brooks and Corey Equation
#assuming isotropic medium
#Silt Loam
a_BC<-2
b_BC<-3
lambda_BC<-0.319 #Pore Size Distribution Index 
theta_r<-theta_r_BC<-0.068 #Residual Water Content
theta_s<-theta_s_BC<-0.398 #Soil Moisture Saturated/Total Porosity
theta_e #effective porosity




