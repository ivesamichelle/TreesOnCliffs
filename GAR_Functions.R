### Green Ampt with Redistribution + Overland Flow - FUNCTIONS ###
##By: Michelle Ives ###
## Fall 2023 ###


#### Calling Required Functions from Other Codes ####

#calling K function from other code
#hc_function(wetness)
#determines hydraulic conductivity depending on wetness
#parameters/constants required are: thetar, thetas, KS, m (thetar, thetas and m are estimated from VG curve fitting)
#output is in m/day, to convert to cm/day --> *(100/24)
source('~/Desktop/Data and R Scripts/Van_Ganuchten.R')

#### Creating All Required Functions ####

#defining functions for all ponding scenarios
#CASE 1
# f1 (of previous timestep) > rainfall
# ponding does not occur during the time step
#F
Ft_Case1<-function(Ft_last,I)
{
  Ft_output_case1<-Ft_last+(I*delta_t)
}
#f Case 1
f_Case1<-function(Ft)
{
  f_output<- Ks*(1+((Md*psi)/Ft))
}

#CASE 2
#f1 (of previous timestep) < rainfall
#ponding occurs during whole timestep
#F
Ft_Case2_40<-function(Ft_last) #output = Ft_40
{
  Ft_1=(Ks*delta_t)+Ft_last+(psi*Md*log(((Ft_last+0.01)+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_2=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_1+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_3=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_2+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_4=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_3+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_5=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_4+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_6=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_5+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_7=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_6+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_8=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_7+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_9=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_8+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_10=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_9+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_11=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_10+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_12=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_11+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_13=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_12+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_14=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_13+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_15=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_14+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_16=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_15+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_17=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_16+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_18=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_17+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_19=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_18+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_20=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_19+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_21=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_20+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_22=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_21+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_23=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_22+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_24=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_23+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_25=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_24+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_26=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_25+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_27=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_26+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_28=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_27+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_29=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_28+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_30=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_29+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_31=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_30+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_32=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_31+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_33=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_32+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_34=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_33+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_35=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_34+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_36=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_35+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_37=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_36+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_38=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_37+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_39=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_38+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_40=(Ks*delta_t)+Ft_last+(psi*Md*log((Ft_39+(psi*Md))/(Ft_last + (psi*Md))))
}
#f
f_case2<-function(Ft){
  outputf<- (Ks * (1+((psi*Md)/Ft)))
}

#CASE 3
#f1 (of previous timestep) > rainfall, but f1 (at the end of timestep) < rainfall
#ponding occurs sometime during the timestep
#F
Ft_Case3_40<-function(Ft_last, dt) #output = Ft_40
{
  Ft_1=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log(((Ft_last+0.01)+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_2=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_1+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_3=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_2+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_4=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_3+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_5=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_4+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_6=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_5+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_7=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_6+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_8=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_7+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_9=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_8+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_10=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_9+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_11=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_10+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_12=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_11+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_13=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_12+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_14=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_13+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_15=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_14+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_16=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_15+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_17=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_16+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_18=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_17+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_19=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_18+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_20=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_19+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_21=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_20+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_22=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_21+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_23=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_22+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_24=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_23+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_25=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_24+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_26=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_25+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_27=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_26+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_28=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_27+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_29=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_28+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_30=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_29+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_31=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_30+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_32=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_31+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_33=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_32+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_34=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_33+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_35=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_34+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_36=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_35+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_37=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_36+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_38=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_37+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_39=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_38+(psi*Md))/(Ft_last + (psi*Md))))
  Ft_40=(Ks*(delta_t-dt))+Ft_last+(psi*Md*log((Ft_39+(psi*Md))/(Ft_last + (psi*Md))))
}
f_case3<-function(Ft) #output = outputf
{
  outputf3<- (Ks * (1+((psi*Md)/Ft)))
}

###### Function to compute the capillary drive G(theta_i, theta_0) #####
compute_capillary_drive <- function(theta_i, theta_0) {
  Psi <- phi  # Average suction across the wetting fronts
  Theta_i <- (theta_i - theta_r) / (theta_s - theta_r)
  Theta_0 <- (theta_0 - theta_r) / (theta_s - theta_r)
  
  G <- Psi * (Theta_0^(3 + 1/lambda) - Theta_i^(3 + 1/lambda)) / (1 - Theta_i^(3 + 1/lambda))
  
  return(G)
}

## Function to simulate Green Ampt infiltration for a single timestep
simulate_GA_model<-function(f_actual_i_last, input_rate_i, F_actual_i_last, dt) {
  if(f_actual_i_last < input_rate_i) {
    #first we calculate F for this time step and we name the output Ft_out
    Ft_out_2<-Ft_Case2_40(F_actual_i_last)
    #then calculate f from that output
    f_out_2<-f_case2(Ft_out_2)
    
    f_new<-f_out_2
    F_new<-Ft_out_2
    
  } else {
    #first we calculate F for this time step and we name the output Ft_out
    Ft_out_1<-Ft_Case1(F_actual_i_last, input_rate_i)
    #then calculate f from that output
    f_out_1<-f_Case1(Ft_out_1)
    
    if(f_out_1>input_rate_i) {
      f_new<-f_out_1
      F_new<-Ft_out_1
    }
    else {
      #find the volume required for surface ponding
      FP<-(Ks*Md*psi)/(input_rate_i-Ks)
      #find the time to the start of ponding
      dt<-(FP-F_actual_i_last)/input_rate_i
      #calculate the new F t+1
      F_out_3<-Ft_Case3_40(F_actual_i_last, dt)
      #calculate f also
      f_out_3<-f_case2(F_out_3)
      #add these results to the data frame
      f_new<-f_out_3
      F_new<-F_out_3
    }
  }
  theta_new<-theta_s
  Z_new<-F_new/(theta_new-theta_i)
  
  return(list(F_new = F_new, f_new = f_new, theta_new=theta_new, Z_new=Z_new))
}

##### Function to simulate Redistribution for a single time step  ####
simulate_gar_model<-function(F_last, theta_last, theta_i, r, Ev, Ki, dt, Z_last) {
  #compute variables for GAR model
  F_new<-F_last+((r-Ev-Ki)*(dt)) #new F is equal to whatever the change in F was during that timestep + the previous F value
  Z<-Z_last #(we start by using Z from the last timestep)
  
  #defining initial values for RK4
  SMII<-theta_last #theta from last time step is the input
  h<-0.01
  t_star<-seq(0,0.5,by=h)
  SM_0_RK <- rep(0, length(t_star))
  SM_0_RK[1]<-SMII
  
  R<-r #rainfall from this timestep
  ET<-Ev #evapotranspiration from this timestep
  G <- compute_capillary_drive(theta_i, theta_last)
  
  #running the RK4
  for (j in 1:(length(t_star)-1)) {
    
    k1<-(1/Z)*(R-ET-Ki-(hc_function(SM_0_RK[j])*(100/24))-((Ks*G)/Z));
    y1<-SM_0_RK[j]+k1*h/2;
    
    k2<-(1/Z)*(R-ET-Ki-(hc_function(y1)*(100/24))-((Ks*compute_capillary_drive(theta_i, y1))/Z));
    y2<-SM_0_RK[j]+k2*h/2;
    
    k3<-(1/Z)*(R-ET-Ki-(hc_function(y2)*(100/24))-((Ks*compute_capillary_drive(theta_i, y2))/Z));
    y3<-SM_0_RK[j]+k3*h;
    
    k4<-(1/Z)*(R-ET-Ki-(hc_function(y3)*(100/24))-((Ks*compute_capillary_drive(theta_i, y3))/Z));
    
    SM_0_RK[j+1]<-SM_0_RK[j] + (k1 + 2*k2 + 2*k3 + k4) * h/6;
  }
  
  #now we have our new soil moisture
  theta_new<-SM_0_RK[j+1]
  
  #now we can calculate out new infiltration depth, Z
  Z_new<-F_new/(theta_new-theta_i)
  
  return(list(F_new=F_new, theta_new=theta_new, Z_new=Z_new))
  
}

#Brooks and Corey Hydraulic Conductivity Function #
#only used if Soil Water Retention Curve isn't available #
hc_function_BC<-function(wetness){
  theta_relative<-(wetness-theta_r_BC)/(theta_s_BC-theta_r_BC)
  HC<-KS*(theta_relative)^(b_BC+(a_BC/lambda_BC))
}
#output units are cm/hr

#Solve hydraulic head for Actual Evapotranspiration (if using AET instead of ET)
solve_hydraulic_head<-function(theta_in){
  h <- ((exp(1)^(log((parameters$thetas - parameters$thetar) / (theta_in - parameters$thetar)) / (1 - 1/parameters$n)) - 1) / (parameters$alpha)^parameters$n)^(1/parameters$n)
  print(h)
}

#### Define Soil Parameters/Constants ####
# Soil Texture = Clay - All From Literature - Rawls
#CURRENT SOIL TEXTURE: Silt Loam
KS<-0.3
Ks<-0.3
psi<-50 #46.65-63.6
phi<-psi #average suction across the wetting front
#theta_s<-parameters$thetas
#theta_e<-0.321
FC<-0.187
WP<-0.148
#theta_r<-parameters$thetar
lambda<-0.319
wilting_point<-WP

#parameters for Brooks and Corey Equation
#assuming isotropic medium
#sandy clay soil
a_BC<-2
b_BC<-3
lambda_BC<-0.319
theta_r_BC<-0.068
theta_r<-theta_r_BC
theta_s_BC<-0.398
theta_s<-theta_s_BC
FC<-0.187
wilting_point<-WP
WP<-WP

#### Replace with these values for Sandy Clay Loam ####
#Also go in VG curve to change KS to KS for Sandy Clay Loam to simulate new constants
#KS #0.30 – 0.43#Saturated Hydraulic Conductivity (cm/hr) - From Literature.
#psi # 57.77 – 64.7 #Suction/Pressure Head (cm) - From Literature.
#theta_s<-0.442 #Soil Moisture Saturated/Total Porosity - From VG Function or Literature.
#theta_e #Effective Porosity - From Literature
#FC<-0.295 #Review This #Field Capacity - From SWRC or Literature
#WP<-0.189 #Review This #Wilting Point - From SWRC or Literature
#theta_r<-0.068 #Residual Water Content - From Literature.
#lambda<-0.319 #Pore Size Distribution Index - From Literature?



#### Define Contributing Area and Fracture Parameters ####
Fracture_Depth<-0.5 #Fracture Width/Depth (m)
Fracture_Length<-2.0 #Frature Length (m)
Fracture_SA<-Fracture_Depth*Fracture_Length #Fracture Surface Area (m^2)

CA_Height<-1 #contributing area height (m)
CA_SA<-CA_Height*Fracture_Length #contrbuting area surface area (m^2)


#### Importing Input Data - Data COULD be Already Cropped to [i-1] of first rainfall event####
#Input Data Should have the following colums:
#DateTime, Rain_mm, Temp_C, ET_0_mm_hr, ET_0_cm_hr
GAR_Input_Data <-read.csv('~/Desktop/Data and R Scripts/GAR_Model_Data_Input_2022.csv')
#turning DateTime into time
GAR_Input_Data$DateTime <- as.POSIXct(GAR_Input_Data$DateTime, format="%Y-%m-%d %H:%M", tz="MST")

#calculate timestep
time_difference<-(GAR_Input_Data$DateTime[2]-GAR_Input_Data$DateTime[1])
minutes <- as.numeric(gsub("\\D", "", time_difference))
time_step<-minutes/60 #timestep in hours


#### Calculating Overland Flow for WHOLE DATA FRAME ####
#calculate input from overland flow

#creating a new column for overland flow
GAR_Input_Data$Overland_Flow<-NA
#creating a new column that is the precipitation rate (mm/hr)
GAR_Input_Data$Rain_cm_hr<-(GAR_Input_Data$Rain_mm*2)/10

#define timestep
timestep<-time_step 

#using the following rules:
#if precipitation rate is less than 0.05 cm/hour, 
#AND the sum of the precipitation in the previous 4 hours is 0
#then the Rate_2 is 0
#else: Rate_2 is equal to the actual rate of precipitation
GAR_Input_Data$Rate_P_OF<-NA

for (i in 5:dim(GAR_Input_Data)[1]) {
  if(GAR_Input_Data$Rain_cm_hr[i] < 0.05 & sum(GAR_Input_Data$Rain_cm_hr[i-1], GAR_Input_Data$Rain_cm_hr[i-2], GAR_Input_Data$Rain_cm_hr[i-3], GAR_Input_Data$Rain_cm_hr[i-4] ==0)) {
    #overland flow should equal zero in this case
    GAR_Input_Data$Rate_P_OF[i]<-0
  } else {
    GAR_Input_Data$Rate_P_OF[i]<- GAR_Input_Data$Rain_cm_hr[i]
  }
}

#Now find volume of water that fell on the whole CA during the time step
GAR_Input_Data$Volume_CA<-NA

for (i in 1:dim(GAR_Input_Data)[1]) {
  GAR_Input_Data$Volume_CA[i]<-(GAR_Input_Data$Rate_P_OF[i]*timestep) * CA_SA
}

#rate of overland flow into fracture at each point (mm/hour)

for (i in 1:dim(GAR_Input_Data)[1]) {
  GAR_Input_Data$Overland_Flow[i]<-(GAR_Input_Data$Volume_CA[i]/Fracture_SA)*(1/timestep)
}

#now adding the rate of precipitation and rate of overland flow input together to get an input rate
GAR_Input_Data$input_rate<-NA
GAR_Input_Data$input_rate<-GAR_Input_Data$Rain_cm_hr+GAR_Input_Data$Overland_Flow

GAR_Input_Data

#make input rate for row 1-4 0
GAR_Input_Data$input_rate[1:4]<-0

#now get rid of any intermediate columns that were created and no longer needed
GAR_Input_Data <- subset(GAR_Input_Data, select = -c(Rate_P_OF,Volume_CA))


#### Cropping Data and Dividing Input Data into Events - DEFINE START DATE ####
##### NEED TO DEFINE THE START DATE FOR DIFFERENT KS 
fig_crop_date <- plot_ly(data = GAR_Input_Data, x = ~DateTime, y = ~input_rate, mode="lines+markers")

#cropping data to include [i-1] from first event
#start date
#startdate<-as.POSIXct("2021-03-23 11:30:00", tz="MST") #define
#startdate<-as.POSIXct("2021-04-20 11:30:00", tz="MST") #define
startdate<-as.POSIXct("2022-04-20 11:30:00", tz="MST") #define


#crop by start date
GAR_Input_Data<-subset(GAR_Input_Data, DateTime>=startdate)
#GAR_Input_Data<-GAR_Input_Data[96:8838,]

#divide input data into rainfall events
#function to identify rainfall events and compute event lengths 
identify_rainfall_events<-function(rainfall){
  #initialize event variables
  event_start<-0
  event_indices<-list()
  
  #iterate over time steps
  for(i in 1:(length(rainfall)-1)) {
    #check if current time step is the start of a rainfall event
    if(rainfall[i]>KS && rainfall[i-1]<=KS) {
      event_start<-i
    }
    #check if current time step is the end of a rainfall event
    if(rainfall[i]<=KS && rainfall[i+1]>KS && event_start>0) {
      event_indices[[length(event_indices) +1]] <-c(event_start: i)
      event_start<-0
    }
  }
  #return event indices
  return(event_indices)
}

#identify rainfall events and compute event lengths
event_indices<-identify_rainfall_events(GAR_Input_Data$input_rate)

#separate data for each rainfall event
rainfall_events_data<-list()
for (i in 1:length(event_indices)) {
  event_data<-GAR_Input_Data[event_indices[[i]], ]
  rainfall_events_data[[i]]<-event_data
}

#can access each rainfall event data using:
#rainfall_events_data[[i]]
event_indices
#event data = rainfall_events_data[[i]]

#### Creating Output Data Frame ####
Z1<-NA
Z2<-NA
Z3<-NA
Z4<-NA
Z5<-NA
Z6<-NA

SM_0_1<-NA
SM_0_2<-NA
SM_0_3<-NA
SM_0_4<-NA
SM_0_5<-NA
SM_0_6<-NA

F1<-NA
F2<-NA
F3<-NA
F4<-NA
F5<-NA
F6<-NA

f<-NA
AET<-NA
N_r1<-NA
T_r1<-NA
N_r2<-NA
T_r2<-NA
N_r3<-NA
T_r3<-NA
N_r4<-NA
T_r4<-NA
N_r5<-NA
T_r5<-NA
N_r6<-NA
T_r6<-NA

event_indices

#create data.frame
GAR_Output_Data<-data.frame(GAR_Input_Data$DateTime, Z1, SM_0_1, F1, Z2, SM_0_2, F2, Z3, SM_0_3, F3, Z4, SM_0_4, F4, Z5, SM_0_5, F5, Z6, SM_0_6, F6, 
                            f, AET, N_r1, T_r1, N_r2, T_r2, N_r3, T_r3, N_r4, T_r4, N_r5, T_r5, N_r6, T_r6)
#rename first column to make sure it matches small output data
colnames(GAR_Output_Data)[1] <- "DateTime"

#### Define Initial Conditions for start of event 1 ####
#add these initial conditions to big GAR_Output_Data data frame
GAR_Output_Data[1, c("Z1", "Z2", "Z3", "Z4", "Z5", "Z6")] <- 0
GAR_Output_Data[1, c("SM_0_1", "SM_0_2", "SM_0_3", "SM_0_4", "SM_0_5", "SM_0_6")] <- FC
GAR_Output_Data[1, c("F1", "F2", "F3", "F4", "F5", "F6")] <- 0
GAR_Output_Data$f[1]<-0

#initial conditions are the row before the row of the start of the event

#############################################################################
### MODEL SIMULATION ###
#############################################################################

###### define redistribution coefficient parameters ######
a_1<-(4.2952+(154.6101*((Ks*10)^(-1))))^((-1)^1)
a_2<-(0.0020+(-0.0010*((Ks*10)^(0.5))))^((-1)^2)
a_3<-(-14.0032+(-61.5429*((Ks*10)^(-1))))^((-1)^3)

##### converting PET to AET #####
#calculate h_50
wilting_point
FC
diff<-(FC-wilting_point)/2
VWC_50<-wilting_point+diff
solve_hydraulic_head(VWC_50) #0.6137369 bar

#creating function to calculate AET #MOVE THIS ABOVE?
calculate_AET<-function(PET, VWC_50, theta){
  AET<-PET*(1/(1+(solve_hydraulic_head(theta)/solve_hydraulic_head(VWC_50))^3))
  print(AET)
}

##### Apply the model to all of the rainfall events #####

for(i in 1:length(event_indices)) {
  #create output data frame for this event
  event_output_data<-data.frame(
    DateTime=rainfall_events_data[[i]]$DateTime,Z1=NA, SM_0_1=NA, F1=NA, Z2=NA, SM_0_2=NA, 
    F2=NA,Z3=NA, SM_0_3=NA, F3=NA, Z4=NA, SM_0_4=NA, F4=NA, Z5=NA,SM_0_5=NA, F5=NA, Z6=NA, SM_0_6=NA, F6=NA, f=NA, AET=NA, N_r1=NA, T_r1=NA, N_r2=NA, T_r2=NA, 
    N_r3=NA, T_r3=NA, N_r4=NA, T_r4=NA, N_r5=NA, T_r5=NA, N_r6=NA, T_r6=NA)
  
  #define initial conditions for event (row from Output Data before start of event)
  initial_conditions_row<-(event_indices[[i]][1])-1
  initial_conditions<-GAR_Output_Data[initial_conditions_row,]
  
  ####initial conditions ####
  #now need to shift if necessary, so re-distributing front always moves forward (Z1<-Z2<-Z3<-Z4<-Z5<-Z6)
  #If WF1 is empty, shift the first non zero wetting front to WF1
  if(initial_conditions$Z1==0 & initial_conditions$Z2>0) {
    initial_conditions$Z1<-initial_conditions$Z2
    initial_conditions$F1<-initial_conditions$F2
    initial_conditions$SM_0_1<-initial_conditions$SM_0_2
    initial_conditions$Z2<-0
    initial_conditions$F2<-0
    initial_conditions$SM_0_2<-wilting_point
  } else if(initial_conditions$Z1==0 & initial_conditions$Z2==0) {
    if(initial_conditions$Z1==0 & initial_conditions$Z3>0) {
      initial_conditions$Z1<-initial_conditions$Z3
      initial_conditions$F1<-initial_conditions$F3
      initial_conditions$SM_0_1<-initial_conditions$SM_0_3
      initial_conditions$Z3<-initial_conditions$Z2
      initial_conditions$F3<-initial_conditions$F2
      initial_conditions$SM_0_3<-initial_conditions$SM_0_2
    } else if(initial_conditions$Z1==0 & initial_conditions$Z3==0 & initial_conditions$Z4>0){
      initial_conditions$Z1<-initial_conditions$Z4
      initial_conditions$F1<-initial_conditions$F4
      initial_conditions$SM_0_1<-initial_conditions$SM_0_4
      initial_conditions$Z4<-0
      initial_conditions$F4<-0
      initial_conditions$SM_0_4<-wilting_point
    } else if(initial_conditions$Z1==0 & initial_conditions$Z3==0 & initial_conditions$Z4==0 & initial_conditions$Z5>0){
      initial_conditions$Z1<-initial_conditions$Z5
      initial_conditions$F1<-initial_conditions$F5
      initial_conditions$SM_0_1<-initial_conditions$SM_0_5
      initial_conditions$Z5<-0
      initial_conditions$F5<-0
      initial_conditions$SM_0_5<-wilting_point
    } else if(initial_conditions$Z1==0 & initial_conditions$Z3==0 & initial_conditions$Z4==0 & initial_conditions$Z5==0 & initial_conditions$Z6>0){
      initial_conditions$Z1<-initial_conditions$Z6
      initial_conditions$F1<-initial_conditions$F6
      initial_conditions$SM_0_1<-initial_conditions$SM_0_6
      initial_conditions$Z6<-0
      initial_conditions$F6<-0
      initial_conditions$SM_0_6<-wilting_point
    } else{}
  } else{}
  #If WF2 is empty, shift the first non-zero wetting front to WF2
  if(initial_conditions$Z2==0 & initial_conditions$Z3>0) {
    initial_conditions$Z2<-initial_conditions$Z3
    initial_conditions$F2<-initial_conditions$F3
    initial_conditions$SM_0_2<-initial_conditions$SM_0_3
    initial_conditions$Z3<-0
    initial_conditions$F3<-0
    initial_conditions$SM_0_3<-wilting_point
  } else if(initial_conditions$Z2==0 & initial_conditions$Z3==0 & initial_conditions$Z4>0) {
    initial_conditions$Z2<-initial_conditions$Z4
    initial_conditions$F2<-initial_conditions$F4
    initial_conditions$SM_0_2<-initial_conditions$SM_0_4
    initial_conditions$Z4<-0
    initial_conditions$F4<-0
    initial_conditions$SM_0_4<-wilting_point
  } else if(initial_conditions$Z2==0 & initial_conditions$Z3==0 & initial_conditions$Z4==0 & initial_conditions$Z5>0) {
    initial_conditions$Z2<-initial_conditions$Z5
    initial_conditions$F2<-initial_conditions$F5
    initial_conditions$SM_0_2<-initial_conditions$SM_0_5
    initial_conditions$Z5<-0
    initial_conditions$F5<-0
    initial_conditions$SM_0_5<-wilting_point
  } else if(initial_conditions$Z2==0 & initial_conditions$Z3==0 & initial_conditions$Z4==0 & initial_conditions$Z5==0 & initial_conditions$Z6>0) {
    initial_conditions$Z2<-initial_conditions$Z6
    initial_conditions$F2<-initial_conditions$F6
    initial_conditions$SM_0_2<-initial_conditions$SM_0_6
    initial_conditions$Z6<-0
    initial_conditions$F6<-0
    initial_conditions$SM_0_6<-wilting_point
  } else{}
  #If WF3 is empty, shift the first non-zero wetting front to WF3
  if(initial_conditions$Z3==0 & initial_conditions$Z4>0) {
    initial_conditions$Z3<-initial_conditions$Z4
    initial_conditions$F3<-initial_conditions$F4
    initial_conditions$SM_0_3<-initial_conditions$SM_0_4
    initial_conditions$Z4<-0
    initial_conditions$F4<-0
    initial_conditions$SM_0_4<-wilting_point
  } else if(initial_conditions$Z3==0 & initial_conditions$Z4==0 & initial_conditions$Z5>0) {
    initial_conditions$Z3<-initial_conditions$Z5
    initial_conditions$F3<-initial_conditions$F5
    initial_conditions$SM_0_3<-initial_conditions$SM_0_5
    initial_conditions$Z5<-0
    initial_conditions$F5<-0
    initial_conditions$SM_0_5<-wilting_point
  } else if(initial_conditions$Z3==0 & initial_conditions$Z4==0 & initial_conditions$Z5==0 & initial_conditions$Z6>0) {
    initial_conditions$Z3<-initial_conditions$Z6
    initial_conditions$F3<-initial_conditions$F6
    initial_conditions$SM_0_3<-initial_conditions$SM_0_6
    initial_conditions$Z6<-0
    initial_conditions$F6<-0
    initial_conditions$SM_0_6<-wilting_point
  } else{}
  #If WF4 is empty, shift the first non-zero wetting front to WF4.
  if(initial_conditions$Z4==0 & initial_conditions$Z5>0) {
    initial_conditions$Z4<-initial_conditions$Z5
    initial_conditions$F4<-initial_conditions$F5
    initial_conditions$SM_0_4<-initial_conditions$SM_0_5
    initial_conditions$Z5<-0
    initial_conditions$F5<-0
    initial_conditions$SM_0_5<-wilting_point
  } else if(initial_conditions$Z4==0 & initial_conditions$Z5==0 & initial_conditions$Z6>0) {
    initial_conditions$Z4<-initial_conditions$Z6
    initial_conditions$F4<-initial_conditions$F6
    initial_conditions$SM_0_4<-initial_conditions$SM_0_6
    initial_conditions$Z6<-0
    initial_conditions$F6<-0
    initial_conditions$SM_0_6<-wilting_point
  } else{}
  # OPTIONAL ADDITIONAL WETTING FRONT
  #If WF5 is empty, shift the first non-zero wetting front to WF5
  #if(initial_conditions$Z5==0 & initial_conditions$Z6>0) #{
  #initial_conditions$Z5<-initial_conditions$Z6
  #initial_conditions$F5<-initial_conditions$F6
  #initial_conditions$SM_0_5<-initial_conditions$SM_0_6
  # initial_conditions$Z6<-0
  #initial_conditions$F6<-0
  #initial_conditions$SM_0_6<-wilting_point
  #} else{}
  
  ######
  #now there are 6 different scenarios and we will apply different functions depending on that
  #Scenario 1: If Z1, Z2, Z3, Z4 =0
  if(initial_conditions$Z1==0 && initial_conditions$Z2==0 && initial_conditions$Z3==0 && initial_conditions$Z4==0 && initial_conditions$Z5==0) {
    ###### SCENARIO 1: All Z=0 #####
    ##### Time step 1 #####
    #Fill output_data SM_0_1 with saturated water content
    event_output_data$SM_0_1[1]<-theta_s
    #define moisture deficit
    Md<-theta_s-wilting_point
    #First, treat like case 1 (no ponding) to get F
    F_out_1<-Ft_Case1(0.001, rainfall_events_data[[i]]$input_rate[1]) 
    f_out_1<-f_Case1(F_out_1)
    
    if(f_out_1>rainfall_events_data[[i]]$input_rate[1]){
      #store data
      event_output_data$F1[1]<-F_out_1
      event_output_data$f[1]<-f_out_1
    } else if (f_out_1<rainfall_events_data[[i]]$input_rate[1]) {
      #find volume required for surface ponding
      ponding_volume<-(KS*Md*psi)/(rainfall_events_data[[i]]$input_rate[1]-KS)
      #find time until ponding
      dt<-(ponding_volume-0.001)/rainfall_events_data[[i]]$input_rate[1]
      #calculate F using Ft_Case3_40
      F_out_3<-Ft_Case3_40(0.001, dt)
      #calculate f
      f_out_3<-f_case3(F_out_3)
      event_output_data$F1[1]<-F_out_3
      event_output_data$f[1]<-f_out_3
    }
    
    #calculate Z
    event_output_data$Z1[1]<-event_output_data$F1[1]/(event_output_data$SM_0_1[1]-wilting_point)
    
    ##### Next Time steps ####
    for(j in 2:dim(rainfall_events_data[[i]])[1]) {
      if(event_output_data$SM_0_1[j-1]>wilting_point+0.0001){
        theta_i<-wilting_point
        if(rainfall_events_data[[i]]$input_rate[j]>KS){
          GA_model_output<-simulate_GA_model(event_output_data$f[j-1],rainfall_events_data[[i]]$input_rate[j], event_output_data$F1[j-1],timestep)
          event_output_data$SM_0_1[j]<-GA_model_output$theta_new
          event_output_data$Z1[j]<-GA_model_output$Z_new
          event_output_data$F1[j]<-GA_model_output$F_new
          event_output_data$f[j]<-GA_model_output$f_new
        } else if (rainfall_events_data[[i]]$input_rate[j]<=KS){
          #may need to define a different initial moisture
          theta_initial<-wilting_point
          #update ET from last time step based on SM from last timestep
          ####event_output_data$AET[j]<-calculate_AET(rainfall_events_data[[i]]$ET0_cm_hr[j-1], VWC_50, event_output_data$SM_0_1[i-1])
          if(event_output_data$SM_0_1[j-1]>FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z1[j-1])
            event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
            event_output_data$Z1[j]<-GAR_model_output$Z_new
            event_output_data$F1[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0
          } else if(event_output_data$SM_0_1[j-1]<=FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], 0, timestep, event_output_data$Z1[j-1])
            event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
            event_output_data$Z1[j]<-GAR_model_output$Z_new
            event_output_data$F1[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0
          }
        }
      } else if(event_output_data$SM_0_1[j-1]<=wilting_point+0.0001){
        event_output_data$SM_0_1[j]<-wilting_point
        event_output_data$Z1[j]<-0
        event_output_data$F1[j]<-0
        event_output_data$f[j]<-0
      }
      if(event_output_data$SM_0_1[j]=="NaN"){
        event_output_data$SM_0_1[j]<-wilting_point
        event_output_data$Z1[j]<-0
      }
      if(event_output_data$SM_0_1[j]<wilting_point){
        event_output_data$SM_0_1[j]<-wilting_point
        event_output_data$Z1[j]<-0 
      }
      
      if(event_output_data$SM_0_1[j]==theta_s){
        event_output_data$T_r1[j]<-0
      } else if (event_output_data$SM_0_1[j]<theta_s){
        event_output_data$T_r1[j]<-event_output_data$T_r1[j-1]+0.5
      }
    }
    ##### Fill in other Z, F and SM_0 ######
    event_output_data$Z2 <- event_output_data$Z3 <- event_output_data$Z4 <- event_output_data$Z5 <- event_output_data$Z6 <-0
    
    event_output_data$F2<-event_output_data$F3<-event_output_data$F4<-event_output_data$F5<-event_output_data$F6<-0
    
    event_output_data$SM_0_2<-event_output_data$SM_0_3<-event_output_data$SM_0_4<-event_output_data$SM_0_5<-event_output_data$SM_0_6<-wilting_point
    
  } else if(initial_conditions$Z1>0 && initial_conditions$Z2==0 && initial_conditions$Z3==0 && initial_conditions$Z4==0 && initial_conditions$Z5==0) {
    ###### SCENARIO 2: Z1>0, Z2-Z6=0 ####
    theta_initial<-wilting_point #may need to define again
    #### Time Step 1 ####
    #Z1 - Redistributing Front
    theta_initial<-wilting_point
    GAR_mod_out<-simulate_gar_model(initial_conditions$F1, initial_conditions$SM_0_1, theta_initial, 0, rainfall_events_data[[i]]$ET0_cm_hr[1], (hc_function_BC(theta_initial)*100/24), timestep, initial_conditions$Z1)
    event_output_data$Z1[1]<-GAR_mod_out$Z_new
    event_output_data$F1[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_1[1]<-GAR_mod_out$theta_new
    #Z2 - Saturated Wetting Front
    #Fill output_data SM_0_2 with saturated water content
    event_output_data$SM_0_2[1]<-theta_s
    #define moisture deficit
    Md<-theta_s-event_output_data$SM_0_1[1]
    #First, treat like case 1 (no ponding) to get F
    F_out_1<-Ft_Case1(0.001, rainfall_events_data[[i]]$input_rate[1]) 
    f_out_1<-f_Case1(F_out_1)
    
    if(f_out_1>rainfall_events_data[[i]]$input_rate[1]){
      #store data
      event_output_data$F2[1]<-F_out_1
      event_output_data$f[1]<-f_out_1
    } else if (f_out_1<rainfall_events_data[[i]]$input_rate[1]) {
      #find volume required for surface ponding
      ponding_volume<-(KS*Md*psi)/(rainfall_events_data[[i]]$input_rate[1]-KS)
      #find time until ponding
      dt<-(ponding_volume-0.001)/rainfall_events_data[[i]]$input_rate[1]
      #calculate F using Ft_Case3_40
      F_out_3<-Ft_Case3_40(0.001, dt)
      #calculate f
      f_out_3<-f_case3(F_out_3)
      event_output_data$F2[1]<-F_out_3
      event_output_data$f[1]<-f_out_3
    }
    #calculate Z
    event_output_data$Z2[1]<-event_output_data$F2[1]/(event_output_data$SM_0_2[1]-event_output_data$SM_0_1[1])
    
    #### Next Time Steps ####
    for(j in 2:dim(rainfall_events_data[[i]])[1]) {
      #calculate Z1 - Redistributing Front
      theta_initial<-wilting_point
      if(event_output_data$SM_0_1[j-1]>wilting_point+0.0001) {
        if(event_output_data$SM_0_1[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z1[j-1])
          event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
          event_output_data$Z1[j]<-GAR_model_output$Z_new
          event_output_data$F1[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_1[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z1[j-1])
          event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
          event_output_data$Z1[j]<-GAR_model_output$Z_new
          event_output_data$F1[j]<-GAR_model_output$F_new}
      } else if(event_output_data$SM_0_1[j-1]<=wilting_point+0.0001){
        event_output_data$SM_0_1[j]<-wilting_point
        event_output_data$Z1[j]<-0
        event_output_data$F1[j]<-0
      }
      #calculate Z2 - Saturated Wetting Front
      if(event_output_data$SM_0_2[j-1]>wilting_point+0.0001){
        if(rainfall_events_data[[i]]$input_rate[j]>KS){
          theta_i<-event_output_data$SM_0_1[j]
          theta_initial<-event_output_data$SM_0_1[j]
          Md<-theta_s-event_output_data$SM_0_1[j]
          GA_model_output<-simulate_GA_model(event_output_data$f[j-1],rainfall_events_data[[i]]$input_rate[j], event_output_data$F2[j-1],timestep)
          event_output_data$SM_0_2[j]<-GA_model_output$theta_new
          event_output_data$Z2[j]<-GA_model_output$Z_new
          event_output_data$F2[j]<-GA_model_output$F_new
          event_output_data$f[j]<-GA_model_output$f_new
        } else if (rainfall_events_data[[i]]$input_rate[j]<=KS){
          #may need to define a different initial moisture
          theta_initial<-event_output_data$SM_0_1[j]
          #event_output_data$AET[j]<-calculate_AET(rainfall_events_data[[i]]$ET0_cm_hr[j-1], VWC_50, event_output_data$SM_0_2[j-1])
          
          if(event_output_data$SM_0_2[j-1]>FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F2[j-1], event_output_data$SM_0_2[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z2[j-1])
            event_output_data$SM_0_2[j]<-GAR_model_output$theta_new
            event_output_data$Z2[j]<-GAR_model_output$Z_new
            event_output_data$F2[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0
          } else if (event_output_data$SM_0_2[j-1]<=FC) {
            GAR_model_output<-simulate_gar_model(event_output_data$F2[j-1], event_output_data$SM_0_2[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], 0, timestep, event_output_data$Z2[j-1])
            event_output_data$SM_0_2[j]<-GAR_model_output$theta_new
            event_output_data$Z2[j]<-GAR_model_output$Z_new
            event_output_data$F2[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0 
          }
        } else{}
      } else if(event_output_data$SM_0_2[j-1]<=wilting_point+0.0001){
        event_output_data$SM_0_2[j]<-wilting_point
        event_output_data$Z2[j]<-0
        event_output_data$F2[j]<-0
        event_output_data$f[j]<-0
      } else{}
      #deal with NaN value with new wetting front
      if(event_output_data$SM_0_2[j]=="NaN"){
        event_output_data$SM_0_2[j]<-wilting_point
        event_output_data$Z2[j]<-0
        event_output_data$F2[j]<-0
      }
      #deal with Z1=Z2, or SM<wilting point
      #if soil moisture is less than wilting point, make soil moisture = wilting point, and make Z=0
      if(event_output_data$SM_0_2[j]< wilting_point){
        event_output_data$SM_0_2[j]<-wilting_point
        event_output_data$Z2[j]<-0
      } 
      #If Z1=Z2, merge wetting fronts
      if(abs(event_output_data$Z2[j]-event_output_data$Z1[j])<=1){
        event_output_data$Z1[j]<-event_output_data$Z2[j]
        event_output_data$F1[j]<-event_output_data$F2[j]+event_output_data$F1[j]
        event_output_data$SM_0_1[j]<-event_output_data$SM_0_2[j]
        event_output_data$Z2[j]<-0
        event_output_data$F2[j]<-0
        event_output_data$SM_0_2[j]<-wilting_point
      } 
      #if soil moisture of wetting front 2 becomes less than wetting front 1, wetting front 2 goes away
      if(event_output_data$SM_0_2[j]<event_output_data$SM_0_1[j]){
        event_output_data$F1[j]<-event_output_data$F1[j]+event_output_data$F2[j]
        event_output_data$SM_0_2[j]<-wilting_point
        event_output_data$Z2[j]<-0
        event_output_data$F2[j]<-0
      }
      #if DIFFERENCE IN soil moisture of wetting front 2 becomes and wetting front 1 is very small
      if(abs(event_output_data$SM_0_2[j]-event_output_data$SM_0_1[j])<0.001){
        event_output_data$F1[j]<-event_output_data$F1[j]+event_output_data$F2[j]
        event_output_data$SM_0_2[j]<-wilting_point
        event_output_data$Z2[j]<-0
        event_output_data$F2[j]<-0
      }
    }
    #### Fill in other Z, F and SM ####
    event_output_data$Z3<-0
    event_output_data$Z4<-0
    event_output_data$Z5<-0
    event_output_data$Z6<-0
    
    event_output_data$F3<-0
    event_output_data$F4<-0
    event_output_data$F5<-0
    event_output_data$F6<-0
    
    event_output_data$SM_0_3<-wilting_point
    event_output_data$SM_0_4<-wilting_point
    event_output_data$SM_0_5<-wilting_point
    event_output_data$SM_0_6<-wilting_point
    
  } else if(initial_conditions$Z1>0 && initial_conditions$Z2>0 && initial_conditions$Z3==0 && initial_conditions$Z4==0 && initial_conditions$Z5==0) {
    #SCENARIO 3: Z1 and Z2 >0, Z3 & Z4 & Z5 & Z6 =0
    #### Time Step 1 ####
    #Z1 - Redistributing Front
    theta_initial<-wilting_point
    GAR_mod_out<-simulate_gar_model(initial_conditions$F1, initial_conditions$SM_0_1, theta_initial, 0, 0, (hc_function_BC(theta_initial)*100/24), timestep, initial_conditions$Z1)
    event_output_data$Z1[1]<-GAR_mod_out$Z_new
    event_output_data$F1[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_1[1]<-GAR_mod_out$theta_new
    #Z2 - Redistributing front
    theta_initial_Z2<-event_output_data$SM_0_1[1]
    GAR_mod_out<-simulate_gar_model(initial_conditions$F2, initial_conditions$SM_0_2, theta_initial_Z2, 0, 0, (hc_function_BC(theta_initial_Z2)*100/24), timestep, initial_conditions$Z2)
    event_output_data$Z2[1]<-GAR_mod_out$Z_new
    event_output_data$F2[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_2[1]<-GAR_mod_out$theta_new
    #Z3 - Saturated Wetting Front
    #Fill output_data SM_0_3 with saturated water content
    event_output_data$SM_0_3[1]<-theta_s
    #define moisture deficit
    Md<-theta_s-event_output_data$SM_0_2[1]
    #First, treat like case 1 (no ponding) to get F
    F_out_1<-Ft_Case1(0.001, rainfall_events_data[[i]]$input_rate[1]) 
    f_out_1<-f_Case1(F_out_1)
    
    if(f_out_1>rainfall_events_data[[i]]$input_rate[1]){
      #store data
      event_output_data$F3[1]<-F_out_1
      event_output_data$f[1]<-f_out_1
    } else if (f_out_1<rainfall_events_data[[i]]$input_rate[1]) {
      #find volume required for surface ponding
      ponding_volume<-(KS*Md*psi)/(rainfall_events_data[[i]]$input_rate[1]-KS)
      #find time until ponding
      dt<-(ponding_volume-0.001)/rainfall_events_data[[i]]$input_rate[1]
      #calculate F using Ft_Case3_40
      F_out_3<-Ft_Case3_40(0.001, dt)
      #calculate f
      f_out_3<-f_case3(F_out_3)
      event_output_data$F3[1]<-F_out_3
      event_output_data$f[1]<-f_out_3
    }
    #calculate Z
    event_output_data$Z3[1]<-event_output_data$F3[1]/(event_output_data$SM_0_3[1]-event_output_data$SM_0_2[1])
    
    #### Next Time Steps ####
    for(j in 2:dim(rainfall_events_data[[i]])[1]) {
      #calculate Z1 - Redistributing Front
      if(event_output_data$SM_0_1[j-1]>wilting_point+0.001) {
        theta_initial<-wilting_point
        if(event_output_data$SM_0_1[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z1[j-1])
          event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
          event_output_data$Z1[j]<-GAR_model_output$Z_new
          event_output_data$F1[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_1[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z1[j-1])
          event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
          event_output_data$Z1[j]<-GAR_model_output$Z_new
          event_output_data$F1[j]<-GAR_model_output$F_new  
        }
      } else if(event_output_data$SM_0_1[j-1]<=wilting_point+0.001){
        event_output_data$SM_0_1[j]<-wilting_point
        event_output_data$Z1[j]<-0
        event_output_data$F1[j]<-0
      }
      #calculate Z2 - Redistributing Front
      theta_initial<-event_output_data$SM_0_1[j]
      if(event_output_data$SM_0_2[j-1]>wilting_point+0.001) {
        if(event_output_data$SM_0_2[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F2[j-1], event_output_data$SM_0_2[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z2[j-1])
          event_output_data$SM_0_2[j]<-GAR_model_output$theta_new
          event_output_data$Z2[j]<-GAR_model_output$Z_new
          event_output_data$F2[j]<-GAR_model_output$F_new
        } else if(event_output_data$SM_0_2[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F2[j-1], event_output_data$SM_0_2[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z2[j-1])
          event_output_data$SM_0_2[j]<-GAR_model_output$theta_new
          event_output_data$Z2[j]<-GAR_model_output$Z_new
          event_output_data$F2[j]<-GAR_model_output$F_new  
        }
      } else if(event_output_data$SM_0_2[j-1]<=wilting_point+0.001){
        event_output_data$SM_0_2[j]<-wilting_point
        event_output_data$Z2[j]<-0
        event_output_data$F2[j]<-0
      }
      #calculate Z3 - Saturated Wetting Front
      theta_initial<-event_output_data$SM_0_2[j]
      theta_i<-event_output_data$SM_0_2[j]
      if(event_output_data$SM_0_3[j-1]>wilting_point+0.001){
        if(rainfall_events_data[[i]]$input_rate[j]>KS){
          theta_i<-event_output_data$SM_0_2[j]
          GA_model_output<-simulate_GA_model(event_output_data$f[j-1],rainfall_events_data[[i]]$input_rate[j], event_output_data$F3[j-1],timestep)
          event_output_data$SM_0_3[j]<-GA_model_output$theta_new
          event_output_data$Z3[j]<-GA_model_output$Z_new
          event_output_data$F3[j]<-GA_model_output$F_new
          event_output_data$f[j]<-GA_model_output$f_new
        } else if (rainfall_events_data[[i]]$input_rate[j]<=KS){
          #may need to define a different initial moisture
          theta_initial<-event_output_data$SM_0_2[j]
          #calculate AET
          #event_output_data$AET[j]<-calculate_AET(rainfall_events_data[[i]]$ET0_cm_hr[j-1], VWC_50, event_output_data$SM_0_3[j-1])
          
          if(event_output_data$SM_0_3[j-1]>FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F3[j-1], event_output_data$SM_0_3[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z3[j-1])
            event_output_data$SM_0_3[j]<-GAR_model_output$theta_new
            event_output_data$Z3[j]<-GAR_model_output$Z_new
            event_output_data$F3[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0
          } else if (event_output_data$SM_0_3[j-1]<=FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F3[j-1], event_output_data$SM_0_3[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], 0, timestep, event_output_data$Z3[j-1])
            event_output_data$SM_0_3[j]<-GAR_model_output$theta_new
            event_output_data$Z3[j]<-GAR_model_output$Z_new
            event_output_data$F3[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0  
          }
        }
      } else if(event_output_data$SM_0_3[j-1]<=wilting_point+0.001){
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0
        event_output_data$f[j]<-0
      }
      #deal with NaN value with new wetting front
      if(event_output_data$SM_0_3[j]=="NaN"){
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0
      }
      #if SM is less than wilting point, make Z=0
      if(event_output_data$SM_0_3[j]<wilting_point){
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
      }
      #if Z3=Z2
      if(abs(event_output_data$Z3[j]-event_output_data$Z2[j])<=1) {
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]+event_output_data$F2[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0
        event_output_data$SM_0_3[j]<-wilting_point
      }
      #If Z2=Z1
      if(abs(event_output_data$Z2[j]-event_output_data$Z1[j])<=1) {
        event_output_data$Z1[j]<-event_output_data$Z2[j]
        event_output_data$F1[j]<-event_output_data$F2[j]+event_output_data$F1[j]
        event_output_data$SM_0_1[j]<-event_output_data$SM_0_2[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0
        event_output_data$SM_0_3[j]<-wilting_point
      }
      #if SM_0_3 < SM_0_2, wetting front 3 dissapears
      if(event_output_data$SM_0_3[j]<event_output_data$SM_0_2[j]){
        event_output_data$F2[j]<-event_output_data$F2[j]+event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0}
      #if SM_0_2<SM_0_1, wetting front 2 dissapears
      if(event_output_data$SM_0_2[j]<event_output_data$SM_0_1[j]){
        event_output_data$F1[j]<-event_output_data$F1[j]+event_output_data$F2[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0}
      #if the difference between SM_0_3 and SM_0_2 becomes very small
      if(abs(event_output_data$SM_0_3[j]-event_output_data$SM_0_2[j])<0.001){
        event_output_data$F2[j]<-event_output_data$F3[j]+event_output_data$F2[j]
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0
      }
      #if the difference between SM_0_2 and SM_0_1 becomes very small
      if(abs(event_output_data$SM_0_2[j]-event_output_data$SM_0_1[j])<0.001){
        event_output_data$F1[j]<-event_output_data$F1[j]+event_output_data$F2[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0
      }
    }
    #### Fill in other Z, F and SM ####
    event_output_data$Z4<-0
    event_output_data$Z5<-0
    event_output_data$Z6<-0
    
    event_output_data$F4<-0
    event_output_data$F5<-0
    event_output_data$F6<-0
    
    event_output_data$SM_0_4<-wilting_point
    event_output_data$SM_0_5<-wilting_point
    event_output_data$SM_0_6<-wilting_point
    
  } else if(initial_conditions$Z1>0 && initial_conditions$Z2>0 && initial_conditions$Z3>0 && initial_conditions$Z4==0 && initial_conditions$Z5==0) {
    #SCENARIO 4: Z1, Z2 and Z3 >0, Z4 & Z5 & Z6 =0
    #### Time Step 1 ####
    #Z1 - Redistributing Front
    theta_initial<-wilting_point
    GAR_mod_out<-simulate_gar_model(initial_conditions$F1, initial_conditions$SM_0_1, theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, initial_conditions$Z1)
    event_output_data$Z1[1]<-GAR_mod_out$Z_new
    event_output_data$F1[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_1[1]<-GAR_mod_out$theta_new
    #Z2 - Redistributing front
    theta_initial<-event_output_data$SM_0_1[1]
    GAR_mod_out<-simulate_gar_model(initial_conditions$F2, initial_conditions$SM_0_2, theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, initial_conditions$Z2)
    event_output_data$Z2[1]<-GAR_mod_out$Z_new
    event_output_data$F2[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_2[1]<-GAR_mod_out$theta_new
    #Z3 - Redistributing front
    theta_initial<-event_output_data$SM_0_2[1]
    GAR_mod_out<-simulate_gar_model(initial_conditions$F3, initial_conditions$SM_0_3, theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, initial_conditions$Z3)
    event_output_data$Z3[1]<-GAR_mod_out$Z_new
    event_output_data$F3[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_3[1]<-GAR_mod_out$theta_new
    #Z4 - Saturated Wetting Front
    #Fill output_data SM_0_4 with saturated water content
    event_output_data$SM_0_4[1]<-theta_s
    #define moisture deficit
    Md<-theta_s-event_output_data$SM_0_3[1]
    #First, treat like case 1 (no ponding) to get F
    F_out_1<-Ft_Case1(0.001, rainfall_events_data[[i]]$input_rate[1]) 
    f_out_1<-f_Case1(F_out_1)
    
    if(f_out_1>rainfall_events_data[[i]]$input_rate[1]){
      #store data
      event_output_data$F4[1]<-F_out_1
      event_output_data$f[1]<-f_out_1
    } else if (f_out_1<rainfall_events_data[[i]]$input_rate[1]) {
      #find volume required for surface ponding
      ponding_volume<-(KS*Md*psi)/(rainfall_events_data[[i]]$input_rate[1]-KS)
      #find time until ponding
      dt<-(ponding_volume-0.001)/rainfall_events_data[[i]]$input_rate[1]
      #calculate F using Ft_Case3_40
      F_out_3<-Ft_Case3_40(0.001, dt)
      #calculate f
      f_out_3<-f_case3(F_out_3)
      event_output_data$F4[1]<-F_out_3
      event_output_data$f[1]<-f_out_3
    }
    #calculate Z
    event_output_data$Z4[1]<-event_output_data$F4[1]/(event_output_data$SM_0_4[1]-event_output_data$SM_0_3[1])
    
    #### Next Time Steps ####
    for(j in 2:dim(rainfall_events_data[[i]])[1]) {
      #calculate Z1 - Redistributing Front
      theta_initial<-wilting_point
      if(event_output_data$SM_0_1[j-1]>wilting_point+0.001) {
        if(event_output_data$SM_0_1[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z1[j-1])
          event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
          event_output_data$Z1[j]<-GAR_model_output$Z_new
          event_output_data$F1[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_1[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z1[j-1])
          event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
          event_output_data$Z1[j]<-GAR_model_output$Z_new
          event_output_data$F1[j]<-GAR_model_output$F_new 
        }
      } else if(event_output_data$SM_0_1[j-1]<=wilting_point+0.001){
        event_output_data$SM_0_1[j]<-wilting_point
        event_output_data$Z1[j]<-0
        event_output_data$F1[j]<-0
      }
      #calculate Z2 - Redistributing Front
      theta_initial<-event_output_data$SM_0_1[j]
      if(event_output_data$SM_0_2[j-1]>wilting_point+0.001) {
        if(event_output_data$SM_0_2[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F2[j-1], event_output_data$SM_0_2[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z2[j-1])
          event_output_data$SM_0_2[j]<-GAR_model_output$theta_new
          event_output_data$Z2[j]<-GAR_model_output$Z_new
          event_output_data$F2[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_2[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F2[j-1], event_output_data$SM_0_2[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z2[j-1])
          event_output_data$SM_0_2[j]<-GAR_model_output$theta_new
          event_output_data$Z2[j]<-GAR_model_output$Z_new
          event_output_data$F2[j]<-GAR_model_output$F_new  
        }
      } else if(event_output_data$SM_0_2[j-1]<=wilting_point+0.001){
        event_output_data$SM_0_2[j]<-wilting_point
        event_output_data$Z2[j]<-0
        event_output_data$F2[j]<-0
      }
      #calculate Z3 - Redistributing Front
      theta_initial<-event_output_data$SM_0_2[j]
      if(event_output_data$SM_0_3[j-1]>wilting_point+0.001) {
        if(event_output_data$SM_0_3[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F3[j-1], event_output_data$SM_0_3[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z3[j-1])
          event_output_data$SM_0_3[j]<-GAR_model_output$theta_new
          event_output_data$Z3[j]<-GAR_model_output$Z_new
          event_output_data$F3[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_3[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F3[j-1], event_output_data$SM_0_3[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z3[j-1])
          event_output_data$SM_0_3[j]<-GAR_model_output$theta_new
          event_output_data$Z3[j]<-GAR_model_output$Z_new
          event_output_data$F3[j]<-GAR_model_output$F_new  
        }
      } else if(event_output_data$SM_0_3[j-1]<=wilting_point+0.001){
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0
      }
      #calculate Z4 - Saturated Wetting Front
      theta_initial<-event_output_data$SM_0_3[j]
      theta_i<-event_output_data$SM_0_3[j]
      if(event_output_data$SM_0_4[j-1]>wilting_point+0.001){
        if(rainfall_events_data[[i]]$input_rate[j]>KS){
          theta_i<-event_output_data$SM_0_3[j]
          GA_model_output<-simulate_GA_model(event_output_data$f[j-1],rainfall_events_data[[i]]$input_rate[j], event_output_data$F4[j-1],timestep)
          event_output_data$SM_0_4[j]<-GA_model_output$theta_new
          event_output_data$Z4[j]<-GA_model_output$Z_new
          event_output_data$F4[j]<-GA_model_output$F_new
          event_output_data$f[j]<-GA_model_output$f_new
        } else if (rainfall_events_data[[i]]$input_rate[j]<=KS){
          #may need to define a different initial moisture
          theta_initial<-event_output_data$SM_0_3[j]
          #calculate AET
          #event_output_data$AET[j]<-calculate_AET(rainfall_events_data[[i]]$ET0_cm_hr[j-1], VWC_50, event_output_data$SM_0_4[j-1])
          
          if(event_output_data$SM_0_4[j-1]>FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F4[j-1], event_output_data$SM_0_4[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z4[j-1])
            event_output_data$SM_0_4[j]<-GAR_model_output$theta_new
            event_output_data$Z4[j]<-GAR_model_output$Z_new
            event_output_data$F4[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0
          } else if (event_output_data$SM_0_4[j-1]<=FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F4[j-1], event_output_data$SM_0_4[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z4[j-1])
            event_output_data$SM_0_4[j]<-GAR_model_output$theta_new
            event_output_data$Z4[j]<-GAR_model_output$Z_new
            event_output_data$F4[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0 
          }
        }
      } else if(event_output_data$SM_0_4[j-1]<=wilting_point+0.001){
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
        event_output_data$f[j]<-0
      }
      #deal with NaN value with new wetting front
      if(event_output_data$SM_0_4[j]=="NaN"){
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
      }
      #if soil moisture becomes less than wikting point
      if(event_output_data$SM_0_4[j]<wilting_point){
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0} 
      #if Z4=Z3, Z4 dissapears
      if(abs(event_output_data$Z4[j]-event_output_data$Z3[j])<=1) {
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]+event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
        event_output_data$SM_0_4[j]<-wilting_point}
      # if Z3=Z2
      if(abs(event_output_data$Z3[j]-event_output_data$Z2[j])<=1) {
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]+event_output_data$F2[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F2[j]<-event_output_data$F4[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
        event_output_data$SM_0_4[j]<-wilting_point}
      #if Z2=Z1
      if(abs(event_output_data$Z2[j]-event_output_data$Z1[j])<=1) {
        event_output_data$Z1[j]<-event_output_data$Z2[j]
        event_output_data$F1[j]<-event_output_data$F2[j]+event_output_data$F1[j]
        event_output_data$SM_0_1[j]<-event_output_data$SM_0_2[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
        event_output_data$SM_0_4[j]<-wilting_point}
      
      #if the difference between SM_0_4 and SM_0_3 becomes very small
      if(abs(event_output_data$SM_0_3[j]-event_output_data$SM_0_4[j])<0.001){
        event_output_data$F3[j]<-event_output_data$F3[j]+event_output_data$F4[j]
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
      }
      #if the difference between SM_0_3 and SM_0_2 becomes very small
      if(abs(event_output_data$SM_0_2[j]-event_output_data$SM_0_3[j])<0.001){
        event_output_data$F2[j]<-event_output_data$F2[j]+event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
      }
      #if the difference between SM_0_1 and SM_0_2 becomes very small
      if(abs(event_output_data$SM_0_2[j]-event_output_data$SM_0_1[j])<0.001){
        event_output_data$F1[j]<-event_output_data$F2[j]+event_output_data$F1[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
      }
      
      #if SM4 < SM3, wetting front 4 dissapears, 
      if(event_output_data$SM_0_4[j]<event_output_data$SM_0_3[j]){
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
      }else{}
      #if SM3<SM2, wetting front 3 dissapears, and wetting front 4 should be replace wetting front 3
      if(event_output_data$SM_0_3[j]<event_output_data$SM_0_2[j]){
        event_output_data$F2[j]<-event_output_data$F3[j]+event_output_data$F2[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
      }else{}
      #if SM_0_2<SM_0_1, wetting front 2 dissapears, and wetting front 2 is replace by 3, and 3 by 4
      if(event_output_data$SM_0_2[j]<event_output_data$SM_0_1[j]){
        event_output_data$F1[j]<-event_output_data$F1[j]+event_output_data$F2[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
      }else{}
    }
    #### Fill in other Z, F and SM ####
    event_output_data$Z5<-0
    event_output_data$Z6<-0
    
    event_output_data$F5<-0
    event_output_data$F6<-0
    
    event_output_data$SM_0_5<-wilting_point
    event_output_data$SM_0_6<-wilting_point
    
  } else if(initial_conditions$Z1>0 && initial_conditions$Z2>0 && initial_conditions$Z3>0 && initial_conditions$Z4>0 && initial_conditions$Z5==0){
    #SCENARIO 5: Z1, Z2, Z3 and Z4 >0, Z5 & Z6 =0
    #### Time Step 1 ####
    #Z1 - Redistributing Front
    theta_initial<-wilting_point
    GAR_mod_out<-simulate_gar_model(initial_conditions$F1, initial_conditions$SM_0_1, theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, initial_conditions$Z1)
    event_output_data$Z1[1]<-GAR_mod_out$Z_new
    event_output_data$F1[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_1[1]<-GAR_mod_out$theta_new
    #Z2 - Redistributing front
    theta_initial<-event_output_data$SM_0_1[1]
    GAR_mod_out<-simulate_gar_model(initial_conditions$F2, initial_conditions$SM_0_2, theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, initial_conditions$Z2)
    event_output_data$Z2[1]<-GAR_mod_out$Z_new
    event_output_data$F2[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_2[1]<-GAR_mod_out$theta_new
    #Z3 - Redistributing front
    theta_initial<-event_output_data$SM_0_2[1]
    GAR_mod_out<-simulate_gar_model(initial_conditions$F3, initial_conditions$SM_0_3, theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, initial_conditions$Z3)
    event_output_data$Z3[1]<-GAR_mod_out$Z_new
    event_output_data$F3[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_3[1]<-GAR_mod_out$theta_new
    #Z4 - Redistributing front
    theta_initial<-event_output_data$SM_0_3[1]
    GAR_mod_out<-simulate_gar_model(initial_conditions$F4, initial_conditions$SM_0_4, theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, initial_conditions$Z4)
    event_output_data$Z4[1]<-GAR_mod_out$Z_new
    event_output_data$F4[1]<-GAR_mod_out$F_new
    event_output_data$SM_0_4[1]<-GAR_mod_out$theta_new
    #Z5 - Saturated Wetting Front
    #Fill output_data SM_0_5 with saturated water content
    event_output_data$SM_0_5[1]<-theta_s
    #define moisture deficit
    Md<-theta_s-event_output_data$SM_0_4[1]
    #First, treat like case 1 (no ponding) to get F
    F_out_1<-Ft_Case1(0.001, rainfall_events_data[[i]]$input_rate[1]) 
    f_out_1<-f_Case1(F_out_1)
    
    if(f_out_1>rainfall_events_data[[i]]$input_rate[1]){
      #store data
      event_output_data$F5[1]<-F_out_1
      event_output_data$f[1]<-f_out_1
    } else if (f_out_1<rainfall_events_data[[i]]$input_rate[1]) {
      #find volume required for surface ponding
      ponding_volume<-(KS*Md*psi)/(rainfall_events_data[[i]]$input_rate[1]-KS)
      #find time until ponding
      dt<-(ponding_volume-0.001)/rainfall_events_data[[i]]$input_rate[1]
      #calculate F using Ft_Case3_40
      F_out_3<-Ft_Case3_40(0.001, dt)
      #calculate f
      f_out_3<-f_case3(F_out_3)
      event_output_data$F5[1]<-F_out_3
      event_output_data$f[1]<-f_out_3
    }
    #calculate Z
    event_output_data$Z5[1]<-event_output_data$F5[1]/(event_output_data$SM_0_5[1]-event_output_data$SM_0_4[1])
    
    #### Next Time Steps ####
    for(j in 2:dim(rainfall_events_data[[i]])[1]) {
      #calculate Z1 - Redistributing Front
      theta_initial<-wilting_point
      if(event_output_data$SM_0_1[j-1]>wilting_point+0.01) {
        if(event_output_data$SM_0_1[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z1[j-1])
          event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
          event_output_data$Z1[j]<-GAR_model_output$Z_new
          event_output_data$F1[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_1[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F1[j-1], event_output_data$SM_0_1[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z1[j-1])
          event_output_data$SM_0_1[j]<-GAR_model_output$theta_new
          event_output_data$Z1[j]<-GAR_model_output$Z_new
          event_output_data$F1[j]<-GAR_model_output$F_new 
        }
      } else if(event_output_data$SM_0_1[j-1]<=wilting_point+0.01){
        event_output_data$SM_0_1[j]<-wilting_point
        event_output_data$Z1[j]<-0
        event_output_data$F1[j]<-0
      }
      #calculate Z2 - Redistributing Front
      theta_initial<-event_output_data$SM_0_1[j]
      if(event_output_data$SM_0_2[j-1]>wilting_point+0.01) {
        if(event_output_data$SM_0_2[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F2[j-1], event_output_data$SM_0_2[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z2[j-1])
          event_output_data$SM_0_2[j]<-GAR_model_output$theta_new
          event_output_data$Z2[j]<-GAR_model_output$Z_new
          event_output_data$F2[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_2[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F2[j-1], event_output_data$SM_0_2[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z2[j-1])
          event_output_data$SM_0_2[j]<-GAR_model_output$theta_new
          event_output_data$Z2[j]<-GAR_model_output$Z_new
          event_output_data$F2[j]<-GAR_model_output$F_new  
        }
      } else if(event_output_data$SM_0_2[j-1]<=wilting_point+0.01){
        event_output_data$SM_0_2[j]<-wilting_point
        event_output_data$Z2[j]<-0
        event_output_data$F2[j]<-0
      }
      #calculate Z3 - Redistributing Front
      theta_initial<-event_output_data$SM_0_2[j]
      if(event_output_data$SM_0_3[j-1]>wilting_point+0.01) {
        if(event_output_data$SM_0_3[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F3[j-1], event_output_data$SM_0_3[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z3[j-1])
          event_output_data$SM_0_3[j]<-GAR_model_output$theta_new
          event_output_data$Z3[j]<-GAR_model_output$Z_new
          event_output_data$F3[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_3[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F3[j-1], event_output_data$SM_0_3[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z3[j-1])
          event_output_data$SM_0_3[j]<-GAR_model_output$theta_new
          event_output_data$Z3[j]<-GAR_model_output$Z_new
          event_output_data$F3[j]<-GAR_model_output$F_new  
        }
      } else if(event_output_data$SM_0_3[j-1]<=wilting_point+0.01){
        event_output_data$SM_0_3[j]<-wilting_point
        event_output_data$Z3[j]<-0
        event_output_data$F3[j]<-0
      }
      #calculate Z4 - Redistributing Front
      theta_initial<-event_output_data$SM_0_3[j]
      if(event_output_data$SM_0_4[j-1]>wilting_point+0.01) {
        if(event_output_data$SM_0_4[j-1]>FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F4[j-1], event_output_data$SM_0_4[j-1], theta_initial, 0, 0, (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z4[j-1])
          event_output_data$SM_0_4[j]<-GAR_model_output$theta_new
          event_output_data$Z4[j]<-GAR_model_output$Z_new
          event_output_data$F4[j]<-GAR_model_output$F_new
        } else if (event_output_data$SM_0_4[j-1]<=FC){
          GAR_model_output<-simulate_gar_model(event_output_data$F4[j-1], event_output_data$SM_0_4[j-1], theta_initial, 0, 0, 0, timestep, event_output_data$Z4[j-1])
          event_output_data$SM_0_4[j]<-GAR_model_output$theta_new
          event_output_data$Z4[j]<-GAR_model_output$Z_new
          event_output_data$F4[j]<-GAR_model_output$F_new  
        }
      } else if(event_output_data$SM_0_4[j-1]<=wilting_point+0.01){
        event_output_data$SM_0_4[j]<-wilting_point
        event_output_data$Z4[j]<-0
        event_output_data$F4[j]<-0
      }
      #calculate Z5 - Saturated Wetting Front
      theta_initial<-event_output_data$SM_0_4[j]
      theta_i<-event_output_data$SM_0_4[j]
      if(event_output_data$SM_0_5[j-1]>wilting_point+0.01){
        if(rainfall_events_data[[i]]$input_rate[j]>KS){
          theta_i<-event_output_data$SM_0_4[j]
          GA_model_output<-simulate_GA_model(event_output_data$f[j-1],rainfall_events_data[[i]]$input_rate[j], event_output_data$F5[j-1],timestep)
          event_output_data$SM_0_5[j]<-GA_model_output$theta_new
          event_output_data$Z5[j]<-GA_model_output$Z_new
          event_output_data$F5[j]<-GA_model_output$F_new
          event_output_data$f[j]<-GA_model_output$f_new
        } else if (rainfall_events_data[[i]]$input_rate[j]<=KS){
          #may need to define a different initial moisture
          theta_initial<-event_output_data$SM_0_4[j]
          #calculate AET
          #event_output_data$AET[j]<-calculate_AET(rainfall_events_data[[i]]$ET0_cm_hr[j-1], VWC_50, event_output_data$SM_0_5[j-1])
          
          if(event_output_data$SM_0_5[j-1]>FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F5[j-1], event_output_data$SM_0_5[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z5[j-1])
            event_output_data$SM_0_5[j]<-GAR_model_output$theta_new
            event_output_data$Z5[j]<-GAR_model_output$Z_new
            event_output_data$F5[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0
          } else if (event_output_data$SM_0_5[j-1]<=FC){
            GAR_model_output<-simulate_gar_model(event_output_data$F5[j-1], event_output_data$SM_0_5[j-1], theta_initial, rainfall_events_data[[i]]$input_rate[j], rainfall_events_data[[i]]$ET0_cm_hr[j], (hc_function_BC(theta_initial)*1), timestep, event_output_data$Z5[j-1])
            event_output_data$SM_0_5[j]<-GAR_model_output$theta_new
            event_output_data$Z5[j]<-GAR_model_output$Z_new
            event_output_data$F5[j]<-GAR_model_output$F_new
            event_output_data$f[j]<-0 
          }
        }
      } else if(event_output_data$SM_0_5[j-1]<=wilting_point+0.01){
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
        event_output_data$f[j]<-0
      }
      #deal with NaN value with new wetting front
      if(event_output_data$SM_0_5[j]=="NaN"){
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }
      #if soil moisture becomes less than wikting point
      if(event_output_data$SM_0_5[j]<wilting_point){
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0} 
      #if Z5=Z4, Z5 dissapears
      if(abs(event_output_data$Z5[j]-event_output_data$Z4[j])<=4) {
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]+event_output_data$F4[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
        event_output_data$SM_0_5[j]<-wilting_point}
      # if Z4=Z3
      if(abs(event_output_data$Z4[j]-event_output_data$Z3[j])<=4) {
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]+event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
        event_output_data$SM_0_5[j]<-wilting_point}
      # if Z3=Z2
      if(abs(event_output_data$Z3[j]-event_output_data$Z2[j])<=4) {
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]+event_output_data$F2[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
        event_output_data$SM_0_5[j]<-wilting_point}
      #if Z2=Z1
      if(abs(event_output_data$Z2[j]-event_output_data$Z1[j])<=4) {
        event_output_data$Z1[j]<-event_output_data$Z2[j]
        event_output_data$F1[j]<-event_output_data$F2[j]+event_output_data$F1[j]
        event_output_data$SM_0_1[j]<-event_output_data$SM_0_2[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
        event_output_data$SM_0_5[j]<-wilting_point}
      
      #if the difference between SM_0_5 and SM_0_4 becomes very small
      if(abs(event_output_data$SM_0_4[j]-event_output_data$SM_0_5[j])<0.005){
        event_output_data$F4[j]<-event_output_data$F4[j]+event_output_data$F5[j]
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }
      #if the difference between SM_0_4 and SM_0_3 becomes very small
      if(abs(event_output_data$SM_0_3[j]-event_output_data$SM_0_4[j])<0.005){
        event_output_data$F3[j]<-event_output_data$F3[j]+event_output_data$F4[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }
      #if the difference between SM_0_2 and SM_0_3 becomes very small
      if(abs(event_output_data$SM_0_3[j]-event_output_data$SM_0_2[j])<0.005){
        event_output_data$F2[j]<-event_output_data$F3[j]+event_output_data$F2[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }
      #if the difference between SM_0_1 and SM_0_2 becomes very small
      if(abs(event_output_data$SM_0_2[j]-event_output_data$SM_0_1[j])<0.005){
        event_output_data$F1[j]<-event_output_data$F2[j]+event_output_data$F1[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }
      
      #if SM5 < SM4, wetting front 5 dissapears, 
      if(event_output_data$SM_0_5[j]<event_output_data$SM_0_4[j]){
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }else{}
      #if SM4<SM3, wetting front 4 dissapears, and wetting front 4 should be replace wetting front 5
      if(event_output_data$SM_0_4[j]<event_output_data$SM_0_3[j]){
        event_output_data$F3[j]<-event_output_data$F4[j]+event_output_data$F3[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }else{}
      #if SM_0_3<SM_0_2, wetting front 3 dissapears, and wetting front 3 is replace by 4, and 4 by 5
      if(event_output_data$SM_0_3[j]<event_output_data$SM_0_2[j]){
        event_output_data$F2[j]<-event_output_data$F2[j]+event_output_data$F3[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }else{}
      #if SM_0_2<SM_0_1, wetting front 2 dissapears, and wetting front 2 is replace by 3, and 3 by 4, 4 by 5
      if(event_output_data$SM_0_2[j]<event_output_data$SM_0_1[j]){
        event_output_data$F1[j]<-event_output_data$F2[j]+event_output_data$F1[j]
        event_output_data$SM_0_2[j]<-event_output_data$SM_0_3[j]
        event_output_data$F2[j]<-event_output_data$F3[j]
        event_output_data$Z2[j]<-event_output_data$Z3[j]
        event_output_data$SM_0_3[j]<-event_output_data$SM_0_4[j]
        event_output_data$F3[j]<-event_output_data$F4[j]
        event_output_data$Z3[j]<-event_output_data$Z4[j]
        event_output_data$SM_0_4[j]<-event_output_data$SM_0_5[j]
        event_output_data$F4[j]<-event_output_data$F5[j]
        event_output_data$Z4[j]<-event_output_data$Z5[j]
        event_output_data$SM_0_5[j]<-wilting_point
        event_output_data$Z5[j]<-0
        event_output_data$F5[j]<-0
      }else{}
    }
    #### Fill in other Z, F and SM ####
    event_output_data$Z6<-0
    event_output_data$F6<-0
    event_output_data$SM_0_6<-wilting_point
    
  } else{
    print("!!!! ERROR: EXCEEDED 5 WETTING FRONTS !!!!")
    #let's test just adding wetting front 5 to wetting front 4
  }
  
  #### adding event data output into the large data frame ####
  matching_indices<-match(event_output_data$DateTime, GAR_Output_Data$DateTime)
  GAR_Output_Data$Z1[matching_indices]<-event_output_data$Z1
  GAR_Output_Data$Z2[matching_indices]<-event_output_data$Z2
  GAR_Output_Data$Z3[matching_indices]<-event_output_data$Z3
  GAR_Output_Data$Z4[matching_indices]<-event_output_data$Z4
  GAR_Output_Data$Z5[matching_indices]<-event_output_data$Z5
  GAR_Output_Data$Z6[matching_indices]<-event_output_data$Z6
  GAR_Output_Data$SM_0_1[matching_indices]<-event_output_data$SM_0_1
  GAR_Output_Data$SM_0_2[matching_indices]<-event_output_data$SM_0_2
  GAR_Output_Data$SM_0_3[matching_indices]<-event_output_data$SM_0_3
  GAR_Output_Data$SM_0_4[matching_indices]<-event_output_data$SM_0_4
  GAR_Output_Data$SM_0_5[matching_indices]<-event_output_data$SM_0_5
  GAR_Output_Data$SM_0_6[matching_indices]<-event_output_data$SM_0_6
  GAR_Output_Data$F1[matching_indices]<-event_output_data$F1
  GAR_Output_Data$F2[matching_indices]<-event_output_data$F2
  GAR_Output_Data$F3[matching_indices]<-event_output_data$F3
  GAR_Output_Data$F4[matching_indices]<-event_output_data$F4
  GAR_Output_Data$F5[matching_indices]<-event_output_data$F5
  GAR_Output_Data$F6[matching_indices]<-event_output_data$F6
  GAR_Output_Data$f[matching_indices]<-event_output_data$f
  GAR_Output_Data$AET[matching_indices]<-event_output_data$AET
}

event_output_data

GAR_Output_Data

#### crop final data by the end date of the last rainfall event ####
#CROP OUTPUT DATA
# Get the length of the list
list_length <- length(rainfall_events_data)
# Access the last item in the list
last_item <- rainfall_events_data[[list_length]]
event_length_2<-nrow(last_item)
end_date<-last_item$DateTime[event_length_2]
#crop
GAR_Output_Data<-subset(GAR_Output_Data, DateTime<=end_date)
#CROP INPUT DATA
GAR_Input_Data<-subset(GAR_Input_Data, DateTime<=end_date)

#### ERROR CHECKING #####
#checking if there are any "infinite" values in the data frame.
# Value to check for
target_value <- "Inf"
# Check if the target value exists anywhere in the data frame
if (any(apply(GAR_Output_Data, 1, function(row) target_value %in% row))) {
  print("True")
} else {
  print("False")
}


######## DEFINING NR and TR for each wetting front #####
# Load required packages
library(dplyr)

#wetting front 1 and 2
# Initialize variables
redistribution_number <- 0
redistribution_number_1<-0
redistribution_number_2<-0
redistribution_number_3<-0
redistribution_number_4<-0
redistribution_number_5<-0
redistribution_time<-0
redistribution_time_1<-0
redistribution_time_2<-0
redistribution_time_3<-0
redistribution_time_4<-0
redistribution_time_5<-0

# Iterate through the data
for (i in 2:nrow(GAR_Output_Data)) {
  # Check if a new redistribution period starts for either wetting front
  if (GAR_Output_Data$SM_0_1[i-1]==theta_s & GAR_Output_Data$SM_0_1[i]<theta_s) {
    redistribution_number <- redistribution_number + 1
    redistribution_number_1<-redistribution_number
    redistribution_time_1 <- 0.0
  } 
  
  if (GAR_Output_Data$SM_0_2[i-1]==theta_s & GAR_Output_Data$SM_0_2[i]<theta_s) {
    redistribution_number <- redistribution_number + 1
    redistribution_number_2<-redistribution_number
    redistribution_time_2 <- 0.0
  } 
  
  if (GAR_Output_Data$SM_0_3[i-1]==theta_s & GAR_Output_Data$SM_0_3[i]<theta_s) {
    redistribution_number <- redistribution_number + 1
    redistribution_number_3<-redistribution_number
    redistribution_time_3 <- 0.0
  } 
  
  if (GAR_Output_Data$SM_0_4[i-1]==theta_s & GAR_Output_Data$SM_0_4[i]<theta_s) {
    redistribution_number <- redistribution_number + 1
    redistribution_number_4<-redistribution_number
    redistribution_time_4 <- 0.0
  } 
  
  if (GAR_Output_Data$SM_0_5[i-1]==theta_s & GAR_Output_Data$SM_0_5[i]<theta_s) {
    redistribution_number <- redistribution_number + 1
    redistribution_number_5<-redistribution_number
    redistribution_time_5 <- 0.0
  } 
  
  # Calculate the redistribution time within the period
  tr_1 <- redistribution_time_1
  tr_2 <- redistribution_time_2
  tr_3 <- redistribution_time_3
  tr_4 <- redistribution_time_4
  tr_5 <- redistribution_time_4
  
  # Update redistribution time for the next time step
  redistribution_time_1 <- redistribution_time_1 + 0.5
  redistribution_time_2 <- redistribution_time_2 + 0.5
  redistribution_time_3 <- redistribution_time_3 + 0.5
  redistribution_time_4 <- redistribution_time_4 + 0.5
  redistribution_time_5 <- redistribution_time_5 + 0.5
  
  # Update the data frame with redistribution number (NR) and redistribution time (TR)
  GAR_Output_Data$N_r1[i] <- redistribution_number_1
  GAR_Output_Data$T_r1[i] <- tr_1
  
  GAR_Output_Data$N_r2[i] <- redistribution_number_2
  GAR_Output_Data$T_r2[i] <- tr_2
  
  GAR_Output_Data$N_r3[i] <- redistribution_number_3
  GAR_Output_Data$T_r3[i] <- tr_3
  
  GAR_Output_Data$N_r4[i] <- redistribution_number_4
  GAR_Output_Data$T_r4[i] <- tr_4
  
  GAR_Output_Data$N_r5[i] <- redistribution_number_5
  GAR_Output_Data$T_r5[i] <- tr_5
  
  
  if(GAR_Output_Data$N_r2[i]==0){
    GAR_Output_Data$N_r2[i]<-NA
    GAR_Output_Data$T_r2[i]<-NA
  }  
  if(GAR_Output_Data$N_r3[i]==0){
    GAR_Output_Data$N_r3[i]<-NA
    GAR_Output_Data$T_r3[i]<-NA
  }
  if(GAR_Output_Data$N_r4[i]==0){
    GAR_Output_Data$N_r4[i]<-NA
    GAR_Output_Data$T_r4[i]<-NA
  }
  if(GAR_Output_Data$N_r5[i]==0){
    GAR_Output_Data$N_r5[i]<-NA
    GAR_Output_Data$T_r5[i]<-NA
  }
}

#####

GAR_Output_Data$R_coeff_1<-a_1+(a_2*log(GAR_Output_Data$T_r1))+(a_3/GAR_Output_Data$N_r1)
GAR_Output_Data$R_coeff_2<-a_1+(a_2*log(GAR_Output_Data$T_r2))+(a_3/GAR_Output_Data$N_r2)
GAR_Output_Data$R_coeff_3<-a_1+(a_2*log(GAR_Output_Data$T_r3))+(a_3/GAR_Output_Data$N_r3)
GAR_Output_Data$R_coeff_4<-a_1+(a_2*log(GAR_Output_Data$T_r4))+(a_3/GAR_Output_Data$N_r4)
GAR_Output_Data$R_coeff_5<-a_1+(a_2*log(GAR_Output_Data$T_r5))+(a_3/GAR_Output_Data$N_r5)

GAR_Output_Data$R_coeff_1[3:4]<-NA

for(i in 1:98){
  if(is.na(GAR_Output_Data$R_coeff_1[i])){
    GAR_Output_Data$R_coeff_1[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_2[i])){
    GAR_Output_Data$R_coeff_2[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_3[i])){
    GAR_Output_Data$R_coeff_3[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_4[i])){
    GAR_Output_Data$R_coeff_4[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_5[i])){
    GAR_Output_Data$R_coeff_5[i]<-0
  }
  if(GAR_Output_Data$R_coeff_1[i]<0){
    GAR_Output_Data$R_coeff_1[i]<-0
  }
}

for(i in 1:nrow(GAR_Output_Data)){
  if(GAR_Output_Data$SM_0_1[i]==theta_s){
    GAR_Output_Data$R_coeff_1[i]<-0
  }
  if(GAR_Output_Data$SM_0_2[i]==theta_s){
    GAR_Output_Data$R_coeff_2[i]<-0
  }
  if(GAR_Output_Data$SM_0_3[i]==theta_s){
    GAR_Output_Data$R_coeff_3[i]<-0
  }
  if(GAR_Output_Data$SM_0_4[i]==theta_s){
    GAR_Output_Data$R_coeff_4[i]<-0
  }
  if(GAR_Output_Data$SM_0_5[i]==theta_s){
    GAR_Output_Data$R_coeff_5[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_1[i])){
    GAR_Output_Data$R_coeff_1[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_2[i])){
    GAR_Output_Data$R_coeff_2[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_3[i])){
    GAR_Output_Data$R_coeff_3[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_4[i])){
    GAR_Output_Data$R_coeff_4[i]<-0
  }
  if(is.na(GAR_Output_Data$R_coeff_5[i])){
    GAR_Output_Data$R_coeff_5[i]<-0
  }
  if(GAR_Output_Data$R_coeff_1[i]=="NaN"){
    GAR_Output_Data$R_coeff_1[i]<-0
  }
  if(GAR_Output_Data$R_coeff_2[i]=="NaN"){
    GAR_Output_Data$R_coeff_2[i]<-0
  }
  if(GAR_Output_Data$R_coeff_3[i]=="NaN"){
    GAR_Output_Data$R_coeff_3[i]<-0
  }
  if(GAR_Output_Data$R_coeff_4[i]=="NaN"){
    GAR_Output_Data$R_coeff_4[i]<-0
  }
  if(GAR_Output_Data$R_coeff_5[i]=="NaN"){
    GAR_Output_Data$R_coeff_5[i]<-0
  }
  if(GAR_Output_Data$R_coeff_1[i]<0){
    GAR_Output_Data$R_coeff_1[i]<-0
  }
  if(GAR_Output_Data$R_coeff_1[i]==Inf){
    GAR_Output_Data$R_coeff_1[i]<-0
  }
  if(GAR_Output_Data$R_coeff_1[i]==-Inf){
    GAR_Output_Data$R_coeff_1[i]<-0
  }
  if(GAR_Output_Data$R_coeff_2[i]== -Inf){
    GAR_Output_Data$R_coeff_2[i]<-0
  }
  if(GAR_Output_Data$R_coeff_2[i]== Inf){
    GAR_Output_Data$R_coeff_2[i]<-0
  }
  if(GAR_Output_Data$R_coeff_3[i]== -Inf){
    GAR_Output_Data$R_coeff_3[i]<-0
  }
  if(GAR_Output_Data$R_coeff_3[i]== Inf){
    GAR_Output_Data$R_coeff_3[i]<-0
  }
  if(GAR_Output_Data$R_coeff_4[i]== -Inf){
    GAR_Output_Data$R_coeff_4[i]<-0
  }
  if(GAR_Output_Data$R_coeff_4[i]== Inf){
    GAR_Output_Data$R_coeff_4[i]<-0
  }
  if(GAR_Output_Data$R_coeff_5[i]== -Inf){
    GAR_Output_Data$R_coeff_5[i]<-0
  }
  if(GAR_Output_Data$R_coeff_5[i]== Inf){
    GAR_Output_Data$R_coeff_5[i]<-0
  }
}

GAR_Output_Data$SM_0_1_Corr<-GAR_Output_Data$SM_0_1-GAR_Output_Data$R_coeff_1
GAR_Output_Data$SM_0_2_Corr<-GAR_Output_Data$SM_0_2-GAR_Output_Data$R_coeff_2
GAR_Output_Data$SM_0_3_Corr<-GAR_Output_Data$SM_0_3-GAR_Output_Data$R_coeff_3
GAR_Output_Data$SM_0_4_Corr<-GAR_Output_Data$SM_0_4-GAR_Output_Data$R_coeff_4
GAR_Output_Data$SM_0_5_Corr<-GAR_Output_Data$SM_0_4-GAR_Output_Data$R_coeff_5


#create a column for surface water content
GAR_Output_Data$Surface_WC<-NA
#choose the highest value and fill in with that
for(i in 1:dim(GAR_Output_Data)[1]){
  GAR_Output_Data$Surface_WC[i]<-max(GAR_Output_Data$SM_0_1[i], GAR_Output_Data$SM_0_2[i], GAR_Output_Data$SM_0_3[i], GAR_Output_Data$SM_0_4[i], GAR_Output_Data$SM_0_5[i])
}

##### Fill in other SM for event 1 ######
#GAR_Output_Data$SM_0_2[1:736]<-FC
#GAR_Output_Data$SM_0_3[1:736]<-FC
#GAR_Output_Data$SM_0_4[1:736]<-FC
#GAR_Output_Data$SM_0_5[1:736]<-FC

##### Calculating Relative Soil Moisture in the top 10cm of soil ####
#weighted soil moisture content in 0.1 m depth of soil (10cm)
#need to do this with 4 possible wetting fronts
GAR_Output_Data$SM_Weighted_10<-NA

initial_soil_moisture<-wilting_point

for(i in 1:dim(GAR_Output_Data)[1]) {
  if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_1[i]
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z1[i]<10){
      SMI_Z<-10-GAR_Output_Data$Z1[i]
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_1[i]*(GAR_Output_Data$Z1[i]/(GAR_Output_Data$Z1[i]+SMI_Z))) + (GAR_Output_Data$SM_0_2[i]*(SMI_Z/(GAR_Output_Data$Z1[i]+SMI_Z)))
    } else if(GAR_Output_Data$Z1[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_1[i]
    }
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z2[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_2[i]
    } else if (GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]<10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_2[i]*(GAR_Output_Data$Z2[i]/10)) +(GAR_Output_Data$SM_0_1[i]*((GAR_Output_Data$Z1[i]-GAR_Output_Data$Z2[i])/(10)))+(GAR_Output_Data$SM_0_3[i]*((10-GAR_Output_Data$Z1[i])/(10)))
    } else if (GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_2[i]*(GAR_Output_Data$Z2[i]/10)) +(GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    }
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z3[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_3[i]
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] < 10 & GAR_Output_Data$Z1[i]<10) {
      SMI_Z<-10-GAR_Output_Data$Z1[i]
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_3[i]*(GAR_Output_Data$Z3[i]/10)) +(GAR_Output_Data$SM_0_2[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10)))+(GAR_Output_Data$SM_0_1[i]*((GAR_Output_Data$Z1[i]-GAR_Output_Data$Z2[i])/(10)))+(GAR_Output_Data$SM_0_4[i]*((10-GAR_Output_Data$Z1[i])/(10)))
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] < 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_3[i]*(GAR_Output_Data$Z3[i]/10)) +(GAR_Output_Data$SM_0_2[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10)))+(GAR_Output_Data$SM_0_5[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] > 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_3[i]*(GAR_Output_Data$Z3[i]/10)) +(GAR_Output_Data$SM_0_2[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    }
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z4[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_4[i]
    } else if (GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i] < 10 & GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]<10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_4[i]*(GAR_Output_Data$Z4[i]/10)) +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1[i]*((GAR_Output_Data$Z1[i]-GAR_Output_Data$Z2[i])/(10))) +(GAR_Output_Data$SM_0_5[i]*((10-GAR_Output_Data$Z1[i])/(10)))
    } else if (GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i] < 10 & GAR_Output_Data$Z2[i] < 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_4[i]*(GAR_Output_Data$Z4[i]/10))  +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] > 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_4[i]*(GAR_Output_Data$Z4[i]/10))  +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if (GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i] > 10 & GAR_Output_Data$Z2[i] > 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_4[i]*(GAR_Output_Data$Z4[i]/10))+(GAR_Output_Data$SM_0_3[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    }
    
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_5[i]
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]<10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]- GAR_Output_Data$Z5[i])/10)) +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1[i]*((GAR_Output_Data$Z1[i]-GAR_Output_Data$Z2[i])/(10))) + (initial_soil_moisture*((10-GAR_Output_Data$Z1[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i] < 10 & GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]- GAR_Output_Data$Z5[i])/10)) +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i] < 10 & GAR_Output_Data$Z2[i]>10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]- GAR_Output_Data$Z5[i])/10)) +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i] > 10 & GAR_Output_Data$Z2[i]>10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]- GAR_Output_Data$Z5[i])/10))+(GAR_Output_Data$SM_0_3[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] > 10 & GAR_Output_Data$Z3[i] > 10 & GAR_Output_Data$Z2[i]>10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4[i]*((10-GAR_Output_Data$Z5[i])/10))
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_5[i]
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i]<10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10))  +(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10)))  +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10)))  + (GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10))  +(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10))) +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i]>10 & GAR_Output_Data$Z2[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10)) +(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10)))  +(GAR_Output_Data$SM_0_3[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] > 10 & GAR_Output_Data$Z3[i]>10 & GAR_Output_Data$Z2[i]>10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10)) +(GAR_Output_Data$SM_0_4[i]*((10-GAR_Output_Data$Z5[i])/(10)))
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_5[i]
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i] < 10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10)) +(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10))) +(GAR_Output_Data$SM_0_3[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10)))+(GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i] > 10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10)) +(GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10)))   +(GAR_Output_Data$SM_0_3[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] > 10 & GAR_Output_Data$Z3[i] > 10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/10))   +(GAR_Output_Data$SM_0_4[i]*((10-GAR_Output_Data$Z5[i])/(10)))
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i]<10 ){
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/(10)))  + (GAR_Output_Data$SM_0_4[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10)))+ (GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if(GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i]>10 ){
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/(10)))  + (GAR_Output_Data$SM_0_4[i]*((10-GAR_Output_Data$Z5[i])/(10)))
    } else if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_5[i]
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]<10){
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_5[i]*(GAR_Output_Data$Z5[i]/(10)))  +  (GAR_Output_Data$SM_0_4[i]*((10-GAR_Output_Data$Z5[i])/(10)))
    } else if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_5[i]
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z2[i]<10){
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_2[i]*(GAR_Output_Data$Z2[i]/(10)))  + (GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if(GAR_Output_Data$Z2[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_2[i]
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z3[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_3[i]
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] < 10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_3[i]*(GAR_Output_Data$Z3[i]/10))  +(GAR_Output_Data$SM_0_2[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] > 10) {
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_3[i]*(GAR_Output_Data$Z3[i]/10))  +(GAR_Output_Data$SM_0_2[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z3[i]<10){
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_3[i]*(GAR_Output_Data$Z3[i]/(10)))  + (GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if(GAR_Output_Data$Z3[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_3[i]
    }
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z4[i]<10){
      GAR_Output_Data$SM_Weighted_10[i]<-(GAR_Output_Data$SM_0_4[i]*(GAR_Output_Data$Z4[i]/(10)))  + (GAR_Output_Data$SM_0_1[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if(GAR_Output_Data$Z4[i]>10){
      GAR_Output_Data$SM_Weighted_10[i]<-GAR_Output_Data$SM_0_4[i]
    }
  } 
}

#Calculating CORRECTED Relative Soil Moisture in top 10cm of Soil
GAR_Output_Data$SM_Weighted_10_Corr<-NA

initial_soil_moisture<-wilting_point

for(i in 1:dim(GAR_Output_Data)[1]) {
  if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_1_Corr[i]
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z1[i]<10){
      SMI_Z<-10-GAR_Output_Data$Z1[i]
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_1_Corr[i]*(GAR_Output_Data$Z1[i]/(GAR_Output_Data$Z1[i]+SMI_Z))) + (GAR_Output_Data$SM_0_2_Corr[i]*(SMI_Z/(GAR_Output_Data$Z1[i]+SMI_Z)))
    } else if(GAR_Output_Data$Z1[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_1_Corr[i]
    }
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z2[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_2_Corr[i]
    } else if (GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]<10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_2_Corr[i]*(GAR_Output_Data$Z2[i]/10)) +(GAR_Output_Data$SM_0_1_Corr[i]*((GAR_Output_Data$Z1[i]-GAR_Output_Data$Z2[i])/(10)))+(GAR_Output_Data$SM_0_3_Corr[i]*((10-GAR_Output_Data$Z1[i])/(10)))
    } else if (GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_2_Corr[i]*(GAR_Output_Data$Z2[i]/10)) +(GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    }
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z3[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_3_Corr[i]
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] < 10 & GAR_Output_Data$Z1[i]<10) {
      SMI_Z<-10-GAR_Output_Data$Z1[i]
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_3_Corr[i]*(GAR_Output_Data$Z3[i]/10)) +(GAR_Output_Data$SM_0_2_Corr[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10)))+(GAR_Output_Data$SM_0_1_Corr[i]*((GAR_Output_Data$Z1[i]-GAR_Output_Data$Z2[i])/(10)))+(GAR_Output_Data$SM_0_4_Corr[i]*((10-GAR_Output_Data$Z1[i])/(10)))
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] < 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_3_Corr[i]*(GAR_Output_Data$Z3[i]/10)) +(GAR_Output_Data$SM_0_2_Corr[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10)))+(GAR_Output_Data$SM_0_5_Corr[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] > 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_3_Corr[i]*(GAR_Output_Data$Z3[i]/10)) +(GAR_Output_Data$SM_0_2_Corr[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    }
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z4[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_4_Corr[i]
    } else if (GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i] < 10 & GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]<10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_4_Corr[i]*(GAR_Output_Data$Z4[i]/10)) +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2_Corr[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1_Corr[i]*((GAR_Output_Data$Z1[i]-GAR_Output_Data$Z2[i])/(10))) +(GAR_Output_Data$SM_0_5_Corr[i]*((10-GAR_Output_Data$Z1[i])/(10)))
    } else if (GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i] < 10 & GAR_Output_Data$Z2[i] < 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_4_Corr[i]*(GAR_Output_Data$Z4[i]/10))  +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2_Corr[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] > 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_4_Corr[i]*(GAR_Output_Data$Z4[i]/10))  +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2_Corr[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if (GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i] > 10 & GAR_Output_Data$Z2[i] > 10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_4_Corr[i]*(GAR_Output_Data$Z4[i]/10))+(GAR_Output_Data$SM_0_3_Corr[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    }
    
  } else if(GAR_Output_Data$Z1[i]>0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_5_Corr[i]
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]<10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]- GAR_Output_Data$Z5[i])/10)) +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2_Corr[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1_Corr[i]*((GAR_Output_Data$Z1[i]-GAR_Output_Data$Z2[i])/(10))) + (initial_soil_moisture*((10-GAR_Output_Data$Z1[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i] < 10 & GAR_Output_Data$Z2[i]<10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]- GAR_Output_Data$Z5[i])/10)) +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2_Corr[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i]<10 & GAR_Output_Data$Z3[i] < 10 & GAR_Output_Data$Z2[i]>10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]- GAR_Output_Data$Z5[i])/10)) +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2_Corr[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i] > 10 & GAR_Output_Data$Z2[i]>10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]- GAR_Output_Data$Z5[i])/10))+(GAR_Output_Data$SM_0_3_Corr[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] > 10 & GAR_Output_Data$Z3[i] > 10 & GAR_Output_Data$Z2[i]>10 & GAR_Output_Data$Z1[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10))+(GAR_Output_Data$SM_0_4_Corr[i]*((10-GAR_Output_Data$Z5[i])/10))
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_5_Corr[i]
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i]<10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10))  +(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10)))  +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2_Corr[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10)))  + (GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10))  +(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10))) +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10))) +(GAR_Output_Data$SM_0_2_Corr[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i]>10 & GAR_Output_Data$Z2[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10)) +(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10)))  +(GAR_Output_Data$SM_0_3_Corr[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] > 10 & GAR_Output_Data$Z3[i]>10 & GAR_Output_Data$Z2[i]>10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10)) +(GAR_Output_Data$SM_0_4_Corr[i]*((10-GAR_Output_Data$Z5[i])/(10)))
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_5_Corr[i]
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i] < 10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10)) +(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10))) +(GAR_Output_Data$SM_0_3_Corr[i]*((GAR_Output_Data$Z3[i]-GAR_Output_Data$Z4[i])/(10)))+(GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] < 10 & GAR_Output_Data$Z3[i] > 10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10)) +(GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10)))   +(GAR_Output_Data$SM_0_3_Corr[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if (GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i] > 10 & GAR_Output_Data$Z3[i] > 10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/10))   +(GAR_Output_Data$SM_0_4_Corr[i]*((10-GAR_Output_Data$Z5[i])/(10)))
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i]<10 ){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/(10)))  + (GAR_Output_Data$SM_0_4_Corr[i]*((GAR_Output_Data$Z4[i]-GAR_Output_Data$Z5[i])/(10)))+ (GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if(GAR_Output_Data$Z5[i]<10 & GAR_Output_Data$Z4[i]>10 ){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/(10)))  + (GAR_Output_Data$SM_0_4_Corr[i]*((10-GAR_Output_Data$Z5[i])/(10)))
    } else if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_5_Corr[i]
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]>0) {
    if(GAR_Output_Data$Z5[i]<10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_5_Corr[i]*(GAR_Output_Data$Z5[i]/(10)))  +  (GAR_Output_Data$SM_0_4_Corr[i]*((10-GAR_Output_Data$Z5[i])/(10)))
    } else if(GAR_Output_Data$Z5[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_5_Corr[i]
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z2[i]<10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_2_Corr[i]*(GAR_Output_Data$Z2[i]/(10)))  + (GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if(GAR_Output_Data$Z2[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_2_Corr[i]
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]>0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z3[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_3_Corr[i]
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] < 10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_3_Corr[i]*(GAR_Output_Data$Z3[i]/10))  +(GAR_Output_Data$SM_0_2_Corr[i]*((GAR_Output_Data$Z2[i]-GAR_Output_Data$Z3[i])/(10))) +(GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z2[i])/(10)))
    } else if (GAR_Output_Data$Z3[i]<10 & GAR_Output_Data$Z2[i] > 10) {
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_3_Corr[i]*(GAR_Output_Data$Z3[i]/10))  +(GAR_Output_Data$SM_0_2_Corr[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    }
    
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]>0 & GAR_Output_Data$Z4[i]==0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z3[i]<10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_3_Corr[i]*(GAR_Output_Data$Z3[i]/(10)))  + (GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z3[i])/(10)))
    } else if(GAR_Output_Data$Z3[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_3_Corr[i]
    }
  } else if(GAR_Output_Data$Z1[i]==0 & GAR_Output_Data$Z2[i]==0 & GAR_Output_Data$Z3[i]==0 & GAR_Output_Data$Z4[i]>0 & GAR_Output_Data$Z5[i]==0) {
    if(GAR_Output_Data$Z4[i]<10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-(GAR_Output_Data$SM_0_4_Corr[i]*(GAR_Output_Data$Z4[i]/(10)))  + (GAR_Output_Data$SM_0_1_Corr[i]*((10-GAR_Output_Data$Z4[i])/(10)))
    } else if(GAR_Output_Data$Z4[i]>10){
      GAR_Output_Data$SM_Weighted_10_Corr[i]<-GAR_Output_Data$SM_0_4_Corr[i]
    }
  } 
}



