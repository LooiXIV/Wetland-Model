# TIBS Wetland Bathtub Model #
##########################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!    READ THIS BEFORE BEGINNING    !#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##########################################################################
# The following R code models the waterflow and nutrient dynamics
# of select wetlands in the St. Lawrence River. This model is an 
# intuitive mass balance model that takes in volumes of daily runoff
# from GWLF and masses of nutrients and models daily: water flow, 
# dilution rates, total nutrient, and nutrient concentration. This
# model accounts for the effects of water control structures placed
# in the TIBS wetlands. Currently the code is only set up to model
# 3 wetland site pairs within TIBS: Butterfield and Dorr Road; Delaney 
# and Plum Tree; Carpenters and Deferno Branches.
# For more detailed documentation and pseudo-code please consult
# the following documents:
#
# -Wetland Model Pseudo-code.docx
# -Wetland Model Read me.docx
# -Wetland Model Instructions.docx
# -wetland Model input file formatting.docx
#
##########################################################################
# Inputs Needed to run Model #
##########################################################################
# 1) GWLF input for the wetland site:
# The watersheds for each wetland site were modeled with GWLF. This
# model uses the GWLF model output and uses it as inputs for this
# wetland model.
#
# 2) Weather data and Lake Ontario or Alexanderia Bay Water level Data:
# The GWLF input does not account for rain falling directly into
# the wetland. It also needs minimum and maximum temperature data
# to estimate evaporation and evapotranspiration. The Alexanderia Bay
# or Lake Ontario water level data is used in conjuction with the next
# model input to estimate water level in wetland sites without water
# control structures. 
#
# 3) Wetland Water level linear model data:
# These are the parameters for linear models relating site water level 
# to St. Lawrence River water level. Each site has and two parameters
# first the slope of the linear relation ship "M", and "B" the intercept
# of the relationship. Together these are used to estimate water levels
# at sites without water control structures based on the water leve in
# the St Lawrence River, specifically Alexandria Bay.
#
# 4) Wetland site specific parameters:
# This file has parameter values that pertain to each wetland specifically.
# Things such as: is there a water control structure, if there is a water
# control structure what is the plank height, the area of open water
# area of typha cover. etc.
#
# 5) Wetland site specific volumetric data:
# This data tells the model the volume of water in the wetland for a given
# wetland water level. This should be in thousandths of meters for the
# water level data and thousandths of cubic meters for the volume data.
#
# 6) Wetland specific Water level management plans:
# Wetland sites with Water control structures have flash boards that are
# either taken out or installed depending on the: current management plan,
# the water level, previous days' rainfall, and ecology. This file should
# have a column for date and the height the Water control structure's weir
# has been raised that day (in cm).
##########################################################################
# Items, files, tables, and folders that this code produces:
# 1) Raw Model data tables and files:
# 2) Monthly mean summary data tables and files:
##########################################################################
graphics.off();
rm(list = ls());
require(lubridate)
#########################################################
# Modeling waterlevel fluctuation and flow in a wetland #
#########################################################
# Enter the Site name
# Site names to enter: Carpenters, Deferno, Delaney, Plum Tree, 
#                      Butterfield, DorrRoad
# Enter site names as they are listed above (Case and Space sensitive)

# These are the time periods to evaluate. S.Years are the starting years
# E.Years are the ending years
S.Years = c(1933, 1943, 1973, 2005)
E.Years = c(1942, 1951, 1982, 2014)

# The main directory where files are set up.
# main.dir = paste("/Users/Looi/Dropbox/NOAA_Wetlands_Ceili-Alex/",
#                  "Alex's Folder/Wetland Model/Watersheds", sep = "")

main.dir = paste("C:/Users/Alexander Looi/Google Drive/Dropbox/NOAA_Wetlands_Ceili-Alex/",
                 "Alex's Folder/Wetland Model/Watersheds", sep = "")
setwd(main.dir)
source("TIBS Wetland Model Plotting Functions v1.R")
##########################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!DO NOT EDIT CODE BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!DO NOT EDIT CODE BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!DO NOT EDIT CODE BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!DO NOT EDIT CODE BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##########################################################################
ptm = proc.time()

sites.TIBS = c("Carpenters", "Deferno", "Delaney", 
               "Plum Tree", "Butterfield", "DorrRoad")

Wetland.Sites = c(c("Carpenters", "CB"),c("Deferno", "DB"), 
                  c("Delaney", "DL"), c("Plum Tree", "PT"),
                  c("Butterfield", "BF"), c("DorrRoad", "DR"))
S.s = 4
S.l = 4
y.l = 4
y.s = 4
for(year.int in y.s:y.l){
  
  Start.Year = S.Years[year.int]
  End.Year = E.Years[year.int]
  
  cat(paste(S.Years[year.int], "-", E.Years[year.int]), fill = T)
  
  for (sw in S.s:S.l) {
    
    graphics.off();
  
    Site.name = sites.TIBS[sw]
    
    cat(Site.name, fill = T)
    
    setwd(main.dir)

    site.pos = which(Wetland.Sites == Site.name)
    site.abv = Wetland.Sites[site.pos + 1]
    
    # Read in All necessary data to calculate: 
    # potential evaporation and water levels
    
    setwd(paste(main.dir, "/Wetland Model Weather/", sep = ""))
    All.Data.filename = list.files(pattern = paste("Wetland Model Weather", 
                                                   Start.Year, "-", 
                                                   End.Year, sep = ""))
    W.raw = read.table(All.Data.filename, header = T, sep = ",")
    
    # Create a new Data frame with just dates and temperatures in Celcius
    ########################################################################
    # Create a vector by calculating a mean between the max temp and min temp 
    # for all available days
    Tmean = ((W.raw$TMAX + W.raw$TMIN)/2)/10
    # Grab all dates from the data frame and convert to POSIX class
    # Dates = as.POSIXlt(as.character(W.raw$DATE), 
    #                    format = "%m/%d/%y")
    Dates = mdy(W.raw$DATE,tz = "EST")
    # Convert from 10's of Celcius to Standard Celcius for both Tmin
    # and Tmax
    Tmin = W.raw$TMIN/10
    Tmax = W.raw$TMAX/10
    # Put all reformated data into a new data table
    temps = data.frame(Dates, Tmean, Tmin, Tmax)
    # Calculate the delta T value or the difference between Tmax and Tmin
    # for use in the Evaporation/Evapotranspiration equation
    D.T = Tmax-Tmin
    # Read in prcp data and convert to meters
    w.prcp = W.raw$PRCP/10000
    # Read in St. Lawrence River Water Level Data
    River.WL = W.raw[,6]
    WL.len = length(River.WL)
    ##############################################################
    # Find water level flash board management file and enter in
    # Data
    ##############################################################
    setwd(paste(main.dir, "/Water Level Management Plans/", sep = ""))
    Flash.boards.name = paste("Water Level Management Plan ", 
                              Start.Year, "-", End.Year, ".csv", sep = "")
    Flash.Boards.plan = read.table(Flash.boards.name, header = T, sep = ",")
    
    ###########################################################
    # Read in Individual wetland parameters #
    ###########################################################
    # Read in the data table that had additional parameters for modeling
    # each wetland site. This includes parameters such as, plank heights
    # that depend on the season, starting water level height, weir 
    # length, and the area of Typha and Open water
    setwd(main.dir)
    w.parms = read.table("Wetland Parameters.csv", sep = ",", header = T)
    row.names(w.parms) = w.parms$X
    w.parms = w.parms[,-1]
    site.parms = w.parms[which(site.abv == row.names(w.parms)),]
    
    for (s in 3:11){
      assign(names(site.parms)[s], site.parms[s])
    }
    
    # is there a WCS? additional parameters if there is a WCS (is.WCS)
    is.WCS = as.logical(is.WCS) # is there a water control structure?
    p.h = as.numeric(p.h) # starting plank height (p.h)
    L = as.numeric(L) # width of weir, in feet (L)
    i.WL = as.numeric(i.WL) # initial water level (i.WL)
    i.PH = as.numeric(i.ph)
    AOW = as.numeric(AOW) # Area of the open water in the wetland in m^2 (AOW) 
    AT = as.numeric(AT) # Area of the typha in m^2 (AT)
    tot.A = (AT + AOW) # Total wetland Area in m^2 (tot.A)
    i.P = as.numeric(i.P) # initial Phosphorus concentrations
    i.N = as.numeric(i.N) # initial Nitrogen concentrations
    #########################################################
    # Read in Water level vs. volume relationship #
    #########################################################
    dir.sites = paste(main.dir, "/Wetland Sites/", sep = "")
    setwd(dir.sites)
    Site.dir = list.files(pattern = Site.name)
    dir = paste(main.dir,"/Wetland Sites/", 
                Site.dir, sep = "")
    setwd(dir)
    file = paste(site.abv, "Vol vs WL.csv")
    WLvsVol = read.table(file, sep = ",", header = T)
    # Read in Linear Regression parameters #
    #########################################################
    # Note that this relationship is only for wetlands that
    # DO NOT have a WCS. This is because wetlands with a 
    # WCS is essentially "decoupled" from the River.
    setwd(main.dir)
    lm.parms = read.table("Waterlevel lm parameters.csv", 
                          header = T, sep = ",")
    row.names(lm.parms) = lm.parms$Parm
    lm.parms = lm.parms[,-1]
    #########################################################
    # Evapotranspiration Parameters #
    #########################################################
    # Crop Coefficients For each season
    # For Typha
    K_c = c(0, 0.4, 1.2, 0.4) 
    # Winter, Spring, Summer, Fall
    
    # Mean daylength times for each month at the 44th degree North Lat. 
    # The vector starts in Jan and ends December
    R_a = c(9.1, 10.3, 11.6, 13.2, 14.6, 15.3, 
            15.0, 13.8, 12.3, 10.7, 9.4, 8.7)
    
    # these are conversion numbers to convert mm 
    # per day per hectare to m^3 per day
    mm.to.m = 1/1000 # millimeters to meters conversion (0.001 per meter)
    hec.to.m = 10000 # hectares to meters squared 
                     # conversion (10,000 m^2 to hectare)
    
    ##########################################################
    # Snowmelt Parameters #
    ##########################################################
    # when Tmean greater than 0
    # Q(n+1) = K_b*Qn + (1-K_b)*(SMC*Tmean*tot.A+Prcp)
    
    # Baseflow recession constant (unitless)
    K_b = 0.924
    # Snow melt constant 
    SMC = 0.0045 # meters*(Celcius^-1)*(day^-1)
    
    # Read in GLWF inputs #
    ###################################################################
    # These inputs are developed from WLF, the Mapshed plugin for GIS
    
    dir.output = paste(dir.sites, 
                       Site.dir, "/GWLF input to wetland model", sep = "")
    setwd(dir.output)
    # Find the file name in the folder (all files have the pattern Dayflow
    # for all sites).
    GWLF.input = list.files(pattern = paste(Start.Year,"-",End.Year, sep = ""))
    model.input = read.table(GWLF.input, header = T, sep = ",")
    model.colnames = names(model.input)
    
    # Take only the Nutrient input from GWLF watershed model results
    # numbers should only be in Kg.
    TP.kg = model.input$TP..kg.
    TN.kg = model.input$TN..kg.
    
    # Conversion factor: Kilograms to grams
    kg.to.g = 1000
    
    #Conversion factor: Moles to Micro-moles
    M.to.uM = 1000000
    
    # Conversion factor for converting Mass (grams) to moles
    P.MM = 30.97376
    N.MM = 14.00728
    
    # Units as is are in m^3 need to convert to kg/L
    # (there are 1000 Liters in 1 m^3)
    m3.to.L = 1000
    
    # Give daily inflow its own variable (in m^3 per day)
    Daily.inflow = model.input$Daily.Flow..m.3
    
    #########################################################
    # The following are vectors are used to estimate losses #
    # and gains of water in the wetland.                    #
    #########################################################
    
    # the total water volume for the next day
    nd.vol = rep(0, WL.len)
      
    # The total water level for the next day
    nd.WL = rep(0, WL.len)
    
    # Water level at each time step
    s.WL = rep(0, WL.len)
    
    # Total volume of water flowing out at each time step
    outflow = rep(0, WL.len)
    
    # the dilution rate at each time step
    delta = rep(0, WL.len)
    
    # volume of water in the wetland at each time step
    peak.vol = rep(0, WL.len)
    
    # Peak water level in the wetland at each time step
    peak.WL = rep(0, WL.len)
    
    # Potential Evapotation each day off the surface water of the wetland
    EV = rep(0, WL.len)
    PEV = rep(0, WL.len)
    
    # Evapotransporation each day by Typha each day out of the wetland
    ET = rep(0, WL.len)
    
    # Precipitation (includes rain and snow) that falls on the wetland
    # every day.
    m.prcp = rep(0, WL.len)
    
    # Snow accumuluation on the wetland, this will be in melted
    # equivalent rainfall (mm)
    S.accu = rep(0, (WL.len+1))
    
    # The amount of water back flowing into the wetland from the 
    # creek/St. Lawrence River
    w.influx = rep(0, WL.len)
    
    # the total amount of water that has flowed out of the wetland
    # over the course of the entire simulation
    w.outflux = rep(0, WL.len)
    
    # The total amount of water that has flowed out of the wetland
    # the time step before the current
    iw.outflux = 0
    
    # A vector used for debugging.
    logic.flow = rep(F, WL.len)
    
    # Track the plank/flash board height
    p.H = rep(0, WL.len)
    
    ########################################################################
    # Variables to model the nutrient concentration in the wetland #
    ########################################################################
    # The following code sets up vectors to log concentrations of nutrients
    # or amounts of water depending on the time step.
    ########################################################################
    # Phosphorus Variables #
    ########################################################################
    P.conc = rep(0, WL.len) # Phosphorus concentration in Kg/L
    P.tot = rep(0, WL.len) # Total Phosphorus in wetland
    i.P.conc = 0 # initial P concentration for each time step in Kg/L
    i.P.tot = 0 # initial total P in Kg for each time step
    # total mass of P back flowing into the wetland from the creek
    P.influx = rep(0, WL.len)
    # Phosphorus molarity of surface waters in wetland
    P.molarity = rep(0, WL.len)
    # total amount of P that has been exported by the system
    P.outflow = rep(0, WL.len)
    # The next day's total P after an out flow event
    nd.P.tot = rep(0, WL.len)
    # The initial total amount of phosphorus in a wetland
    iP.tot.exp = 0
    # The total amount of the exported P (P and water that has
    # accummulated over the course of the simulation) 
    P.tot.exp = rep(0, WL.len)
    # The concentration of the exported P (P and water that has
    # accummulated over the course of the simulation) 
    P.exp.conc = rep(0, WL.len)
    # the next day's P concentration after an outflow event
    nd.P.conc = rep(0, WL.len)
    # The influx of P into the wetland (total P)
    P.influx = rep(0, WL.len)
    ###########################################################
    # Nitrogen Variables #
    ###########################################################
    N.conc = rep(0, WL.len) # Nitrogen concentration in Kg/L
    N.tot = rep(0, WL.len) # Total Nitrogen in wetland
    i.N.conc = 0 # initial N concentration for each time step in Kg/L
    i.N.tot = 0 # initial total N in Kg for each time step
    # total mass of N back flowing into the wetland from the creek
    N.influx = rep(0, WL.len) 
    # Nitrogen molarity of surface waters in wetland
    N.molarity = rep(0, WL.len)
    # total amount of N that has been exported by the system
    N.outflow = rep(0, WL.len)
    # The next day's total N after an out flow event
    nd.N.tot = rep(0, WL.len)
    # The initial total amount of nitrogen in a wetland
    iN.tot.exp = 0
    # The total amount of the exported N (N and water that has
    # accummulated over the course of the simulation) 
    N.tot.exp = rep(0, WL.len)
    # The concentration of the exported N (N and water that has
    # accummulated over the course of the simulation) 
    N.exp.conc = rep(0, WL.len)
    # the next day's N concentration after an outflow event
    nd.N.conc = rep(0, WL.len)
    # The influx of N into the wetland (total P)
    N.influx = rep(0, WL.len)
    # The concentration of input nutrients
    P.GWLF.conc = rep(0, WL.len)
    N.GWLF.conc = rep(0, WL.len)
    ################################################################
    # Parameters for LM ABay vs. Site
    ################################################################
    M = lm.parms$DB[1]
    B = lm.parms$DB[2]
    # Capital "M" is the slope in the linear model for each wetland 
    # without a water control structure, used to calculate the water 
    # level based on A Bay water level. This is not to be confused 
    # with lower case "m" which stands for month, and is used in 
    # determining the crop coefficient.
    ################################################################
    # find the next day's water level using a LR.
    # If the site is not PT. This is because
    # PT follows the water level dynamics of the river
    ################################################################
    if(site.abv != "PT"){
      nd.WL = round(M*River.WL+B, 3)
    } else if (site.abv == "PT") {
      nd.WL = round(River.WL, 3)
    }
    
    # At Dorr Road the water level does not go below 73.38
    DR.WL = 74
    
    # Initial Conditions
    # This calculates the initial volume for the wetlands given a 
    # particular starting/initial water level.
    if(is.WCS){
      s.WL[1] = i.WL
      p.H[1] = i.PH
      i.vol = WLvsVol$vol[which(i.WL == WLvsVol$WLs)]
      site.which.plan = which(names(Flash.Boards.plan) == Site.name)
      PH.MP = Flash.Boards.plan[,site.which.plan]/100
    } else {
      # Wetlands without WCS have starting water levels based on
      # the starting water level in ABay. (is this really necessary?).
      i.WL = round(M*River.WL[1]+B, 3)
      i.vol = WLvsVol$vol[which(i.WL == WLvsVol$WLs)]
      s.WL[1] = i.WL
    }
    # Other Parameters
    n.days = WL.len
    
    # Initialize the initial total nutrients (N and P) for the wetland
    i.P.conc = i.P
    i.N.conc = i.N
    i.P.tot = i.P.conc*i.vol
    i.N.tot = i.N.conc*i.vol
    
    for(d in 1:n.days){
      # Grab Just the months from the date class
      m = as.character(month(Dates[d]))
      
      # Find the mean day length for the given month
      R_a.m = R_a[as.numeric(m)]
      
      # Find the appropriate K_c value for the season
      if(m == "12" || m == "1" || m == "2")K_c.s = K_c[1]
      if(m == "3" || m == "4" || m == "5")K_c.s = K_c[2]
      if(m == "6" || m == "7" || m == "8")K_c.s = K_c[3]
      if(m == "9" || m == "10" || m == "11")K_c.s = K_c[4]
      
      # If the temperature is below zero degrees C assume prcp is 
      # frozen and sits on top of the wetland. 
      if(Tmean[d] > 0){
        
        # total water volume input from snow melt and precipitation
        m.prcp[d] = (SMC*Tmean[d]*S.accu[d] + w.prcp[d]*tot.A)
        
        # subtract the melted snow from the total accumulated snow
        S.accu[d+1] = S.accu[d] - SMC*Tmean[d]*S.accu[d]
         
        # Make sure that if the accumulated snow goes below zero
        # (negative) accumulated snow goes back to zero
        if(S.accu[d] < 0) S.accu[d] = 0
       
      } else { # if the temperature is below zero prcp falls as snow
               # and doesn't melt. Accumulate snow if there is snow 
               # present already
        
        S.accu[d+1] = w.prcp[d]*tot.A + S.accu[d]
         
      }
      # Calculate the potential Evaporation
      # if the mean of Tmin and Tmax is less than 0
      # there is no evaporation or evapotranspiration
      if(Tmean[d] <= 0){ 
        
        PEV[d] = 0
        
      } else {
        
        # Potential evaporation using Hargreaves equation
        PEV[d] = 0.0023*(Tmean[d]+17.8)*(D.T[d]^0.5)*R_a.m
        
      }
      # Calculate the potential Evapotranspiration
      ET[d] = PEV[d]*K_c.s
      
      # Convert EV and ET from mm/day/hectares to m^3/day
      # Make the conversion from mm per day per hectare to meters^3
      # then multiply by area of surface Water
      # multiply by area of typha
      EV[d] = PEV[d]*mm.to.m*AOW
      ET[d] = ET[d]*mm.to.m*AT
      
      # Calculate what the "peak volume" in 
      # the wetland would be for the Current day.
      # add in any snow melt and precipitation
      # subtract and evapotranspiration or evaporation
      peak.vol[d] = i.vol + Daily.inflow[d] + m.prcp[d] - ET[d] - PEV[d]
    
      if (site.abv == "DR"){
        # Determine what the volume of water is in the wetland at DR
        test.WL = WLvsVol[which.min(peak.vol[d] == WLvsVol$vol),1]
        
        # The level can't go below 73.38
        if(test.WL > DR.WL){
          peak.vol[d] = WLvsVol[which(DR.WL == WLvsVol$WL),2]
        }
      }
      
      # Calculate the total amount of nutrients in the wetland before
      # the "outflow event".
      P.tot[d] = TP.kg[d] + i.P.tot
      N.tot[d] = TN.kg[d] + i.N.tot
      
      # Calculate the peak nutrient concentration in the wetland
      P.conc[d] = P.tot[d]/peak.vol[d]
      N.conc[d] = N.tot[d]/peak.vol[d]
      
      # GWLF nutrient concentrations using Daily inflow
      ######################################################################
      P.GWLF.conc[d] = TP.kg[d]/Daily.inflow[d]
      N.GWLF.conc[d] = TN.kg[d]/Daily.inflow[d]
      
      # make sure the model doesn't calculate negative volume
      # Additionally if there is no water assume that there are no nutrients
      # in the wetland surface waters any more. 
      if(peak.vol[d] < 0){ 
        
        peak.vol[d] = 0
        P.conc[d] = 0
        N.conc[d] = 0
        P.GWLF.conc[d] = 0
        N.GWLF.conc[d] = 0
        
      }
    
      # for the Scenario that the wetland has a water control structure
      if (is.WCS){
        
        # vector value that matches peak volume
        # with the WL in the wetland
        pWL.vec = which.min(peak.vol[d] >= WLvsVol$vol)
        
        # find the peak Water level using the vector value.
        peak.WL[d] = WLvsVol$WLs[pWL.vec]
        
        # add or remove flash boards depending on the time of year
        # uses a file that is an implimentation of TIBS' management plan
        # for flash boards. Some years are different based on data 
        # collected by levelloggers
        p.H[d] = i.PH + PH.MP[d]
        
        i.PH = p.H[d]
        
        # Find the head height of the water
        # Convert to feet from meters
        H = (peak.WL[d] - p.H[d])*3.28
        
        # if the head height is zero or less there is no outflow
        if(H <= 0){
    
          outflow[d] = 0
          
          ###################################################
          # Calculate new total nutrient amounts of N and P #
          ###################################################
          # Since the outflow is zero there is no loss of P or N
          # set the new initial total nutrients
          P.tot[d] = i.P.tot
          N.tot[d] = i.N.tot
          
          # new initial total nutrient and initial nutrient 
          # concentrations don't change so there isn't a need 
          # to change the variables i.P.conc, i.N.conc
          
          # Set new initial volume (same as before since there is no outflow)
          i.vol = peak.vol[d]
          
          # Set next day Water level
          nd.vol[d] = peak.vol[d]
          
          # Set new initial WL (same as before since there is no outflow)
          nd.WL[d] = peak.WL[d]
            
        } else {
            
          # Calculate the outflow in cubic feet per day. The equation is in
          # cubic feet per second, the 86400 are the number of seconds in 
          # a day Convert to meters cubed (0.0283168466 ft^3 = 1m^3)
          pot.flow = ((3.33*(L - 0.2*H)*H^(3/2))*86400)*0.0283168466
    
          # calculate the minimum volume allowed in the wetland beacuse of 
          # the planks.
          vol.min = WLvsVol$vol[which(p.H[d] == WLvsVol$WLs)]
          
          # calculate max amount of water that can leave the system
          out.max = peak.vol[d] - vol.min
          
          # If max amount of water that can leave the system is greater than
          # total potential outflow. 
          if(out.max > pot.flow){
            
            logic.flow[d] = T 
            # In the case that there is more water in the wetland
            # that can flow out at the rate of pot.flow
            outflow[d] = pot.flow
            
            # Calculate the outflow rate for this time step
            delta[d] = outflow[d]/peak.vol[d]
            
            # calculate the next day's water volume by subtracting
            # out the the total that can flow out.
            nd.vol[d] = vol.min + (out.max-pot.flow)
            
            ###################################################
            # Calculate new total nutrient amounts of N and P #
            ###################################################
            # Calculate the total amount of nutrients that have left
            # the wetland as a result of the outflow
            nd.P.tot[d] = i.P.tot - outflow[d]*P.conc[d]
            nd.N.tot[d] = i.N.tot - outflow[d]*N.conc[d]
            
            # calculate the next day's P concentration
            nd.P.conc[d] = nd.P.tot[d]/nd.vol[d]
            nd.N.conc[d] = nd.N.tot[d]/nd.vol[d]
          
            # Calculate the new water volume after all potential water
            # has flowed out.
            i.vol = nd.vol[d]
            
          } else {
          
            # In the case that more water can flow out of the weir
            # in a day than is allowed or pot.flow > out.max
            outflow[d] = out.max
            
            # calculate the dilution rate (proportion flowing out)
            delta[d] = outflow[d]/peak.vol[d]
            
            # Set the "next day" water volume
            nd.vol[d] = vol.min
            
            ###################################################
            # Calculate new total nutrient amounts of N and P #
            ###################################################
            # Calculate the total amount of nutrients that have left
            # the wetland as a result of the outflow
            nd.P.tot[d] = i.P.tot - outflow[d]*P.conc[d]
            nd.N.tot[d] = i.N.tot - outflow[d]*N.conc[d]
            
            # calculate the next day's P concentration
            nd.P.conc[d] = nd.P.tot[d]/nd.vol[d]
            nd.N.conc[d] = nd.N.tot[d]/nd.vol[d]
            
            # set the new initial water volume for the wetland
            i.vol = nd.vol[d]
            
          }
          # set the next day's water volume
          nd.WL[d] = WLvsVol$WLs[which.min(i.vol >= WLvsVol$vol)]
                                  
        } 
               #####################################
      } else { # For the case when there is no WCS #
               #####################################
        
        # vector value that matches peak volume 
        # with the WL in the wetland
        pWL.vec = which.min(peak.vol[d] >= WLvsVol$vol)
        
        # find the peak Water level using the vector value.
        peak.WL[d] = WLvsVol$WLs[pWL.vec]
        
        # PT is essentially part of the St. Lawrence River
        # so the WL at PT is the same as ABay.
        if (site.abv == "PT"){
          nd.WL[d] = round(River.WL[d], 3)
        }
        
        # Calculate the next day's water level
        if(d != n.days){
          nd.WL[d] = round(M*River.WL[d+1]+B, 3)
        } else {
          # For now. 
          nd.WL[d] = River.WL[1]
        }
        
        # nd.WL at Dorr Road doesn't go below 74.38
        if (site.abv == "DR" && nd.WL[d] <= 73.38){
           nd.WL[d] = 73.38
        }  
        
        # find the next day's wetland water volume 
        nd.vol[d] = WLvsVol$vol[which(nd.WL[d] == WLvsVol$WLs)]
        
        # calculate the outflow, the total amount of water that 
        # has flowed out of the wetland
        outflow[d] = peak.vol[d] - nd.vol[d]
        
        # calculate the dilution rate. Need to also make sure
        # we're not dividing by zero.
        if (peak.vol[d] == 0){
          delta[d] = 1
        } else {
          delta[d] = outflow[d]/peak.vol[d]
        }
        ###################################################
        # Calculate new total nutrient amounts of N and P #
        ###################################################
        
        if(delta[d] > 0){
          
          # Calculate the total amount of water that has flowed out
          w.outflux[d] = outflow[d] + iw.outflux
          
          # The total amount of nutrient that has been exported
          # out of the system at time step "d"
          P.outflow[d] = outflow[d] * P.conc[d]
          N.outflow[d] = outflow[d] * N.conc[d]
          
          # Calculate the amount of nutrient P left in the system
          # at time "d+1" or the next days total nutrient.
          nd.P.tot[d] = P.tot[d] - P.outflow[d]
          nd.N.tot[d] = N.tot[d] - N.outflow[d]
          
          # calculate the total amount of nutrient that has been
          # exported from the wetland.
          P.tot.exp[d] = iP.tot.exp + P.outflow[d]
          N.tot.exp[d] = iN.tot.exp + N.outflow[d]
          
          # set a new initial total amount of P exported from the
          # system.
          iP.tot.exp = P.tot.exp[d]
          iN.tot.exp = N.tot.exp[d]
          
          # calculate the concentration of total nutrients exported
          # from the wetland
          if (w.outflux[d] == 0){
            P.exp.conc[d] = 0
            N.exp.conc[d] = 0
          } else {
            P.exp.conc[d] = P.tot.exp[d]/w.outflux[d]
            N.exp.conc[d] = N.tot.exp[d]/w.outflux[d]
          }
          # set the new initial total nutrient amount so that 
          # calculations can be made for the next time step.
          i.P.tot = nd.P.tot[d]
          i.N.tot = nd.N.tot[d]
          
          # set a new initial total amount of water outflow value
          iw.outflux = w.outflux[d]
          
        } else { # When delta is negative assume there is 
                 # a back flow back into the wetland from
                 # the main creek
          
          # Calculate the total amount of water that flows back
          # into the wetland from the creek
          # (This should be outflow)
          w.influx[d] = nd.vol[d] - peak.vol[d]
          
          # Calculate the total amount of nutrients that are
          # flowing back in from the creek into the wetland
          if ((d-1) == 0) {
            # for   cases when the first day there is a negative outflow
            P.influx[d] = 0
            N.influx[d] = 0
          } else {
            # calculate the influx of total nutrients (N and P)
            # multiply the nutrient concentration by total volume
            # of water that has flowed back into the wetland
            P.influx[d] = w.influx[d]*P.exp.conc[d]
            N.influx[d] = w.influx[d]*N.exp.conc[d]
          }
          
          # Add the imported about of nutrients from the back flow
          # to the total nutrient amount for this time step.
          nd.P.tot[d] = P.influx[d] + i.P.tot
          nd.N.tot[d] = N.influx[d] + i.N.tot
          
          # calculate the nutrient concentration after an export event
          nd.P.conc[d] = nd.P.tot[d]/nd.vol[d]
          nd.N.conc[d] = nd.N.tot[d]/nd.vol[d]
          
          # set the new initial total nutrient amount so that 
          # calculations can be made for the next time step.
          i.P.tot = nd.P.tot[d]
          i.N.tot = nd.N.tot[d]
        
        }
        
        # set the next day's "starting" water level
        s.WL[d+1] = nd.WL[d]
        
        # set a new initial volume
        i.vol = nd.vol[d] 
      }
    }

##########################################################################        
    
    ###################################################################
    # Round all output to 3 decimal places #
    ###################################################################
    
    # calculate concentrations of nutrients in micromoles per liter
    P.conc = P.conc/m3.to.L*kg.to.g/P.MM*M.to.uM
    N.conc = N.conc/m3.to.L*kg.to.g/P.MM*M.to.uM
    
    # Calculate the average water level
    avg.WL = (peak.WL+nd.WL)/2
    
    # GWLF nutrient concentrations using Peak volume
    P.GWLF.conct = P.GWLF.conc*kg.to.g/m3.to.L/P.MM*M.to.uM
    N.GWLF.conct = N.GWLF.conc*kg.to.g/m3.to.L/P.MM*M.to.uM
    
    # Round Peak volume to three decimal places
    peak.vol = round(peak.vol, digits = 3)
    
    # Calculate the total inflow (precipitation, snow melt, and back flow
    back.vec = which(outflow < 0)
    tot.inflow = m.prcp + Daily.inflow
    tot.inflow[back.vec] = abs(outflow[back.vec]) + tot.inflow[back.vec]
    # Find all the backflow values
    backflow = rep(0, length(tot.inflow))
    backflow[which(outflow < 0)] = abs(outflow[which(outflow < 0)])
    ####################################################################
    # Set a new work directory to write .csv files and figures #
    ####################################################################
    model.file = paste(site.abv, " ", 
                       Start.Year, "-", End.Year, 
                       " model output.csv", sep = "")
    sites.wd = paste(main.dir ,"/Wetland Sites/", 
                     Site.name, " TIBS", sep = "")
    ####################################################################
    # Export Model results to the work directory #
    ####################################################################
    # Put all model results into a data frame
    
    P.GWLF.conc[which(is.na(P.GWLF.conc))] = NA
    P.GWLF.conc[which(P.GWLF.conc == 0)] = NA
    
    N.GWLF.conc[which(is.na(N.GWLF.conc))] = NA
    P.GWLF.conc[which(P.GWLF.conc == 0)] = NA
    
    
    new.table = data.frame(Dates, delta, outflow, peak.vol, nd.vol, ET, EV,
                           peak.WL, nd.WL, P.conc, P.tot, TP.kg, P.GWLF.conct,
                           N.conc, N.tot, TN.kg, N.GWLF.conct, avg.WL,
                           Daily.inflow, m.prcp, tot.inflow, backflow)
    
    
    # Remove the last day of the simulation
    new.table = new.table[-length(new.table$Dates),]
    
    # Round all the data to three digits
    for(cols in 2:length(new.table[1,])){
      
      new.table[,cols] = round(new.table[,cols], digits = 3)
      
    }
    # set the work the main site directory
    setwd(sites.wd)
    # Write the model model data to a .csv file
    write.table(new.table, file = model.file, sep = ",", row.names = F)
    ####################################################################
    # Plots to see the preliminary model results #
    ####################################################################
    # Create a .png file to create a figure of outflow
    # Create a new folder to house the new figures
    if(site.abv == "DR"){
      out.max = 2500
      out.min = -1000
    } else if (site.abv == "BF") {
      out.max = 10000  
      out.min = -2000
    } else if (site.abv == "CB") {
      out.max = 8500
      out.min = -2250
    } else if (site.abv == "DL") {
      out.max = 15000  
      out.min = -2000
    } else if (site.abv == "PT") {
      out.max = 6500  
      out.min = -6000
    } else if (site.abv == "DB") {
      out.max = 35000  
      out.min = -15000
    }

    # Create a new folder name (Site name S.Year - E.Year Raw Figures)
    site.folder.raw = paste(site.abv, "Raw Figures")
    # Check to see if the folder exists already
    check.folder = list.files(pattern = site.folder.raw)
    if(length(check.folder) == 0) {
      # Create the new raw data figures directory if it doesn't already exist
      dir.create(site.folder.raw)
    }
    
    # Set the newly created raw data figures folder as the new
    # working directory
    setwd(paste(sites.wd, "/", site.folder.raw, sep = ""))
    
    # is the time period before or after the building of the MSD?
    if(End.Year > 1955){
      Pre.Pos.MSD = "Post MSD"
    } else {
      Pre.Pos.MSD = "Pre MSD"  
    }
    
    # is the time period during high or low supply?
    if(End.Year == 1942|| End.Year == 1982){
      LH.Sup = "Low Prcp"
    } else if(End.Year == 2014 || End.Year == 1951){
      LH.Sup = "High Prcp"
    }  
    # PNG parameters
    mag = 1.5
    p.width = 1300
    p.length = 700
    point.types = 16
    # Create a jpg to depict the daily outflow 
    png(filename = paste(Site.name, " ", 
                         Start.Year, "-", End.Year,
                         " outflow Raw",".jpg", sep = ""),
        width = p.width, height = p.length)
    # Make the plot
    y.lab = expression("Outflow ( m"^3~" Day"^-1~")")
    plot(Dates, outflow, ylab = "", type = "l", ylim = c(out.min, out.max),
         main = paste(Site.name, " Raw Outflow Model ", 
                      Start.Year, "-", End.Year, sep = ""))
    mtext(side = 2.75, line = 2.25, text = y.lab, cex = mag)
    # Create a Zero line to show zero outflow (for visualizing negative outflow
    # or backflow)
    abline(0, 0)
    dev.off()
    graphics.off()
    
    # Create a jpg file to create a figure of raw daily dilution rate
    png(filename = paste(Site.name, ": ", Pre.Pos.MSD, " ", LH.Sup, 
                         " ", Start.Year, "-", End.Year,
                         " Delta Raw",".jpg", sep = ""),
        width = p.width, height = p.length)
    y.lab = expression("Delta (day"^-1~")")
    plot(Dates, delta, type = "l", ylab = "",
         main = paste(Site.name, ": ", Pre.Pos.MSD, " ", LH.Sup, 
                            " ", Start.Year, "-", End.Year,
                            " Delta Raw", sep = ""))
    mtext(side = 2.75, line = 2.25, text = y.lab, cex = mag)
    
    abline(0, 0, col = 2)
    dev.off()
    graphics.off()
    
    # Create a jpg file to create a figure of phosphorus concentration 
    png(filename = paste(Site.name, ": ", Pre.Pos.MSD, " ", LH.Sup, 
                         " ", Start.Year, "-", End.Year,
                         " Phosphorus Conc. Raw",".jpg", sep = ""),
        width = p.width, height = p.length)
    y.lab = expression("[P] ("~mu~"Moles L"^-1~")")
    plot(Dates, P.conc, ylab = "", 
         type = "l", main = paste(Site.name, ": ", Pre.Pos.MSD, " ", LH.Sup, 
                                  " ", Start.Year, "-", End.Year,
                                  " [P]", sep = ""))
    mtext(side = 2.75, line = 2.25, text = y.lab, cex = mag)
    
    dev.off()
    graphics.off()
    
    # Create a .png file to create a figure of nitrogen concentration 
    png(filename = paste(Site.name, ": ", Pre.Pos.MSD, " ", LH.Sup, 
                         " ", Start.Year, "-", End.Year,
                         " Nitrogen Conc. Raw",".jpg", sep = ""),
        width = p.width, height = p.length)
    y.lab = expression("[N] ("~mu~"Moles L"^-1~")")
    plot(Dates, N.conc, ylab = "", 
         type = "l", main = paste(Site.name, ": ", Pre.Pos.MSD, " ", LH.Sup, 
                                  " ", Start.Year, "-", End.Year,
                                  " [N]", sep = ""))
    mtext(side = 2.75, line = 2.25, text = y.lab, cex = mag)
    dev.off()
    graphics.off()
    ###############################################################
    # Create 1:1 plots of wetland model run vs. GWLF model Data #
    # of Nitrogen and Phosphorus                                 #
    ###############################################################
    # Create a 1:1 line for visual reference
    x = 0:1000
    y = x
    # Water level vs. Backflow Outflow #
    plot.WL = peak.WL
    max.Flow = max(outflow[1:(length(outflow)-1)])
    min.Flow = min(outflow[1:(length(outflow)-1)])
    max.WL = max(plot.WL[1:(length(outflow)-1)])
    min.WL = min(plot.WL[1:(length(outflow)-1)])
    png(filename = paste(Site.name, ": ", Pre.Pos.MSD, " ", LH.Sup, 
                         " ", Start.Year, "-", End.Year,
                         " Waterlevel vs Water Flow",".jpg", sep = "")
        ,width = 800, height = 800)
    par(cex = mag, mar = c(5, 4, 2, 1))
    y.lab = expression("Flow (m"^3~"Day"^-1~")")
    plot(plot.WL[1:(length(outflow)-1)], outflow[1:(length(outflow)-1)],
         xlab = "Waterlevel (mIGLD)",
         ylab = "",
         ylim=c(min.Flow, max.Flow), xlim = c(min.WL, max.WL))
    mtext(side = 2.75, line = 2.25, text = y.lab, cex = mag)
    dev.off()
    graphics.off()
  }
  
  ######################################################################
  # Create summary figures and tables for the model runs and write #
  # them to the  appropriate folders.                              #
  ######################################################################
  graphics.off();
  
  # Average Months for the model simulated dilution rates and outflows
  
  Year.S = Start.Year
  Year.E = End.Year
  
  sites.TIBS = c("Carpenters", "Deferno", "Delaney", "Plum Tree", 
                 "Butterfield", "DorrRoad")
  
  setwd(main.dir)
  
  Cl = rep(0,12)
  
  num.days.table = data.frame(Cl, Cl, Cl, 
                              Cl, Cl, Cl)
  colnames(num.days.table) = c("CB", "DB", "DL", 
                                "PT", "BF", "DR")
  
  for (sw in S.s:S.l) {
    
    graphics.off();
    
    Site.name = sites.TIBS[sw]
    
    Wetland.Sites = c(c("Carpenters", "CB"), c("Deferno", "DB"), 
                      c("Delaney", "DL"), c("Plum Tree", "PT"),
                      c("Butterfield", "BF"), c("DorrRoad", "DR"))
    
    site.wd = paste(main.dir, "/Wetland Sites/", 
                    Site.name, " TIBS", sep = "")
    setwd(site.wd)
    
    if(Site.name == "Deferno" 
       || Site.name == "DorrRoad" 
       || Site.name == "Plum Tree"){
      WCS = F
    } else {
      WCS = T
    }
    site.pos = which(Wetland.Sites == Site.name)
    site = Wetland.Sites[site.pos + 1]
    
    file.name = list.files(pattern = paste(Year.S, "-", Year.E, 
                                           " model output", sep = ""))
    model.data = read.table(file.name, sep = ",", header = T)
    
    field.data.name = list.files(pattern = "Nutrient Field Data")
    
    FN.data = read.table(field.data.name, header = T, sep = ",")
    
    # reformat Dates of nutrient field data to POSIX
    FN.data$Date = as.POSIXlt(as.character(FN.data$Date), format = "%m/%d/%Y")
    
    model.data$Dates = as.Date(model.data$Dates)
    
    FN.m = as.numeric(format(FN.data$Date, "%m"))
    
    # Read in Raw field data
    setwd(paste(main.dir, "/Field Nutrient Data", sep = ""))
    FN.nameraw = paste(site, "Nutrient Field Data.csv")
    
    FN.raw = read.csv(FN.nameraw, header = T, sep = ",")
    setwd(site.wd)
    # Average Months
    Y = format(model.data$Dates, "%Y")
    M = as.numeric(format(model.data$Dates, "%m"))
    
    months = c("01", "02", "03", "04", "05", "06", 
               "07", "08", "09", "10", "11", "12")
    
    month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    num.m = rep(0, length(months))
    
    d.avg = num.m
    d.sd = num.m

    r.avg = num.m
    r.sd = num.m
    
    b.avg = num.m
    b.sd = num.m
        
    out.days = num.m
    in.days = num.m
    
    o.avg = num.m
    o.sd = num.m
    
    P.avg = num.m
    P.sd = num.m
    wsP.avg = num.m
    wsP.sd = num.m
    
    N.avg = num.m
    N.sd = num.m
    
    avg.outdays = num.m
    avg.indays = num.m
    sd.outdays = num.m
    sd.indays = num.m
    delta.data = model.data$delta
    outflow.data = model.data$outflow
    runoff.data = Daily.inflow
    GWLF.P.conc = P.GWLF.conct
    box.delta = matrix(NA, ncol = 12, nrow = 310)
    box.outflow = matrix(NA, ncol = 12, nrow = 310)
    box.runoff = matrix(NA, ncol = 12, nrow = 310)
    box.back = matrix(NA, ncol = 12, nrow = 310)
    box.P = matrix(NA, ncol = 12, nrow = 310)
    
    # Calculate the mean flow, dilution rate, runoff, etc.
    for(ms in 1:12){
      
      m.vec = which(as.numeric(months[ms]) == M)
      
      m.flow.data = model.data$outflow[m.vec]
      m.back.data = model.data$backflow[m.vec]
      m.delta.data = model.data$delta[m.vec]
      m.runoff.data = runoff.data[m.vec]
      m.P.data = model.data$P.GWLF.conct[m.vec]
      
      d.avg[ms] = mean(model.data$delta[m.vec], na.rm = T)
      d.sd[ms] = sd(model.data$delta[m.vec], na.rm = T)
      
      b.avg[ms] = mean(model.data$backflow[m.vec], na.rm = T)
      b.sd[ms] = sd(model.data$backflow[m.vec], na.rm = T)
      
      o.avg[ms] = mean(m.flow.data, na.rm = T)
      o.sd[ms] = sd(m.flow.data, na.rm = T)
      
      r.avg[ms] = mean(Daily.inflow[m.vec], na.rm = T)
      r.sd[ms] = sd(Daily.inflow[m.vec], na.rm = T)
      
      
      P.avg[ms] = mean(model.data$P.conc[m.vec], na.rm = T)
      P.sd[ms] = sd(model.data$P.conc[m.vec], na.rm = T)
      wsP.avg[ms] = mean(GWLF.P.conc[m.vec], na.rm = T)
      wsP.sd[ms] = sd(GWLF.P.conc[m.vec], na.rm = T)
      
      N.avg[ms] = mean(model.data$N.conc[m.vec], na.rm = T)
      N.sd[ms] = sd(model.data$N.conc[m.vec], na.rm = T)
      
      in.vec = which(m.flow.data <= 0)
      out.vec = which(m.flow.data > 0)
      
      out.days[ms] = length(m.flow.data[out.vec])
      in.days[ms] = length(m.flow.data[in.vec])
      
      # Seperate loop to calculate mean days per month outflow/backflow
      uni.y = unique(Y)
      holder.data.in = rep(0, length(uni.y))
      holder.data.out = rep(0, length(uni.y))
      
      # get all outflow data and put into a matrix for box plots
      box.outflow[1:length(m.vec), ms] = m.flow.data
      
      # get all outflow data and put into a matrix for box plots
      box.delta[1:length(m.vec), ms] = m.delta.data
      
      # get all runoff data and put into a matrix for box plots
      box.runoff[1:length(m.vec), ms] = m.runoff.data
      
      # get all runoff data and put into a matrix for box plots
      box.back[1:length(m.vec), ms] = m.back.data
      
      # get all Phosphorus and put in a matrix for box plots
      box.P[1:length(m.vec), ms] = m.P.data
      
      for(y in 1:length(uni.y)){
        # gather the data for a particular year
        month.data = model.data[which(as.numeric(M) == ms),]
        
        Y.compare = format(month.data$Dates, "%Y")
        
        flow.MY = month.data$outflow[which(Y.compare == uni.y[y])]
        
        holder.data.in[y] = length(which(flow.MY <= 0))
        holder.data.out[y] = length(which(flow.MY > 0))
        
      }
      
      avg.indays[ms] = mean(holder.data.in, na.rm = T)
      avg.outdays[ms] = mean(holder.data.out, na.rm = T)
      sd.indays[ms] = sd(holder.data.in, na.rm = T)
      sd.outdays[ms] = sd(holder.data.out, na.rm = T)
      
    }
    
    if (WCS == T) {
      title = paste(site, ": ","Site with WCS, ",
                    Pre.Pos.MSD, ", ", LH.Sup, " ",
                    Start.Year, "-", End.Year, sep = "")
    } else {
      title = paste(site, ": ", "Site without WCS, ",
                    Pre.Pos.MSD, " ", LH.Sup, " ", 
                    Start.Year, "-", End.Year, sep = "")
    }
    
    # Write the monthly means to a new .csv file
    month.table = data.frame(month.names, d.avg, d.sd, o.avg, o.sd)
    month.table.name = paste(site, ": ", Pre.Pos.MSD, " ", LH.Sup, 
                             " ", Start.Year, "-", End.Year, 
                             "-Monthly Avgs", ".csv", sep = "")
    
    write.table(month.table, file = month.table.name, sep = ",", row.names = F)
    
    # Set work directory to the folder created by the model to house new figures
    # setwd(paste(site, " ", Year.S,"-", Year.E, " Figures", sep = ""))
    
    par.default = par()
    
    # set the max and min for dilution rate and outflow
    if(site == "DR"){
      out.max = 5000
      out.min = -5000
      del.max = 0.15
      del.min = -.15
      run.max = 5000
      run.min = 0
    } else if (site == "BF") {
      out.max = 8000  
      out.min = 0
      del.max = 0.15
      del.min = 0
      run.max = 7000
      run.min = 0
    } else if (site == "CB") {
      out.max = 15000
      out.min = 0
      del.max = 0.3
      del.min = 0
      run.max = 12000
      run.min = 0
    } else if (site == "DL") {
      out.max = 25000  
      out.min = 0
      del.max = 0.15
      del.min = 0
      run.max = 15000
      run.min = 0
    } else if (site == "PT") {
      out.max = 19000
      out.min = -19000
      del.max = 0.1
      del.min = -0.1
      run.max = 9000
      run.min = 0
    } else if (site == "DB") {
      out.max = 40000  
      out.min = -30000
      del.max = 0.15
      del.min = -0.15
      run.max = 15000
      run.min = 0
    }
    
    ######################################
    # Plot monthly mean of dilution Rate #
    ######################################
    # Create specific folder for housing the 
    # mean daily dilution rate for each month
    setwd(site.wd)
    
    Box.data = box.delta
    y.min = del.min
    y.max = del.max
    means = d.avg
    sds = d.sd
    y.lab = expression("Dilution Rate (Day"^-1~")")
    file.name = paste(site, " Dilution Rate ",
                      Pre.Pos.MSD, " ", LH.Sup,
                      " ", Start.Year, "-", End.Year,
                      ".jpg", sep = "")
    
    folder.name = paste(site, "Monthly Dilution rate")
    model.boxplots(Box.data, y.min, y.max, y.lab,
                   means, sds,
                   folder.name, file.name, year.int)
    
    ###################################################
    # Set the WD to the site's main working directory
    setwd(site.wd)
    #########################################################################
    # plot nutrients #
    #########################################################################
    
    # Nitrogen
    #########################################################################
    # Create a folder to house the monthly nutrient data
    setwd(site.wd)
    folder.nut.name = paste(site, "Monthly Mean Nitrogen Nutrients")
    y.lab = expression("[N]"~mu~"Moles Liter"^-1)
    Nut.avg = N.avg
    Nut.sd = N.sd
    FieldN.Data = FN.data$TDN..micro.mols.L.
    FieldN.Data.m = FN.m
    FieldN.raw.D = FN.raw$TDN
    FieldN.raw.M = FN.raw$Month
    y.min = 0
    y.max = max(c(Nut.avg+Nut.sd, FieldN.Data, FieldN.raw.D), na.rm = T)
    file.name = paste(site, " ",
                      Year.S, "-", Year.E, 
                      " Nitrogen Concentration",
                      ".jpg", sep = "")
    
    Nutrient.Month.plot(Nut.avg, Nut.sd, y.min, y.max, y.lab, 
                        FieldN.Data, FieldN.Data.m,
                        FieldN.raw.D, FieldN.raw.M,
                        folder.nut.name, file.name, mag)
    
    # Phosphorus
    ########################################################################
    # Create a new jpg of monthly mean nutrient concentration in the wetland
    setwd(site.wd)
    # Create a folder to house the monthly nutrient data
    folder.nut.name = paste(site, "Monthly Mean Phosphorus Nutrients")
    y.lab = expression("[P]"~mu~"Moles Liter"^-1)
    Nut.avg = P.avg
    Nut.sd = P.sd
    FieldN.Data = FN.data$TDP..micro.mols.L.
    FieldN.Data.m = FN.m
    FieldN.raw.D = FN.raw$TDP
    FieldN.raw.M = FN.raw$Month
    y.min = 0
    y.max = max(c(Nut.avg+Nut.sd, FieldN.Data, FieldN.raw.D), na.rm = T)
    file.name = paste(site, " ",
                      Year.S, "-", Year.E, 
                      " Phosphorus Concentration",
                      ".jpg", sep = "")
    
    Nutrient.Month.plot(Nut.avg, Nut.sd, y.min, y.max, y.lab, 
                        FieldN.Data, FieldN.Data.m,
                        FieldN.raw.D, FieldN.raw.M,
                        folder.nut.name, file.name, mag)
  
    # set the WD back to the original work directory 
    # for the particular wetland site
    setwd(site.wd)
    
    ################
    # plot outflow #
    ################
    # Create a WD for monthly daily mean outflow
    Box.data = box.outflow
    y.min = out.min
    y.max = out.max
    y.lab = expression("outflow (m"^{"3"}~"day"^-1~")")
    file.name = paste(site, " Outflow ",
                      Year.S, "-", Year.E,".jpg", sep = "")
    means = o.avg
    sds = o.sd
    
    folder.name = paste(site, "Monthly Outflow rate")
    model.boxplots(Box.data, y.min, y.max, y.lab, 
                   means, sds,
                   folder.name, file.name, year.int)
    
    setwd(site.wd)
    ################
    # plot Runoff #
    ################
    # Create a WD for monthly daily mean Runoff
    Box.data = box.runoff
    y.min = run.min
    y.max = run.max
    y.lab = expression("runoff (m"^{"3"}~"day"^-1~")")
    file.name = paste(site, " Runoff ",
                      Year.S, "-", Year.E,".jpg", sep = "")
    means = r.avg
    sds = r.sd
    
    folder.name = paste(site, "Monthly Runoff")
    model.boxplots(Box.data, y.min, y.max, y.lab, 
                   means, sds,
                   folder.name, file.name, year.int)
    setwd(site.wd)
    ##################
    # plot Backflows #
    ##################
    # Create a WD for monthly daily mean backflow
    Box.data = box.back
    y.min = run.min
    y.max = run.max
    file.name = paste(site, " backflow ",
                      Year.S, "-", Year.E,".jpg", sep = "")
    means = b.avg
    sds = b.sd
    y.lab = expression("Backflow (m"^3~"day"^-1~")")
    folder.name = paste(site, "Monthly Backflow")
    model.boxplots(Box.data, y.min, y.max, y.lab, 
                   means, sds,
                   folder.name, file.name, year.int)
    
    setwd(site.wd)
    ############################
    # plot Watershed Nutrients #
    ############################
    
    FieldN.Data = FN.data$TDP..micro.mols.L.
    FieldN.Data.m = FN.m
    FieldN.raw.D = FN.raw$TDP
    FieldN.raw.M = FN.raw$Month
    
    # Create a WD for monthly daily mean Watershed Nutrients
    folder.monthly.run = paste(site, "Monthly Mean Watershed Nutrients")
    check.folder = list.files(pattern = folder.monthly.run)
    
    if(length(check.folder) == 0){
      dir.create(folder.monthly.run)
    }
    
    y.max = max(wsP.avg+wsP.sd, na.rm = T)
    y.min = min(wsP.avg-wsP.sd, na.rm = T)
    setwd(paste(site.wd, "/", folder.monthly.run, sep = ""))
    png(filename = paste(site, " Watershed phosphorus ",
                         Year.S, "-", Year.E,".jpg", sep = ""),
        width = p.width, height = p.length)
    
    par(cex = mag, mar = c(5, 4, 2, 1))
    y.lab = expression("[P] "~mu~"Moles Liter"^-1)
    plot(wsP.avg, xlab = "", ylab = "", ylim = c(0, 5), 
            xaxt="n", type = "b", col = 2, lwd = 2)
    points(FieldN.Data.m, FieldN.Data)
    points(FieldN.raw.M, FieldN.raw.D)
    
    mtext(side = 2, line = 2, text = y.lab, cex = mag)
    mtext(side = 1, line = 2.75, text = "Month", cex = mag)
    
    arrows(c(1:12), wsP.avg-wsP.sd, c(1:12), wsP.avg+wsP.sd, 
           length=0.05, angle=90, code=3, col = 2, lwd = 2)
    axis(side = 1, at = 1:12, label = month.names)
    
    dev.off()
    graphics.off()
    
    ##############################################################
    
    # Reset the working directory
    setwd(site.wd)
    # Number of days each month that are outflow or backflow
    # Change legend label depending on if the sites has a WCS or not
    if(WCS){
      leg.lab = c("Q = 0")
    } else {
      leg.lab = "Backflow"
    }
    # Create a new directory for number of days outflow/backflow
    folder.numdays = paste(site, "Number Days outflow backflow")
    # Check to see if the directory has been created
    check.folder = list.files(pattern = folder.numdays)
    if(length(check.folder) == 0){
      dir.create(folder.numdays)
    }
    # Plot the number of days of backflow
    setwd(paste(site.wd, "/", folder.numdays, sep = ""))
    png(filename = paste(site, " number of days inflow backflow ",
                         Year.S, "-", Year.E,".jpg", sep = ""),
        width = p.width, height = p.length)
    par(cex = mag,  mar = c(5, 4, 2, 1))
    plot(out.days, type = "b", xlab = "months", ylab = "Number of Days",
         main = paste(site, " ", Pre.Pos.MSD, " ", LH.Sup, " ",
                      Year.S, "-", Year.E, 
                      "Number of Days Backflow Outflow", 
                      sep = ""),
         xaxt = "n", col = "1", ylim = c(0,310))
    lines(in.days, type = "b", col = 2)
    axis(side = 1, at = 1:12, label = month.names)
    legend("topleft", c("Outflow", leg.lab), 
           lty = c(1,1), col = c(1, 2))
    dev.off()
    graphics.off()
    # % Days backflow or Outflow
    per.out = out.days/(out.days+in.days)
    per.in = in.days/(out.days+in.days)
    png(filename = paste(site, " percent days Outflow backflow ",
                         Year.S, "-", Year.E,".jpg", sep = ""),
        width = p.width, height = p.length)
    plot(per.out, type = "b", ylim = c(0, 1), xaxt = "n", 
         ylab = "% Days", xlab = "months")
         # main = paste(site, " ", Pre.Pos.MSD, " ", LH.Sup, " ",
         #              "% Days of Backflow and Outflow", sep = ""))
    lines(per.in, type = "b", col = "2")
    axis(side = 1, at = 1:12, labels = month.names)
    legend("topleft", c("Outflow", leg.lab), 
           lty = c(1,1), col = c(1, 2))
    dev.off()
    graphics.off()

    # plot mean monthly outflow and back flow
    j.x1 = jitter(c(1:12))
    j.x2 = jitter(c(1:12))
    y.max = max(c(avg.indays+sd.indays, avg.outdays+sd.outdays))
    y.min = min(c(avg.indays-sd.indays, avg.outdays-sd.outdays))
    png(filename = paste(site, " Mean Number of Days Outflow Backflow ",
                         Year.S, "-", Year.E,".jpg", sep = ""),
        width = p.width, height = p.length)
    plot(j.x1, avg.indays, type = "b", ylim = c(y.min, 45), 
         ylab = "Number of Days", xlab = "months", xaxt = "n")
         # main = paste(site, " ", Pre.Pos.MSD, " ", LH.Sup, " ",
         #              " Number of days backflow or outflow", sep = ""))
    lines(j.x2, avg.outdays, type = "b", col = "2")
    arrows(j.x1, avg.indays-sd.indays, j.x1, avg.indays+sd.indays, 
           length=0.05, angle=90, code=3)
    arrows(j.x2, avg.outdays-sd.outdays, j.x2, avg.outdays+sd.outdays, 
           length=0.05, angle=90, code=3)
    axis(side = 1, at = 1:12, labels = month.names)
    legend("topleft", c("Outflow", leg.lab),
           lty = c(1,1), col = c(1,2))
    dev.off()
    graphics.off()
    setwd(site.wd)
    
    num.days.table[,sw] = per.out
  }
  setwd(main.dir)
  write.csv(num.days.table, file = paste("% days out ", Pre.Pos.MSD, " ", 
                                         LH.Sup, ".csv", sep = ""))
}

# Proportion days with outflow or backflow
########################################################################

setwd(main.dir)

per.files = list.files(pattern = "%")

site.abv = c("CB", "DB", "DL", "PT", "BF", "DR")

# tables for each site
for (ws in 1:6){
  
  t.table = matrix(0, 12, 4)
  rownames(t.table) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(t.table) = c("PostMSD_HP", "PostMSD_LP",
                        "PreMSD_HP", "PreMSD_LP")
  assign(site.abv[ws], t.table)
  
}

# Compile tables by wetland site
for(sc in 1:4){
  
  per.days.table = read.csv(per.files[sc], header = T)
  per.days.table = per.days.table[,-1]
  
  for (ws in 1:6){
    
    t.table = get(site.abv[ws])
    t.table[,sc] = per.days.table[,ws]
    assign(site.abv[ws], t.table)
    
  }
  
}

leg.labs = substr(per.files, 12, c(nchar(per.files) - 4))
month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Create a new directory for number of days outflow/backflow
folder.numdays = paste("Proportion Days outflow backflow")
# Check to see if the directory has been created
check.folder = list.files(pattern = folder.numdays)
if(length(check.folder) == 0){
  dir.create(folder.numdays)
}

setwd(paste(main.dir, "/", folder.numdays, sep = ""))
mag = 1.5
p.width = 1000
p.length = 500
point.types = 16
# plot the data in the newly compiled tables
for(ws in 1:6){
  
  png(filename = paste(site.abv[ws], 
                       " Porportion days inflow backflow.jpg",
                       sep = ""),
      width = p.width, height = p.length)
  
  layout(rbind(1,2), heights = c(7,1))
  if(ws %% 2 == 0){
    par(mar=c(4,1,1,7))
  } else {
    par(mar=c(4,7,1,1))
  }
  
  plot.t = get(site.abv[ws])
  
  plot(plot.t[,1], type = "l", ylim = c(0,1), lwd = 2, yaxt = "n",
       ylab = "", xlab = "", xaxt = "n")
  axis(side = 1, at = 1:12, labels = month.names, cex.axis = mag + 0.5)
  mtext(side = 1, line = 3, text = "Month", cex = mag+0.5)
  y.ticks = round(seq(0, 1, by = 0.2), digits = 1)
  
  if(ws == 2 || ws == 4 || ws == 6){
    axis(side = 4, at = y.ticks, 
         labels = y.ticks, cex.axis = mag + 0.5, las = 2)
    x.lab.pos = 15
    text(x.lab.pos, 0.5, labels = "Proportion", cex = mag+0.5, srt = -90, xpd = T)
  } else {
    axis(side = 2, at = y.ticks, 
         labels = y.ticks, cex.axis = mag + 0.5, las = 2)
    x.lab.pos = -2
    text(x.lab.pos, 0.5, labels = "Proportion", cex = mag+0.5, srt = 90, xpd = T)
  }
  
  
  lines(plot.t[,2], lwd = 2, col = 2)
  
  lines(plot.t[,3], lty = 2, lwd = 2)
  
  lines(plot.t[,4], lty = 2, lwd = 2, col = 2)

  par(mar = c(0,0,0,0))
  plot.new()
  
  legend("center", c(leg.labs),
         lty = c(1, 1, 2, 2), col = c(1, 2, 1, 2),
         lwd = 2, ncol = 2, bty ="n", cex = mag)
  dev.off()
  graphics.off()
  
}

# time in seconds for model to run
proc.time() - ptm