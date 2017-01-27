##########################################################################
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
#########################################################
# Modeling waterlevel fluctuation and flow in a wetland #
#########################################################
# Enter the Site name
# Site names to enter: Carpenters, Deferno, Delaney, Plum Tree, 
#                      Butterfield, DorrRoad
# Enter site names as they are listed above (Case and Space sensitive)

ptm = proc.time()

S.Years = c(1933, 1943, 1973, 2005)
E.Years = c(1942, 1951, 1982, 2014)

for(year.int in 1:4){
  
    Start.Year = S.Years[year.int]
    End.Year = E.Years[year.int]
    main.dir = paste("/Users/Looi/Dropbox/NOAA_Wetlands_Ceili-Alex/",
                     "Alex's Folder/Wetland Model/Watersheds", sep = "")
    pTA = seq(0.0, 1.0, by = 0.1)
    
    ##########################################################################
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
    #!!!!!!!!!!!!!!!!!!!!DO NOT EDIT CODE BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!#
    #!!!!!!!!!!!!!!!!!!!!DO NOT EDIT CODE BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!#
    #!!!!!!!!!!!!!!!!!!!!DO NOT EDIT CODE BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!#
    #!!!!!!!!!!!!!!!!!!!!DO NOT EDIT CODE BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!#
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
    ##########################################################################
    
    for (sw in 1:6) {
      
      sen.col = rep(0, length(pTA))
      
      new.table = data.frame(sen.col, sen.col, sen.col, 
                             sen.col, sen.col, sen.col)
      
      colnames(new.table) = c("TA", "ET", "Evap", "Tot.In", "Outflow", "WL")
        
        for(Per in 1:length(pTA)){
          
        Percent = pTA[Per]
        
        sites.TIBS = c("Carpenters", "Deferno", "Delaney", "Plum Tree", 
                       "Butterfield", "DorrRoad")
        
        graphics.off();
      
        Site.name = sites.TIBS[sw]
        
        setwd(main.dir)
        Wetland.Sites = c(c("Carpenters", "CB"),c("Deferno", "DB"), 
                          c("Delaney", "DL"), c("Plum Tree", "PT"),
                          c("Butterfield", "BF"), c("DorrRoad", "DR"))
        site.pos = which(Wetland.Sites == Site.name)
        site.abv = Wetland.Sites[site.pos + 1]
        
        # Read in All necessary data to calculate: potential evaporation and water levels
        
        setwd(paste(main.dir, "/Wetland Model Data/", sep = ""))
        All.Data.filename = list.files(pattern = paste("Wetland Model Data ", 
                                                       Start.Year, "-", 
                                                       End.Year, sep = ""))
        W.raw = read.table(All.Data.filename, header = T, sep = ",")
        
        # Create a new Data frame with just dates and temperatures in Celcius
        ########################################################################
        # Create a vector by calculating a mean between the max temp and min temp 
        # for all available days
        Tmean = ((W.raw$TMAX + W.raw$TMIN)/2)/10
        # Grab all dates from the data frame and convert to POSIX class
        Dates = as.POSIXlt(as.character(W.raw$DATE), format = "%m/%d/%Y", tz = "EST")
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
        AT = tot.A * Percent
        AOW = tot.A *(1 - Percent)
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
        row.names(lm.parms) = lm.parms$X
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
        R_a = c(9.1, 10.3, 11.6, 13.2, 14.6, 15.3, 15.0, 13.8, 12.3, 10.7, 9.4, 8.7)
        
        # these are conversion numbers to convert mm per day per hectare to m^3 per day
        mm.to.m = 1/1000 # millimeters to meters conversion (0.001 per meter)
        hec.to.m = 10000 # hectares to meters squared conversion (10,000 m^2 to hectare)
        
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
        # These inputs are developed from GWLF, the Mapshed plugin for GIS
        
        dir.output = paste(dir.sites, 
                           Site.dir, "/Output", sep = "")
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
        
        # Give daily inflow its own variable
        Daily.inflow = model.input$Daily.Flow..m.3
        
        # Read in ABay Water Levels Data needed
        #########################################################
        # 
        #  = read.table(ABayWL.file, sep = ",", header = T)
        # 
        # # Convert the variable type of 
        # Dates = as.character(River.WL$Date)
        # Dates = as.POSIXlt(Dates, format = "%m/%d/%Y")
        # M = format(Dates , "%m")
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
        # Capital "M" is the slope in the linear model for each wetland without a
        # water control structure, used to calculate the water level based on
        # A Bay water level. This is not to be confused with lower case "m" which
        # stands for month, and is used in determining the crop coefficient.
        ################################################################
        # find the next day's water level using a LR.
        # If the site is not PT. This is because
        # PT follows the water level dynamics of the river
        ################################################################
        if(site.abv != "PT"){
          nd.WL = round(M*River.WL+B, 5)
        } else if (site.abv == "PT") {
          nd.WL = round(River.WL, 5)
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
          i.WL = round(M*River.WL[1]+B, 5)
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
          m = format(Dates[d], "%m")
          
          # Find the mean day length for the given month
          R_a.m = R_a[as.numeric(m)]
          
          # Find the appropriate K_c value for the season
          if(m == "12" || m == "01" || m == "02")K_c.s = K_c[1]
          if(m == "03" || m == "04" || m == "05")K_c.s = K_c[2]
          if(m == "06" || m == "07" || m == "08")K_c.s = K_c[3]
          if(m == "09" || m == "10" || m == "11")K_c.s = K_c[4]
          
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
          P.GWLF.conc[d] = TP.kg[d]/peak.vol[d]
          N.GWLF.conc[d] = TN.kg[d]/peak.vol[d]
          
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
              # cubic feet per second, the 86400 are the number of seconds in a day
              # Convert to meters cubed (0.0283168466 ft^3 = 1m^3)
              pot.flow = ((3.33*(L - 0.2*H)*H^(2/3))*86400)*0.0283168466
        
              # calculate the minimum volume allowed in the wetland beacuse of 
              # the planks.
              vol.min = WLvsVol$vol[which(p.H[d] == WLvsVol$WLs)]
              
              # calculate max amount of water that can leave the system
              out.max = peak.vol[d] - vol.min
                
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
              nd.WL[d] = round(River.WL[d], 5)
            }
            
            # Calculate the next day's water level
            if(d != n.days){
              nd.WL[d] = round(M*River.WL[d+1]+B, 5)
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
                # for cases when the first day there is a negative outflow
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
        
        ###################################################################
        # Round all output to 5 decimal places #
        ###################################################################
        
        # calculate concentrations of nutrients in micromoles per liter
        P.conc = P.conc/m3.to.L*kg.to.g/P.MM*M.to.uM
        N.conc = N.conc/m3.to.L*kg.to.g/P.MM*M.to.uM
        
        # Calculate the average water level
        avg.WL = (peak.WL+nd.WL)/2
        
        # GWLF nutrient concentrations using Peak volume
        P.GWLF.conc = P.GWLF.conc/m3.to.L*kg.to.g/P.MM*M.to.uM
        N.GWLF.conc = N.GWLF.conc/m3.to.L*kg.to.g/P.MM*M.to.uM
        
        # Round Peak volume to three decimal places
        peak.vol = round(peak.vol, digits = 5)
        
        # Calculate the total inflow
        back.vec = which(outflow < 0)
        tot.inflow = m.prcp + Daily.inflow
        tot.inflow[back.vec] = abs(outflow[back.vec]) + tot.inflow[back.vec]
    
        model.file = paste(site.abv, " ", 
                          Start.Year, "-", End.Year, " ", (Percent*100), "%",
                          " Typha model output.csv", sep = "")
        
        # Put all model results into a data frame
        raw.data = data.frame(Dates, delta, outflow, peak.vol, nd.vol, ET, 
                               peak.WL, nd.WL, P.conc, P.tot, TP.kg, P.GWLF.conc,
                               N.conc, N.tot, TN.kg, N.GWLF.conc, avg.WL,
                               Daily.inflow, m.prcp, tot.inflow)
        for(cols in 2:length(raw.data[1,])){
          
          raw.data[,cols] = round(raw.data[,cols], digits = 5)
          
        }
        
        # Calculate Total Annual ET, Evap, Total Inflow, Outflow, and WL
          new.table$TA[Per] = pTA[Per]
          new.table$ET[Per] = mean(ET)
          new.table$Evap[Per] = mean(EV)
          new.table$Tot.In[Per] = mean(tot.inflow)
          new.table$Outflow[Per] = mean(outflow)
          new.table$WL[Per] = mean(avg.WL)
          
          
          
          sites.wd = paste(main.dir ,"/Wetland Sites/", 
                           Site.name, " TIBS", sep = "")
          
          Sen.folder = paste(sites.wd, "/", Site.name, " Typha Sensitivity", sep = "")
          
          dir.create(Sen.folder)
          
          setwd(Sen.folder)
          
          write.table(raw.data, model.file, sep = ",", row.names = F)
          
      }
      ####################################################################
      # Set a new work directory to write .csv files and figures #
      ####################################################################
      sites.wd = paste(main.dir ,"/Wetland Sites/", 
                       Site.name, " TIBS", sep = "")
      
      setwd(Sen.folder)
      
      sen.file.name = paste(Site.name, " ", Start.Year, "-", End.Year,
                            " Sensitivity Analysis Total Typha Area.csv",
                            sep = "")
      
      write.table(new.table, sen.file.name, sep = ",", row.names = F)
      
    }
    
}
run.time = proc.time() - ptm
run.time
