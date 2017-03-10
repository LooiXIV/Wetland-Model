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
# WARNING: DO NOT MESS AROUND WITH THE FILE STRUCTURE UNLESS YOU ARE
# 100% SURE OF WHAT YOU ARE DOING.
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
# Items, files, tables, and folders that this code produces for all 
#    wetland sites:
# 1) Raw Model data tables and files:
#   -Daily raw data simulations of waterlevel, nitrogen concentration, phosphorus 
#   concentration, total outflow, total flow reversal, and dilution rate.
#   -Also produced are the monthly means of nitrogen concentration, phosphorus 
#   concentration, total outflow, total flow reversal, and dilution rate.
# 2) Monthly mean summary data tables and files for all wetland sites:
#   -
##########################################################################
