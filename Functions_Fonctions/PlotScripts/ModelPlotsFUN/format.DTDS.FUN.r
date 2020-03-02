#############################################################################
# Extract and Format the DTDSmodel.Input list 
#############################################################################

format.DTDSModel <- function(mod.ls){
  
  #===============================================================================
  # STEP 1: Extract the different variables from the DTDSmodel.Input list  
  #===============================================================================

  #extract all elements in DTDSmodel.Input list and assign as list item names
  for(i in 1:length(mod.ls)) {
    assign(paste0(names(mod.ls[i])), mod.ls[[i]])
  } 

  #THIS CREATES THE FOLLOWING VARIABLES:
  #   - DS.formula
  #   - model.DT
  #   - model.DS
  #   - model.DTDS
  #   - DTDS.data
  #   - DTDS.formula

  #===============================================================================
  # STEP 2: Format datasets for plotting
  #===============================================================================
  
  #Adds sm of DT data, and the residuals after substracing sm from DT  (????)
  #DTDS.data <- add.SMcol(DTDS.data,data.col="DT")   #which column should I use?
  
  #add span value for lowess
  if(unique(DTDS.data$DTmethod) == "Lowess"){
    DTDS.data$loess.span <- model.DT$pars$span
  }
 
  
  #===============================================================================
  # STEP3: Send to Global environment
  #===============================================================================
  assign("DTDS.data", DTDS.data, envir = .GlobalEnv)
  assign("DS.formula", DS.formula, envir = .GlobalEnv)
  assign("model.DT", model.DT, envir = .GlobalEnv)
  assign("model.DS", model.DS, envir = .GlobalEnv)
  assign("model.DTDS", model.DTDS, envir = .GlobalEnv)
  assign("DTDS.formula", DTDS.formula, envir = .GlobalEnv)
  
}
