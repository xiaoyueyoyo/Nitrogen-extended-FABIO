# Nitrogen-extended-FABIO
Use FABIO to calculate nitrogen pollution/footprint :

Nitrogen footprint comprises two parts:  production N footprint and consumption (direct) N footprint. 

The direct consumption N footprint is easy to calculate based on the previously developed N-Calculator Model. 

The calculation of production N footprint in MRIO model with crop details requires crop detailed nitrogen input data to derive nitrogen emissions in different forms ( NO3-, NH3,N2O), which are reactive nitrogen losses to the environment that do harm to ecological security and human health. The total steps are as follows:
  (1) Derive crop detailed Ninput data
  (2) Calculate nitrogen emission using the N cycle model (provided by the Intergovernmental Panel of Climate Change, IPCC)
  
Then, FABIO can be extended with the production N emission data and consumption N emission data and MRIO analysis can be conducted to understand the nitrogen flow in global food trade and the mismatch of nitrogen pollution hotspots and benefited consumers. 
