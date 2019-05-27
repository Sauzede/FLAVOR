# FLAVOR
FLAVOR method

Auxiliary material for paper "Retrieving the vertical distribution of chlorophyll a concentration and phytoplankton community composition from in situ fluorescence profiles: A method based on a neural network with potential for global-scale applications" published in Journal of Geophysical Research: Oceans (doi:10.1002/2014JC010355)

R. Sauzede, Laboratoire d'Océanographie de Villefranche, UMR 7093 CNRS, Université Pierre et Marie Curie-Paris 6, Quai de la Darse BP 08, 06238 Villefranche-Sur-Mer, France 

The codes provided are in R language and allow to retrieve the (1) total chlorophyll-a concentration and (2) simultaneously chlorophyll-a concentrations associated to three phytoplankton size classes (microphytoplankton, nanophytoplankton and picophytoplankton) from a normalized fluorescence profile for ten normalized depths.
There are four sub-directory: one for the code to create the input data for the MLP with a fluorescence profile associated with its location and date of acquisition (INPUT), one for the code for the retrieval of chlorophyll-a concentration (CHL), one for the simultaneously retrievals of the chlorophyll-a concentrations associated to three phytoplankton size classes (PSC) and one for the second step of the method which consists in retrieveing a quasi-continuous profile from the ten points retrieved by the MLP. For this last sub-directory, there are two sub-directories: one for the total chlorophyll-a concentration (CHL) and one for the three phytoplankton size (PSC). Each sub-directory contains a README file explaining how to use the codes. 

How to cite: Sauzède, R., H. Claustre, C. Jamet, J. Uitz, J. Ras, A. Mignot, and F. D’Ortenzio (2015), Retrieving the vertical distribution of chlorophyll a concentration and phytoplankton community composition from in situ fluorescence profiles: A method based on a neural network with potential for global-scale applications, J. Geophys. Res. Oceans, 120, doi:10.1002/ 2014JC010355.
