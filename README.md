# solar

> Note: This analysis and code is currently specific to Australia only.

Analysis of daily solar-panel energy production against total solar exposure as published by bom.gov.au

## Background
Solar panel output is affected by a number of variables such as:
- Number, age and quality of panels
- Panel orientation and inclination
- Inverter size, minimum/maximum amperages and efficiency ratings
- Operating temperatures of panels and inverter 
- Daily cloud cover, length of day (season), trees, dust, etc. 

The Australian Bureau of Meteorology (BOM) publish a daily map of solar exposure for the nation. These maps give relatively high-resolution potential energy values for any lat-lon coordinate on the continent. This datasource can be used to determine the total system efficiency of a solar production system against the total potential solar exposure.

![image](https://user-images.githubusercontent.com/2888260/38193667-82d8b126-36b6-11e8-92d2-f48a0765a86c.png)  
Source: http://www.bom.gov.au/jsp/awap/solar/index.jsp

Total solar exposure is measured as potential energy falling on a square meter of ground in a flat orientation over the period of a day (solar cycle). The units are Megajoules per square meter (MJ/m<sup>2</sup>).

Residential panels typically peak around the ~14% efficiency rating, and inverters in the 85-95% range depending on their temperature and input amperage. Inclination and orientation of the panels further reduce the potential energy generated.

A total energy conversion percentage of 8-12% is expected for residential systems. Efficency will be higher in the cooler seasons, even though less total energy is available.

## Data

This analysis utilises the following data sources:
1. Total solar exposure grids published at http://www.bom.gov.au/jsp/awap/solar/index.jsp
1. Log of energy production readings from a solar inverter.   
This may be a manual or automated log, and does not need to be daily readings however more frequent readings increase the accuracy of the comparisons. [ECAS](https://play.google.com/store/apps/details?id=at.topfen.ecas) has been used as a mobile-friendly manual data logging application in this example.

## Method



## Results

## Observations


