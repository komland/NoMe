# NoMe (Northern Metalmark)  
Code and data associated with manuscript:  
  
* Authors: Weston J Henry, Kristian S Omland, Henry Aaron Frye, David L Wagner  
* Submitted: 2021-04-04  
* Title: Mark-Recapture Study and Critical Habitat Assessment for the Northern Metalmark
Butterfly, Calephelis borealis (Lepidoptera: Riodinidae)  
* Submitted to: Journal of Insect Conservation  

Last updated 2021-04-05  

## Mark-recapture analysis  
  
### Data  

  * resight.csv: mark-recapture (and un-captured/un-marked) records in field-form-like format  
  * observer.csv: list of dates and observers  
  
### R scripts  
  
  * `prepareinp.r` produces MARK input files as .RDS file in a `/Data` directory  
  * `analysis201X.r` in which `focalYr` can be specified (accommodating minor differences among the years)  
  * `MARKformulas.r` to be `source()`-ed  
  
## Genetic analysis and Vegetation ordination  
  
### Data  

* Metadata: PackeraVegCodeData/PackeraMetadata.docx  
  * PackeraVegCodeData/TransectClean.csv  
  * PackeraVegCodeData/spe.csv  
  * PackeraVegCodeData/env.csv  
  
### R scripts  
  
* PackeraVegCodeData/PackeraBayes.R  
* PackeraVegCodeData/VegetationOrdination.R, which calls  
  * PackeraVegCodeData/packera_full_jags.R  
  