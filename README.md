# NoMe  
Code and data associated with Northern Metalmark manuscript by Henry et al.  
Last updated 2021-01-20  

Working file to be deleted before publication:  
  
  * `anonymizeData.r`  
  
  
Mark-recapture (and un-captured/un-marked) records in field-form-like format:  
  
  * resight.csv  
  
  
List of dates and observers:  
  
  * observer.csv  
  
  
Shell script:  
  
  * runs `prepareinp.r` produces MARK input files as .RDS file in a `/Data` directory  
  * runs the `analysis201X.r` scripts (each of which sources `MARKformulas.r`) within a call to `sink()` to write the output to text files  
  