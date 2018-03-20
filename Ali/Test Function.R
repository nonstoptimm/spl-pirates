#Test Function

fee = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
ree = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "random")

## Write Function 
Estimate.appropriate.model = function(fe, re) {
  
  HausmanTest = phtest(fe, re)
      if(HausmanTest[2] < 0.05){
          Hausmann = 1 }
        else{
           Hausmann = 0 } 
      return(Hausmann)
  
}

test.result$Hausmann = Estimate.appropriate.model(fee, ree)



