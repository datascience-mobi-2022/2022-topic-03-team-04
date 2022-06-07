```{r}
# Global and Maxima Detection
maximafunction = function(input){
  threshold = 0.5
  output = 0
  for (i in 1:(length(input))){
    # 1st Fraction
    if (i == 1){
      if(input[i]<input[i+1] && input[i] > threshold*(max(input))){
        output[i] = input[i]}
      else{
        output[i] = 0}
    }
    
    # 2nd-24th Fraction
    if (i>1 && i<length(input)){
      if(input[i]<input[i+1] && input[i]>input[i-1] && input[i] > threshold*(max(input))){
        output[i] = input[i]}
      else{
        output[i] = 0}
    }
    
    # 25th Fraction
    if (i==length(input)){
      if(input[i]>input[i-1] && input[i] > threshold*(max(input))){
        output[i] = input[i]
      }
      else{
        output[i] = 0}
    }
  }
  return(output)}

# A Vector for Fraction's Names
fractionnames = 0
for (i in 1:25) {
  fractionnames[i] = paste("Fraction",i,sep="_")
}

# Results
allmaxima_Ctrl = data.frame(apply(t(Ctrl_Norm), 2, maximafunction), row.names = fractionnames)
allmaxima_RNase = data.frame(apply(t(RNase_Norm), 2, maximafunction), row.names = fractionnames )
```