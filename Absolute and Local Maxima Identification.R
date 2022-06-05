# Global and Maxima Detection (need to add threshold in if function)
maximafunction = function(input){
  output = 0
  for (i in 1:(length(input))){
    # 1st Fraction
    if (i == 1){
      if(input[i]<input[i+1]){
        output[i] = input[i]}
      else{
        output[i] = 0}
    }
    
    # 2nd-24th Fraction
    if (i>1 && i<length(input)){
      if(input[i]<input[i+1] && input[i]>input[i-1]){
        output[i] = input[i]}
      else{
        output[i] = 0}
    }
    
    # 25th Fraction
    if (i==length(input)){
      if(input[i]>input[i-1]){
        output[i] = input[i]
      }
      else{
        output[i] = 0}
    }
  }
  
  # Only the first 2 highest values are taken. Others = 0
  for (i in 1:length(output)){
    if(output[i]==max(output) || output[i]== sort(output, decreasing = TRUE)[2]){
      output[i] = output [i]}
    else{
      output[i] = 0}
  }
  return(output)
}

  # A Vector for Fraction's Names
  fractionnames = 0
  for (i in 1:25) {
    fractionnames[i] = paste("Fraction",i,sep="_")
  }

# Results
allmaxima_Ctrl = data.frame(apply(tCtrl_Norm, 2, maximafunction), row.names = fractionnames)
allmaxima_RNase = data.frame(apply(tRNase_Norm, 2, maximafunction), row.names = fractionnames )
