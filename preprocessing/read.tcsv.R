read.tcsv = function(file, header=TRUE, sep=",", ...) {
  
  n = max(count.fields(file, sep=sep), na.rm=TRUE)
  n2=max(count.fields(file, sep=';'), na.rm=TRUE)
  if(n2>n) {
    n=n2
  }
  
  lines = readLines(file)
  
  for(i in 1:length(lines)) {
    lines[i]<-gsub(';',',',lines[i])
  }
  
  
  .splitvar = function(x, sep, n) {
    var = unlist(strsplit(x, split=sep))
    length(var) = n
    return(var)
  }
  
  lines_split = do.call(cbind, lapply(lines, .splitvar, sep=sep, n=n))
  new_lines=c();
  for(i in 1:nrow(lines_split)) {
    new_lines=c(new_lines, paste(lines_split[i,],collapse=','))
  }
  
  out = read.csv(text=new_lines, sep=',', header=header, ...)
  return(out)
  
}
