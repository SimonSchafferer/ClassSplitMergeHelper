
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

splitClassFile = function( inPath, filename, outPath, splitTag = "#--split "){
  dir.create(outPath)
  singleClassFile = readLines(con = file.path(inPath, filename))
  tagIndx = grep(splitTag, singleClassFile)
  classFileNames = sub(splitTag,"",singleClassFile[tagIndx])
  classFileL = splitAt(singleClassFile, tagIndx)
  
  names(classFileL) = classFileNames
  tmp = lapply( names(classFileL), function(x){
    fn = paste0(x,".R")
    message("Writing file: ", file.path(outPath, fn))
     writeLines( classFileL[[x]], con = file.path(outPath, fn), sep="\n")
  })
  return(TRUE)
}

mergeClassFiles = function( inPath, filenames, outPath, outFilename, ... ){
  
  if(missing(filenames)){
    fnames = list.files(path = inPath, ... )    
  } else{
    fnames = filenames
  }
  
  fnames = file.path(inPath, fnames)
  file.create(file.path(outPath, outFilename), force=TRUE)
  fileInL = lapply( fnames, function(x){
    
    cat( readLines(con = x), file = file.path(outPath, outFilename), append = TRUE, sep = "\n")
    return("")
  } )
  message(paste0("All files written to ", file.path(outPath, outFilename))  )
  return(TRUE)
}
