make = function(runtests = TRUE, bibclear = FALSE, quiet = TRUE){
  source("MAKE_support.R")
  source("Rnoweb.R")
  
  if(bibclear == TRUE){
  ## remove bibtex files if any exist
    wdfiles = list.files()
    wdext = tools::file_ext(wdfiles)
    bibext = (wdext == "bbl") | (wdext == "blg")
    if(any(bibext)){
      cat("Deleting", paste0(wdfiles[bibext], "\n"))
      inp = readline("\nConfirm Delete: y/n ")
      if(inp == "y")
        file.remove(wdfiles[bibext])
    }
  }
  
  Rnoweb("TableToLongForm_base.Rnw")
  makeTCDataFile()
  makeTCRunout()
  makeTCROimport()
  
  Rnoweb("TableToLongForm_noAppendix.Rnw")
  tools::texi2dvi("TableToLongForm_noAppendix.tex", pdf = TRUE, quiet = quiet)
  
  Rnoweb("TableToLongForm.Rnw")
  tools::texi2dvi("TableToLongForm.tex", pdf = TRUE, quiet = quiet)
  
  ## makeNookVer()
  ## tools::texi2dvi("TableToLongForm_Nook.tex", pdf = TRUE, quiet = quiet)
  
  if(runtests == TRUE) runTests()
}

maketestcases = function(){
  source("MAKE_support.R")
  makeTCreqs()
}

makemiscdocs = function(quiet = TRUE){
  drawTable = function(){
    TEC = TCDatalist$ToyExByEmptyBelow
    highmat = matrix(list(NA), nrow = nrow(TEC), ncol = ncol(TEC))
    ## matRowLabel
    highmat[2:5, 1:2] = list("#B8D8F8")
    ## matColLabel
    highmat[1, 3:6] = list("#ADE0BA")
    ## matData
    highmat[2:5, 3:6] = list("#FFC5D0")
    
    ## drawTableFront(TEC, cellwidth = 1.2, highmat = highmat)
    drawTableFront(TEC, cellwidth = 1.2, highmat = highmat,
      pdfout = "TableToLongForm_WorkingWithModules_ExampleTable.pdf")
  }
  
  ## drawTable()
  tools::texi2dvi("TableToLongForm_WorkingWithModules.tex", pdf = TRUE, quiet = quiet)
}