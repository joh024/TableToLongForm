WithEachTC =
  ## Loads every TestCase appropriately, then
  ## Runs the given function for each TestCase
  ## func should take args curCase and curdatamat
  ## If func is a list, each element is applied to each
  ##  TestCase subgroup.
  ## [1] = ToyEx
  ## [2] = NZQA
  ## [3] = StatsNZ
  ## [4] = DIA
  function(func){
    if(!is.list(func))
      func = list(func)
    if(length(func) != 4)
      func = rep(func, length = 4)
    
    ## ToyEx TestCases
    TestCases = paste0("./TestCase/",
      list.files("./TestCase", pattern = "^ToyEx[[:alnum:]]+.csv$"))
    for(curCase in TestCases){
      curdatamat = as.matrix(read.csv(curCase, header = FALSE,
        na.strings = c("")))
      func[[1]](curCase, curdatamat)
    }
    
    ## NZQA TestCases
    TestCases = paste0("./TestCase/",
      list.files("./TestCase", pattern = "^NZQA[[:alnum:]]+.xls$"))
    for(curCase in TestCases){
      ##curdatamat = XLmatrix(curCase)[[1]]
      ## For efficiency, xls has been converted to csv, but named xls
      ##  for display.
      ## See xls.real for the actual xls
      ## Write syntax
      ## write.table(..., sep = ",", na = "", row.names = FALSE, col.names = FALSE)
      curdatamat = as.matrix(read.csv(curCase, header = FALSE,
        na.strings = c("")))
      func[[2]](curCase, curdatamat)
    }
    
    ## StatsNZ TestCases
    TestCases = paste0("./TestCase/",
      list.files("./TestCase", pattern = "^StatsNZ[[:alnum:]]+.csv$"))
    for(curCase in TestCases){
      curdatamat = as.matrix(read.csv(curCase, header = FALSE,
        na.strings = c("", "..")))
      func[[3]](curCase, curdatamat)
    }
    
    ## DIA TestCAses
    TestCases = paste0("./TestCase/",
      list.files("./TestCase", pattern = "^DIA[[:alnum:]]+.xls$"))
    for(curCase in TestCases){
      ##curdatamat = XLmatrix(curCase)[[1]]
      ## For efficiency, xls has been converted to csv, but named xls
      ##  for display.
      ## See xls.real for the actual xls
      curdatamat = as.matrix(read.csv(curCase, header = FALSE,
        na.strings = c("")))
      func[[4]](curCase, curdatamat)
    }
    
  }

makeTCDataFile =
  ## Exports all TestCase files as a single .R file
  ## Collated into a single list for convenience
  function(){
    assign("TCDatalist", NULL, envir = .GlobalEnv)

    func = function(curCase, curdatamat){
      cleanname = gsub("(./TestCase/)|(.[[:alpha:]]+$)", "", curCase)
      curlist = list(curdatamat)
      names(curlist) = cleanname
      assign("TCDatalist", c(get("TCDatalist", envir = .GlobalEnv), curlist), envir = .GlobalEnv)
    }
    
    WithEachTC(func)
    
    TCDataout = file("TCData.R", "w")
    cat("TCData = ", file = TCDataout)
    dput(get("TCDatalist", envir = .GlobalEnv), file = TCDataout)
    close(TCDataout)
  }

makehighmat =
  ## Make the highlight matrix for drawTable
  ## using info stored in TableToLongForm full output
  function(convfull)
  with(convfull, {
    highmatrowlabels =
      function(plist, labelmat,
               hdf = 2, starth = 0.75, prevcols = NULL){
        if(is.list(plist)){
          plocs = eval(parse(text = comment(plist)))
          curmat = labelmat[plocs$rows, plocs$cols]
          npare = length(curmat)
          ##curh = (seq(-0.5, 0.5, length = npare + 2)[-c(1, npare + 2)]
          ##        /hdf + starth) %% 1
          curh = (seq(0, 1, length = npare + 2)[-c(1, npare + 2)]
                  /hdf + starth) %% 1
        } else {
          plocs = eval(parse(text = comment(plist)))
          plocs$rows = plist
          curmat = labelmat[plocs$rows, plocs$cols]
          curh = rep(starth, length = length(plist))
        }
        curcol = hsv(curh, s = 0.4, v = 0.8)
        ##curcol = hsv(curh, s = 0.7/sqrt(hdf), v = 0.8)
        for(i in 1:length(curmat))
          curmat[[i]] = c(prevcols, curcol[i])
        labelmat[plocs$rows, plocs$cols] = curmat
        if(is.list(plist))
          for(i in 1:length(plist))
            labelmat = highmatrowlabels(plist[[i]], labelmat,
              hdf = npare * hdf, starth = curh[i],
              ##prevcols = c(prevcols, curcol[i]))
              ##prevcols = curcol[i])
              )
        labelmat
      }

    labelcols = IdentResult$cols$label
    datacols = IdentResult$cols$data
    labelrows = IdentResult$rows$label
    datarows = IdentResult$rows$data

    highmat = matrix(list(NA),
        nrow = nrow(datamat), ncol = ncol(datamat))

    ## Highlight data bits
    highmat[datarows, unlist(datacols)] = list("#CCCCCC")
    
    ## Highlight Row Labels Parentage
    labelmat = highmat[datarows, labelcols]
    labelmat = highmatrowlabels(rowplist, labelmat)
    highmat[datarows, labelcols] = labelmat
      
    highmat
  })

makePareMatRow =
  ## Make an adjusted datamat for displaying Row Parentage
  ## using info stored in TableToLongForm full output
  function(convfull)
  with(convfull, {
    labelmat = datamat[IdentResult$rows$data, IdentResult$cols$label]
    labeldepth = listdepth(rowplist)
    if(length(labeldepth) != 1)
      stop("labeldepth is not uniform, don't know how to handle")
    adjmat = matrix(NA, nrow = nrow(labelmat), ncol = labeldepth)
    highmat = matrix(NA, nrow = nrow(labelmat), ncol = labeldepth)
    matlist = list(adjmat = adjmat, highmat = highmat)
    
    shiftcol =
      function(matlist, plist, curdepth, hdf = 2, starth = 0.75){
        if(is.list(plist)){
          plocs = eval(parse(text = comment(plist)))
        } else {
          plocs = eval(parse(text = comment(plist)))
          plocs$rows = plist
        }
        plocs$cols = curdepth
        matlist$adjmat[plocs$rows, plocs$cols] = names(plist)
        if(is.list(plist)){
          rowends = c(plocs$rows[-1], max(unlist(plist)))
          npare = length(rowends)
          ##if(npare < 3) hdf = max(hdf, 3)
          curh = (seq(0, 1, length = npare + 2)[-c(1, npare + 2)]
                  /hdf + starth) %% 1
          curcol = hsv(curh, s = 0.4, v = 0.8)
          ##curcol = hsv(curh, s = 0.7/sqrt(hdf), v = 0.8)
          for(i in 1:npare){
            matlist$highmat[plocs$rows[i]:rowends[i], plocs$cols] =
              curcol[i]
            matlist = shiftcol(matlist, plist[[i]],
              curdepth + 1, hdf = npare * hdf, starth = curh[i])
          }
        } else{
          curcol = hsv(starth, s = 0.4, v = 0.8)
          ##curcol = hsv(starth, s = 0.7/sqrt(hdf), v = 0.8)
          matlist$highmat[min(plist):max(plist), plocs$cols] = curcol
        }
        ##if(is.list(plist))
        ##  for(i in 1:length(plist))
        ##    matlist = shiftcol(matlist, plist[[i]], curdepth + 1)
        matlist
      }
    
    shiftcol(matlist, rowplist, 1)
  })

listdepth =
  ## Returns depth (nesting level) of a list
  ## Where:
  ## 1 = not a list
  ## 2 = list containing things not lists
  ## 3 = list containing lists containing things not lists
  ## etc
  ## If depth isn't uniform for all nested lists
  ## a vector of all the depths are returned
  function(curlist){
    if(is.list(curlist) && !is.data.frame(curlist))
      unique(1 + unlist(lapply(curlist, listdepth)))
    else
      1
  }

makeTCreqs =
  ## Make TestCase converted.txt and image pdf
  function(){
    source("TableToLongForm.R")
    source("SpreadsheetDrawingCode.R")
    assign("TCconverted", NULL, envir = .GlobalEnv)
    
    func = function(curCase, curdatamat, rows = NULL, cols = NULL,
      cellwidth = 1.1){
      cleanname = gsub("(./TestCase/)|(.[[:alpha:]]+$)", "", curCase)
      
      ## Make Converted File
      convfull = TableToLongForm(curdatamat, fulloutput = TRUE)
      conv = convfull$datafr
      write.table(conv, paste0(curCase, "converted.txt"))
      
      curlist = list(conv)
      names(curlist) = cleanname
      assign("TCconverted", c(get("TCconverted", envir = .GlobalEnv), curlist), envir = .GlobalEnv)
      
      ## If cols = NULL, find max cols
      ##  so output table share same number of columns and
      ##  hence has equal font sizes
      ## with a min col count of 6
      if(is.null(cols))
        cols = 1:max(ncol(curdatamat), ncol(conv), 6)
      
      ## Draw Original Table
      size = drawTableSize(curdatamat, rows = rows, cols = cols,
        cellwidth = cellwidth)
      pdf(gsub(".[[:alnum:]]+$", ".pdf", curCase),
          width = size$width, height = size$height)
      drawTable(curdatamat, rows = rows, cols = cols)
      dev.off()
      
      
      ## Draw Converted Table
      conv = apply(conv, 2, as.character)
      conv = rbind(gsub("UNKNOWN", NA, colnames(conv)), conv)
      size = drawTableSize(conv, rows = rows, cols = cols,
        cellwidth = cellwidth)
      pdf(gsub(".[[:alnum:]]+$", "converted.pdf", curCase),
          width = size$width, height = size$height)
      drawTable(conv, rows = rows, cols = cols)
      dev.off()
    }
    func2 = function(...)
      func(..., rows = 1:43, cols = 1:13, cellwidth = 0.9)
    
    #WithEachTC(func)
    WithEachTC(list(func, func2, func2, func2))
    
    save(TCconverted, file = "TCconverted.RData")
  }

makeTCRunout =
  ## Should only be run after makeTCRun()
  ## Make TestCase .TCRunout (or try to)
  function(){
    source("TableToLongForm.R")
    
    func = function(curCase, curdatamat)
      TableToLongForm(curdatamat, diagnostics = curCase)
    
    WithEachTC(func)
  }

makeTCROimport =
  function(){
    oriRnw = readLines("TableToLongForm_base.Rnw")

    LinesToReplace = grep("^%TCEx", oriRnw)
    VariablesToGet = strsplit(
      gsub("^%TCEx ", "", oriRnw[LinesToReplace]), " ")

    ## Import ToyEx_Complete TCRO
    ToyTCRO = readLines("./TestCase/ToyExComplete.csv.TCRunout")
    LinesMark = grep("###TCR", ToyTCRO)

    LinesNew = vector("list", length(LinesToReplace))
    ## Loop through each line
    for(i in 1:length(LinesNew)){
      ## Loop through each variable, note:
      ## [1] = Chunk Identifier
      ## [-1] = The Variables
      curChunkId = VariablesToGet[[i]][1]
      curVars = VariablesToGet[[i]][-1]
      for(j in 1:length(curVars)){
        curMark = grep(paste(curChunkId, curVars[j]),
          ToyTCRO[LinesMark], fixed = TRUE)
        ## If there is more than 1 match, take first
        if(length(curMark) > 1) curMark = curMark[1]
        
        if(length(curMark) == 0)
          curImpo = "Never occurs"
        else{
          if(curMark == length(LinesMark))
            endLine = length(ToyTCRO)
          else
            endLine = LinesMark[curMark + 1] - 1
          curImpo = ToyTCRO[(LinesMark[curMark] + 1):endLine]
          
          ## Replace double square brackets with single
          ## (bandaid fix for Rnoweb)
          curImpo = gsub("[[][[]", "[", curImpo)
          curImpo = gsub("[]][]]", "]", curImpo)
        }
        LinesNew[[i]] = c(LinesNew[[i]],
                  "\\begin{verbatim}",
                  paste(">", curVars[j]),
                  curImpo,
                  "\\end{verbatim}")
      }
      LinesNew[[i]] =
        c("", paste0(
          "Example values for \\textbf{ToyExComplete.csv} \\texttt{(ID: ", curChunkId, ")}"),
          LinesNew[[i]],
          "\\vspace{-1.5em}", "\\noindent\\rule{0.25\\textwidth}{0.4pt}", "\\vspace{0.5em}")
    }

    ## Compute line numbers to keep
    LineStarts = c(1, LinesToReplace + 1)
    LineEnds = c(LinesToReplace - 1, length(oriRnw))
    ## Weave in LinesNew
    outRnw = NULL
    for(i in 1:length(LinesNew))
      outRnw = c(outRnw,
        oriRnw[LineStarts[i]:LineEnds[i]], LinesNew[[i]])
    outRnw = c(outRnw,
      oriRnw[LineStarts[length(LineStarts)]:LineEnds[length(LineEnds)]])
    
    ## Make no Appendix ver
    writeLines(outRnw, "TableToLongForm_noAppendix.Rnw")
    
    ## Insert Appendices for TCRunout
    TCRunouts = paste0("./TestCase/",
      list.files("./TestCase", pattern = ".TCRunout$"))
    TCROPics = paste0("./TestCase/",
      list.files("./TestCase", pattern = ".pdf$"))
    ## Remove converted.pdf files from TCROPics
    TCROPics = TCROPics[-grep("converted.pdf", TCROPics)]
    TCAp = c("", "\\section{Appendix: TCRO}",
      paste0("The following appendix is automatically generated ",
             "and consists of the diagnostics output of various ",
             "Tables."))
    for(i in 1:length(TCRunouts)){
      curTCRO = TCRunouts[i]
      TCRO = readLines(curTCRO)
      ## Make it more familiar to R users
      TCRO = gsub("###TCR [[:alnum:]]+ ", "> ", TCRO)
      ## Add to TCAp
      curName = gsub("([.]/TestCase/)|(.TCRunout)", "", curTCRO)
      ## Replace double square brackets with single
      ## (bandaid fix for Rnoweb)
      TCRO = gsub("[[][[]", "[", TCRO)
      TCRO = gsub("[]][]]", "]", TCRO)
      TCAp = c(TCAp,
        "\\newpage",
        paste0("\\subsection{", curName, "}"),
        paste0("\\label{sec:TCRO_", curName, "}"),
        "\\begin{figure}[!h]",
        "\\centering",
        paste0("\\includegraphics[width=\\textwidth]{", TCROPics[i],
               "}"),
        "\\end{figure}",
        "\\begin{verbatim}", TCRO,
        "\\end{verbatim}", "")
    }
    
    EndDoc = grep("\\end{document}", outRnw, fixed = TRUE)
    outRnw = c(outRnw[1:(EndDoc - 1)],
      TCAp, outRnw[EndDoc:length(outRnw)])
    
    writeLines(outRnw, "TableToLongForm.Rnw")
  }

runTests =
  ## Run latest TableToLongForm.R against previously generated
  ##  converted.txt to check for changes.
  function(){
    source("TableToLongForm.R")
    source("TCData.R")
    load("TCconverted.RData")
    
    for(curCase in names(TCData)){
      cat("Checking", curCase, "\n")
      curconv = TableToLongForm(TCData[[curCase]])
      checkres = all.equal(curconv, TCconverted[[curCase]])
      print(checkres)
    }
  }

makeNookVer =
  ## Produce a version with paper size for Nook
  function(){
    oriTex = readLines("TableToLongForm_noAppendix.tex")
    LinesToRemove = grep("^\\\\addtolength", oriTex)
    oriTex = oriTex[-LinesToRemove]
    
    Linebegindoc = grep("^\\\\begin[{]document[}]", oriTex)
    outTex = c("\\documentclass[9pt]{article}",
      oriTex[2:(Linebegindoc - 1)],
      "\\usepackage[paperwidth=90mm,paperheight=109.5mm,margin=2mm]{geometry}",
      oriTex[Linebegindoc:length(oriTex)])
    writeLines(outTex, "TableToLongForm_Nook.tex")
  }