##----------------------------------------------------------------------
## The code in this .R file is machine generated from the literate
##  program, TableToLongForm.Rnw
## Documentation can be found in the literate description for this
##  program, TableToLongForm.pdf
##----------------------------------------------------------------------
TableToLongForm =
  function(datamat, IdentResult = NULL,
           fulloutput = FALSE, diag = FALSE, diagname = NULL){
    if(diag){
      if(is.null(diagname)) diagname = deparse(substitute(datamat))
      assign("TCRunout", file(paste0(diagname, ".TCRunout"),
                              "w"), envir = .GlobalEnv)
      on.exit({
        close(TCRunout)
        rm("TCRunout", envir = .GlobalEnv)
      })
    }
    
    fullout = ReconsMain(datamat, IdentResult)
    if(fulloutput) fullout else fullout$datafr
  }
IdentMain =
  function(datamat){
    rowNonempty = (1:nrow(datamat))[IdentNonEmpty(datamat, 1)]
    colNonempty = (1:ncol(datamat))[IdentNonEmpty(datamat, 2)]
    rowData = IdentMostCommonBoundary(datamat, 2)
    colData = IdentMostCommonBoundary(datamat, 1)
    ## Temporary fix for first col being all numbers (e.g. years)
    if(colData[1] == 1) colData[1] = 2
    TCRsink("CIMCB", rowData, colData)
    rowslist = list(label = rowNonempty[rowNonempty < rowData[1]],
                    data = rowNonempty[(rowNonempty >= rowData[1]) &
                                       (rowNonempty <= rowData[2])])
    colslist = list(label = colNonempty[colNonempty < colData[1]],
                    data = colNonempty[(colNonempty >= colData[1]) &
                                       (colNonempty <= colData[2])])
    TCRsink("CRAC", rowslist, colslist)
    curcol = colslist$data
    Patvec = NULL
    for(currow in rowslist$label){
      curlabel = datamat[currow, curcol]
      if(any(is.na(curlabel))){
        if(!all(is.na(curlabel)))
          Patvec = c(Patvec, IdentPattern(is.na(curlabel)))
      } else Patvec = c(Patvec, IdentPattern(curlabel))
    }
    TCRsink("GPV", Patvec)
    Patvec = max(Patvec)
    if(!is.na(Patvec)){
      megacolnum = length(curcol)/Patvec
      megacollist = list()
      for(i in 1:megacolnum)
        megacollist =
          c(megacollist, list(curcol[1:Patvec + Patvec * (i - 1)]))
      colslist$data = megacollist
    } else colslist$data = list(curcol)
    TCRsink("IM", rowslist, colslist)
    list(rows = rowslist, cols = colslist)
  }
IdentNonEmpty =
  function(datamat, margin, emptyident = is.na){
    isnonempty = apply(datamat, margin, function(x) !all(emptyident(x)))
    which(isnonempty)
  }
IdentPattern =
  function(vec){
    len = length(vec)
    res = NA
    for(i in 1:floor(len/2)){
      curseg = paste("^(", paste(vec[1:i], collapse = ""),
        ")+$", sep = "")
      if(nchar(curseg) > 2559){
        warning("Label lengths too long for regular expressions to ",
                "work. IdentPattern has been aborted. A pattern may ",
                "exist but it cannot be found with the current ",
                "algorithm.")
        break
      } else if(length(grep(curseg, paste(vec, collapse = ""))) > 0){
        res = i
        break
      }
    }
    res
  }
IdentMostCommonBoundary =
  function(datamat, margin){
    isnumber = suppressWarnings(apply(datamat, margin,
      function(x) which(!is.na(as.numeric(x)))))
    nstarts = table(sapply(isnumber,
      function(x) if(length(x) > 0) min(x) else NA))
    nends = table(sapply(isnumber,
      function(x) if(length(x) > 0) max(x) else NA))
    as.numeric(names(c(which.max(nstarts), which.max(rev(nends)))))
  }
PareFront =
  function(datamat)
  PareMain(datamat = datamat, plist =
           list(rows = 1:nrow(datamat), cols = 1:ncol(datamat)))
PareCol =
  function(datamat, datacols, labelrows){
    for(j in 1:length(datacols)){
      curfamily = datamat[labelrows, datacols[[j]], drop = FALSE]
      firstcolempty = is.na(curfamily[,1])
      if(any(firstcolempty))
        for(i in which(firstcolempty)){
          notempty = !is.na(curfamily[i,])
          if(sum(notempty) == 1){
            curfamily[i, 1] = curfamily[i, notempty]
            curfamily[i, notempty] = NA
          }
        }
      datamat[labelrows, datacols[[j]]] = curfamily
    }
    datacols = unlist(datacols)
    notfullrows = apply(datamat[labelrows, datacols, drop = FALSE], 1,
      function(x) any(is.na(x)))
    if(any(diff(notfullrows) > 1))
      warning("full rows followed by not full rows!")
    pastestring = ""
    pasterows = which(!notfullrows)
    for(i in 1:length(pasterows))
      pastestring[i] = paste("datamat[labelrows[", pasterows[i],
                   "], datacols]", sep = "")
    collapsedlabels = eval(parse(text = paste("paste(",
                                   paste(pastestring, collapse = ", "),
                                   ")", sep = "")))
  labeldatamat = rbind(datamat[labelrows[notfullrows], datacols],
    collapsedlabels)
  PareFront(t(labeldatamat))
}
PareMain =
  function(datamat, plist){
    if(length(plist$cols) == 1){
      res = structure(plist$rows, .Names = datamat[plist$rows, plist$cols])
      res = attrLoc(res, cols = plist$col)
      TCRsink("IOOC", plist, res)
    }
    else if(all(is.na(datamat[plist$rows, plist$cols[1]]))){
      plist$cols = plist$cols[-1]
      res = PareMain(datamat, plist)
    }
    else if(length(plist$rows) == 1){
      res = structure(plist$rows,
        .Names = datamat[plist$rows, plist$cols[length(plist$cols)]])
      res = attrLoc(res, cols = plist$cols[length(plist$cols)])
      for(i in (length(plist$cols) - 1):1){
        res = list(res)
        names(res) = datamat[plist$rows, plist$cols[i]]
        res = attrLoc(res, rows = plist$rows, cols = plist$cols[i])
      }
      TCRsink("IOOR", plist, res)
    }
    else if(is.na(datamat[plist$rows[1], plist$cols[1]])){
      warning("cell[1, 1] is empty")
      print(plist)
      res = NA
    }
    else{
      res = PareByEmptyRight(datamat, plist)
      if(any(is.na(res)))
        res = PareByEmptyBelow(datamat, plist)
      for(i in 1:length(res))
        res[[i]] = PareMain(datamat, res[[i]])
      res
    }
    class(res) = "plist"
    res
  }
PareByEmptyRight =
  function(datamat, plist)
  with(plist,
       if(all(is.na(datamat[rows[1], cols[-1]]))){
         emptyrights = apply(datamat[rows, cols[-1], drop = FALSE], 1,
           function(x) all(is.na(x)))
         rowemptyright = rows[emptyrights]
         if(length(rowemptyright) == 1){
           res = list(list(rows = rows[-1], cols = cols))
           names(res) = datamat[rows[1], cols[1]]
           res = attrLoc(res, rows = rows[1], cols = cols[1])
           TCRsink("CSER", res)
         }
         else{
           rowdiff = diff(rowemptyright)
           if(any(rowdiff == 1))
             rowemptyright = rowemptyright[c(rowdiff == 1, FALSE)]
           
           rowstart = pmin(rowemptyright + 1, max(rows))
           rowend = c(pmax(rowemptyright[-1] - 1, min(rows)), max(rows))
           
           res = list()
           for(i in 1:length(rowstart))
             res[i] = list(list(rows = rowstart[i]:rowend[i], cols = cols))
           names(res) = datamat[rowemptyright, cols[1]]
           res = attrLoc(res, rows = rowemptyright, cols = cols[1])
           TCRsink("CMER", res)
         }
         res
       } else NA)
PareByEmptyBelow =
  function(datamat, plist)
  with(plist, {
    emptybelow = is.na(datamat[rows, cols[1]])
    rowstart = rows[!emptybelow]
    rowend = c(rowstart[-1] - 1, max(rows))
    res = list()
    for(i in 1:length(rowstart))
      res[i] = list(list(rows = rowstart[i]:rowend[i], cols = cols[-1]))
    names(res) = datamat[rowstart, cols[1]]
    res = attrLoc(res, rows = rowstart, cols = cols[1])
    TCRsink("PBEB", res)
    res
  })
ReconsMain =
  function(datamat, IdentResult){
    if(is.null(IdentResult))
      IdentResult = IdentMain(datamat)
    labelcols = IdentResult$cols$label
    datacols = IdentResult$cols$data
    labelrows = IdentResult$rows$label
    datarows = IdentResult$rows$data
    datamatRowLabels = datamat[datarows, labelcols, drop = FALSE]
    datamatRowLabels = datamatRowLabels[,
      IdentNonEmpty(datamatRowLabels, 2), drop = FALSE]
    rowplist = PareFront(datamatRowLabels)
    rowvecs = ReconsRowLabels(rowplist)
    TCRsink("RRL", rowplist, rowvecs[1:4,])
    colplist = PareCol(datamat, datacols, labelrows)
    datamatColLabels = datamat[datarows[unlist(rowplist)], unlist(datacols)]
    res = ReconsColLabels(colplist, datamatColLabels, rowvecs)
    TCRsink("RCL", colplist, res[1:4,])
    list(datafr = res, datamat = datamat, IdentResult = IdentResult,
         rowplist = rowplist, colplist = colplist)
  }
ReconsRowLabels =
  function(plist)
  if(is.list(plist)){
    rowvecs = as.list(names(plist))
    for(i in 1:length(rowvecs))
      rowvecs[[i]] = cbind(rowvecs[[i]], ReconsRowLabels(plist[[i]]))
    do.call(rbind, rowvecs)
  } else as.matrix(names(plist))
ReconsColLabels =
  function(plist, datamat, rowvecs){
    if(is.list(plist)){
      colvecs = as.list(names(plist))
      for(i in 1:length(colvecs)){
        colvecs[[i]] = cbind(colvecs[[i]],
                 ReconsColLabels(plist[[i]], datamat, rowvecs))
        colnames(colvecs[[i]])[1] = "UNKNOWN"
      }
      datfr = do.call(rbind, colvecs)
    }
    else{
      datbit = datamat[,plist]
      mode(datbit) = "numeric"
      ## Specify row.names to avoid annoying warnings
      datfr =
        cbind(as.data.frame(rowvecs, row.names = 1:nrow(rowvecs)), datbit)
      colnames(datfr) =
        c(rep("UNKNOWN", length = ncol(rowvecs)), names(plist))
    }
    datfr
  }
print.plist = function(plist){
  plistC = function(plist){
    pLoc = attr(plist, "Loc")
    if(is.list(plist)){
      namevec = names(plist)
      if(!is.null(pLoc))
        namevec = paste0(names(plist),
          " (", pLoc[,"rows"], ", ", pLoc[,"cols"], ")")
      namelist = as.list(namevec)
      for(i in 1:length(namelist))
        namelist[[i]] =
          c(paste("+", namelist[[i]]),
            paste("-", plistC(plist[[i]])))
      do.call(c, namelist)
    } else{
      if(!is.null(names(plist))){
        namevec = names(plist)
        if(!is.null(pLoc))
          namevec = paste0(names(plist),
            " (", plist, ", ", pLoc[,"cols"], ")")
        paste("+", namevec)
      } else paste(plist, collapse = " ")
    }
    }
  cat(plistC(plist), sep = "\n")
}

attrLoc =
  function(plist, rows = NULL, cols = NULL){
    attr(plist, "Loc") = cbind(rows, cols)
    class(plist) = "plist"
    plist
  }

TCRsink =
  function(ID, ...)
  if(exists("TCRunout", envir = .GlobalEnv)){
    varlist = list(...)
    names(varlist) = gsub(" ", "", as.character(match.call()[-(1:2)]))
    sink(TCRunout)
    for(i in 1:length(varlist)){
      cat("###TCR", ID, names(varlist)[i], "\n")
      print(varlist[[i]])
    }
    sink()
  }
