drawTable =
  ## Function for drawing a matrix or dataframe
  ## to look a bit like how it would be drawn in
  ## a spreadsheet software like Excel.
  ## Args:
  ## --datamat--
  ##
  ## --rows/cols (numeric)--
  ## numeric vector specifying which rows and cols to draw
  ## Default: NULL (draw all)
  ##
  ## --highmat (matrix)--
  ## a matrices of the same dimensions as datamat,
  ## each cell containing a list of length 1,
  ## containing a character vector of any length,
  ## that specifies the background colour of a portion
  ## of the matching cell, depending on the length of the vector
  ## length = 1 will be full background
  ## length = 2 means [1] gives left half, [2] gives right half, etc.
  ## Default: NULL (use white background for all)
  ##
  ## --labwidth (numeric)--
  ## numeric vector specifying width of the row number labels
  ## as a proportion of data-cell widths
  ## Default: 0.5
  ##
  ## --colNotNum (numeric)--
  ## numeric vector specifying columns that should never
  ## be formatted as numbers
  ## Default: NULL (any cell that can be coerced to a number
  ##                will be formatted as a number)
  ##
  ## --digits--
  ## passed to 'formatC' function for number formatting
  ## numbers are formatted as "f" (floats)
  ## Default: 0 (no decimal places)
  function(datamat, rows = NULL, cols = NULL, highmat = NULL,
           labwidth = 0.5, colNotNum = NULL, digits = 0){
    datamat = as.matrix(datamat)
    if(is.null(rows))
      rows = 1:nrow(datamat)
    else{
      if(max(rows) > nrow(datamat)){
        extramat = matrix(NA, nrow = max(rows) - nrow(datamat),
          ncol = ncol(datamat))
        datamat = rbind(datamat, extramat)
      }
      if(!is.null(highmat))
        if(max(rows) > nrow(highmat)){
          extramat = matrix(NA, nrow = max(rows) - nrow(highmat),
            ncol = ncol(highmat))
          highmat = rbind(highmat, extramat)
        }
    }
    if(is.null(cols))
      cols = 1:ncol(datamat)
    else{
      if(max(cols) > ncol(datamat)){
        extramat = matrix(NA, nrow = nrow(datamat),
          ncol = max(cols) - ncol(datamat))
        datamat = cbind(datamat, extramat)
      }
      if(!is.null(highmat))
        if(max(cols) > ncol(highmat)){
          extramat = matrix(NA, nrow = nrow(highmat),
            ncol = max(cols) - ncol(highmat))
          highmat = cbind(highmat, extramat)
        }
    }
    
    ## Define Colours
    labelbg = "#EEEEEE"
    bordercol = "#777777"
    
    ## Start plot
    opar = par(mar = rep(0, 4), bg = "white")
    on.exit(par(opar))
    xlim = c(0.5 - labwidth, length(cols) + 0.5)
    ylim = c(length(rows) + 0.5, -0.5)
    plot.new()
    plot.window(xlim = xlim, xaxs = "i",
                ylim = ylim, yaxs = "i")
    
    ## Draw rows/cols numbers
    ## Background
    rect(0.5 - labwidth, -0.5, labwidth, ylim[1], border = NA,
         col = labelbg)
    rect(0.5, -0.5, xlim[2], 0.5, border = NA, col = labelbg)
    ## Text
    text(0.5 - labwidth/2, (1:length(rows)) + 0.25, rows,
         col = bordercol, adj = c(0.5, 0))
    text(1:length(cols), 0.25, cols, col = bordercol, adj = c(0.5, 0))

    ## Draw cells
    ## textx = rep((1:length(cols)) - 0.45, each = length(rows))
    ## texty = rep(1:length(rows), times = length(cols))
    ## text(textx, texty, datamat, adj = 0)
    textx = (1:length(cols)) - 0.45
    texty = (1:length(rows)) + 0.25
    for(j in 1:length(textx)){
      for(i in 1:length(texty)){
        curcell = datamat[rows[i], cols[j]]
        if(is.null(highmat)){
          ## If cell isn't empty, white out cell first
          if(!is.na(curcell))
            rect(textx[j] - 0.05, texty[i] - 0.75,
                 textx[j] + 0.95, texty[i] + 0.25,
                 col = "white", border = NA)
        } else{
          ## Highlight every cell according to highmat
          curcol = highmat[rows[i], cols[j]][[1]]
          if(!all(is.na(curcol))){
            for(colpart in 1:length(curcol))
              rect(textx[j] - 0.05 + (colpart - 1) * 1/length(curcol),
                   texty[i] - 0.75,
                   textx[j] - 0.05 + colpart * 1/length(curcol),
                   texty[i] + 0.25,
                   col = curcol[colpart],
                   border = NA)
          } else if(!is.na(curcell))
            rect(textx[j] - 0.05, texty[i] - 0.75,
                 textx[j] + 0.95, texty[i] + 0.25,
                 col = "white", border = NA)
        }
        ## Check if it might be a number, in which case format appropriately
        ## By default, digits = 0 (so rounded to whole number)
        ccnumer = suppressWarnings(as.numeric(curcell))
        if(!is.na(ccnumer) && !any(j == colNotNum)){
          curcell = formatC(ccnumer, format = "f", big.mark = ",", digits = digits)
          ## Then draw cell (right-just)
          text(textx[j] + 0.9, texty[i], curcell, adj = c(1, 0))
        } else{
          ## Then draw cell (left-just)
          text(textx[j], texty[i], curcell, adj = c(0, 0))
        }
      }
    }

    ## Draw bounding boxes (intelligently)
    ## Rows always drawn
    segments(xlim[1], -1:length(rows) + 0.5, xlim[2], col = bordercol)
    ## Cols only drawn if it doesn't clash with a cell
    segments(c(0.5 - c(labwidth, 0), xlim[2]), ylim[2], y1 = ylim[1],
             col = bordercol)
    ## Compute overflow of cells into next col
    overfl = apply(datamat, 2, function(x){
      out = strwidth(x) + 0.1
      out[is.na(x)] = NA
      out
    })
    ## Construct overflow logical, taking into account various rules
    overflT = matrix(FALSE, nrow = nrow(overfl), ncol = ncol(overfl))
    for(j in 1:(ncol(overfl) - 1)){
      for(i in 1:nrow(overfl))
        ## If current cell is not empty
        ##    AND next cell is empty,
        ## Then, If current cell > 1
        ## compute max cells overflow can occur and loop
        if(!is.na(overfl[i, j]) && is.na(overfl[i, j + 1]))
          if(overfl[i, j] > 1){
            maxoverfl = min(floor(overfl[i, j]), ncol(overfl) - j)
            for(addj in 1:maxoverfl)
              if(all(is.na(overfl[i, (j + 1):(j + addj)])))
                overflT[i, j - 1 + addj] = TRUE
          }
      ## If no overflow, draw single line
      ## Else draw piece-wise
      if(all(!overflT[, j]))
        segments(j + 0.5, ylim[2], y1 = ylim[1], col = bordercol)
      else{
        ystarts = c(ylim[2], (1:nrow(overfl))[overflT[,j]] + 0.5)
        yends = c((1:nrow(overfl))[overflT[,j]] - 0.5, ylim[1])
        segments(j + 0.5, ystarts, y1 = yends, col = bordercol)
      }
    }
    
  }

drawTableSize =
  ## Given a datamat, compute the appropriate device size
  ##  for a good looking table using drawTable.
  ## Args:
  ## --datamat, rows, cols--
  ## same as drawTable
  ## --cellwidth--
  ## width of the cells in in
  ## --labwidth--
  ## width of row label column, as a proportion of cellwidth
  function(datamat, rows = NULL, cols = NULL, cellwidth = 0.9,
           labwidth = 0.5){
    datamat = as.matrix(datamat)
    if(is.null(rows))
      rows = 1:nrow(datamat)
    else if(max(rows) > nrow(datamat)){
      extramat = matrix("", nrow = max(rows) - nrow(datamat),
        ncol = ncol(datamat))
      datamat = rbind(datamat, extramat)
    }
    if(is.null(cols))
      cols = 1:ncol(datamat)
    else if(max(cols) > ncol(datamat)){
      extramat = matrix("", nrow = nrow(datamat),
        ncol = max(cols) - ncol(datamat))
      datamat = cbind(datamat, extramat)
    }

    nlines = (length(rows) + 1) * 1.1
    height = nlines * par("csi")
    width = (length(cols) + labwidth) * cellwidth
    dev.off()
    list(width = width, height = height)
  }

drawTableFront =
  ## Front-end that takes care of sizing
  ## if pdfout != NULL, output will be to pdf
  function(datamat, rows = NULL, cols = NULL, ...,
           cellwidth = 0.9, labwidth = 0.5, pdfout = NULL){
    size = drawTableSize(datamat, rows = rows, cols = cols,
      cellwidth = cellwidth, labwidth = labwidth)
    if(!is.null(pdfout)){
      pdf(pdfout, width = size$width, height = size$height)
      on.exit(dev.off())
    } else{
      windows(width = size$width, height = size$height)
    }
    drawTable(datamat, rows = rows, cols = cols, labwidth = labwidth, ...)
  }
