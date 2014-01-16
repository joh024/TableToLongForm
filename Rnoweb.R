Rnoweb = local({
    chunkStarts =
        function(lines)
        grep("(^<<[-. 0-9A-Za-z]*>>=)|(^\\@ )|(^\\@$)", lines)
    chunkName =
        function(chunkstart)
        ifelse(grepl("(^\\@ )|(^\\@$)", chunkstart), "",
               gsub("^ +", "",
                    gsub(" *>>.*$", "",
                         gsub("^.*<< *", "",
                              chunkstart))))
    chunkLabelPrefix =
        function(name, filename, fileno) {
            number = as.numeric(factor(name, unique(name)))
            ifelse(name == "", "",
                   paste("RNW",
                         substring(gsub(" ", "", filename), 1, 3),
                         fileno, "-",
                         substring(gsub(" ", "", name), 1, 3),
                         number,
                         sep = ""))
        }
    chunkModdef =
        function(prefix)
        ifelse(prefix == "", "", paste(prefix, 1, sep = "-"))
    chunkSublabel =
        function(prefix) {
            index = seq(along = prefix)
            fprefix = factor(prefix, unique(prefix))
            index[order(fprefix)] =
                sequence(table(fprefix))
            ifelse(prefix == "", "", 
                   paste(prefix, index, sep = "-"))
    }
    includedChunks =
        function(chunk)
        gsub("^ +", "",
             gsub(" *>>.*$", "",
                  gsub("^.*<< *", "",
                       grep("^ *<<[-. 0-9A-Za-z]* *>>",
                            chunk, value = TRUE))))
    chunkUsesChunks =
        function(lines, start, end, type) {
            uses = vector("list", length(start))
            for(i in 1:length(start))
                if (type[i] == "doc")
                    uses[[i]] = character()
                else
                    uses[[i]] =
                        includedChunks(lines[start[i]:end[i]])
            uses
        }               
    chunkUsesSublabels =
        function(uses, names, sublabels)
        lapply(uses, function(u)
               sublabels[match(u, names)])
    chunkIsUsedInChunks =
        function(names, uses, sublabel) {
            usedin = vector("list", length(names))
            for(i in 1:length(names)) {
                usedin[[i]] = sublabel[sapply(uses,
                          function(u) any(u == names[i]))]
            }
            usedin            
        }
    chunkDefines =
        function(header, type) {
            defs = c(header[-1], "")
            defs = ifelse(grepl("^@ +%def +", defs), defs, "")
            defs = ifelse(type == "code" &
                          c(type[-1], "code") == "doc",
                sub(" *$", "", 
                    sub("^@ +%def +", "", defs)), "")
            strsplit(defs, " +")
        }
    chunkUsesDefines =
        function(lines, start, end, type, define) {
            usesdefines = vector("list", length(start))
            for(i in 1:length(start)) {
                if (type[i] == "code") {
                    chunk = lines[start[i]:end[i]]
                    defs = unlist(define[-i])
                    pats = paste("(^|[^a-zA-Z0-9._])",
                        gsub("\\.", "\\\\.", defs),
                        "($|[^a-zA-Z0-9._])", sep = "")
                    usesdefines[[i]] = sort(defs[sapply(pats,
                                   function(p)
                                   any(grepl(p, chunk)))])
                }
                else usesdefines[[i]] = character()
            }
            usesdefines
        }
    extractChunkInfo =
        function(lines, filename, fileno) {    
            start = chunkStarts(lines)
            end = c(start[-1] - 1, length(lines))
            header = lines[start] 
            name = chunkName(header)
            type = ifelse(name == "", "doc", "code")
            prefix = chunkLabelPrefix(name, filename, fileno)
            moddef = chunkModdef(prefix)
            sublabel = chunkSublabel(prefix)
            uses = chunkUsesChunks(lines, start + 1, end, type)
            usessublabels = chunkUsesSublabels(uses, name, sublabel)
            usedin = chunkIsUsedInChunks(name, uses, sublabel)
            defines = chunkDefines(header, type)
            usesdefines = chunkUsesDefines(lines, start + 1, end,
                type, defines)
            info = vector("list", length = length(start))
            for(i in 1:length(start))
                info[[i]] =
                    list(number = i,
                         type = type[i],
                         name = name[i],
                         moddef = moddef[i],
                         sublabel = sublabel[i],
                         start = start[i],
                         end = end[i],
                         uses = uses[[i]],
                         usessublabels = usessublabels[[i]],
                         usedin = usedin[[i]],
                         defines = defines[[i]],
                         usesdefines = usesdefines[[i]])
            
            info
        }
    weaveFile =
        function(lines, info, filename, fileno) {
            lastcode = max(which(sapply(info,
                function(i) i$type) == "code"))
            file = openWeaveFile(filename)
            start = info[[1]]$start
            if (start > 1)
                weaveInitial(lines[1:(start - 1)], file)
            for (i in seq(along = info)) {
                if (i == 1) weaveFilename(filename, file)
                if (info[[i]]$type == "doc")
                    weaveDoc(lines, info[[i]], file)
                else
                    weaveCode(lines, info[[i]], file)
                if (i == lastcode) {
                    weaveChunkIndex(info, file)
                    weaveIdentifierIndex(info, file)
                }
            }
            weaveNewline(file = file)
            close(file)
        }
    openWeaveFile =
        function(filename) {
            texfilename = sub("\\.[^.]*$", ".tex", filename)
            file(texfilename, "w")
        }
    weaveDocLine =
        function(line)
        gsub("\\[\\[(.*?)\\]\\]", "\\\\verb?\\1?", line)
    weaveInitial =
        function(lines,  file) {
            for(i in 1:length(lines))
                cat(weaveDocLine(lines[i]), "\n",
                    sep = "", file = file)
        }
    weaveDoc =
        function(lines, info, file) {
            with(info, {
                chunk = lines[start:end]
                weaveBeginDoc(number, file)
                for(i in 2:length(chunk))
                    cat(weaveDocLine(chunk[i]), "\n",
                        sep = "", file = file)
                weaveEndDoc(file)
            })
        }
    isInsert =
        function(line) {
            if (grepl("<<[-. 0-9A-Za-z]*>>", line))
                gsub("^ +", "",
                     gsub(" *>>.*$", "",
                          gsub("^.*<< *", "",
                               grep("^ *<<[-. A-Za-z]* *>>",
                                    line, value = TRUE))))
            else
                NULL
        }
    weaveCode =
        function(lines, info, file) {
            with(info, {
                chunk = lines[(start + 1):end]
                unused = length(usedin) == 0
                weaveBeginCode(number, name, moddef, sublabel, file)
                if (length(usedin) == 0)
                    weaveNotUsedHeader(file)
                weaveNewline(file)
                for(i in 1:length(chunk))
                    if (is.null((insert = isInsert(chunk[i]))))
                        weaveCodeLine(chunk[i], file = file)
                    else
                        weaveInsert(chunk[i], insert,
                                    usessublabels[uses == insert],
                                    file)
                if (number == 1)
                    cat("\\nosublabel{", sublabel, "-u4}",
                        sep = "", file = file)
                weaveDefines(defines, sublabel, file)
                weaveDefineUses(usesdefines, sublabel, file)
                if (length(usedin) == 0)
                    weaveNotUsedChunk(name, file)
                ##  if (notused) notusedfile(name, file)
                weaveEndCode(file)
                })
        }
    weaveFilename =
        function(filename, file)
        cat("\\nwfilename{", filename, "}", sep = "",
            file = file)
    weaveBeginDoc =
        function(n, file)
        cat("\\nwbegindocs{", n, "}\\nwdocspar\n",
            sep = "", file = file)
    weaveEndDoc =
        function(file)
        cat("\\nwenddocs{}", file = file)
    weaveBeginCode =
        function(n, name, moddef, sublabel, file)
        cat(paste("\\nwbegincode{", n, "}",
                  "\\sublabel{", sublabel, "}",      
                  "\\nwmargintag{{\\nwtagstyle{}\\subpageref{",
                  sublabel, "}}}", "\\moddef{", name,
                  "~{\\nwtagstyle{}\\subpageref{", moddef, "}}}\\",
                  if(sublabel != moddef) "plus" else "",
                  "endmoddef", sep = ""),
            file = file)
    weaveNotUsedHeader =
        function(file)
        cat("\\let\\nwnotused=\\nwoutput{}", file = file)
    weaveDefines =
        function(defines, sublabel, file)
        if (length(defines) > 0) {
            cat(paste("\\nwindexdefn{", defines, "}{",
                      defines, "}{", sublabel, "}", sep = ""),
                "\\eatline\n", sep = "", file = file)
            cat("\\nwidentdefs{",paste("\\\\{{", defines, "}{",
                                       defines,"}}", sep = ""),
                "}", sep = "", file = file)
        }
    weaveDefineUses =
        function(usesdefines, sublabel, file) {
            if (length(usesdefines) > 0) {
                cat("\\nwidentuses{",
                    paste("\\\\{{", usesdefines, "}{",
                          usesdefines, "}}", sep = ""),
                    "}", sep = "", file = file)
                cat(paste("\\nwindexuse{",
                          usesdefines, "}{", usesdefines,
                          "}{", sublabel, "}", sep = ""),
                    sep = "", file = file)
                }
            }
    weaveNotUsedChunk =
        function(name, file)
        cat(paste("\\nwnotused{", name, "}", sep = ""),
            file = file)
    weaveEndCode =
        function(file)
        cat("\\nwendcode{}", file = file)
    weaveNewline =
        function(file)
        cat("\n", file = file)
    weaveInsert =
        function(line, name, sublabel, file) {
            indent = gsub("( *).*", "\\1", line)
            cat(indent, "\\LA{}", name,
                "~{\\nwtagstyle{}\\subpageref{",
                sublabel, "}}\\RA{}\n", sep = "", file = file)
        }
    weaveCodeLine =
        function(line, file)
        cat(gsub("\\{", "\\\\{",
                 gsub("\\}", "\\\\}",
                      gsub("\\\\", "\\\\\\\\",
                           gsub("@<<", "<<",
                                line)))), "\n",
            sep = "", file = file)
    weaveChunkIndex =
        function(info, file) {
            code= sapply(info, function(i) i$type) == "code"
            name = sapply(info, function(i) i$name)[code]
            sublabel = sapply(info, function(i) i$sublabel)[code]
            usedin = lapply(info, function(i) i$usedin)[code]
            o = order(name)
            name = name[o]
            sublabel = sublabel[o]
            usedin = sapply(usedin[o],
                function(u) {
                    if (length(u) == 0) "" else
                    paste("\\nwixu{", u, "}",
                          sep = "", collapse = "")
                    })
            cat("\n", file = file)
            cat(paste("\\nwixlogsorted{c}{{", name,
                "}{", sublabel, "}{\\nwixd{", sublabel, "}",
                usedin, "}}%\n", sep = ""), sep = "", file = file)
        }
    weaveIdentifierIndex =
        function(info, file) {
            code= sapply(info, function(i) i$type) == "code"
            defines = lapply(info, function(i) i$defines)[code]
            if (length(defines) > 0) {
                defines = unique(sort(unlist(defines)))
                cat(paste("\\nwixlogsorted{i}{{", defines,
                          "}{", defines, "}}%\n", sep = ""),
                    sep = "", file = file)
            }
        }
    tangleFile =
        function(lines, info, filename, fileno) {
            chunktable = new.env(parent = emptyenv())
            code = which(sapply(info, function(i) i$type) == "code")
            for(i in code)
                with(info[[i]],
                     storeChunk(name, lines[(start+1):end],
                                chunktable))
            if (!is.logical((x = sapply(info[code],
                                   function(i) length(i$used) == 0))))
                print(x)
            unusedChunks = code[sapply(info[code],
                function(i) length(i$used) == 0)]
            for(i in unusedChunks)
                with(info[[i]], {
                    filecon = file(name, "w")
                    expandChunk(name, "", chunktable, filecon)
                    close(filecon)
                })
        }
    storeChunk =
        function(name, lines, chunktable) {
            if (exists(name, chunktable, inherits = FALSE))
                lines = c(fetchChunk(name, chunktable), lines)
            assign(name, lines, envir = chunktable)
        }
    fetchChunk =
        function(name, chunktable)
        get(name, envir = chunktable)
    expandChunk =
        function(name, indent, chunktable, file) {
            for(line in fetchChunk(name, chunktable)) {
                if (grepl("^ *<<.*>> *$", line)) {
                    space = gsub("<<.*$", "", line)
                    expandChunk(chunkName(line),
                                paste(indent, space, sep = ""),
                                chunktable, file)
                }
                else 
                    cat(indent, gsub("@<<", "<<", line), "\n",
                        sep = "", file = file)
            }
        }
    function(files) {
        for(fileno in seq(along = files)) {
            filename = files[fileno]
            lines = readLines(filename)
            info = extractChunkInfo(lines, filename, fileno)
            weaveFile(lines, info, filename, fileno)
            tangleFile(lines, info, filename, fileno)
        }
    }
})
