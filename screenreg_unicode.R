#' @import texreg
screenreg_unicode <- function(l,
                       file = NULL,
                       single.row = FALSE,
                       stars = c(0.001, 0.01, 0.05),
                       custom.header = NULL,
                       custom.model.names = NULL,
                       custom.coef.names = NULL,
                       custom.coef.map = NULL,
                       custom.gof.names = NULL,
                       custom.gof.rows = NULL,
                       custom.note = NULL,
                       digits = 2,
                       leading.zero = TRUE,
                       star.symbol = "*",
                       symbol = ".",
                       override.coef = 0,
                       override.se = 0,
                       override.pvalues = 0,
                       override.ci.low = 0,
                       override.ci.up = 0,
                       omit.coef = NULL,
                       reorder.coef = NULL,
                       reorder.gof = NULL,
                       ci.force = FALSE,
                       ci.force.level = 0.95,
                       ci.test = 0,
                       groups = NULL,
                       custom.columns = NULL,
                       custom.col.pos = NULL,
                       column.spacing = 2,
                       outer.rule = "=",
                       inner.rule = "-",
                       ...) {
  # matrixreg produces the output matrix
  output.matrix <- texreg::matrixreg(
    l,
    single.row = single.row,
    stars = stars,
    custom.model.names = custom.model.names,
    custom.coef.names = custom.coef.names,
    custom.coef.map = custom.coef.map,
    custom.gof.names = custom.gof.names,
    custom.gof.rows = custom.gof.rows,
    digits = digits,
    leading.zero = leading.zero,
    star.symbol = star.symbol,
    symbol = symbol,
    override.coef = override.coef,
    override.se = override.se,
    override.pvalues = override.pvalues,
    override.ci.low = override.ci.low,
    override.ci.up = override.ci.up,
    omit.coef = omit.coef,
    reorder.coef = reorder.coef,
    reorder.gof = reorder.gof,
    ci.force = ci.force,
    ci.force.level = ci.force.level,
    ci.test = ci.test,
    groups = groups,
    custom.columns = custom.columns,
    custom.col.pos = custom.col.pos,
    include.attributes = TRUE,
    trim = FALSE,
    ...
  )

  gof.names <- attr(output.matrix, "gof.names")
  coef.names <- attr(output.matrix, "coef.names")
  mod.names <- attr(output.matrix, "mod.names")
  coltypes <-
    texreg::customcolumnnames(mod.names, custom.columns, custom.col.pos, types = TRUE)
  ci <- attr(output.matrix, "ci")
  ci.test <- attr(output.matrix, "ci.test")

  # add spaces
  for (j in 1:ncol(output.matrix)) {
    nc <- nchar(output.matrix[, j], "width")
    width <- max(nc)
    for (i in 1:nrow(output.matrix)) {
      spaces <- paste(rep(" ", width - nc[i]), collapse = "")
      output.matrix[i, j] <- paste0(output.matrix[i, j], spaces)
    }
  }

  string <- "\n"

  # horizontal rule above the table
  table.width <- sum(nchar(output.matrix[1,], "width")) +
    (ncol(output.matrix) - 1) * column.spacing
  if (!is.character(outer.rule)) {
    stop("outer.rule must be a character.")
  } else if (nchar(outer.rule) > 1) {
    stop("outer.rule must be a character of maximum length 1.")
  } else if (outer.rule == "") {
    o.rule <- ""
  } else {
    o.rule <- paste(rep(outer.rule, table.width), collapse = "")
    string <- paste0(string, o.rule, "\n")
  }

  # specify multicolumn header
  spacing <- paste(rep(" ", column.spacing), collapse = "")
  mod.names <- c("", mod.names)
  if (!is.null(custom.header) &&
      length(custom.header) > 0 && !any(is.na(custom.header))) {
    if (!"list" %in% class(custom.header) ||
        length(custom.header) >= length(mod.names) ||
        is.null(names(custom.header)) ||
        !all(sapply(custom.header, is.numeric))) {
      stop("'custom.header' must be a named list of numeric vectors.")
    }
    ch <- unlist(custom.header)
    for (i in 1:length(ch)) {
      if (is.na(ch[i])) {
        stop(
          "NA values are not permitted in 'custom.header'. Try leaving out the model indices that should not be included in the custom header."
        )
      }
      if (ch[i] %% 1 != 0) {
        stop("The model column indices in 'custom.header' must be provided as integer values.")
      }
      if (ch[i] < 1 || ch[i] >= length(mod.names)) {
        stop(
          "The model column indices in 'custom.header' must be between 1 and the number of models."
        )
      }
      if (i > 1 && ch[i] <= ch[i - 1]) {
        stop("The model column indices in 'custom.header' must be strictly increasing.")
      }
    }

    ch <-
      rules <-
      paste0(rep(" ", nchar(output.matrix[1, 1])), collapse = "") # multicolumn header labels and mid-rules
    counter <-
      0  # keeps track of how many columns we have processed to the left of the current start column
    for (i in 1:length(custom.header)) {
      # check if there are gaps within multicolumn blocks and throw error
      if (length(custom.header[[i]]) != custom.header[[i]][length(custom.header[[i]])] - custom.header[[i]][1] + 1) {
        stop(
          "Each item in 'custom.header' must have strictly consecutive column indices, without gaps."
        )
      }

      # find out corrected column indices (ignoring the coef label column) of the current model after taking into account custom columns
      numCoefCol <- 0
      numCustomCol <- 0
      for (j in 1:length(coltypes)) {
        if (coltypes[j] == "coef") {
          numCoefCol <- numCoefCol + 1
        } else if (coltypes[j] == "customcol") {
          numCustomCol <- numCustomCol + 1
        }
        if (numCoefCol == custom.header[[i]][1]) {
          break()
          # break the loop if we have reached the number of models so far
        }
      }
      startIndex <-
        numCoefCol + numCustomCol # corrected column index
      numCoefCol <- 0
      numCustomCol <- 0
      for (j in 1:length(coltypes)) {
        if (coltypes[j] == "coef") {
          numCoefCol <- numCoefCol + 1
        } else if (coltypes[j] == "customcol") {
          numCustomCol <- numCustomCol + 1
        }
        if (numCoefCol == custom.header[[i]][length(custom.header[[i]])]) {
          break()
          # break the loop if we have reached the number of models so far
        }
      }
      stopIndex <-
        numCoefCol + numCustomCol # corrected column index

      # add empty cells for gaps and custom text columns
      if (startIndex > counter + 1) {
        spaces <- ""
        for (j in (startIndex):(counter + 2)) {
          spaces <-
            paste0(spaces, spacing, paste0(rep(" ", nchar(
              output.matrix[1, j]
            )), collapse = ""))
          counter <- counter + 1
        }
        ch <- paste0(ch, spaces)
        rules <- paste0(rules, spaces)
      }

      # add multicolumn cells (+ 1 is for the coefficient label column)
      chars <- 0
      for (j in (startIndex + 1):(stopIndex + 1)) {
        chars <- chars + nchar(output.matrix[1, j]) + column.spacing
      }
      chars <-
        chars - column.spacing # last column does not have extra spacing
      label <- names(custom.header)[i]
      chars_label <- chars - nchar(label)
      if (chars_label < 0) {
        label <- substr(label, 1, chars)
      }
      before <- after <- (chars - nchar(label)) / 2
      if (before %% 1 != 0) {
        before <- before + 0.5
        after <- after - 0.5
      }
      ch <- paste0(ch,
                   spacing,
                   paste0(rep(" ", before), collapse = ""),
                   label,
                   paste0(rep(" ", after), collapse = ""))
      rules <- paste0(rules,
                      spacing,
                      paste0(rep(inner.rule, chars), collapse = ""))
      counter <- counter + stopIndex - startIndex + 1
    }
    string <- paste0(string, ch, "\n", rules, "\n")
  }

  # specify model names
  string <- paste(string, output.matrix[1, 1], sep = "")
  for (i in 2:ncol(output.matrix)) {
    string <- paste0(string, spacing, output.matrix[1, i])
  }
  string <- paste0(string, "\n")

  # mid rule 1
  if (!is.character(inner.rule)) {
    stop("inner.rule must be a character.")
  } else if (nchar(inner.rule) > 1) {
    stop("inner.rule must be a character of maximum length 1.")
  } else if (inner.rule == "") {
    i.rule <- ""
  } else {
    i.rule <- paste(rep(inner.rule, table.width), collapse = "")
    string <- paste0(string, i.rule, "\n")
  }

  # write coefficients
  for (i in 2:(length(output.matrix[, 1]) - length(gof.names))) {
    for (j in 1:length(output.matrix[1,])) {
      string <- paste0(string, output.matrix[i, j])
      if (j == length(output.matrix[1,])) {
        string <- paste0(string, "\n")
      } else {
        string <- paste0(string, spacing)
      }
    }
  }

  if (length(gof.names) > 0) {
    # mid rule 2
    if (inner.rule != "") {
      string <- paste0(string, i.rule, "\n")
    }

    # write GOF part of the output matrix
    for (i in (length(output.matrix[, 1]) - (length(gof.names) - 1)):(length(output.matrix[, 1]))) {
      for (j in 1:length(output.matrix[1,])) {
        string <- paste0(string, output.matrix[i, j])
        if (j == length(output.matrix[1,])) {
          string <- paste0(string, "\n")
        } else {
          string <- paste0(string, spacing)
        }
      }
    }
  }

  # write table footer
  if (outer.rule != "") {
    string <- paste0(string, o.rule, "\n")
  }

  # stars note
  snote <- texreg::get_stars_note(
    stars = stars,
    star.symbol = star.symbol,
    symbol = symbol,
    ci = ci,
    ci.test = ci.test,
    output = "ascii"
  )

  # custom note
  if (is.null(custom.note)) {
    note <- paste0(snote, "\n")
  } else if (custom.note == "") {
    note <- ""
  } else {
    note <- paste0(custom.note, "\n")
    note <- gsub("%stars", snote, note)
  }
  string <- paste0(string, note)

  #write to file
  if (is.null(file) || is.na(file)) {
    class(string) <- c("character", "texregTable")
    return(string)
  } else if (!is.character(file)) {
    stop("The 'file' argument must be a character string.")
  } else {
    sink(file)
    cat(string)
    sink()
    message(paste0("The table was written to the file '", file, "'.\n"))
  }
}
