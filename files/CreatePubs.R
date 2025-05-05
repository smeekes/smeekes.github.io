
create_authors <- function(author, type = "html") {
  if (type == "html") {
    dotspace <- "."
  } else if (type == "tex") {
    dotspace <- ".\\"
  }
  n <- nrow(texts)
  authors <- strsplit(author, " and ")
  names_text <- rep("", n)
  for (i in 1:n) {
    authors_first_i <- strsplit(substring(authors[[i]], regexpr(",", authors[[i]]) + 2, 
                                          nchar(authors[[i]])), " ")
    authors_last_i <- substring(authors[[i]], 1, regexpr(",", authors[[i]]) - 1)
    ni <- length(authors_first_i)
    names_text[i] <- paste0(authors_last_i[1], ", ", 
                            create_initials(authors_first_i[[1]], dotspace))
    if (ni > 1) {
      for (j in 2:ni) {
        if (j == ni) {
          connect <- " and "
        } else {
          connect <- ", "
        }
        names_text[i] <- paste0(names_text[i], connect, 
                                create_initials(authors_first_i[[j]], dotspace),
                                " ", authors_last_i[j])
      }
    }
  }
  return(names_text)
}

create_initials <- function(txt, dotspace = ".") {
  hyphens <- grep("-", txt)
  n <- length(txt)
  initials <- ""
  for (i in 1:n) {
    if (i %in% hyphens) {
      initials <- paste0(initials, 
                         paste0(substring(strsplit(txt[i], "-")[[1]], 1, 1), dotspace, collapse = "-"),
                         " ")
    } else {
      initials <- paste0(initials, substring(txt[i], 1, 1), dotspace, " ")
    }
  }
  return(substring(initials, 1, nchar(initials) - 1))
}

create_journal_entry <- function(texts) {
  paste0("*", texts$journal, "*",
         ifelse(is.na(texts$volume), ", ", 
                paste0(" ", texts$volume, 
                       ifelse(is.na(texts$number), ",",
                              paste0(" (", texts$number, "), ")))),
         texts$pages)
}

create_archive_entry <- function(texts) {
  paste0(texts$type, " ", texts$number)
}

create_icon <- function(icon) {
  paste0("<i class=\"bi bi-", icon, "\"></i>")
}

create_link <- function(txt, urlt, icon = NULL, lsp = TRUE) {
  out <- ""
  if (!is.null(icon)) {
    if (lsp) {
      out <- paste0(out, "&nbsp;", create_icon(icon), " ")
    } else {
      out <- paste0(out, create_icon(icon), " ")
    }
  }
  out <- paste0(out, "[", txt, "](", urlt, ")")
  return(out)
}

create_spaces <- function(n) {
  paste0(rep("&nbsp; ", n), collapse = "")
}

create_article <- function(texts) {
  paste0(create_authors(texts$author), " (", texts$year, "). ", 
         create_link(texts$title, paste0("https://doi.org/", texts$doi)), ". ",
         create_journal_entry(texts), ".",
         ifelse(texts$OA == 1, paste0(" ", create_icon("unlock")), ""),
         "\n\n")
}

create_bookchapter <- function(texts, type = "html") {
  if (type == "html") {
    dotspace <- "."
  } else if (type == "tex") {
    dotspace <- ".\\"
  }
  paste0(create_authors(texts$author), " (", texts$year, "). ", 
         create_link(texts$title, paste0("https://doi.org/", texts$doi)), ". ", 
         "In ", create_authors(texts$editor), dotspace, "(Ed.), ",
         create_link(texts$booktitle, paste0("https://doi.org/", texts$doibook)),
         ", Chapter ", texts$chapter, ", pp.~", texts$pages,
         ifelse(nchar(texts$series) > 0, 
                paste0(". *", texts$series, "*, vol", dotspace, " ", texts$volume), ""),
         ifelse(nchar(texts$publisher) > 0, paste0(", ", texts$publisher, "")), ".",
         ifelse(texts$OA == 1, paste0(" ", create_icon("unlock")), ""),
         "\n\n")
}

create_wp <- function(texts) {
  paste0(create_authors(texts$author), " (", texts$year, "). ", 
         create_link(texts$title, paste0("https://doi.org/", texts$doi)), ". ",
         create_archive_entry(texts), ".", paste0(" ", create_icon("unlock")), "\n\n")
}

create_margin_article <- function(texts){
  n <- nrow(texts)
  elements <- rep("", n)
  nr_elements <- rep(0, n)
  for (i in 1:n) {
    elements_i <- NULL
    if (texts$OA[i] == 0){
      elements_i <- c(elements_i, create_link(texts$OAtype[i], texts$OAref[i], "unlock"))
    }
    if (!is.na(texts$cref[i])) {
      if (nchar(texts$cref[i]) > 0) {
        elements_i <- c(elements_i, create_link(texts$code[i], texts$cref[i], "code-slash"))
      }
    }
    if (!is.na(texts$cref[i])) {
      if (nchar(texts$repl[i]) > 0){
      elements_i <- c(elements_i, create_link("Replication", texts$repl[i], "arrow-repeat"))
      }
    }
    if (!is.na(texts$cref[i])) {
      if (nchar(texts$suppl[i]) > 0){
        elements_i <- c(elements_i, create_link("Supplement", texts$suppl[i], "file-text"))
      }
    }
    if (!is.na(texts$cref[i])) {
      if (nchar(texts$otherref[i]) > 0){
      elements_i <- c(elements_i, create_link(texts$othername[i], 
                                              texts$otherref[i], "file-earmark"))
      }
    }
    nr_elements[i] <- length(elements_i)
    elements[i] <- paste0(elements_i, collapse = "<br>")
    if (nchar(elements[i]) > 0) {
      elements[i] <- paste0("\n", "::: {.column-margin .bg-danger .border .rounded}\n",
                            elements[i], "\n:::\n\n")
    }
  }
  return(list(elements = elements, nr = nr_elements))
}

create_margin_wp <- function(texts){
  n <- nrow(texts)
  elements <- rep("", n)
  nr_elements <- rep(0, n)
  for (i in 1:n) {
    elements_i <- NULL
    if (nchar(texts$cref[i]) > 0){
      elements_i <- c(elements_i, create_link(texts$code[i], texts$cref[i], "code-slash"))
    }
    if (nchar(texts$repl[i]) > 0){
      elements_i <- c(elements_i, create_link("Replication", texts$repl[i], "arrow-repeat"))
    }
    nr_elements[i] <- length(elements_i)
    elements[i] <- paste0(elements_i, collapse = "<br>")
    if (nchar(elements[i]) > 0) {
      elements[i] <- paste0("\n", "::: {.column-margin .bg-danger .border .rounded}\n",
                            elements[i], "\n:::\n\n")
    }
  }
  return(list(elements = elements, nr = nr_elements))
}

create_markdown <- function(texts, type = "article") {
  if (type == "article") {
    margins <- create_margin_article(texts)
    papers <- create_article(texts)
  } else if (type == "wp") {
    margins <- create_margin_wp(texts)
    papers <- create_wp(texts)
  } else if (type == "chapter") {
    margins <- create_margin_article(texts)
    papers <- create_bookchapter(texts)
  }
  newlines <- sapply(margins$nr, function(i){
    ifelse(i > 2, paste0(paste0(rep("<br>", i - 2), collapse = ""), "\n\n"), "")
  })
  mdtext <- paste0(papers, margins$elements, "<br>\n\n", collapse = "")
}
