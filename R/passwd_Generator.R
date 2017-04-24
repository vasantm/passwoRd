passwd_generator <-
  function(len = 16, digits = 3, caps = 3, no_letters = 8, spc = 2) {
    seed <- as.integer(format(Sys.time(), "%Y%m%d"))
    pi.mult <- as.character(pi * seed)
    div <- sample(1:no_letters, 1)
    first_letters <- no_letters %% div
    sec_letters <- no_letters - div
    tmp.passwd <- c(
      sample(c(0:9), digits),
      sample(LETTERS, caps),
      sample(letters, first_letters),
      sample(c("#", "$", "%", "^", "@", "!"), 2),
      sample(letters, sec_letters),
      substring(pi.mult, first = 2, last = 4)
    )
    passwd <-
      paste0(tmp.passwd[sample(1:length(tmp.passwd), length(tmp.passwd))],
             collapse = "")
    return(passwd)
  }
