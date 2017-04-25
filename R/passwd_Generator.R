passwd_generator <-
  function(len = 16, digits = 2, caps = 2, no_letters = 10, spc = 2) {
   # if(sum(digits,caps,no_letters,spc) > len)
    #  stop("Please ensure number of characters is less than length of password")
    if(len != 16){ #defalt and resultant passwd is always atleast 10 chars
      digits <- max(round((2/16) * len),2)
      caps <- max(round((2/16) * len),1)
      no_letters <- max(round((8/16) * len),4)
      spc <- max(round((2/16) * len),1)
      comp.len <- sum(digits,caps,no_letters,spc)
      print(paste0("Length of passwd: ",comp.len))
      if(comp.len != len) no_letters <-no_letters + (len - comp.len)
      print(paste0("length of letters: ", no_letters))
      recomp.len <- sum(digits,caps,no_letters,spc)
      print(paste0("Length of passwd recomp: ",recomp.len))
    }
    seed <- as.numeric(format(Sys.time(), "%Y%m%d%I%M%S"))
    set.seed(as.integer(format(Sys.time(), "%H%M%S%w%W")))
    randoms <-.Random.seed
    randoms.indx <-which(nchar(randoms)>5)

    rand.no.mult<-  seed * abs(randoms[sample(randoms.indx,1)]) # make a number to sample from

    print(paste0("random number: ",substring(rand.no.mult, first = 5, last = 6)))
    print(paste0("random number: ",substring(rand.no.mult, first = 7, last = 7)))
    print(paste0("random number: ",substring(rand.no.mult, first = nchar(rand.no.mult)-1,
                                             last = nchar(rand.no.mult)-1)))
    div <- 2
    first_letters <- no_letters %/% div
    sec_letters <- no_letters - first_letters


    tmp.passwd <- c(
      sample(c(0:9), digits),
      sample(LETTERS, caps),
      sample(letters, first_letters),
      sample(c("#", "$", "%", "^", "@", "!"), spc),
      sample(letters, sec_letters),
      substring(rand.no.mult, first = 5, last = 6),
      substring(rand.no.mult, first=7, last = 7),
      substring(rand.no.mult, first = nchar(rand.no.mult)-1,
                last = nchar(rand.no.mult))
    )
    passwd <-
      paste0(tmp.passwd[sample(1:length(tmp.passwd), length(tmp.passwd))],
             collapse = "")
    print(paste0("length of passwd: ", nchar(passwd)))
    return(passwd)
  }
