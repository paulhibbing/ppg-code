# Setup -------------------------------------------------------------------


  rm(list = ls())
  library(magrittr)
  
  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  setwd()

  features <- readRDS("4c_Features.rds")
  
  source("zz_Internal.R")

  
# Determine scaling factors -----------------------------------------------

  
  set.seed(1919)

  d <-
    "data-raw/physionet_wrist/rds" %>%
    list.files(full.names = TRUE) %>%
    purrr::map_df(read_format, output = "raw") %>%
    dplyr::select(id, activity, ppg_filt:gyro_filt)

  aggregate(ppg_filt~id+activity, d, length) %>%
  {min(.$ppg_filt)}
  # [1] 56321  ---> 3.7 minutes

  d %>%
  dplyr::group_by(id, activity) %>%
  dplyr::slice_sample(n = 56321) %>%
  dplyr::ungroup(.) %>%
  dplyr::select(where(is.numeric)) %>%
  scale(.) %>%
  {list(
    center = attr(., "scaled:center"),
    scale = attr(., "scaled:scale")
  )} %>%
  dput(.)

  # list(
  #   center = c(
  #     ppg_filt = 1218.15668986436,
  #     acc_filt = 0.936966123780022,
  #     gyro_filt = -0.413754202047323
  #   ),
  #   scale = c(
  #     ppg_filt = 208.067893274326,
  #     acc_filt = 2.739427216933,
  #     gyro_filt = 33.4674666736241
  #   )
  # )

  
# Determine balancing scheme ----------------------------------------------


  ## SUMMARY: Reduces dataset from 839 epochs to 636 (75.8%) to ensure
  ##          even representation of each activity as well as each
  ##          participant within activities, to the extent possible

  set.seed(1919)

  initial_n <-
    features %>%
    dplyr::group_by(id, activity) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = activity, values_from = n) %>%
    janitor::adorn_totals(.)

  target_n <-
    initial_n[ ,-1] %>%
    utils::tail(1) %>%
    min(.)

  final_n <-
    utils::head(initial_n, -1) %>%
    tidyr::pivot_longer(!id, names_to = "activity", values_to = "n_epochs") %>%
    dplyr::group_split(activity) %>%
    lapply(na.exclude) %>%
    purrr::map_df(
      function(x, target_n) {
        while (sum(x$n_epochs) != target_n) {
          target_row <- nnet::which.is.max(x$n_epochs)
          x$n_epochs[target_row] %<>% {. - 1}
        }
        x
      },
      target_n = target_n
    )

  
      ## Check epochs per participant:
  
      final_n %>%
      tidyr::pivot_wider(names_from = activity, values_from = n_epochs) %>%
      janitor::adorn_totals(.)
      
      #    id Bike High Bike Low Run Walk
      #    s1        73       32  NA   26
      #    s2        51       32  NA   27
      #    s3        35       32  32   27
      #    s5        NA       31  32   NA
      #    s6        NA       32  32   26
      #    s4        NA       NA  31   NA
      #    s8        NA       NA  32   27
      #    s9        NA       NA  NA   26
      # Total       159      159 159  159
  

  features %>%
  dplyr::mutate(include = 1:dplyr::n()) %>%
  dplyr::group_split(id, activity) %>%
  purrr::map(
    function(x, final_n) {
      final_n %>%
      dplyr::filter(
        id == unique(x$id),
        activity == unique(x$activity)
      ) %T>%
      {stopifnot(nrow(.) == 1)} %>%
      {.$n_epochs} %>%
      dplyr::slice_sample(x, n = .) %>%
      {.$include}
    },
    final_n = final_n
  ) %>%
  do.call(c, .) %>%
  sort(.) %>%
  dput(.)

  # c(
  #   1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
  #   15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L,
  #   28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L,
  #   41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L,
  #   54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L,
  #   67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 79L, 81L, 82L, 83L,
  #   84L, 85L, 86L, 88L, 89L, 90L, 95L, 98L, 99L, 100L, 104L, 107L,
  #   109L, 110L, 111L, 116L, 119L, 124L, 126L, 128L, 129L, 131L, 137L,
  #   142L, 143L, 144L, 146L, 151L, 153L, 156L, 160L, 162L, 169L, 171L,
  #   172L, 174L, 177L, 182L, 183L, 187L, 188L, 189L, 196L, 197L, 199L,
  #   200L, 204L, 205L, 207L, 211L, 214L, 218L, 219L, 220L, 221L, 222L,
  #   223L, 224L, 225L, 226L, 227L, 228L, 229L, 230L, 231L, 232L, 233L,
  #   234L, 235L, 236L, 237L, 238L, 239L, 240L, 241L, 242L, 243L, 244L,
  #   245L, 246L, 247L, 248L, 249L, 250L, 251L, 252L, 253L, 254L, 255L,
  #   256L, 257L, 258L, 259L, 260L, 261L, 262L, 263L, 264L, 265L, 266L,
  #   267L, 268L, 269L, 271L, 273L, 275L, 276L, 277L, 279L, 280L, 281L,
  #   282L, 283L, 285L, 286L, 288L, 289L, 291L, 292L, 293L, 294L, 295L,
  #   296L, 297L, 298L, 299L, 300L, 301L, 302L, 303L, 306L, 307L, 309L,
  #   310L, 311L, 312L, 313L, 317L, 318L, 321L, 322L, 324L, 326L, 327L,
  #   328L, 330L, 331L, 332L, 333L, 334L, 337L, 339L, 340L, 341L, 347L,
  #   348L, 351L, 355L, 357L, 358L, 359L, 360L, 361L, 362L, 363L, 364L,
  #   365L, 366L, 367L, 368L, 369L, 370L, 371L, 372L, 373L, 374L, 375L,
  #   376L, 377L, 378L, 379L, 380L, 381L, 382L, 383L, 384L, 385L, 386L,
  #   387L, 388L, 389L, 390L, 391L, 392L, 393L, 394L, 395L, 396L, 397L,
  #   399L, 400L, 403L, 404L, 405L, 406L, 407L, 408L, 409L, 410L, 411L,
  #   412L, 413L, 414L, 415L, 416L, 417L, 418L, 420L, 421L, 422L, 423L,
  #   424L, 425L, 426L, 427L, 428L, 429L, 430L, 431L, 432L, 433L, 434L,
  #   436L, 437L, 438L, 439L, 440L, 441L, 442L, 443L, 444L, 445L, 447L,
  #   449L, 450L, 452L, 453L, 454L, 456L, 457L, 458L, 459L, 460L, 461L,
  #   462L, 463L, 464L, 465L, 466L, 467L, 468L, 470L, 471L, 472L, 473L,
  #   475L, 477L, 478L, 479L, 481L, 482L, 483L, 485L, 486L, 487L, 488L,
  #   489L, 493L, 494L, 495L, 496L, 498L, 499L, 500L, 501L, 502L, 503L,
  #   504L, 506L, 507L, 508L, 509L, 510L, 511L, 512L, 514L, 516L, 517L,
  #   519L, 520L, 521L, 522L, 523L, 524L, 525L, 526L, 527L, 528L, 529L,
  #   530L, 531L, 532L, 533L, 534L, 535L, 536L, 538L, 539L, 540L, 541L,
  #   542L, 543L, 544L, 545L, 546L, 547L, 548L, 549L, 550L, 551L, 552L,
  #   553L, 554L, 555L, 556L, 557L, 558L, 559L, 560L, 562L, 563L, 564L,
  #   565L, 566L, 567L, 568L, 569L, 571L, 572L, 574L, 576L, 577L, 578L,
  #   579L, 580L, 581L, 582L, 583L, 584L, 585L, 588L, 589L, 590L, 591L,
  #   592L, 593L, 594L, 595L, 598L, 599L, 600L, 602L, 603L, 604L, 605L,
  #   606L, 607L, 608L, 609L, 610L, 611L, 613L, 615L, 616L, 617L, 618L,
  #   619L, 620L, 621L, 622L, 623L, 624L, 625L, 626L, 627L, 628L, 629L,
  #   630L, 631L, 632L, 633L, 634L, 635L, 637L, 638L, 639L, 640L, 641L,
  #   643L, 644L, 645L, 646L, 647L, 648L, 649L, 650L, 651L, 652L, 653L,
  #   655L, 657L, 658L, 659L, 661L, 663L, 664L, 666L, 667L, 668L, 669L,
  #   670L, 671L, 672L, 673L, 674L, 675L, 676L, 677L, 678L, 679L, 680L,
  #   681L, 682L, 683L, 684L, 685L, 686L, 688L, 691L, 692L, 693L, 694L,
  #   695L, 697L, 699L, 701L, 702L, 703L, 704L, 705L, 706L, 708L, 712L,
  #   713L, 714L, 716L, 719L, 722L, 724L, 725L, 726L, 727L, 728L, 729L,
  #   730L, 732L, 733L, 734L, 735L, 736L, 737L, 738L, 739L, 740L, 742L,
  #   743L, 744L, 745L, 746L, 747L, 748L, 749L, 750L, 751L, 752L, 754L,
  #   755L, 756L, 757L, 758L, 759L, 760L, 761L, 762L, 763L, 764L, 765L,
  #   766L, 769L, 770L, 771L, 772L, 774L, 775L, 776L, 779L, 780L, 782L,
  #   784L, 785L, 787L, 789L, 798L, 799L, 801L, 802L, 805L, 806L, 807L,
  #   809L, 811L, 813L, 814L, 815L, 816L, 817L, 818L, 819L, 820L, 821L,
  #   822L, 823L, 824L, 825L, 826L, 827L, 828L, 830L, 831L, 832L, 833L,
  #   834L, 835L, 836L, 837L, 838L, 839L
  # )
