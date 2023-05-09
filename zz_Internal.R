# Scaling coefficients (see "zz_Misc.R") ----------------------------------


  .scaling_coefficients <- list(
    center = c(
      ppg_filt = 1218.15668986436,
      acc_filt = 0.936966123780022,
      gyro_filt = -0.413754202047323
    ),
    scale = c(
      ppg_filt = 208.067893274326,
      acc_filt = 2.739427216933,
      gyro_filt = 33.4674666736241
    )
  )


# Balanced indices (see "zz_Misc.R") --------------------------------------

  
  .balanced_indices <- c(
    1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
    15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L,
    28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L,
    41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L,
    54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L,
    67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 79L, 81L, 82L, 83L,
    84L, 85L, 86L, 88L, 89L, 90L, 95L, 98L, 99L, 100L, 104L, 107L,
    109L, 110L, 111L, 116L, 119L, 124L, 126L, 128L, 129L, 131L, 137L,
    142L, 143L, 144L, 146L, 151L, 153L, 156L, 160L, 162L, 169L, 171L,
    172L, 174L, 177L, 182L, 183L, 187L, 188L, 189L, 196L, 197L, 199L,
    200L, 204L, 205L, 207L, 211L, 214L, 218L, 219L, 220L, 221L, 222L,
    223L, 224L, 225L, 226L, 227L, 228L, 229L, 230L, 231L, 232L, 233L,
    234L, 235L, 236L, 237L, 238L, 239L, 240L, 241L, 242L, 243L, 244L,
    245L, 246L, 247L, 248L, 249L, 250L, 251L, 252L, 253L, 254L, 255L,
    256L, 257L, 258L, 259L, 260L, 261L, 262L, 263L, 264L, 265L, 266L,
    267L, 268L, 269L, 271L, 273L, 275L, 276L, 277L, 279L, 280L, 281L,
    282L, 283L, 285L, 286L, 288L, 289L, 291L, 292L, 293L, 294L, 295L,
    296L, 297L, 298L, 299L, 300L, 301L, 302L, 303L, 306L, 307L, 309L,
    310L, 311L, 312L, 313L, 317L, 318L, 321L, 322L, 324L, 326L, 327L,
    328L, 330L, 331L, 332L, 333L, 334L, 337L, 339L, 340L, 341L, 347L,
    348L, 351L, 355L, 357L, 358L, 359L, 360L, 361L, 362L, 363L, 364L,
    365L, 366L, 367L, 368L, 369L, 370L, 371L, 372L, 373L, 374L, 375L,
    376L, 377L, 378L, 379L, 380L, 381L, 382L, 383L, 384L, 385L, 386L,
    387L, 388L, 389L, 390L, 391L, 392L, 393L, 394L, 395L, 396L, 397L,
    399L, 400L, 403L, 404L, 405L, 406L, 407L, 408L, 409L, 410L, 411L,
    412L, 413L, 414L, 415L, 416L, 417L, 418L, 420L, 421L, 422L, 423L,
    424L, 425L, 426L, 427L, 428L, 429L, 430L, 431L, 432L, 433L, 434L,
    436L, 437L, 438L, 439L, 440L, 441L, 442L, 443L, 444L, 445L, 447L,
    449L, 450L, 452L, 453L, 454L, 456L, 457L, 458L, 459L, 460L, 461L,
    462L, 463L, 464L, 465L, 466L, 467L, 468L, 470L, 471L, 472L, 473L,
    475L, 477L, 478L, 479L, 481L, 482L, 483L, 485L, 486L, 487L, 488L,
    489L, 493L, 494L, 495L, 496L, 498L, 499L, 500L, 501L, 502L, 503L,
    504L, 506L, 507L, 508L, 509L, 510L, 511L, 512L, 514L, 516L, 517L,
    519L, 520L, 521L, 522L, 523L, 524L, 525L, 526L, 527L, 528L, 529L,
    530L, 531L, 532L, 533L, 534L, 535L, 536L, 538L, 539L, 540L, 541L,
    542L, 543L, 544L, 545L, 546L, 547L, 548L, 549L, 550L, 551L, 552L,
    553L, 554L, 555L, 556L, 557L, 558L, 559L, 560L, 562L, 563L, 564L,
    565L, 566L, 567L, 568L, 569L, 571L, 572L, 574L, 576L, 577L, 578L,
    579L, 580L, 581L, 582L, 583L, 584L, 585L, 588L, 589L, 590L, 591L,
    592L, 593L, 594L, 595L, 598L, 599L, 600L, 602L, 603L, 604L, 605L,
    606L, 607L, 608L, 609L, 610L, 611L, 613L, 615L, 616L, 617L, 618L,
    619L, 620L, 621L, 622L, 623L, 624L, 625L, 626L, 627L, 628L, 629L,
    630L, 631L, 632L, 633L, 634L, 635L, 637L, 638L, 639L, 640L, 641L,
    643L, 644L, 645L, 646L, 647L, 648L, 649L, 650L, 651L, 652L, 653L,
    655L, 657L, 658L, 659L, 661L, 663L, 664L, 666L, 667L, 668L, 669L,
    670L, 671L, 672L, 673L, 674L, 675L, 676L, 677L, 678L, 679L, 680L,
    681L, 682L, 683L, 684L, 685L, 686L, 688L, 691L, 692L, 693L, 694L,
    695L, 697L, 699L, 701L, 702L, 703L, 704L, 705L, 706L, 708L, 712L,
    713L, 714L, 716L, 719L, 722L, 724L, 725L, 726L, 727L, 728L, 729L,
    730L, 732L, 733L, 734L, 735L, 736L, 737L, 738L, 739L, 740L, 742L,
    743L, 744L, 745L, 746L, 747L, 748L, 749L, 750L, 751L, 752L, 754L,
    755L, 756L, 757L, 758L, 759L, 760L, 761L, 762L, 763L, 764L, 765L,
    766L, 769L, 770L, 771L, 772L, 774L, 775L, 776L, 779L, 780L, 782L,
    784L, 785L, 787L, 789L, 798L, 799L, 801L, 802L, 805L, 806L, 807L,
    809L, 811L, 813L, 814L, 815L, 816L, 817L, 818L, 819L, 820L, 821L,
    822L, 823L, 824L, 825L, 826L, 827L, 828L, 830L, 831L, 832L, 833L,
    834L, 835L, 836L, 837L, 838L, 839L
  )


# Other global vars, configuration, and imports ---------------------------


  .min_hr <- 30
  .max_hr <- 220
  .resamp <- 32
  .activity_levels <- c(
    "low_resistance_bike", "high_resistance_bike",
    "walk", "run"
  )
  .activity_labels <- c("Bike Low", "Bike High", "Walk", "Run")
  .samp_rate <- 256
  .width_sec <- 8
  .slide_sec <- 8
  .frqRange <- c(0.4, 4)


# Modeling functions ------------------------------------------------------


  caret_summary <- function (data, lev = NULL, model = NULL) {

    if (is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)

    pred <- data$pred
    obs <- data$obs

    isNA <- is.na(pred)
    pred <- pred[!isNA]
    obs <- obs[!isNA]

    stopifnot(is.numeric(pred), is.numeric(obs))

    if (length(obs) + length(pred) == 0) {

      out <- rep(NA, 3)

    } else {

      if (
        length(unique(pred)) < 2 || length(unique(obs)) <  2
      ) {

        resamplCor <- NA

      } else {

        resamplCor <- try(
          cor(pred, obs, use = "pairwise.complete.obs"),
          silent = TRUE
        )

        if (inherits(resamplCor, "try-error"))
          resamplCor <- NA

      }

      diffs <- pred - obs
      abs_diffs <- abs(diffs)
      bias <- mean(diffs)
      mse <- mean(diffs^2)
      mae <- mean(abs_diffs)
      mape <- mean(abs_diffs / obs)*100
      out <- c(bias, sqrt(mse), resamplCor^2, mae, mape)

    }

    names(out) <- c("mean_bias", "RMSE", "Rsquared", "MAE", "MAPE")

    if (any(is.nan(out))) out[is.nan(out)] <- NA

    out

  }


  reduce_rf <- function(m, keep = c(
    "modelInfo", "terms", "xlevels", "contrasts", "modelType",
    "finalModel", "preProcess", "control", "yLimit", "levels"
  )) {

      names(m) %>%
      intersect(keep) %>%
      m[.] %>%
      structure(class = "train")

  }
  
  predict.randomForest <- function (
    object, newdata, type = "response", norm.votes = TRUE,
    predict.all = FALSE, proximity = FALSE, nodes = FALSE, cutoff,
    min_hr = .min_hr, max_hr = .max_hr, ...
  ) {
    if (!inherits(object, "randomForest"))
      stop("object not of class randomForest")
    if (is.null(object$forest))
      stop("No forest component in the object")
    out.type <- charmatch(tolower(type), c("response", "prob",
                                           "vote", "class"))
    if (is.na(out.type))
      stop("type must be one of 'response', 'prob', 'vote'")
    if (out.type == 4)
      out.type <- 1
    if (out.type != 1 && object$type == "regression")
      stop("'prob' or 'vote' not meaningful for regression")
    if (out.type == 2)
      norm.votes <- TRUE
    if (missing(newdata)) {
      p <- if (!is.null(object$na.action)) {
        napredict(object$na.action, object$predicted)
      }
      else {
        object$predicted
      }
      if (object$type == "regression")
        return(p)
      if (proximity & is.null(object$proximity))
        warning(
          "cannot return proximity without new data if random",
          " forest object does not already have proximity"
        )
      if (out.type == 1) {
        if (proximity) {
          return(list(pred = p, proximity = object$proximity))
        }
        else return(p)
      }
      v <- object$votes
      if (!is.null(object$na.action))
        v <- napredict(object$na.action, v)
      if (norm.votes) {
        t1 <- t(apply(v, 1, function(x) {
          x/sum(x)
        }))
        class(t1) <- c(class(t1), "votes")
        if (proximity)
          return(list(pred = t1, proximity = object$proximity))
        else return(t1)
      }
      else {
        if (proximity)
          return(list(pred = v, proximity = object$proximity))
        else return(v)
      }
    }
    if (missing(cutoff)) {
      cutoff <- object$forest$cutoff
    }
    else {
      if (sum(cutoff) > 1 || sum(cutoff) < 0 ||
          !all(cutoff > 0) || length(cutoff) != length(object$classes)) {
        stop("Incorrect cutoff specified.")
      }
      if (!is.null(names(cutoff))) {
        if (!all(names(cutoff) %in% object$classes)) {
          stop("Wrong name(s) for cutoff")
        }
        cutoff <- cutoff[object$classes]
      }
    }
    if (object$type == "unsupervised")
      stop("Can't predict unsupervised forest.")
    if (inherits(object, "randomForest.formula")) {
      newdata <- as.data.frame(newdata)
      rn <- row.names(newdata)
      Terms <- delete.response(object$terms)
      x <- model.frame(Terms, newdata, na.action = na.omit)
      keep <- match(row.names(x), rn)
    }
    else {
      if (is.null(dim(newdata)))
        dim(newdata) <- c(1, length(newdata))
      x <- newdata
      if (nrow(x) == 0)
        stop("newdata has 0 rows")
      if (any(is.na(x)))
        stop("missing values in newdata")
      keep <- 1:nrow(x)
      rn <- rownames(x)
      if (is.null(rn))
        rn <- keep
    }
    vname <- if (is.null(dim(object$importance))) {
      names(object$importance)
    }
    else {
      rownames(object$importance)
    }
    if (is.null(colnames(x))) {
      if (ncol(x) != length(vname)) {
        stop(
          "number of variables in newdata does not match that",
          " in the training data"
        )
      }
    }
    else {
      if (any(!vname %in% colnames(x)))
        stop("variables in the training data missing in newdata")
      x <- x[, vname, drop = FALSE]
    }
    if (is.data.frame(x)) {
      isFactor <- function(x) is.factor(x) & !is.ordered(x)
      xfactor <- which(sapply(x, isFactor))
      if (length(xfactor) > 0 && "xlevels" %in% names(object$forest)) {
        for (i in xfactor) {
          if (any(!levels(x[[i]]) %in% object$forest$xlevels[[i]]))
            stop("New factor levels not present in the training data")
          x[[i]] <- factor(
            x[[i]],
            levels = levels(x[[i]])[
              match(levels(x[[i]]), object$forest$xlevels[[i]])
            ]
          )
        }
      }
      cat.new <- sapply(
        x,
        function(x) if (is.factor(x) && !is.ordered(x)) length(levels(x)) else 1
      )
      if (!all(object$forest$ncat == cat.new))
        stop(
          "Type of predictors in new data do not match that of",
          " the training data."
        )
    }
    mdim <- ncol(x)
    ntest <- nrow(x)
    ntree <- object$forest$ntree
    maxcat <- max(object$forest$ncat)
    nclass <- object$forest$nclass
    nrnodes <- object$forest$nrnodes
    op <- options(warn = -1)
    on.exit(options(op))
    x <- t(data.matrix(x))
    if (predict.all) {
      treepred <- if (object$type == "regression") {
        matrix(double(ntest * ntree), ncol = ntree)
      }
      else {
        matrix(integer(ntest * ntree), ncol = ntree)
      }
    }
    else {
      treepred <- numeric(ntest)
    }
    proxmatrix <- if (proximity)
      matrix(0, ntest, ntest)
    else numeric(1)
    nodexts <- if (nodes)
      integer(ntest * ntree)
    else integer(ntest)
    if (object$type == "regression") {
      if (!is.null(object$forest$treemap)) {
        object$forest$leftDaughter <-
          object$forest$treemap[, 1, , drop = FALSE]
        object$forest$rightDaughter <-
          object$forest$treemap[, 2, , drop = FALSE]
        object$forest$treemap <- NULL
      }
      keepIndex <- "ypred"
      if (predict.all)
        keepIndex <- c(keepIndex, "treepred")
      if (proximity)
        keepIndex <- c(keepIndex, "proximity")
      if (nodes)
        keepIndex <- c(keepIndex, "nodexts")
      if (!is.integer(object$forest$leftDaughter))
        storage.mode(object$forest$leftDaughter) <- "integer"
      if (!is.integer(object$forest$rightDaughter))
        storage.mode(object$forest$rightDaughter) <- "integer"
      if (!is.integer(object$forest$nodestatus))
        storage.mode(object$forest$nodestatus) <- "integer"
      if (!is.double(object$forest$xbestsplit))
        storage.mode(object$forest$xbestsplit) <- "double"
      if (!is.double(object$forest$nodepred))
        storage.mode(object$forest$nodepred) <- "double"
      if (!is.integer(object$forest$bestvar))
        storage.mode(object$forest$bestvar) <- "integer"
      if (!is.integer(object$forest$ndbigtree))
        storage.mode(object$forest$ndbigtree) <- "integer"
      if (!is.integer(object$forest$ncat))
        storage.mode(object$forest$ncat) <- "integer"
      
      ans <- .C(
        "regForest", as.double(x), ypred = double(ntest),
        as.integer(mdim), as.integer(ntest), as.integer(ntree),
        object$forest$leftDaughter, object$forest$rightDaughter,
        object$forest$nodestatus, nrnodes, object$forest$xbestsplit,
        object$forest$nodepred, object$forest$bestvar, object$forest$ndbigtree,
        object$forest$ncat, as.integer(maxcat), as.integer(predict.all),
        treepred = as.double(treepred), as.integer(proximity),
        proximity = as.double(proxmatrix), nodes = as.integer(nodes),
        nodexts = as.integer(nodexts), PACKAGE = "randomForest"
      )[keepIndex]
      
      yhat <- rep(NA, length(rn))
      # names(yhat) <- rn
      
      if (!is.null(object$coefs)) {
        yhat[keep] <- object$coefs[1] + object$coefs[2] *
          ans$ypred
      }
      else {
        yhat[keep] <- ans$ypred
      }
      if (predict.all) {
        treepred <- matrix(
          NA,
          length(rn),
          ntree,
          dimnames = list(rn, NULL)
        )
        treepred[keep, ] <- ans$treepred
      }
      if (!proximity) {
        res <- if (predict.all) list(
          aggregate = yhat, individual = treepred
        ) else yhat
      }
      else {
        res <- list(
          predicted = yhat,
          proximity = structure(
            ans$proximity,
            dim = c(ntest, ntest),
            dimnames = list(rn, rn))
        )
      }
      if (nodes) {
        attr(res, "nodes") <- matrix(
          ans$nodexts, ntest, ntree, dimnames = list(rn[keep], 1:ntree)
        )
      }
      
      res %<>%
        round(.) %>%
        pmax(min_hr) %>%
        pmin(max_hr)
      
    }
    else {
      countts <- matrix(0, ntest, nclass)
      t1 <- .C("classForest", mdim = as.integer(mdim), ntest = as.integer(ntest),
               nclass = as.integer(object$forest$nclass), maxcat = as.integer(maxcat),
               nrnodes = as.integer(nrnodes), jbt = as.integer(ntree),
               xts = as.double(x), xbestsplit = as.double(object$forest$xbestsplit),
               pid = object$forest$pid, cutoff = as.double(cutoff),
               countts = as.double(countts), treemap = as.integer(aperm(object$forest$treemap,
                                                                        c(2, 1, 3))), nodestatus = as.integer(object$forest$nodestatus),
               cat = as.integer(object$forest$ncat), nodepred = as.integer(object$forest$nodepred),
               treepred = as.integer(treepred), jet = as.integer(numeric(ntest)),
               bestvar = as.integer(object$forest$bestvar), nodexts = as.integer(nodexts),
               ndbigtree = as.integer(object$forest$ndbigtree),
               predict.all = as.integer(predict.all), prox = as.integer(proximity),
               proxmatrix = as.double(proxmatrix), nodes = as.integer(nodes),
               PACKAGE = "randomForest")
      if (out.type > 1) {
        out.class.votes <- t(matrix(t1$countts, nrow = nclass,
                                    ncol = ntest))
        if (norm.votes)
          out.class.votes <- sweep(out.class.votes, 1,
                                   rowSums(out.class.votes), "/")
        z <- matrix(NA, length(rn), nclass, dimnames = list(rn,
                                                            object$classes))
        z[keep, ] <- out.class.votes
        class(z) <- c(class(z), "votes")
        res <- z
      }
      else {
        out.class <- factor(rep(NA, length(rn)), levels = 1:length(object$classes),
                            labels = object$classes)
        out.class[keep] <- object$classes[t1$jet]
        names(out.class)[keep] <- rn[keep]
        res <- out.class
      }
      if (predict.all) {
        treepred <- matrix(object$classes[t1$treepred],
                           nrow = length(keep), dimnames = list(rn[keep],
                                                                NULL))
        res <- list(aggregate = res, individual = treepred)
      }
      if (proximity)
        res <- list(predicted = res, proximity = structure(t1$proxmatrix,
                                                           dim = c(ntest, ntest), dimnames = list(rn[keep],
                                                                                                  rn[keep])))
      if (nodes)
        attr(res, "nodes") <- matrix(t1$nodexts, ntest,
                                     ntree, dimnames = list(rn[keep], 1:ntree))
    }
    
    res
    
  }


# Confusion matrix functions ----------------------------------------------


  cm <- function(m) {
    caret::confusionMatrix(m$pred, m$obs) %>%
    {data.frame(
      model = unique(m$model),
      cm_overall(.),
      cm_byclass(.)
    )} %>%
    dplyr::relocate(model, activity) %>%
    tidyr::pivot_longer(!model:activity) %>%
    dplyr::mutate(
      activity =
        as.character(activity) %>%
        replace(name %in% c("accuracy", "kappa"), "Overall")
    ) %>%
    dplyr::distinct(.)
  }
  
  cm_overall <- function(cm) {
    t(cm$overall) %>%
    data.frame(.) %>%
    stats::setNames( ., tolower(names(.)) ) %>%
    dplyr::select(accuracy:kappa)
  }
  
  cm_byclass <- function(cm) {
    data.frame(cm$byClass) %>%
    tibble::rownames_to_column("activity") %>%
    dplyr::mutate(
      activity =
        gsub("^Class: ", "", activity) %>%
        factor_activity(., .activity_labels, .activity_labels)
    ) %>%
    stats::setNames( ., tolower(names(.)) ) %>%
    dplyr::rename_with(function(x) gsub(
      "^([pn])[oseg]{2}.pred.value$",
      "\\1pv",
      x
    )) %>%
    dplyr::select(activity:npv, f1)
  }


# Feature-related functions -----------------------------------------------


  fd_features <- function(
    x, width_sec, frqRange, normalize = FALSE,
    resamp = .resamp, ...
  ) {
  
    {width_sec * resamp} %>% ## Ideal number of samples
    {2^ceiling(log2(.))} %T>% ## Tweak to make tuneR::periodogram happy
    {if (. != resamp * width_sec) warning(
      "Downsampling to ", . / width_sec,
      " Hz instead of ", resamp,
      " Hz, as the target rate must be a power of 2",
      call. = FALSE
    )} %>%
    AGread::sensor_resample(x, .) %>%
    get_pgram(
      samp_rate = resamp, frqRange = frqRange,
      normalize = normalize, ...
    ) %>%
    {list(
      freq2_freq = .@freq[  order(.@spec[[1]], decreasing = TRUE)[1:2]  ],
      freq2_spec = sort(.@spec[[1]], decreasing = TRUE)[1:2],
      freq = .@freq,
      spec = .@spec[[1]]
    )}
  
  }
  
  
  td_features <- function(df, ppg, acc, gyro, width_sec, ...) {
  
    df %>%
    dplyr::group_by(time) %>%
    dplyr::select(-(acc_filt:gyro_filt)) %>%
    dplyr::summarise(
      dplyr::across(
        vm:gvm,
        list(
          mean = mean,
          sd = sd,
          q10 = ~ q_wrap(.x, 0.1),
          q25 = ~ q_wrap(.x, 0.25),
          q50 = ~ q_wrap(.x, 0.50),
          q75 = ~ q_wrap(.x, 0.75),
          q90 = ~ q_wrap(.x, 0.90)
        )
      ),
      hr_ecg = round(sum(is_heartbeat) / width_sec * 60),
      .groups = "drop"
    )
  
  }


# Plotting ----------------------------------------------------------------


  scatter <- function(
    m, ..., results = globalenv()$results,
    features = globalenv()$features
  ) {
  
  
    scatter_prep(m) %>%
  
    ggplot(aes(pred_mean, obs_mean)) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 1.3,
      colour = "black",
      linetype = "dashed"
    ) +
    stat_function(fun = identity) +
    geom_errorbarh(aes(
      xmin = pred_mean - pred_sd,
      xmax = pred_mean + pred_sd
    ), alpha = 0.5) +
    geom_errorbar(aes(
      ymin = obs_mean - obs_sd,
      ymax = obs_mean + obs_sd
    ), alpha = 0.5) +
    geom_point(
      aes(shape = activity),
      colour = "black",
      fill = "black",
      size = 4.5,
      alpha = 0.5
    ) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    guides(shape = guide_legend(nrow = 1)) +
    scale_shape_manual("Activity", values = c(21:22, 24:25)) +
    theme_classic() +
    scale_y_continuous(
      "Measured Heart Rate (bpm)",
      limits = c(-80, 80),
      breaks = seq(-75, 75, 25)
    ) +
    scale_x_continuous(
      "Predicted Heart Rate (bpm)",
      limits = c(-80, 80),
      breaks = seq(-75, 75, 25)
    ) +
    theme(
      axis.line = element_blank(),
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 16),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      ...
    )
  
  
  }
  
  
  ba <- function(
    m, ..., results = globalenv()$results,
    features = globalenv()$features
  ) {
  
    
    d  <- ba_prep(m)
    ba <- get_ba(d)
  
    d %>%
    ggplot(aes(
      x = obs_mean,
      y = bias_mean
    )) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 1.3,
      colour = "black"
    ) +
    geom_hline(yintercept = ba$loa_upper, linetype = "dashed", linewidth = 1.3) +
    geom_hline(yintercept = ba$mean_bias, linewidth = 1.3) +
    geom_hline(yintercept = ba$loa_lower, linetype = "dashed", linewidth = 1.3) +
    geom_errorbarh(aes(
      xmin = obs_mean - obs_sd,
      xmax = obs_mean + obs_sd
    ), alpha = 0.5) +
    geom_errorbar(aes(
      ymin = bias_mean - bias_sd,
      ymax = bias_mean + bias_sd
    ), alpha = 0.5) +
    geom_point(
      aes(shape = activity),
      colour = "black",
      fill = "black",
      size = 4.5,
      alpha = 0.5
    ) +
    ggpmisc::stat_poly_eq(formula = y ~ x, label.x = 0.95, size = 6) +
    guides(shape = guide_legend(nrow = 1)) +
    scale_shape_manual("Activity", values = c(21:22, 24:25)) +
    theme_classic() +
    scale_y_continuous(
      "Observed - Predicted (bpm)",
      limits = c(-35, 35),
      breaks = seq(-30, 30, 10)
    ) +
    scale_x_continuous(
      "Observed Heart Rate (bpm)",
      limits = c(50, 185),
      breaks = seq(50, 175, 25)
    ) +
    theme(
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 16),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      ...
    )
  
    
  }


# Statistics --------------------------------------------------------------


  scatter_prep <- function(
    m, results = globalenv()$results, features = globalenv()$features
  ) {
    results[[m]] %>%
    .$pred %>%
    dplyr::mutate(
      id = features$id[rowIndex],
      activity = features$activity[rowIndex],
      rowIndex = NULL,
      Resample = NULL,
      mtry = NULL,
      ## Center on mean of participant-activity means (n = 19)
      center = mean(stats::aggregate(
        pred~id+activity,
        results[[m]]$pred,
        mean
      )$pred),
      pred = pred - center,
      obs = obs - center,
      center = NULL
    ) %>%
    dplyr::group_by(id, activity) %>%
    dplyr::summarise(dplyr::across(
      everything(),
      list(mean = mean, sd = sd)
    ), .groups = "drop")
  }


  get_ba <- function(d) {
    dplyr::tibble(
      mean_bias = mean(d$bias_mean),
      sd_bias = sd(d$bias_mean),
      limit = sd_bias * 1.96,
      loa_upper = mean_bias + limit,
      loa_lower = mean_bias - limit,
      loa_width = local_format(loa_upper - loa_lower, 1)
    )
  }


  ba_prep <- function(
    m, results = globalenv()$results, features = globalenv()$features
  ) {
    results[[m]] %>%
    .$pred %>%
    dplyr::mutate(
      id = features$id[rowIndex],
      activity = features$activity[rowIndex],
      bias = obs - pred,
      rowIndex = NULL,
      Resample = NULL,
      mtry = NULL
    ) %>%
    dplyr::group_by(id, activity) %>%
    dplyr::summarise(dplyr::across(
      everything(),
      list(mean = mean, sd = sd)
    ), .groups = "drop")
  }


# Reading and formatting --------------------------------------------------


  #* Main function ####
  
    read_format <- function(
      filename, output = c("epochs", "raw"),
      scale_filtered = TRUE, verbose = FALSE,
      scaling_coefficients = .scaling_coefficients,
      samp_rate = .samp_rate, frqRange = .frqRange,
      width_sec = .width_sec, slide_sec = .slide_sec,
      ...
    ) {
    
    
      filename <- as.character(filename)
    
    
      output <- match.arg(output)
    
    
      timer <- proc.time()
      if (verbose) cat(
        "\nBeginning the read/format/predict process for",
        basename(filename)
      )
    
    
      if (verbose) cat("\n...Parsing info")
      info <-
        basename(filename) %>%
        gsub(".rds$", "", .) %>%
        data.frame(
          info = .,
          samp_rate = samp_rate,
          frqRange = paste(
            format(frqRange, digits = 1, nsmall = 1),
            collapse = " to "
          ),
          width_sec = width_sec,
          slide_sec = slide_sec
        ) %>%
        tidyr::separate(
          col = info,
          into = c("id", "activity"),
          sep = "_",
          extra = "merge"
        ) %>%
        dplyr::mutate(activity = factor_activity(activity))
    
    
      if (verbose) cat("\n...Reading and pre-formatting data")
      d <-
        readRDS(filename) %>%
        dplyr::mutate(
          ppg_filt = filter_signal(ppg, frqRange, samp_rate),
          acc_filt =
            {(accel_wide_x + accel_wide_y + accel_wide_z) / 3} %>%
            filter_signal(frqRange, samp_rate),
          gyro_filt =
            {(gyro_x + gyro_y + gyro_z) / 3} %>%
            filter_signal(frqRange, samp_rate),
          vm = sqrt(accel_wide_x^2 + accel_wide_y^2 + accel_wide_z^2),
          gvm = sqrt(gyro_x^2 + gyro_y^2 + gyro_z^2),
          is_heartbeat = as.logical(is_heartbeat)
        )
    
    
      if (output == "raw") {
        return(cbind(info, d))
      } else {
        d %<>%
          dplyr::select(
            !dplyr::matches(c(
              "^accel.*[xyz]$", "^gyro.*[xyz]$", "^mag.*[xyz]$")),
            -c(ekg:ppg)
          ) %>%
          scale_data(scale_filtered, scaling_coefficients)
      }
    
    
      if (verbose) cat("\n...Collapsing and formatting -- Please be patient")
      d %<>%
        roll_collapse(
          samp_rate, frqRange,
          width_sec, slide_sec,
          ...
        ) %>%
        cbind(info, .)
    
    
      if (verbose) {
        {proc.time() - timer} %>%
        .["elapsed"] %>%
        {. / 60} %>%
        round(1) %>%
        format(digits = 1, nsmall = 1) %>%
        cat(
          "\n...Process successful. Elapsed time",
          ., "minutes."
        )
      }
    
    
      d
    
    
    }

  #* Primary helper functions ####

    scale_data <- function(df, do = TRUE, scaling_coefficients) {
      
      if (!do) return(df)
      
      ## Eventual order of names
      o <- names(df)
      
      ## New variables
      n <-
        purrr::map(scaling_coefficients, names) %>%
        Reduce(intersect, .) %>%
        {dplyr::select(df, dplyr::any_of(.))}
      n <-
        lapply(scaling_coefficients, `[`, names(n)) %>%
        {scale(x = n, center = .$center, scale = .$scale)} %>%
        apply(2, as.vector) %>%
        data.frame(.)
      
      ## Finish up
      df %>%
      dplyr::select(!dplyr::any_of(names(n))) %>%
      cbind(n) %>%
      dplyr::select(dplyr::all_of(o))
      
    }
    
    roll_collapse <- function(
      d, samp_rate, frqRange,
      width_sec, slide_sec, ...
    ) {
  
      each_window <- width_sec * samp_rate
      each_slide <- slide_sec * samp_rate
  
      n_windows <-
        {nrow(d) - each_window} %>%
        {. / each_slide} %>%
        floor(.) %>%
        {. + 1}
  
      {1:n_windows - 1} %>%
      {each_slide * .} %>%
      purrr::map_df(
        function(i, d, each_window, frqRange, samp_rate, width_sec, ...) {
          dplyr::slice(d, i + 1:each_window) %>%
          roll_df(frqRange, samp_rate, width_sec, ...)
        },
        d = d, each_window = each_window,
        frqRange = frqRange, samp_rate = samp_rate,
        width_sec = width_sec, ...
      ) %>%
      dplyr::relocate(!hr_ecg) %>%
      dplyr::select(
        -dplyr::matches("ppg.*[345]"),
        -dplyr::matches("acc.*[5]"),
        -dplyr::matches("gyro.*[5]")
      )
  
    }

  #* Secondary helpers

    roll_df <- function(df, frqRange, samp_rate, width_sec, ...) {
  
      df %>%
      dplyr::mutate(time = dplyr::nth(time, round(dplyr::n()/2))) %>%
      collapse_period(frqRange, width_sec, ...)
  
    }
  
    collapse_period <- function(df, frqRange, width_sec, ...) {
  
      ppg <- fd_features(df$ppg_filt, width_sec, frqRange)
      acc <- fd_features(df$acc_filt, width_sec, frqRange)
      gyro <- fd_features(df$gyro_filt, width_sec, frqRange)
  
      td_features(
        df, ppg, acc, gyro,
        width_sec, ...
      ) %>%
      data.frame(
        freq_unpack(ppg),
        freq_unpack(acc, "freq"),
        freq_unpack(gyro, "freq")#,
      )
  
    }

    
# Utilities ---------------------------------------------------------------


  local_format <- function(x, digits, ...) {
    round(x, digits) %>%
    format(digits = digits, nsmall = digits, trim = TRUE)
  }
  
  
  get_pgram <- function(x, samp_rate = .samp_rate, bit = 16, ...) {
  
    waveform <- tuneR::Wave(
      x,
      samp.rate = samp_rate,
      bit = bit
    )
  
    tuneR::periodogram(
      object = waveform,
      ...
    )
  
  }
  
  
  filter_signal <- function(
    x, frqRange, samp_rate, zi = TRUE
  ) {
  
    f <-
      {samp_rate / 2} %>% ## Nyquist frequency
      {frqRange / .} %>% ## Bandpass frequencies
      gsignal::cheby2(
        n = 4, w = ., type = "pass",
        Rs = 1, output = "Sos"
      )
  
    if (zi) {
      gsignal::filter_zi(f) %>%
      {. * x[1]} %>%
      gsignal::filter(f, x, zi = .) %>%
      .$y
    } else {
      gsignal::filter(f, x)
    }
  
  }
  
  
  factor_activity <- function(
    x,
    levels = .activity_levels,
    labels = .activity_labels
  ) {
  
    factor(x, levels, labels) %T>%
    {stopifnot(!anyNA(.))}
  
  }
  
  
  q_wrap <- function(.x, p) {
    quantile(.x, probs = p) %>%
    unname(.)
  }
  
  
  freq_unpack <- function(x, match = c("freq", "spec")) {
  
    label <- substitute(x)
  
    data.frame(
      freq = x$freq2_freq,
      spec = x$freq2_spec
    ) %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    tidyr::pivot_longer(!n) %>%
    tidyr::pivot_wider(
      names_from = c("name", "n"),
      names_sep = "",
      values_from = "value"
    ) %>%
    dplyr::rename_with(~ paste(label, .x, sep = "_")) %>%
    dplyr::select(dplyr::matches(match))
  
  }
  
  
  to_df <- function(x, ...) {
    UseMethod("to_df", x)
  }
  
  
  to_df.default <- function(x, ...) {
  
    ci <-
      stats::confint(x, level = 0.95) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      stats::setNames(c("low", "high")) %>%
      tibble::rownames_to_column("parameter")
  
    s <- summary(x)
  
    data.frame(s$coefficients) %>%
    tibble::rownames_to_column("parameter") %>%
    dplyr::rename_with(function(x) ifelse(grepl("^P", x), "p", x)) %>%
    dplyr::mutate(
      t.value = (Estimate - c(0, 1)) / Std..Error,
      p = 2 * pt( abs(t.value), x$df.residual,  lower.tail = FALSE)
    ) %>%
    merge(ci) %>%
    dplyr::mutate(
      parameter = dplyr::recode(
        parameter, "(Intercept)" = "Intercept", "pred_mean" = "Slope"
      ),
      b_ci = paste0(
        local_format(Estimate, 1),
        " (", local_format(low, 1), ", ",
        local_format(high, 1), ")"
      ),
      r2 = local_format(s$r.squared, 2),
      sig = ifelse(p < 0.001, "<0.001", local_format(p, 3))
    ) %>%
    dplyr::select(-c(Std..Error, t.value)) %>%
    dplyr::relocate(!c(p, sig))
  
  
  }
  
  
  balance_features <- function(features, ...) {
  
    dplyr::slice(features, .balanced_indices, ...)
  
  }
