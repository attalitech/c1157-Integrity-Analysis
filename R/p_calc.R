# Primary Statistical Function for Monte Carlo Simulation
P_Calc <- function(TRIAL)
{
  data <- DATA[DATA$TRIAL == TRIAL,]
  RowIDs <- unique(data$ROW)

  x <- foreach(
    j = 1:length(RowIDs),
    .combine = rbind
    #  .options.future = list(seed = TRUE)
    #.export = c("CategoryNames", "m"),
    #.packages = c("Rfast", "dqrng")
  ) %do%
    {
      Row <- RowIDs[j]
      ROWS <- data[data$ROW == Row,]

      # Greater than 1 line?
      if (nrow(ROWS) > 1)
      {
        # Is this categorical?
        if (all(!is.na(ROWS$N)))
        {
          COLS <- nrow(ROWS)
          N <- sum(ROWS$N)
          Meanmean <- sum(ROWS$N*ROWS$MEAN) / N
          # The calculation of Meanvar is OK. SD^2 is an unbiased estimate
          # of variance
          Meanvar <-  sum(ROWS$N*ROWS$SD^2) / N

          # However, this next calculatiion is biased. s.u. will correct it
          # If N > 30, then the correction is < 1 %. It blows up if N > 343!
          if (N < 30)
          {
            Meansd <- s.u(sqrt(Meanvar), N)
          } else {
            Meansd <- sqrt(Meanvar)
          }
          # Protect size of simulation
          if ((m*N) < 1000000000) # One billion
          {
            m1 <- m
          } else {
            m1 <- 1000000000 / N
          }
          SEMsample <- Meansd/sqrt(mean(ROWS$N))
          DiffSample <- sum((ROWS$MEAN - Meanmean)^2) # Squared difference of column means
          # Monte Carlo Simulation
          meansim <- dqrnorm(m,mean=Meanmean,sd=SEMsample) # Generate a new mean for each simulation
          MonteCarloMean <- matrix(NA, nrow = m1, ncol = COLS) # I want one row for each simulation
          # Need to do each column separately. Couldn't think of an efficient way to do this without
          # a loop.
          for (i in 1:COLS)
            MonteCarloMean[,i] <-
            round(
              rowmeans(
                round(
                  # The matrix below will have one row for each replication (m rows),
                  # and one column for each person (N[i] columns)
                  # Cannot use dqrnorm because it won't support the array
                  # of meansim needed for each replication
                  matrix(
                    rnorm(ROWS$N[i] * m1, rep(meansim, ROWS$N[i]), Meansd),
                    nrow = m1, byrow = FALSE
                  ),
                  ROWS$ROUND_OBSERVATION[i]
                )
              ),
              ROWS$ROUND_OBSERVATION[i]
            )
          N <- matrix(ROWS$N, nrow = m1, ncol = COLS, byrow = TRUE)
          # Calculate the weighted mean, and then round
          MeanSamples <- rowsums (MonteCarloMean * N) / sum(ROWS$N)
          DiffSamples <- rowsums((MonteCarloMean - MeanSamples)^2)

          PEQ <- sum(DiffSamples == DiffSample) / m1
          PLE <- sum(DiffSamples < DiffSample)/m1 + PEQ
          PGE <- sum(DiffSamples > DiffSample)/m1 + PEQ
        } else {
          ROWS <- ROWS[,CategoryNames]
          for (NAME in CategoryNames)
          {
            if (all(is.na(ROWS[,NAME])))
              ROWS[,NAME] <- NULL
          }
          PLE <- chisq.test(ROWS, simulate.p.value=m)$p.value
          PGE <- 1-PLE
        }
        # Need to be sure P != 0 or 1
        if(PLE == 1) PLE <- 0.999
        if(PLE == 0) PLE <- 0.001
        if(PGE == 1) PGE <- 0.999
        if(PGE == 0) PGE <- 0.001
        PLE = as.character(signif(PLE,4))
        PGE = as.character(signif(PGE, 4))
      } else {
        PLE = "Only 1 Row"
        PGE = NA
      }

      c(as.character(Row), PLE, PGE)
    } %seed% TRUE

  # This bizarre code is because if there is only 1 row, R creates a data.frame
  # with 3 columns and 1 row.
  if (length(x) == 3)
  {
    x <- as.data.frame(t(x))
  } else {
    x <- as.data.frame(x)
  }

  x <- cbind(NA, x)
  x[1,1] <- TRIAL
  x <- as.data.frame(x)
  names(x) <- c("TRIAL", "ROW", "PLE", "PGE")
  cat("Row IDs", RowIDs, "\n")
  print(x)
  cat("match results", match(x$ROW, RowIDs), "\n")
  x <- x[match(x$ROW, RowIDs),]
  print(x)

  PLEvalues <- as.numeric(x$PLE)
  PGEvalues <- as.numeric(x$PGE)

  PLEvalues <- PLEvalues[!is.na(PLEvalues)]
  PGEvalues <- PGEvalues[!is.na(PGEvalues)]

  if (length(PLEvalues) > 1) {
    PLE <- signif(sumz(PLEvalues)$p, 4)
  } else if (length(PLEvalues) == 1) {
    PLE <- PLEvalues
  } else if (length(PLEvalues) == 0) {
    PLE <- "No values"
  }

  if (length(PGEvalues) > 1) {
    PGE <- signif(sumz(PGEvalues)$p,4)
  } else if (length(PGEvalues) == 1) {
    PGE <- PGEvalues
  } else if (length(PGEvalues) == 0) {
    PGE <- "No values"
  }

  lastline <- data.frame(
    TRIAL = c(NA, NA),
    ROW = c("Summary", NA),
    PLE = c(as.character(PLE), NA),
    PGE = c(as.character(PGE), NA)
  )

  x <- rbind(x, lastline)
  outputComments(
    paste0("Trial ", TRIAL,": p = ", PLE, "\n")
  )
  return(x)
}
