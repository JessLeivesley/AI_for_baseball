#packages

library(dplyr)
library(car)
library(ggplot2)

#------------------------------------

#functions

#get state
get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}

get.state.bases <- function(runner1, runner2, runner3){
  paste(runner1, runner2, runner3, sep="")
}

#sum of number of runners and outs
count.runners.outs <- function(s){
  sum(as.numeric(strsplit(s,"")[[1]]), na.rm=TRUE)
}

# simulate a half inning
simulate.half.inning <- function(P, R, start=1){
  s <- start; path <- NULL; runs <- 0
  while(s < 25){
    s.new <- sample(1:25, 1, prob=P[s, ])
    path <- c(path, s.new)
    runs <- runs + R[s, s.new]
    s <- s.new
  }
  runs
}

#------------------------------------
data2023 <- read.csv("all2023.csv", header=FALSE)
fields <- read.delim("descriptions.txt")
names(data2023) <- fields[, "Header"]

#half inning id
data2023$HALF.INNING <- with(data2023, paste(GAME_ID, INN_CT, BAT_HOME_ID))

#runs scored
data2023$RUNS.SCORED <- with(data2023, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

#current state
RUNNER1 <- ifelse(as.character(data2023[,"BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data2023[,"BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data2023[,"BASE3_RUN_ID"])=="", 0, 1)
data2023$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2023$OUTS_CT)
data2023$STATE.BASES <- get.state.bases(RUNNER1, RUNNER2, RUNNER3)

#new state
NRUNNER1 <- with(data2023, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
NRUNNER2 <- with(data2023, as.numeric(RUN1_DEST_ID==2 |RUN2_DEST_ID==2 | BAT_DEST_ID==2))
NRUNNER3 <- with(data2023, as.numeric(RUN1_DEST_ID==3 |RUN2_DEST_ID==3 | RUN3_DEST_ID==3 | BAT_DEST_ID==3))
NOUTS <- with(data2023, OUTS_CT + EVENT_OUTS_CT)
data2023$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
data2023$NEW.STATE.BASES <- get.state.bases(NRUNNER1, NRUNNER2, NRUNNER3)


#only consider plays where there is a change in state or in the number of runs scored
data2023 <- subset(data2023, (STATE != NEW.STATE) | (RUNS.SCORED > 0))
data2023B <- subset(data2023, (STATE.BASES != NEW.STATE.BASES) | RUNS.SCORED > 0)

#only consider full innings where there are three outs
data.outs <- data2023 %>% summarise(Outs.Inning=sum(EVENT_OUTS_CT), .by = c(HALF.INNING))
data2023 <- merge(data2023, data.outs)
data2023B <- merge(data2023B, data.outs)
data2023B <- subset(data2023B, Outs.Inning == 3)
data2023C <- subset(data2023, Outs.Inning == 3)

#only consider plays where there is a batting event, non-batting plays ignored
data2023C <- subset(data2023, BAT_EVENT_FL == TRUE)
data2023B <- subset(data2023B, BAT_EVENT_FL == TRUE)

#recode new states with 3 outs
data2023C$NEW.STATE <- recode(data2023C$NEW.STATE,
                              "c('000 3', '100 3', '010 3', '001 3','110 3', '101 3', '011 3', '111 3')='3'")
data2023B$NEW.STATE <- recode(data2023B$NEW.STATE,
                              "c('000 3', '100 3', '010 3', '001 3','110 3', '101 3', '011 3', '111 3')='3'")

#------------------------------------
#matrix of counts
T.matrix <- with(data2023C, table(STATE, NEW.STATE))
T.matrix.base <- with(data2023B, table(STATE.BASES, NEW.STATE))

#matrix of probabilities (full)
P.matrix <- prop.table(T.matrix, 1)
P.matrix <- rbind(P.matrix, c(rep(0, 24), 1))

#matrix of probabilities (base only to bases with outs)
P.matrix.base <- prop.table(T.matrix.base, 1)

#half-inning simulation
runners.outs <- sapply(dimnames(T.matrix)[[1]], count.runners.outs)[-25]
R <- outer(runners.outs + 1, runners.outs, FUN="-")
dimnames(R)[[1]] <- dimnames(T.matrix)[[1]][-25]
dimnames(R)[[2]] <- dimnames(T.matrix)[[1]][-25]
R <- cbind(R, rep(0, 24))

simulate.half.inning(P.matrix, R, 1)