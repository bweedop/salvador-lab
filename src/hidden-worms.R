# Loading (and installing, if necessary) any packages needed
required.packages <- c("HMM", "depmixS4")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) 
    install.packages(new.packages)
lapply(required.packages, require, character.only=TRUE)

set.seed(18)

# Loading data
worms.data <- read.csv("./data/table_matrix_p_line_atr_tded.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE)

# Processing the data
data.cp <- worms.data[, 2:9]
copper.codes <- c("NaN", "81", "82", "83", "84", "91", "92", "93", "94")
# states <- c("crawl", "reorient", "copper_ring")
data.cp <- data.cp[-which(data.cp$beh1_name %in% copper.codes), ]
behaviors <- unique(data.cp$beh1_name)
for (i in 1:nrow(data.cp)) {
    data.cp$beh1_num[i] <- which(behaviors == data.cp$beh1_name[i])
}

# Example worm behaviors that will be plotted
plot.ids <- sample(unique(data.cp$worm_ID), 5)
# Observation that models will be fitted to...
fit.obs <- sample(unique(data.cp$worm_ID), 1)

for (i in plot.ids) {
    # Plot worm #1 behavior as a discrete function of time
    png(paste("./figures/no-copper/worm-", i, "-behavior.png", sep=""), width = 1200, height = 480,
        units = "px", pointsize = 12, bg = "white", res = NA)
    plot(data.cp$frame[which(data.cp$worm_ID == i)], 
         data.cp$beh1_num[which(data.cp$worm_ID == i)],
         xlab = "Frame",
         ylab = "Behavior (Code)",
         main = paste("Worm_ID =>", i, "(No Copper)"),
         type = "l")
    dev.off()
}

# trans.mat - Transition matrix for the states of our hmm. These values are 
#   established using the data that we have from Dr. Salvador's experiments.

trans.mat <- matrix(NA, 
                    length(behaviors), 
                    length(behaviors), 
                    dimnames = list(behaviors, behaviors))

# tmp and total are counters for the number of transitions from one behavior to 
#   another and the total number of transitions between one behavior to all 
#   others, respectively.
tmp <- 0
total <- 0
# Loop through data and count the number of transitions from one behavior to 
# another (including itself).
for (g in 1:length(behaviors)) {
    for (h in 1:length(behaviors)) {
        # Do each worm separately
        for (i in unique(data.cp$worm_ID)) {
            # Remove the last frame from the loop
            for (j in which(data.cp$worm_ID == i)[-length(which(data.cp$worm_ID == i))]) {
                # increase total count if behavior in focus seen
                if (data.cp$beh1_name[j] == behaviors[h]) {
                    total <- total + 1
                }
                # increase tmp count if behavior in focus transitions to 2nd behavior in focus
                if (data.cp$beh1_name[j] == behaviors[h] && data.cp$beh1_name[j+1] == behaviors[g]) {
                    tmp <- tmp + 1
                }
            }
        }
        # Divide tmp by total to get probability of the transition in focus happening given the data
        trans.mat[g, h] <- tmp / total
        # Reset tmp and total
        total <- 0
        tmp <- 0
    }
}

# Initial HMM
hmm <- initHMM(behaviors, behaviors, transProbs = trans.mat)

# Sequence of observation; Do each separately
for (i in fit.obs) {
    observation <- data.cp$beh1_name[which(data.cp$worm_ID == i)]
    bw <- baumWelch(hmm, observation, 10)
}

# Simulating worm behavior given our model
sim.worm <- simHMM(bw$hmm, 1289)
sim.worm$obs.code <- rep(NA, 1289)
for(i in 1:length(sim.worm$observation)) {
    sim.worm$obs.code[i] <- which(behaviors == sim.worm$observation[i])
}

# Plot simulated worm behavior as a discrete function of time
png("figures/no-copper/sim-worm-behavior.png", width = 1200, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA)
plot(seq(1, 1289), 
     sim.worm$obs.code,
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Simulated Worm (No Copper)",
     type = "l")
dev.off()

write.csv(bw$hmm$emissionProbs,
          "data/no-copper/hmm-emission-matrix.csv")

################################################################################
# Include Copper behaviors
# *** This is the same as above but it incorporates the copper behaviors. 
#     Including these behaviors (as can be seen in some of the plots) swamps 
#     out the other states/observations
################################################################################

# Processing the data
data.cp <- worms.data[, 2:9]
copper.codes <- c("NaN", "81", "82", "83", "84", "91", "92", "93", "94")
# states <- c("crawl", "reorient", "copper_ring")
data.cp$beh1_name[which(data.cp$beh1_name %in% copper.codes)] <- "copper_beh"
behaviors <- unique(data.cp$beh1_name)
for (i in 1:nrow(data.cp)) {
    data.cp$beh1_num[i] <- which(behaviors == data.cp$beh1_name[i])
}

for (i in plot.ids) {
    # Plot worm #1 behavior as a discrete function of time
    png(paste("./figures/with-copper/worm-", i, "-behavior.png", sep=""), 
        width = 1200, 
        height = 480,
        units = "px", 
        pointsize = 12, 
        bg = "white", 
        res = NA)
    plot(data.cp$frame[which(data.cp$worm_ID == i)], 
         data.cp$beh1_num[which(data.cp$worm_ID == i)],
         xlab = "Frame",
         ylab = "Behavior (Code)",
         main = paste("Worm_ID =>", i, "(Copper)"),
         type = "l")
    dev.off()
}

# trans.mat - Transition matrix for the states of our hmm. These values are 
#   established using the data that we have from Dr. Salvador's experiments.

trans.mat <- matrix(NA, 
                    length(behaviors), 
                    length(behaviors), 
                    dimnames = list(behaviors, behaviors))

# tmp and total are counters for the number of transitions from one behavior to 
#   another and the total number of transitions between one behavior to all 
#   others, respectively.
tmp <- 0
total <- 0
# Loop through data and count the number of transitions from one behavior to 
# another (including itself).
for (g in 1:length(behaviors)) {
    for (h in 1:length(behaviors)) {
        # Do each worm separately
        for (i in unique(data.cp$worm_ID)) {
            # Remove the last frame from the loop
            for (j in which(data.cp$worm_ID == i)[-length(which(data.cp$worm_ID == i))]) {
                # increase total count if behavior in focus seen
                if (data.cp$beh1_name[j] == behaviors[h]) {
                    total <- total + 1
                }
                # increase tmp count if behavior in focus transitions to 2nd behavior in focus
                if (data.cp$beh1_name[j] == behaviors[h] && data.cp$beh1_name[j+1] == behaviors[g]) {
                    tmp <- tmp + 1
                }
            }
        }
        # Divide tmp by total to get probability of the transition in focus happening given the data
        trans.mat[g, h] <- tmp / total
        # Reset tmp and total
        total <- 0
        tmp <- 0
    }
}

# Initial HMM
hmm <- initHMM(behaviors, behaviors, transProbs = trans.mat)

# Sequence of observation; Do each separately
for (i in fit.obs) {
    observation <- data.cp$beh1_name[which(data.cp$worm_ID == i)]
    bw <- baumWelch(hmm, observation, 10)
}

# Simulating worm behavior given our model
sim.worm <- simHMM(bw$hmm, 1289)
sim.worm$obs.code <- rep(NA, 1289)
for(i in 1:length(sim.worm$observation)) {
    sim.worm$obs.code[i] <- which(behaviors == sim.worm$observation[i])
}

# Plot simulated worm behavior as a discrete function of time
png("figures/with-copper/sim-worm-behavior.png", 
    width = 1200, 
    height = 480,
    units = "px", 
    pointsize = 12, 
    bg = "white", 
    res = NA)
plot(seq(1, 1289), 
     sim.worm$obs.code,
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Simulated Worm (Copper)",
     type = "l")
dev.off()

write.csv(bw$hmm$emissionProbs,
          "data/with-copper/hmm-emission-matrix.csv")

###############################################################################
# HMM w/ Simple States
# *** I wanted to see if we could simplify the states that are producing the 
#     observations. I chose to exclude the copper behaviors here and just have
#     two states (crawling and reorienting). It doesn't look as good but maybe
#     it something that deserves some more attention.
###############################################################################

# Processing the data
data.cp <- worms.data[, 2:9]
copper.codes <- c("NaN", "81", "82", "83", "84", "91", "92", "93", "94")
states <- c("crawl", "reorient")
data.cp <- data.cp[-which(data.cp$beh1_name %in% copper.codes), ]
behaviors <- unique(data.cp$beh1_name)
crawls <- c("line", "oparc", "clarc", "loop")
reorients <- c("om", "rev", "pir", "pauses")
data.cp$beh1_state <- NA
for (i in 1:nrow(data.cp)) {
    data.cp$beh1_num[i] <- which(behaviors == data.cp$beh1_name[i])
    if (data.cp$beh1_name[i] %in% crawls) {
        data.cp$beh1_state[i] <- "crawl"
    } else {
        data.cp$beh1_state[i] <- "reorient"
    }
}

# trans.mat - Transition matrix for the states of our hmm. These values are 
#   established using the data that we have from Dr. Salvador's experiments.

trans.mat <- matrix(NA, 
                    length(states), 
                    length(states), 
                    dimnames = list(states, states))

# tmp and total are counters for the number of transitions from one behavior to 
#   another and the total number of transitions between one behavior to all 
#   others, respectively.
tmp <- 0
total <- 0
# Loop through data and count the number of transitions from one behavior to 
# another (including itself).
for (g in 1:length(states)) {
    for (h in 1:length(states)) {
        # Do each worm separately
        for (i in unique(data.cp$worm_ID)) {
            # Remove the last frame from the loop
            for (j in which(data.cp$worm_ID == i)[-length(which(data.cp$worm_ID == i))]) {
                # increase total count if behavior in focus seen
                if (data.cp$beh1_state[j] == states[h]) {
                    total <- total + 1
                }
                # increase tmp count if behavior in focus transitions to 2nd behavior in focus
                if (data.cp$beh1_state[j] == states[h] && data.cp$beh1_state[j+1] == states[g]) {
                    tmp <- tmp + 1
                }
            }
        }
        # Divide tmp by total to get probability of the transition in focus happening given the data
        trans.mat[g, h] <- tmp / total
        # Reset tmp and total
        total <- 0
        tmp <- 0
    }
}

# Initial HMM
hmm <- initHMM(states, behaviors, transProbs = trans.mat)

# Sequence of observation; Do each separately
for (i in fit.obs) {
    observation <- data.cp$beh1_name[which(data.cp$worm_ID == i)]
    bw <- baumWelch(hmm, observation, 10)
}

# Simulating worm behavior given our model
sim.worm <- simHMM(bw$hmm, 1289)
sim.worm$obs.code <- rep(NA, 1289)
for(i in 1:length(sim.worm$observation)) {
    sim.worm$obs.code[i] <- which(behaviors == sim.worm$observation[i])
}

# Plot simulated worm behavior as a discrete function of time
png("figures/two-state/sim-worm-behavior_simple-states.png", 
    width = 1200, 
    height = 480,
    units = "px", 
    pointsize = 12, 
    bg = "white", 
    res = NA)
plot(seq(1, 1289), 
     sim.worm$obs.code,
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Simulated Worm",
     type = "l")
dev.off()

write.csv(bw$hmm$emissionProbs,
          "data/two-state/hmm-emission-matrix_simple-states.csv")