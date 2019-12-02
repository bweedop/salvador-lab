# Loading (and installing, if necessary) any packages needed
required.packages <- c("HMM", "depmixS4")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) 
    install.packages(new.packages)
lapply(required.packages, require, character.only=TRUE)

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

# Plot worm #1 behavior as a discrete function of time
png("./figures/worm-1-behavior.png", width = 1200, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA)
plot(data.cp$frame[which(data.cp$worm_ID == 1)], 
     data.cp$beh1_num[which(data.cp$worm_ID == 1)],
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Worm_ID => 1",
     type = "l")
dev.off()

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
print(hmm)

# Sequence of observation; Do each separately
for (i in unique(data.cp$worm_ID)[2]) {
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
png("figures/sim-worm-behavior_no-copper.png", width = 1200, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA)
plot(seq(1, 1289), 
     sim.worm$obs.code,
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Simulated Worm",
     type = "l")
dev.off()

write.csv(bw$hmm$emissionProbs,
          "data/hmm-emission-matrix_no-copper.csv")

################################################################################
# Include Copper behaviors
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

# Plot worm #1 behavior as a discrete function of time
png("./figures/worm-1-behavior.png", width = 1200, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA)
plot(data.cp$frame[which(data.cp$worm_ID == 1)], 
     data.cp$beh1_num[which(data.cp$worm_ID == 1)],
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Worm_ID => 1",
     type = "l")
dev.off()

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
print(hmm)

# Sequence of observation; Do each separately
for (i in unique(data.cp$worm_ID)[2]) {
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
png("figures/sim-worm-behavior_copper.png", width = 1200, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA)
plot(seq(1, 1289), 
     sim.worm$obs.code,
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Simulated Worm",
     type = "l")
dev.off()

write.csv(bw$hmm$emissionProbs,
          "data/hmm-emission-matrix_with-copper.csv")