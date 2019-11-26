# Loading (and installing, if necessary) any packages needed
required.packages <- c("HMM", "depmixS4")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) 
    install.packages(new.packages)

# Loading data
worms.data <- read.csv("./data/table_matrix_p_line_atr_tded.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE)

# Processing the data
data.cp <- worms.data[, 2:9]
copper.codes <- c("NaN", "81", "82", "83", "84", "91", "92", "93", "94")
states <- c("crawl", "reorient", "copper_ring")
data.cp$beh1_name[which(data.cp$beh1_name %in% copper.codes)] <- "copper_beh"
behaviors <- unique(data.cp$beh1_name)
for (i in 1:nrow(data.cp)) {
    data.cp$beh1_num[i] <- which(behaviors == data.cp$beh1_name[i])
}

# Plot worm #1 behavior as a discrete function of time
png("worm-1-behavior.png", width = 1200, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA)
plot(data.cp$frame[which(data.cp$worm_ID == 1)], 
     data.cp$beh1_num[which(data.cp$worm_ID == 1)],
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Worm_ID => 1",
     type = "l")
dev.off()

# Transition matrix the Markov model
trans.mat <- matrix(rep(1/length(behaviors), length(behaviors)), 
                    ncol = length(behaviors), 
                    nrow = length(behaviors))

# Emission matrix
emiss.mat <- matrix(rep(1/length(behaviors), length(behaviors)), 
                    ncol = length(behaviors), 
                    nrow = length(behaviors))

# Initial HMM
hmm = initHMM(behaviors, 
              behaviors, 
              transProbs = trans.mat,
              emissionProbs = emiss.mat)
print(hmm)

# Sequence of observation
observation <- c()
for (i in unique(data.cp$worm_ID)) {
    worm_i <- data.cp$beh1_name[which(data.cp$worm_ID == i)]
    observation <- c(observation, worm_i)
}

# Baum-Welch
bw = baumWelch(hmm, observation, 10)
print(bw$hmm)

sim.worm <- simHMM(bw$hmm, 1289)
sim.worm$obs.code <- rep(NA, 1289)
for(i in 1:length(sim.worm$observation)) {
    sim.worm$obs.code[i] <- which(behaviors == sim.worm$observation[i])
}

# Plot worm #1 behavior as a discrete function of time
png("sim-worm-behavior.png", width = 1200, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA)
plot(data.cp$frame[which(data.cp$worm_ID == 1)], 
     sim.worm$obs.code,
     xlab = "Frame",
     ylab = "Behavior (Code)",
     main = "Worm_ID => 1",
     type = "l")
dev.off()