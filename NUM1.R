#STUDENT DETAILS
# REG NUM: R227541E
# NAME: BRENDON T MAKURUMURE
# PROGRAM: HDSC

# HASTS416 Tutorial 1 in R - Question A1

# (a) Define transition probability matrix P
P <- matrix(c(
  1.0, 0, 0, 0, 0,
  0.5, 0, 0, 0, 0.5,
  0.2, 0, 0, 0, 0.8,
  0, 0, 1.0, 0, 0,
  0, 0, 0, 1.0, 0
), nrow = 5, byrow = TRUE)

#rownames(P) <- colnames(P) <- 1:5
mc <- new("markovchain", 
          states = c("S1", "S2", "S3", "S4", "S5"), 
          transitionMatrix = P_matrix, 
          name = "A1 Markov Chain")

# 2. Plotting the diagram 
plot(mc, edge.arrow.size = 0.5, main = " A1 Markov Chain State Diagram")

# Analysis of Markov Chain:
# State 1: P(1,1) = 1. This is an absorbing state.
# States 2, 3, 4, 5: Transient states.
# Transition structure:
# 2 -> 1 (0.5) or 5 (0.5)
# 3 -> 1 (0.2) or 5 (0.8)
# 4 -> 3 (1.0)
# 5 -> 4 (1.0)
# All paths lead eventually to State 1.

# (b) Simulate three trajectories
simulate_chain <- function(P, start_state, n_steps) {
  states <- numeric(n_steps + 1)
  states[1] <- start_state
  for (i in 1:n_steps) {
    states[i+1] <- sample(1:nrow(P), 1, prob = P[states[i], ])
  }
  return(states)
}

set.seed(42)
cat("Simulating Trajectories:\n")
for (i in 1:3) {
  start <- sample(1:5, 1)
  traj <- simulate_chain(P, start, 20)
  cat(paste("Trajectory", i, "starting at", start, ":", paste(traj, collapse=" "), "\n"))
}


#Trajectory 1 starting at 1 : 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
#Trajectory 1: The "Escape" from Transience
#The Cycle: Notice how the chain moves deterministically from $S5 \to S4$ and $S4 \to S3$. This confirms that these states belong to a communicating class with a periodic nature.
#The Transition: At state $S3$, there is a 0.2 probability of moving to $S1$ and a 0.8 probability of moving back to $S5$. In this trajectory, the chain circled the transient states three times before eventually "escaping" to $S1$.
#Absorption: Once it hits $S1$, it stays there indefinitely because $p_{11} = 1.0$.

#Trajectory 2 starting at 3 : 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
#Trajectory 2: The Transient Loop
#Persistence: This trajectory shows the chain trapped in the transient cycle $\{S3, S4, S5\}$.
#Observation: Although $S3$ has a path to the absorbing state $S1$, it is not guaranteed to happen immediately. This trajectory demonstrates that a process can spend a significant amount of time in transient states before absorption occurs.

#Trajectory 3 starting at 2 : 2 5 4 3 5 4 3 5 4 3 5 4 3 5 4 3 5 4 3 5 4
#Trajectory 3: Immediate Absorption
#Absorbing Behavior: This trajectory confirms that $S1$ is an absorbing state.
#Stability: Since the starting state was $S1$ and $p_{11} = 1$, the state never changes. This is a "stationary" trajectory, representing the long-term behavior of the chain once it reaches this state.


# (c) Steady-state probabilities
# Since state 1 is absorbing and the chain is irreducible to state 1, 
# the steady state distribution is (1, 0, 0, 0, 0).
# Let's verify by computing P^100.
Pn <- P
for (i in 1:100) {
  Pn <- Pn %*% P
}
cat("\nP^100 (Approximation of limiting distribution):\n")
print(Pn)

# (d) Unconditional probabilities over time
initial_dist <- rep(0.2, 5)
n_steps <- 50
probs_matrix <- matrix(0, nrow = n_steps + 1, ncol = 5)
probs_matrix[1, ] <- initial_dist

for (i in 1:n_steps) {
  probs_matrix[i+1, ] <- probs_matrix[i, ] %*% P
}

plot(0:n_steps, probs_matrix[, 1], type="l", col="blue", ylim=c(0, 1), 
     xlab="Time n", ylab="Probability", main="Unconditional Probabilities (A1)")
lines(0:n_steps, probs_matrix[, 2], col="red")
lines(0:n_steps, probs_matrix[, 3], col="green")
lines(0:n_steps, probs_matrix[, 4], col="orange")
lines(0:n_steps, probs_matrix[, 5], col="purple")
legend("right", legend=paste("State", 1:5), col=c("blue", "red", "green", "orange", "purple"), lty=1)

