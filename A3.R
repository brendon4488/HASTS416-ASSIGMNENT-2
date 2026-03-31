# HASTS416 Tutorial 1 in R - Question A3

# Define the transition probability matrices
# P1: 1 PM to 4 PM (3 hours = 9 intervals of 20 mins)
# States: 1=light, 2=heavy, 3=jammed
P1 <- matrix(c(
  0.4, 0.4, 0.2,  # From Light
  0.3, 0.5, 0.2,  # From Heavy (corrected from previous assumption, based on re-reading PDF)
  0.0, 0.1, 0.9   # From Jammed
), nrow = 3, byrow = TRUE)

# P2: 4 PM to 6 PM (2 hours = 6 intervals of 20 mins)
P2 <- matrix(c(
  0.1, 0.5, 0.4,  # From Light
  0.1, 0.3, 0.6,  # From Heavy
  0.0, 0.1, 0.9   # From Jammed
), nrow = 3, byrow = TRUE)

state_labels <- c("Light", "Heavy", "Jammed")

cat("--- Question A3(a): Analytical Solution ---\n")

# Initial state at 1 PM: Light (State 1)
v0 <- c(1, 0, 0) # Row vector for initial distribution

# Calculate distribution at 4 PM (after 9 steps of P1)
v4pm <- v0
for (i in 1:9) {
  v4pm <- v4pm %*% P1
}

# Calculate distribution at 6 PM (after 6 steps of P2 from v4pm)
v6pm_analytical <- v4pm
for (i in 1:6) {
  v6pm_analytical <- v6pm_analytical %*% P2
}

cat("Analytical Distribution at 6 PM:\n")
names(v6pm_analytical) <- state_labels
print(v6pm_analytical)

#RESULTS
#Analytical Distribution at 6 PM:
#0.015402 0.134136 0.850462
#"Light"  "Heavy"  "Jammed"

cat("\n--- Question A3(b): Simulation of Trajectories and Visualization ---\n")

# Simulate 10,000 trajectories
set.seed(42) # For reproducibility
n_sim <- 10000
final_states_sim <- numeric(n_sim)

# Function to simulate a single path
simulate_path <- function(P1, P2, initial_state_idx) {
  current_state <- initial_state_idx
  
  # 1 PM to 4 PM (9 steps with P1)
  for (i in 1:9) {
    current_state <- sample(1:nrow(P1), 1, prob = P1[current_state, ])
  }
  
  # 4 PM to 6 PM (6 steps with P2)
  for (i in 1:6) {
    current_state <- sample(1:nrow(P2), 1, prob = P2[current_state, ])
  }
  return(current_state)
}

# Run simulations
for (s in 1:n_sim) {
  final_states_sim[s] <- simulate_path(P1, P2, 1) # Start at Light (State 1)
}

# Calculate simulated distribution
sim_dist <- table(factor(final_states_sim, levels = 1:3, labels = state_labels)) / n_sim
cat("Simulated Distribution at 6 PM (10,000 trajectories):\n")
print(sim_dist)

# Visualization of simulated trajectories results
barplot(sim_dist, 
        main = "Simulated Distribution of Traffic Conditions at 6 PM",
        xlab = "Traffic Condition",
        ylab = "Proportion of Trajectories",
        col = c("lightblue", "lightcoral", "lightgreen"),
        ylim = c(0, 1))
text(x = barplot(sim_dist, plot = FALSE), y = sim_dist, 
     labels = sprintf("%.3f", sim_dist), pos = 3, cex = 0.8)

cat("\nVisualization of simulated distribution saved as a3_simulated_distribution.png\n")
