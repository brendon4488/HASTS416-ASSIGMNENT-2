# Load libraries
library(markovchain)
library(igraph)

# Define your matrix as provided in the tutorial 
P_mat <- matrix(c(
  0, 1, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0,
  0.3, 0, 0, 0.1, 0.3, 0.1, 0.2,
  0, 0, 0, 0.4, 0.2, 0.2, 0.2,
  0, 0, 0, 0.2, 0.4, 0.4, 0,
  0, 0, 0, 0.2, 0.2, 0.3, 0.3,
  0, 0, 0, 0.5, 0.2, 0.2, 0.1
), nrow = 7, byrow = TRUE)

# Create the Markov Chain object
mcA2 <- new("markovchain", transitionMatrix = P_mat, name = "A2_Chain")

# Enhanced Plotting
# We use 'layout_with_fr' to separate the disjoint components visually
plot(mcA2, 
     package = "igraph", 
     layout = layout_with_fr, 
     vertex.size = 25, 
     vertex.color = "lightblue",
     edge.arrow.size = 0.5,
     main = "A2 Markov Chain: Visualizing Disjoint Classes")



#Recurrent and Transient Classes
#Recurrent Class 1 $\{1, 2\}$: These two states communicate only with each other ($1 \to 2$ and $2 \to 1$). Once the process enters this set, it never leaves.
#Recurrent Class 2 $\{4, 5, 6, 7\}$: These states all communicate with each other with non-zero probabilities.
#Transient Class $\{3\}$: State 3 can move to state 1 (in Recurrent Class 1) or states 4, 5, 6, and 7 (in Recurrent Class 2). However, there is no path back to state 3 from any other state.

#Periods
#States 1 and 2: Period is 2. The chain must oscillate between 1 and 2 at every step.
#States 4, 5, 6, and 7: Period is 1 (Aperiodic). These states have self-loops (e.g., $P_{4,4} = 0.4$), which automatically makes them aperiodic.
#State 3: Period is 1 (though as a transient state, it is typically described by its transition destination)
#.Absorbing and Reflecting States
#Absorbing States: There are no absorbing states (a state where $P_{i,i} = 1$).
#Reflecting States: States 1 and 2 act as a reflecting pair—if you are in 1, you are "reflected" back to 2, and vice versa.

# Simulate two trajectories of 20 steps each
sim1 <- rmarkovchain(n = 20, object = mcA2, t0 = sample(1:7, 1))
sim2 <- rmarkovchain(n = 20, object = mcA2, t0 = sample(1:7, 1))

print(sim1)
print(sim2)


#Analysis of sim1: The $\{1, 2\}$ Recurrent Class
#The output for sim1 shows a perfect oscillation: "2" "1" "2" "1" "2" "1"...
#Behavior: This trajectory has entered (or started in) the first recurrent class consisting of states 1 and 2.
#The Period of 2: This confirms our classification that states 1 and 2 have a period of 2. Because $P_{1,2} = 1$ and $P_{2,1} = 1$, the chain is forced to "reflect" back and forth between these two states with 100% certainty at every step.
#Deterministic Nature: Once the chain is in this class, it is impossible for it to escape to any other state.
#


#Analysis of sim2: The $\{4, 5, 6, 7\}$ Recurrent Class
#The output for sim2 shows a stochastic (random) walk: "5" "6" "5" "6" "7" "6" "6" "6"...
#Behavior: This trajectory is moving within the second recurrent class. Unlike sim1, there is no fixed pattern here. It moves between states 4, 5, 6, and 7 with varying frequencies.
#Aperiodicity: Note that you see sequences like "6" "6" "6". This is possible because these states have non-zero probabilities of transitioning to themselves (self-loops), such as $P_{6,6} = 0.3$ and $P_{4,4} = 0.4$. This confirms these states are aperiodic.
#Communication: The trajectory demonstrates that states 4, 5, 6, and 7 all "communicate," meaning you can eventually reach any one of these states from any other within the same class.

