import type { EquationSystem, SimulationResult, QuantumState } from "@shared/schema";
import { solveLinearSystem } from "./equation-solver";

export function simulateGroverAlgorithm(
  equations: EquationSystem,
  iterations: number
): SimulationResult {
  // Get classical solution
  const classicalSolution = solveLinearSystem(equations);
  
  if (!classicalSolution) {
    throw new Error("No unique solution exists for the given system");
  }

  // Simulate quantum algorithm
  const numStates = 16; // 2^4 for 4 qubits (2 bits each for x,y)
  const numSolutions = countValidSolutions(equations);
  
  // Calculate success probability after iterations
  const theta = Math.asin(Math.sqrt(numSolutions / numStates));
  const probability = Math.pow(Math.sin((2 * iterations + 1) * theta), 2);
  
  // Quantum solution (same as classical for this deterministic case)
  const quantumSolution: QuantumState = {
    x: classicalSolution.x,
    y: classicalSolution.y,
    probability: probability
  };

  // Calculate speedup (theoretical)
  const classicalComplexity = numStates; // O(N) for classical search
  const quantumComplexity = Math.sqrt(numStates); // O(√N) for Grover
  const speedup = classicalComplexity / quantumComplexity;

  return {
    quantumSolution,
    classicalSolution: {
      x: classicalSolution.x,
      y: classicalSolution.y,
      probability: 1.0
    },
    iterations,
    successProbability: probability,
    speedup
  };
}

function countValidSolutions(equations: EquationSystem): number {
  let count = 0;
  
  // Check all possible 2-bit values for x and y (0-3 each)
  for (let x = 0; x <= 3; x++) {
    for (let y = 0; y <= 3; y++) {
      if (satisfiesEquations(equations, x, y)) {
        count++;
      }
    }
  }
  
  return Math.max(1, count); // Ensure at least 1 to avoid division by zero
}

function satisfiesEquations(equations: EquationSystem, x: number, y: number): boolean {
  const eq1Satisfied = equations.equation1.a * x + equations.equation1.b * y === equations.equation1.c;
  const eq2Satisfied = equations.equation2.a * x + equations.equation2.b * y === equations.equation2.c;
  
  return eq1Satisfied && eq2Satisfied;
}

export function calculateOptimalIterations(equations: EquationSystem): number {
  const numStates = 16;
  const numSolutions = countValidSolutions(equations);
  
  // Optimal number of iterations is approximately π/4 * √(N/M)
  return Math.ceil((Math.PI / 4) * Math.sqrt(numStates / numSolutions));
}

export function generateQuantumStates(
  equations: EquationSystem,
  iteration: number
): Array<{ state: string; amplitude: number; phase: number }> {
  const states = [];
  
  for (let x = 0; x <= 3; x++) {
    for (let y = 0; y <= 3; y++) {
      const state = `|${x.toString(2).padStart(2, '0')}${y.toString(2).padStart(2, '0')}⟩`;
      const isSolution = satisfiesEquations(equations, x, y);
      
      // Simplified amplitude calculation
      let amplitude = 1 / Math.sqrt(16); // Initial uniform superposition
      
      if (iteration > 0 && isSolution) {
        // Amplify solution states
        amplitude *= Math.sqrt(iteration + 1);
      }
      
      states.push({
        state,
        amplitude: Math.abs(amplitude),
        phase: isSolution && iteration > 0 ? Math.PI : 0
      });
    }
  }
  
  return states;
}
