import { useState } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { ChevronDown, ChevronUp } from "lucide-react";
import type { SimulationResult, EquationSystem } from "@shared/schema";

interface EducationalPanelProps {
  simulationResult: SimulationResult | null;
  equations: EquationSystem;
}

interface ConceptItem {
  id: string;
  title: string;
  description: string;
  color: string;
}

export default function EducationalPanel({
  simulationResult,
  equations
}: EducationalPanelProps) {
  const [expandedExplanations, setExpandedExplanations] = useState<Set<string>>(new Set());

  const toggleExplanation = (id: string) => {
    setExpandedExplanations(prev => {
      const newSet = new Set(prev);
      if (newSet.has(id)) {
        newSet.delete(id);
      } else {
        newSet.add(id);
      }
      return newSet;
    });
  };

  const concepts: ConceptItem[] = [
    {
      id: 'superposition',
      title: 'Superposition',
      description: 'Qubits exist in multiple states simultaneously, allowing quantum algorithms to process all possible solutions in parallel.',
      color: 'bg-blue-500'
    },
    {
      id: 'interference',
      title: 'Interference',
      description: 'Quantum amplitudes can add constructively or destructively, allowing us to amplify correct answers and suppress incorrect ones.',
      color: 'bg-purple-500'
    },
    {
      id: 'entanglement',
      title: 'Entanglement',
      description: 'Quantum correlations between qubits that allow for complex logical operations and constraints in our equation system.',
      color: 'bg-emerald-500'
    }
  ];

  const explanations = [
    {
      id: 'grover',
      title: "Grover's Algorithm",
      content: "Grover's algorithm provides a quadratic speedup for searching unsorted databases. It uses quantum superposition and interference to amplify the probability of measuring the correct answer. For our 2-variable system, it searches through all possible combinations of x and y values to find solutions that satisfy both equations."
    },
    {
      id: 'oracle',
      title: "Oracle Function",
      content: `The oracle marks solutions by flipping the phase of states that satisfy our equation system. It implements the constraints ${equations.equation1.a}x + ${equations.equation1.b}y = ${equations.equation1.c} and ${equations.equation2.a}x + ${equations.equation2.b}y = ${equations.equation2.c} using quantum logic gates. When both constraints are satisfied, the oracle flips the phase of that quantum state.`
    },
    {
      id: 'diffusion',
      title: "Diffusion Operator",
      content: "The diffusion operator reflects the amplitude about the average, amplifying marked states and suppressing unmarked ones. This creates constructive interference for solutions and destructive interference for non-solutions. It's implemented using Hadamard gates, X gates, and a multi-controlled Z gate."
    }
  ];

  return (
    <div className="space-y-6">
      {/* Results Panel */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Results</CardTitle>
          <p className="text-sm text-muted-foreground">Quantum vs Classical comparison</p>
        </CardHeader>
        <CardContent className="space-y-4">
          {simulationResult ? (
            <>
              <div className="bg-emerald-50 border border-emerald-200 rounded-lg p-4">
                <div className="text-sm font-medium text-emerald-700 mb-2">Quantum Solution</div>
                <div className="font-mono text-lg">
                  x={simulationResult.quantumSolution.x}, y={simulationResult.quantumSolution.y}
                </div>
                <div className="text-xs text-muted-foreground mt-1">
                  Probability: {(simulationResult.quantumSolution.probability * 100).toFixed(1)}%
                </div>
              </div>
              
              <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
                <div className="text-sm font-medium text-slate-700 mb-2">Classical Solution</div>
                <div className="font-mono text-lg">
                  x={simulationResult.classicalSolution.x}, y={simulationResult.classicalSolution.y}
                </div>
                <div className="text-xs text-muted-foreground mt-1">Time: O(n²)</div>
              </div>

              <div className="text-xs text-muted-foreground space-y-1">
                <div className="flex justify-between">
                  <span>Quantum speedup:</span>
                  <span className="font-semibold text-purple-600">~{simulationResult.speedup.toFixed(1)}x</span>
                </div>
                <div className="flex justify-between">
                  <span>Success probability:</span>
                  <span className="font-semibold">{(simulationResult.successProbability * 100).toFixed(1)}%</span>
                </div>
                <div className="flex justify-between">
                  <span>Iterations used:</span>
                  <span className="font-semibold">{simulationResult.iterations}</span>
                </div>
              </div>
            </>
          ) : (
            <div className="text-center py-8 text-muted-foreground">
              <p>Run a simulation to see results</p>
            </div>
          )}
        </CardContent>
      </Card>

      {/* Educational Content */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">How It Works</CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          {explanations.map(explanation => (
            <div key={explanation.id}>
              <Button
                variant="ghost"
                className="w-full justify-between p-3 h-auto"
                onClick={() => toggleExplanation(explanation.id)}
              >
                <span className="text-sm font-medium">{explanation.title}</span>
                {expandedExplanations.has(explanation.id) ? (
                  <ChevronUp className="h-4 w-4" />
                ) : (
                  <ChevronDown className="h-4 w-4" />
                )}
              </Button>
              {expandedExplanations.has(explanation.id) && (
                <div className="px-3 py-2 text-xs text-muted-foreground leading-relaxed">
                  {explanation.content}
                </div>
              )}
            </div>
          ))}
        </CardContent>
      </Card>

      {/* Quantum Concepts */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Key Concepts</CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          {concepts.map(concept => (
            <div key={concept.id} className="flex items-start space-x-3">
              <div className={`w-2 h-2 ${concept.color} rounded-full mt-2`}></div>
              <div>
                <div className="text-sm font-medium">{concept.title}</div>
                <div className="text-xs text-muted-foreground">{concept.description}</div>
              </div>
            </div>
          ))}
        </CardContent>
      </Card>

      {/* Mathematical Notation */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Mathematical Foundation</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="text-sm">
            <h4 className="font-medium mb-2">Current System:</h4>
            <div className="bg-slate-50 p-3 rounded font-mono text-sm">
              <div>${equations.equation1.a}x + ${equations.equation1.b}y = ${equations.equation1.c}$</div>
              <div>${equations.equation2.a}x + ${equations.equation2.b}y = ${equations.equation2.c}$</div>
            </div>
          </div>
          
          <div className="text-sm">
            <h4 className="font-medium mb-2">Grover Operator:</h4>
            <div className="bg-slate-50 p-3 rounded font-mono text-sm">
              $G = -U_s O U_s^\dagger$
            </div>
            <p className="text-xs text-muted-foreground mt-2">
              Where $U_s$ is the diffusion operator and $O$ is the oracle
            </p>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
