import { useState, useEffect } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { RotateCcw, Download, SkipBack, SkipForward } from "lucide-react";
import QuantumCircuitCanvas from "./quantum-circuit-canvas";
import { simulateGroverAlgorithm } from "@/lib/quantum-simulator";
import type { EquationSystem, SimulationResult } from "@shared/schema";

interface CircuitVisualizationProps {
  equations: EquationSystem;
  groverIterations: number;
  isSimulating: boolean;
  onSimulationComplete: (result: SimulationResult) => void;
  currentStep: number;
  onStepChange: (step: number) => void;
}

interface ExecutionStep {
  id: number;
  name: string;
  description: string;
  duration: string;
  type: 'initialization' | 'oracle' | 'diffusion' | 'measurement';
}

export default function CircuitVisualization({
  equations,
  groverIterations,
  isSimulating,
  onSimulationComplete,
  currentStep,
  onStepChange
}: CircuitVisualizationProps) {
  const [executionSteps, setExecutionSteps] = useState<ExecutionStep[]>([]);
  const [circuitKey, setCircuitKey] = useState(0);

  useEffect(() => {
    if (isSimulating) {
      runSimulation();
    }
  }, [isSimulating]);

  useEffect(() => {
    generateExecutionSteps();
  }, [groverIterations]);

  const generateExecutionSteps = () => {
    const steps: ExecutionStep[] = [
      {
        id: 1,
        name: "Initialize Superposition",
        description: "Apply Hadamard gates to create uniform superposition of all states",
        duration: "0.5ms",
        type: "initialization"
      }
    ];

    for (let i = 0; i < groverIterations; i++) {
      steps.push({
        id: steps.length + 1,
        name: `Oracle Application (Iteration ${i + 1})`,
        description: "Mark target states satisfying the equation system",
        duration: "1.2ms",
        type: "oracle"
      });

      steps.push({
        id: steps.length + 1,
        name: `Diffusion Operator (Iteration ${i + 1})`,
        description: "Amplify marked states and suppress unmarked ones",
        duration: "0.8ms",
        type: "diffusion"
      });
    }

    steps.push({
      id: steps.length + 1,
      name: "Measurement",
      description: "Measure qubits to extract final result",
      duration: "0.3ms",
      type: "measurement"
    });

    setExecutionSteps(steps);
  };

  const runSimulation = async () => {
    // Simulate step-by-step execution
    for (let i = 0; i < executionSteps.length; i++) {
      onStepChange(i);
      await new Promise(resolve => setTimeout(resolve, 500));
    }

    // Generate final result
    const result = simulateGroverAlgorithm(equations, groverIterations);
    onSimulationComplete(result);
  };

  const resetCircuit = () => {
    onStepChange(0);
    setCircuitKey(prev => prev + 1);
  };

  const exportCircuit = () => {
    const haskellCode = generateHaskellCode();
    const blob = new Blob([haskellCode], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'grover_circuit.hs';
    a.click();
    URL.revokeObjectURL(url);
  };

  const generateHaskellCode = () => {
    const { equation1, equation2 } = equations;
    return `{-# LANGUAGE FlexibleContexts #-}

module GroverCircuit where

import Quipper
import Control.Monad (replicateM_)

-- | Oracle for the system ${equation1.a}x+${equation1.b}y=${equation1.c} and ${equation2.a}x+${equation2.b}y=${equation2.c}
oracleSystem :: [Qubit] -> [Qubit] -> Qubit -> Circ ()
oracleSystem vars aux out = do
  let [v0,v1,v2,v3] = vars
      [a0,a1]      = aux
  -- Implement equation constraints
  -- ${equation1.a}x + ${equation1.b}y = ${equation1.c}
  -- ${equation2.a}x + ${equation2.b}y = ${equation2.c}
  -- (Implementation details would depend on specific equation encoding)
  
-- | Grover circuit with ${groverIterations} iterations
groverCircuit :: Circ [Bit]
groverCircuit = do
  vars <- qinit (replicate 4 False)
  aux  <- qinit (replicate 2 False)
  out  <- qinit False
  
  mapM_ hadamard vars
  
  replicateM_ ${groverIterations} $ do
    oracleSystem vars aux out
    diffuser vars
    
  mapM measure vars

main :: IO ()
main = print_simple ASCII groverCircuit`;
  };

  const stepColors = {
    initialization: "bg-purple-500",
    oracle: "bg-emerald-500",
    diffusion: "bg-blue-500",
    measurement: "bg-slate-500"
  };

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="text-lg">Quantum Circuit</CardTitle>
              <p className="text-sm text-muted-foreground">Grover's algorithm implementation</p>
            </div>
            <div className="flex items-center space-x-2">
              <Button variant="outline" size="sm" onClick={resetCircuit}>
                <RotateCcw className="h-4 w-4 mr-1" />
                Reset
              </Button>
              <Button variant="outline" size="sm" onClick={exportCircuit}>
                <Download className="h-4 w-4 mr-1" />
                Export
              </Button>
            </div>
          </div>
        </CardHeader>
        <CardContent>
          <QuantumCircuitCanvas
            key={circuitKey}
            equations={equations}
            groverIterations={groverIterations}
            currentStep={currentStep}
            isSimulating={isSimulating}
          />
        </CardContent>
      </Card>

      {/* Execution Timeline */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Execution Timeline</CardTitle>
          <p className="text-sm text-muted-foreground">Step-by-step algorithm execution</p>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            {executionSteps.map((step, index) => (
              <div
                key={step.id}
                className={`flex items-center space-x-4 p-3 rounded-lg transition-colors ${
                  index <= currentStep ? 'bg-slate-50' : 'bg-muted/50'
                } ${index === currentStep ? 'ring-2 ring-blue-500' : ''}`}
              >
                <div className={`flex-shrink-0 w-8 h-8 ${stepColors[step.type]} text-white rounded-full flex items-center justify-center text-sm font-semibold`}>
                  {step.id}
                </div>
                <div className="flex-1">
                  <div className="text-sm font-medium">{step.name}</div>
                  <div className="text-xs text-muted-foreground">{step.description}</div>
                </div>
                <div className="text-xs text-emerald-600 font-mono">{step.duration}</div>
              </div>
            ))}
          </div>

          <div className="mt-6 flex items-center justify-between">
            <div className="text-sm text-muted-foreground">
              Step <span className="font-semibold">{currentStep + 1}</span> of{" "}
              <span>{executionSteps.length}</span>
            </div>
            <div className="flex items-center space-x-2">
              <Button
                variant="outline"
                size="sm"
                onClick={() => onStepChange(Math.max(0, currentStep - 1))}
                disabled={currentStep === 0}
              >
                <SkipBack className="h-4 w-4 mr-1" />
                Previous
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={() => onStepChange(Math.min(executionSteps.length - 1, currentStep + 1))}
                disabled={currentStep === executionSteps.length - 1}
              >
                Next
                <SkipForward className="h-4 w-4 ml-1" />
              </Button>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
