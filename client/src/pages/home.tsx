import { useState, useEffect } from "react";
import { Card } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Atom, HelpCircle, Settings } from "lucide-react";
import InputPanel from "@/components/input-panel";
import CircuitVisualization from "@/components/circuit-visualization";
import EducationalPanel from "@/components/educational-panel";
import type { EquationSystem, SimulationResult } from "@shared/schema";

export default function Home() {
  const [equations, setEquations] = useState<EquationSystem>({
    equation1: { a: 1, b: 1, c: 2 },
    equation2: { a: 0, b: 1, c: 1 }
  });

  const [groverIterations, setGroverIterations] = useState(2);
  const [isSimulating, setIsSimulating] = useState(false);
  const [simulationResult, setSimulationResult] = useState<SimulationResult | null>(null);
  const [currentStep, setCurrentStep] = useState(0);

  return (
    <div className="min-h-screen bg-slate-50">
      {/* Header */}
      <header className="bg-white border-b border-slate-200 px-6 py-4 shadow-sm">
        <div className="max-w-7xl mx-auto flex items-center justify-between">
          <div className="flex items-center space-x-3">
            <div className="w-10 h-10 bg-gradient-to-br from-blue-600 to-purple-600 rounded-lg flex items-center justify-center">
              <Atom className="text-white text-xl" size={20} />
            </div>
            <div>
              <h1 className="text-xl font-bold text-slate-900">Quantum Circuit Simulator</h1>
              <p className="text-sm text-slate-600">Grover's Algorithm for 2-Variable Systems</p>
            </div>
          </div>
          <div className="flex items-center space-x-4">
            <Button variant="ghost" size="sm">
              <HelpCircle size={18} />
            </Button>
            <Button variant="ghost" size="sm">
              <Settings size={18} />
            </Button>
          </div>
        </div>
      </header>

      <div className="max-w-7xl mx-auto px-6 py-8">
        <div className="grid grid-cols-1 lg:grid-cols-12 gap-8">
          {/* Input Panel */}
          <div className="lg:col-span-3">
            <InputPanel
              equations={equations}
              onEquationsChange={setEquations}
              groverIterations={groverIterations}
              onIterationsChange={setGroverIterations}
              onSimulate={() => setIsSimulating(true)}
              isSimulating={isSimulating}
            />
          </div>

          {/* Circuit Visualization */}
          <div className="lg:col-span-6">
            <CircuitVisualization
              equations={equations}
              groverIterations={groverIterations}
              isSimulating={isSimulating}
              onSimulationComplete={(result) => {
                setSimulationResult(result);
                setIsSimulating(false);
              }}
              currentStep={currentStep}
              onStepChange={setCurrentStep}
            />
          </div>

          {/* Educational Panel */}
          <div className="lg:col-span-3">
            <EducationalPanel
              simulationResult={simulationResult}
              equations={equations}
            />
          </div>
        </div>
      </div>

      {/* Footer */}
      <footer className="bg-white border-t border-slate-200 mt-16">
        <div className="max-w-7xl mx-auto px-6 py-8">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            <div>
              <h4 className="text-sm font-semibold text-slate-900 mb-3">Resources</h4>
              <ul className="space-y-2 text-sm text-slate-600">
                <li><a href="#" className="hover:text-slate-900 transition-colors">Quantum Computing Basics</a></li>
                <li><a href="#" className="hover:text-slate-900 transition-colors">Grover's Algorithm Theory</a></li>
                <li><a href="#" className="hover:text-slate-900 transition-colors">Haskell & Quipper</a></li>
              </ul>
            </div>
            <div>
              <h4 className="text-sm font-semibold text-slate-900 mb-3">Examples</h4>
              <ul className="space-y-2 text-sm text-slate-600">
                <li><a href="#" className="hover:text-slate-900 transition-colors">Database Search</a></li>
                <li><a href="#" className="hover:text-slate-900 transition-colors">SAT Solving</a></li>
                <li><a href="#" className="hover:text-slate-900 transition-colors">Optimization Problems</a></li>
              </ul>
            </div>
            <div>
              <h4 className="text-sm font-semibold text-slate-900 mb-3">Tools</h4>
              <ul className="space-y-2 text-sm text-slate-600">
                <li><a href="#" className="hover:text-slate-900 transition-colors">Circuit Builder</a></li>
                <li><a href="#" className="hover:text-slate-900 transition-colors">State Visualizer</a></li>
                <li><a href="#" className="hover:text-slate-900 transition-colors">Code Generator</a></li>
              </ul>
            </div>
            <div>
              <h4 className="text-sm font-semibold text-slate-900 mb-3">Community</h4>
              <ul className="space-y-2 text-sm text-slate-600">
                <li><a href="#" className="hover:text-slate-900 transition-colors">Documentation</a></li>
                <li><a href="#" className="hover:text-slate-900 transition-colors">GitHub Repository</a></li>
                <li><a href="#" className="hover:text-slate-900 transition-colors">Discussion Forum</a></li>
              </ul>
            </div>
          </div>
          <div className="border-t border-slate-200 mt-8 pt-8 text-center text-sm text-slate-600">
            © 2024 Quantum Circuit Simulator. Built for educational purposes.
          </div>
        </div>
      </footer>
    </div>
  );
}
