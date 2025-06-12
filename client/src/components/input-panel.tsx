import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Slider } from "@/components/ui/slider";
import { Play, Loader2 } from "lucide-react";
import { solveLinearSystem } from "@/lib/equation-solver";
import type { EquationSystem } from "@shared/schema";

interface InputPanelProps {
  equations: EquationSystem;
  onEquationsChange: (equations: EquationSystem) => void;
  groverIterations: number;
  onIterationsChange: (iterations: number) => void;
  onSimulate: () => void;
  isSimulating: boolean;
}

export default function InputPanel({
  equations,
  onEquationsChange,
  groverIterations,
  onIterationsChange,
  onSimulate,
  isSimulating
}: InputPanelProps) {
  const handleEquationChange = (
    equationKey: 'equation1' | 'equation2',
    field: 'a' | 'b' | 'c',
    value: string
  ) => {
    const numValue = parseFloat(value) || 0;
    onEquationsChange({
      ...equations,
      [equationKey]: {
        ...equations[equationKey],
        [field]: numValue
      }
    });
  };

  const loadExample = (example: 'simple' | 'linear') => {
    if (example === 'simple') {
      onEquationsChange({
        equation1: { a: 1, b: 1, c: 2 },
        equation2: { a: 0, b: 1, c: 1 }
      });
    } else {
      onEquationsChange({
        equation1: { a: 2, b: 1, c: 3 },
        equation2: { a: 1, b: -1, c: 0 }
      });
    }
  };

  const solution = solveLinearSystem(equations);
  const optimalIterations = Math.ceil(Math.sqrt(16)); // √(N/M) where N=16, M≈1

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">System of Equations</CardTitle>
          <p className="text-sm text-muted-foreground">Define your 2-variable linear system</p>
        </CardHeader>
        <CardContent className="space-y-6">
          {/* Equation 1 */}
          <div>
            <Label className="text-sm font-medium mb-3 block">Equation 1</Label>
            <div className="flex items-center space-x-2">
              <Input
                type="number"
                value={equations.equation1.a}
                onChange={(e) => handleEquationChange('equation1', 'a', e.target.value)}
                className="w-16"
                placeholder="a"
              />
              <span className="text-muted-foreground font-mono">x +</span>
              <Input
                type="number"
                value={equations.equation1.b}
                onChange={(e) => handleEquationChange('equation1', 'b', e.target.value)}
                className="w-16"
                placeholder="b"
              />
              <span className="text-muted-foreground font-mono">y =</span>
              <Input
                type="number"
                value={equations.equation1.c}
                onChange={(e) => handleEquationChange('equation1', 'c', e.target.value)}
                className="w-16"
                placeholder="c"
              />
            </div>
          </div>

          {/* Equation 2 */}
          <div>
            <Label className="text-sm font-medium mb-3 block">Equation 2</Label>
            <div className="flex items-center space-x-2">
              <Input
                type="number"
                value={equations.equation2.a}
                onChange={(e) => handleEquationChange('equation2', 'a', e.target.value)}
                className="w-16"
                placeholder="a"
              />
              <span className="text-muted-foreground font-mono">x +</span>
              <Input
                type="number"
                value={equations.equation2.b}
                onChange={(e) => handleEquationChange('equation2', 'b', e.target.value)}
                className="w-16"
                placeholder="b"
              />
              <span className="text-muted-foreground font-mono">y =</span>
              <Input
                type="number"
                value={equations.equation2.c}
                onChange={(e) => handleEquationChange('equation2', 'c', e.target.value)}
                className="w-16"
                placeholder="c"
              />
            </div>
          </div>

          {/* Solution Preview */}
          <div className="bg-muted rounded-lg p-4">
            <h3 className="text-sm font-medium mb-2">Classical Solution</h3>
            <div className="text-sm font-mono text-muted-foreground">
              {solution ? (
                <>
                  <div>x = {solution.x.toFixed(2)}</div>
                  <div>y = {solution.y.toFixed(2)}</div>
                </>
              ) : (
                <div className="text-destructive">No unique solution</div>
              )}
            </div>
          </div>

          {/* Grover Parameters */}
          <div>
            <Label className="text-sm font-medium mb-3 block">Grover Iterations</Label>
            <div className="flex items-center space-x-3">
              <Slider
                value={[groverIterations]}
                onValueChange={(value) => onIterationsChange(value[0])}
                min={1}
                max={5}
                step={1}
                className="flex-1"
              />
              <span className="w-8 text-sm text-muted-foreground font-mono">{groverIterations}</span>
            </div>
            <p className="text-xs text-muted-foreground mt-2">
              Optimal: ~√(N/M) = √(16/1) ≈ {optimalIterations}
            </p>
          </div>

          <Button 
            onClick={onSimulate} 
            className="w-full bg-blue-600 hover:bg-blue-700"
            disabled={isSimulating || !solution}
          >
            {isSimulating ? (
              <>
                <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                Simulating...
              </>
            ) : (
              <>
                <Play className="mr-2 h-4 w-4" />
                Run Quantum Simulation
              </>
            )}
          </Button>
        </CardContent>
      </Card>

      {/* Quick Examples */}
      <Card>
        <CardHeader>
          <CardTitle className="text-sm">Quick Examples</CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          <Button
            variant="ghost"
            className="w-full justify-start p-3 h-auto"
            onClick={() => loadExample('simple')}
          >
            <div className="text-left">
              <div className="text-sm font-medium">Simple Addition</div>
              <div className="text-xs text-muted-foreground font-mono">x + y = 2, y = 1</div>
            </div>
          </Button>
          <Button
            variant="ghost"
            className="w-full justify-start p-3 h-auto"
            onClick={() => loadExample('linear')}
          >
            <div className="text-left">
              <div className="text-sm font-medium">Linear System</div>
              <div className="text-xs text-muted-foreground font-mono">2x + y = 3, x - y = 0</div>
            </div>
          </Button>
        </CardContent>
      </Card>
    </div>
  );
}
