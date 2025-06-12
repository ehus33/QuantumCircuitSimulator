import { useEffect, useRef } from "react";
import type { EquationSystem } from "@shared/schema";

interface QuantumCircuitCanvasProps {
  equations: EquationSystem;
  groverIterations: number;
  currentStep: number;
  isSimulating: boolean;
}

interface Gate {
  type: 'H' | 'CNOT' | 'Toffoli' | 'X' | 'M';
  qubit: number;
  controls?: number[];
  x: number;
  active?: boolean;
}

interface QubitLine {
  label: string;
  y: number;
  type: 'variable' | 'auxiliary' | 'output';
}

export default function QuantumCircuitCanvas({
  equations,
  groverIterations,
  currentStep,
  isSimulating
}: QuantumCircuitCanvasProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null);

  const qubitLines: QubitLine[] = [
    { label: "|x₀⟩", y: 50, type: 'variable' },
    { label: "|x₁⟩", y: 90, type: 'variable' },
    { label: "|y₀⟩", y: 130, type: 'variable' },
    { label: "|y₁⟩", y: 170, type: 'variable' },
    { label: "|a₀⟩", y: 230, type: 'auxiliary' },
    { label: "|a₁⟩", y: 270, type: 'auxiliary' },
    { label: "|out⟩", y: 330, type: 'output' }
  ];

  useEffect(() => {
    drawCircuit();
  }, [equations, groverIterations, currentStep, isSimulating]);

  const drawCircuit = () => {
    const canvas = canvasRef.current;
    if (!canvas) return;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    // Clear canvas
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    // Set up drawing context
    ctx.font = '14px JetBrains Mono, monospace';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    // Draw qubit lines and labels
    drawQubitLines(ctx, canvas);
    
    // Draw gates based on current step
    const gates = generateGates();
    drawGates(ctx, gates);
    
    // Draw control lines
    drawControlLines(ctx, gates);
    
    // Draw legend
    drawLegend(ctx, canvas);
  };

  const drawQubitLines = (ctx: CanvasRenderingContext2D, canvas: HTMLCanvasElement) => {
    qubitLines.forEach((line, index) => {
      // Draw label
      ctx.fillStyle = line.type === 'variable' ? '#64748b' : '#94a3b8';
      ctx.textAlign = 'left';
      ctx.fillText(line.label, 10, line.y);

      // Draw line
      ctx.strokeStyle = line.type === 'variable' ? '#cbd5e1' : '#e2e8f0';
      ctx.lineWidth = 2;
      ctx.beginPath();
      ctx.moveTo(80, line.y);
      ctx.lineTo(canvas.width - 50, line.y);
      ctx.stroke();

      // Add separators between sections
      if (index === 3 || index === 5) {
        ctx.strokeStyle = '#e2e8f0';
        ctx.lineWidth = 1;
        ctx.setLineDash([5, 5]);
        ctx.beginPath();
        ctx.moveTo(0, line.y + 20);
        ctx.lineTo(canvas.width, line.y + 20);
        ctx.stroke();
        ctx.setLineDash([]);
      }
    });
  };

  const generateGates = (): Gate[] => {
    const gates: Gate[] = [];
    let x = 120;

    // Initialization: Hadamard gates on variable qubits
    for (let i = 0; i < 4; i++) {
      gates.push({
        type: 'H',
        qubit: i,
        x: x,
        active: currentStep >= 0
      });
    }
    x += 80;

    // Grover iterations
    for (let iter = 0; iter < groverIterations; iter++) {
      // Oracle gates
      gates.push({
        type: 'Toffoli',
        qubit: 4, // aux0
        controls: [0, 1], // x0, x1
        x: x,
        active: currentStep >= 1 + iter * 2
      });

      gates.push({
        type: 'CNOT',
        qubit: 5, // aux1  
        controls: [3], // y1
        x: x + 20,
        active: currentStep >= 1 + iter * 2
      });

      gates.push({
        type: 'Toffoli',
        qubit: 6, // output
        controls: [4, 5], // aux0, aux1
        x: x + 40,
        active: currentStep >= 1 + iter * 2
      });

      x += 100;

      // Diffusion gates (simplified representation)
      for (let i = 0; i < 4; i++) {
        gates.push({
          type: 'H',
          qubit: i,
          x: x,
          active: currentStep >= 2 + iter * 2
        });
      }
      x += 80;
    }

    // Measurement
    for (let i = 0; i < 4; i++) {
      gates.push({
        type: 'M',
        qubit: i,
        x: x,
        active: currentStep >= 1 + groverIterations * 2
      });
    }

    return gates;
  };

  const drawGates = (ctx: CanvasRenderingContext2D, gates: Gate[]) => {
    gates.forEach(gate => {
      const y = qubitLines[gate.qubit].y;
      
      ctx.save();
      
      // Set gate appearance based on activity
      if (gate.active) {
        ctx.fillStyle = getGateColor(gate.type);
        ctx.strokeStyle = '#ffffff';
      } else {
        ctx.fillStyle = '#f1f5f9';
        ctx.strokeStyle = '#cbd5e1';
      }

      ctx.lineWidth = 2;

      // Draw gate based on type
      switch (gate.type) {
        case 'H':
          drawRectGate(ctx, gate.x, y, 'H');
          break;
        case 'CNOT':
          drawCNOTGate(ctx, gate.x, y);
          break;
        case 'Toffoli':
          drawToffoliGate(ctx, gate.x, y);
          break;
        case 'X':
          drawRectGate(ctx, gate.x, y, 'X');
          break;
        case 'M':
          drawMeasurementGate(ctx, gate.x, y);
          break;
      }

      ctx.restore();
    });
  };

  const drawRectGate = (ctx: CanvasRenderingContext2D, x: number, y: number, label: string) => {
    const width = 32;
    const height = 32;
    
    ctx.fillRect(x - width/2, y - height/2, width, height);
    ctx.strokeRect(x - width/2, y - height/2, width, height);
    
    ctx.fillStyle = '#ffffff';
    ctx.textAlign = 'center';
    ctx.font = 'bold 14px JetBrains Mono, monospace';
    ctx.fillText(label, x, y);
  };

  const drawCNOTGate = (ctx: CanvasRenderingContext2D, x: number, y: number) => {
    const radius = 16;
    
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, 2 * Math.PI);
    ctx.fill();
    ctx.stroke();
    
    ctx.fillStyle = '#ffffff';
    ctx.font = 'bold 16px JetBrains Mono, monospace';
    ctx.fillText('⊕', x, y);
  };

  const drawToffoliGate = (ctx: CanvasRenderingContext2D, x: number, y: number) => {
    drawCNOTGate(ctx, x, y); // Same visual as CNOT for simplicity
  };

  const drawMeasurementGate = (ctx: CanvasRenderingContext2D, x: number, y: number) => {
    drawRectGate(ctx, x, y, 'M');
  };

  const drawControlLines = (ctx: CanvasRenderingContext2D, gates: Gate[]) => {
    ctx.strokeStyle = '#3b82f6';
    ctx.lineWidth = 2;
    ctx.setLineDash([4, 4]);

    gates.forEach(gate => {
      if (gate.controls && gate.active) {
        const targetY = qubitLines[gate.qubit].y;
        
        gate.controls.forEach(controlQubit => {
          const controlY = qubitLines[controlQubit].y;
          
          // Draw control dot
          ctx.fillStyle = '#3b82f6';
          ctx.beginPath();
          ctx.arc(gate.x, controlY, 6, 0, 2 * Math.PI);
          ctx.fill();
          
          // Draw control line
          ctx.beginPath();
          ctx.moveTo(gate.x, controlY);
          ctx.lineTo(gate.x, targetY);
          ctx.stroke();
        });
      }
    });

    ctx.setLineDash([]);
  };

  const drawLegend = (ctx: CanvasRenderingContext2D, canvas: HTMLCanvasElement) => {
    const legendY = canvas.height - 60;
    const legendItems = [
      { symbol: 'H', color: '#8b5cf6', label: 'Hadamard' },
      { symbol: '⊕', color: '#059669', label: 'CNOT/Toffoli' },
      { symbol: '●', color: '#3b82f6', label: 'Control' },
      { symbol: 'M', color: '#64748b', label: 'Measure' }
    ];

    ctx.font = '12px Inter, sans-serif';
    ctx.textAlign = 'left';

    legendItems.forEach((item, index) => {
      const x = 20 + index * 120;
      
      // Draw symbol
      ctx.fillStyle = item.color;
      if (item.symbol === '●') {
        ctx.beginPath();
        ctx.arc(x + 8, legendY, 6, 0, 2 * Math.PI);
        ctx.fill();
      } else {
        ctx.fillRect(x, legendY - 8, 16, 16);
        ctx.fillStyle = '#ffffff';
        ctx.textAlign = 'center';
        ctx.font = 'bold 12px JetBrains Mono, monospace';
        ctx.fillText(item.symbol, x + 8, legendY);
      }
      
      // Draw label
      ctx.fillStyle = '#64748b';
      ctx.textAlign = 'left';
      ctx.font = '12px Inter, sans-serif';
      ctx.fillText(item.label, x + 24, legendY);
    });
  };

  const getGateColor = (type: string): string => {
    switch (type) {
      case 'H': return '#8b5cf6';
      case 'CNOT':
      case 'Toffoli': return '#059669';
      case 'X': return '#ef4444';
      case 'M': return '#64748b';
      default: return '#64748b';
    }
  };

  return (
    <div className="bg-slate-50 rounded-lg p-6">
      <canvas
        ref={canvasRef}
        width={800}
        height={450}
        className="circuit-canvas w-full h-auto border border-slate-200 rounded bg-white"
      />
    </div>
  );
}
