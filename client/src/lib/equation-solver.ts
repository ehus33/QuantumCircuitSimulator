import type { EquationSystem } from "@shared/schema";

export interface Solution {
  x: number;
  y: number;
  isUnique: boolean;
}

export function solveLinearSystem(equations: EquationSystem): Solution | null {
  const { equation1, equation2 } = equations;
  
  // System of equations:
  // a1*x + b1*y = c1
  // a2*x + b2*y = c2
  
  const a1 = equation1.a;
  const b1 = equation1.b;
  const c1 = equation1.c;
  
  const a2 = equation2.a;
  const b2 = equation2.b;
  const c2 = equation2.c;
  
  // Calculate determinant
  const det = a1 * b2 - a2 * b1;
  
  // Check for no solution or infinite solutions
  if (Math.abs(det) < 1e-10) {
    // Lines are parallel
    if (Math.abs(a1 * c2 - a2 * c1) < 1e-10 && Math.abs(b1 * c2 - b2 * c1) < 1e-10) {
      // Infinite solutions (same line)
      return null;
    } else {
      // No solution (parallel lines)
      return null;
    }
  }
  
  // Unique solution using Cramer's rule
  const x = (c1 * b2 - c2 * b1) / det;
  const y = (a1 * c2 - a2 * c1) / det;
  
  return {
    x: Math.round(x * 100) / 100, // Round to 2 decimal places
    y: Math.round(y * 100) / 100,
    isUnique: true
  };
}

export function validateEquationSystem(equations: EquationSystem): {
  isValid: boolean;
  errors: string[];
} {
  const errors: string[] = [];
  
  // Check for zero coefficients in both equations
  if (equations.equation1.a === 0 && equations.equation1.b === 0) {
    errors.push("Equation 1 has all zero coefficients");
  }
  
  if (equations.equation2.a === 0 && equations.equation2.b === 0) {
    errors.push("Equation 2 has all zero coefficients");
  }
  
  // Check for identical equations
  const eq1 = equations.equation1;
  const eq2 = equations.equation2;
  
  if (eq1.a === eq2.a && eq1.b === eq2.b && eq1.c === eq2.c) {
    errors.push("Both equations are identical");
  }
  
  // Check for proportional equations (parallel lines)
  if (eq1.a !== 0 || eq2.a !== 0) {
    const ratioA = eq2.a / eq1.a;
    const ratioB = eq2.b / eq1.b;
    const ratioC = eq2.c / eq1.c;
    
    if (Math.abs(ratioA - ratioB) < 1e-10 && Math.abs(ratioA - ratioC) > 1e-10) {
      errors.push("Equations represent parallel lines (no solution)");
    }
  }
  
  return {
    isValid: errors.length === 0,
    errors
  };
}

export function generateEquationExamples(): EquationSystem[] {
  return [
    {
      equation1: { a: 1, b: 1, c: 2 },
      equation2: { a: 0, b: 1, c: 1 }
    },
    {
      equation1: { a: 2, b: 1, c: 3 },
      equation2: { a: 1, b: -1, c: 0 }
    },
    {
      equation1: { a: 3, b: 2, c: 7 },
      equation2: { a: 1, b: 1, c: 3 }
    },
    {
      equation1: { a: 1, b: 2, c: 5 },
      equation2: { a: 2, b: -1, c: 1 }
    }
  ];
}
