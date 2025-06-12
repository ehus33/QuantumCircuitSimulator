import { pgTable, text, serial, integer, boolean, jsonb } from "drizzle-orm/pg-core";
import { createInsertSchema } from "drizzle-zod";
import { z } from "zod";

export const users = pgTable("users", {
  id: serial("id").primaryKey(),
  username: text("username").notNull().unique(),
  password: text("password").notNull(),
});

export const circuits = pgTable("circuits", {
  id: serial("id").primaryKey(),
  name: text("name").notNull(),
  equations: jsonb("equations").notNull(),
  groverIterations: integer("grover_iterations").notNull().default(2),
  userId: integer("user_id"),
  createdAt: text("created_at").notNull(),
});

export const insertUserSchema = createInsertSchema(users).pick({
  username: true,
  password: true,
});

export const insertCircuitSchema = createInsertSchema(circuits).pick({
  name: true,
  equations: true,
  groverIterations: true,
  userId: true,
});

export type InsertUser = z.infer<typeof insertUserSchema>;
export type User = typeof users.$inferSelect;
export type InsertCircuit = z.infer<typeof insertCircuitSchema>;
export type Circuit = typeof circuits.$inferSelect;

// Equation system types
export const equationSchema = z.object({
  a: z.number(),
  b: z.number(),
  c: z.number(),
});

export const equationSystemSchema = z.object({
  equation1: equationSchema,
  equation2: equationSchema,
});

export type Equation = z.infer<typeof equationSchema>;
export type EquationSystem = z.infer<typeof equationSystemSchema>;

// Quantum simulation types
export const quantumStateSchema = z.object({
  x: z.number(),
  y: z.number(),
  probability: z.number(),
});

export const simulationResultSchema = z.object({
  quantumSolution: quantumStateSchema,
  classicalSolution: quantumStateSchema,
  iterations: z.number(),
  successProbability: z.number(),
  speedup: z.number(),
});

export type QuantumState = z.infer<typeof quantumStateSchema>;
export type SimulationResult = z.infer<typeof simulationResultSchema>;
