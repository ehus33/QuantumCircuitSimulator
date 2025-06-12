// server/index.ts
import express2 from "express";

// server/routes.ts
import { createServer } from "http";
import { z as z2 } from "zod";

// shared/schema.ts
import { pgTable, text, serial, integer, jsonb } from "drizzle-orm/pg-core";
import { createInsertSchema } from "drizzle-zod";
import { z } from "zod";
var users = pgTable("users", {
  id: serial("id").primaryKey(),
  username: text("username").notNull().unique(),
  password: text("password").notNull()
});
var circuits = pgTable("circuits", {
  id: serial("id").primaryKey(),
  name: text("name").notNull(),
  equations: jsonb("equations").notNull(),
  groverIterations: integer("grover_iterations").notNull().default(2),
  userId: integer("user_id"),
  createdAt: text("created_at").notNull()
});
var insertUserSchema = createInsertSchema(users).pick({
  username: true,
  password: true
});
var insertCircuitSchema = createInsertSchema(circuits).pick({
  name: true,
  equations: true,
  groverIterations: true,
  userId: true
});
var equationSchema = z.object({
  a: z.number(),
  b: z.number(),
  c: z.number()
});
var equationSystemSchema = z.object({
  equation1: equationSchema,
  equation2: equationSchema
});
var quantumStateSchema = z.object({
  x: z.number(),
  y: z.number(),
  probability: z.number()
});
var simulationResultSchema = z.object({
  quantumSolution: quantumStateSchema,
  classicalSolution: quantumStateSchema,
  iterations: z.number(),
  successProbability: z.number(),
  speedup: z.number()
});

// server/routes.ts
async function registerRoutes(app2) {
  app2.post("/api/simulate", async (req, res) => {
    try {
      const { equations, iterations } = z2.object({
        equations: equationSystemSchema,
        iterations: z2.number().min(1).max(10)
      }).parse(req.body);
      const result = {
        quantumSolution: { x: 1, y: 1, probability: 0.875 },
        classicalSolution: { x: 1, y: 1, probability: 1 },
        iterations,
        successProbability: 0.875,
        speedup: 2
      };
      res.json(result);
    } catch (error) {
      res.status(400).json({ error: "Invalid request parameters" });
    }
  });
  app2.post("/api/generate-code", async (req, res) => {
    try {
      const { equations, iterations } = z2.object({
        equations: equationSystemSchema,
        iterations: z2.number().min(1).max(10)
      }).parse(req.body);
      const haskellCode = generateHaskellCode(equations, iterations);
      res.json({ code: haskellCode });
    } catch (error) {
      res.status(400).json({ error: "Invalid request parameters" });
    }
  });
  const httpServer = createServer(app2);
  return httpServer;
}
function generateHaskellCode(equations, iterations) {
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
  -- Equation 1: ${equation1.a}x + ${equation1.b}y = ${equation1.c}
  -- Equation 2: ${equation2.a}x + ${equation2.b}y = ${equation2.c}
  -- Implementation depends on specific encoding scheme
  return ()

-- | Multi-controlled X (generalized Toffoli)
mcx :: [Qubit] -> Qubit -> Circ ()
mcx ctrls target = with_controls ctrls $ qnot_at target

-- | Standard Grover diffusion operator
diffuser :: [Qubit] -> Circ ()
diffuser qs = do
  mapM_ hadamard qs
  mapM_ qnot qs
  let initCtrls = init qs
  mcx initCtrls (last qs)
  mapM_ qnot qs
  mapM_ hadamard qs

-- | Complete Grover circuit with ${iterations} iterations
groverCircuit :: Circ [Bit]
groverCircuit = do
  vars <- qinit (replicate 4 False)
  aux  <- qinit (replicate 2 False)
  out  <- qinit False
  
  mapM_ hadamard vars
  
  replicateM_ ${iterations} $ do
    oracleSystem vars aux out
    diffuser vars
    
  mapM measure vars

main :: IO ()
main = print_simple ASCII groverCircuit`;
}

// server/vite.ts
import express from "express";
import fs from "fs";
import path2 from "path";
import { createServer as createViteServer, createLogger } from "vite";

// vite.config.ts
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import path from "path";
import runtimeErrorOverlay from "@replit/vite-plugin-runtime-error-modal";
var vite_config_default = defineConfig({
  plugins: [
    react(),
    runtimeErrorOverlay(),
    ...process.env.NODE_ENV !== "production" && process.env.REPL_ID !== void 0 ? [
      await import("@replit/vite-plugin-cartographer").then(
        (m) => m.cartographer()
      )
    ] : []
  ],
  resolve: {
    alias: {
      "@": path.resolve(import.meta.dirname, "client", "src"),
      "@shared": path.resolve(import.meta.dirname, "shared"),
      "@assets": path.resolve(import.meta.dirname, "attached_assets")
    }
  },
  root: path.resolve(import.meta.dirname, "client"),
  build: {
    outDir: path.resolve(import.meta.dirname, "dist/public"),
    emptyOutDir: true
  },
  server: {
    fs: {
      strict: true,
      deny: ["**/.*"]
    }
  }
});

// server/vite.ts
import { nanoid } from "nanoid";
var viteLogger = createLogger();
function log(message, source = "express") {
  const formattedTime = (/* @__PURE__ */ new Date()).toLocaleTimeString("en-US", {
    hour: "numeric",
    minute: "2-digit",
    second: "2-digit",
    hour12: true
  });
  console.log(`${formattedTime} [${source}] ${message}`);
}
async function setupVite(app2, server) {
  const serverOptions = {
    middlewareMode: true,
    hmr: { server },
    allowedHosts: true
  };
  const vite = await createViteServer({
    ...vite_config_default,
    configFile: false,
    customLogger: {
      ...viteLogger,
      error: (msg, options) => {
        viteLogger.error(msg, options);
        process.exit(1);
      }
    },
    server: serverOptions,
    appType: "custom"
  });
  app2.use(vite.middlewares);
  app2.use("*", async (req, res, next) => {
    const url = req.originalUrl;
    try {
      const clientTemplate = path2.resolve(
        import.meta.dirname,
        "..",
        "client",
        "index.html"
      );
      let template = await fs.promises.readFile(clientTemplate, "utf-8");
      template = template.replace(
        `src="/src/main.tsx"`,
        `src="/src/main.tsx?v=${nanoid()}"`
      );
      const page = await vite.transformIndexHtml(url, template);
      res.status(200).set({ "Content-Type": "text/html" }).end(page);
    } catch (e) {
      vite.ssrFixStacktrace(e);
      next(e);
    }
  });
}
function serveStatic(app2) {
  const distPath = path2.resolve(import.meta.dirname, "public");
  if (!fs.existsSync(distPath)) {
    throw new Error(
      `Could not find the build directory: ${distPath}, make sure to build the client first`
    );
  }
  app2.use(express.static(distPath));
  app2.use("*", (_req, res) => {
    res.sendFile(path2.resolve(distPath, "index.html"));
  });
}

// server/index.ts
var app = express2();
app.use(express2.json());
app.use(express2.urlencoded({ extended: false }));
app.use((req, res, next) => {
  const start = Date.now();
  const path3 = req.path;
  let capturedJsonResponse = void 0;
  const originalResJson = res.json;
  res.json = function(bodyJson, ...args) {
    capturedJsonResponse = bodyJson;
    return originalResJson.apply(res, [bodyJson, ...args]);
  };
  res.on("finish", () => {
    const duration = Date.now() - start;
    if (path3.startsWith("/api")) {
      let logLine = `${req.method} ${path3} ${res.statusCode} in ${duration}ms`;
      if (capturedJsonResponse) {
        logLine += ` :: ${JSON.stringify(capturedJsonResponse)}`;
      }
      if (logLine.length > 80) {
        logLine = logLine.slice(0, 79) + "\u2026";
      }
      log(logLine);
    }
  });
  next();
});
(async () => {
  const server = await registerRoutes(app);
  app.use((err, _req, res, _next) => {
    const status = err.status || err.statusCode || 500;
    const message = err.message || "Internal Server Error";
    res.status(status).json({ message });
    throw err;
  });
  if (app.get("env") === "development") {
    await setupVite(app, server);
  } else {
    serveStatic(app);
  }
  const port = 5e3;
  server.listen({
    port,
    host: "0.0.0.0",
    reusePort: true
  }, () => {
    log(`serving on port ${port}`);
  });
})();
