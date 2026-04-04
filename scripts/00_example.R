# ── 0. Simuleret eksempel ─────────────────────────────────────
# Simuler en AR(1) tidsserie
set.seed(42)
n   <- 200
phi <- 0.8
z   <- rnorm(n)
x   <- numeric(n)

for (t in 2:n) {
  x[t] <- phi * x[t-1] + z[t]
}

# ── 1. Plot ───────────────────────────────────────────────────
plot(x, type = "l",
     main = "Simuleret AR(1) tidsserie",
     xlab = "Tid", ylab = "Værdi",
     col  = "steelblue")

# Gem plottet
dev.copy(png, "output/example_plot.png", width = 800, height = 400)
dev.off()

cat("Plot gemt i output/example_plot.png\n")
