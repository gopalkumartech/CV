install.packages(diffeqr)
library(diffeqr)
de <- diffeqr::diffeq_setup()
degpu <- diffeqr::diffeqgpu_setup()

lorenz <- function (u,p,t){
  du1 = p[1]*(u[2]-u[1])
  du2 = u[1]*(p[2]-u[3]) - u[2]
  du3 = u[1]*u[2] - p[3]*u[3]
  c(du1,du2,du3)
}
u0 <- c(1.0,1.0,1.0)
tspan <- c(0.0,100.0)
p <- c(10.0,28.0,8/3)
prob <- de$ODEProblem(lorenz,u0,tspan,p)
fastprob <- diffeqr::jitoptimize_ode(de,prob)

prob_func <- function (prob,i,rep){
  de$remake(prob,u0=runif(3)*u0,p=runif(3)*p)
}
ensembleprob = de$EnsembleProblem(fastprob, prob_func = prob_func, safetycopy=FALSE)
sol <- de$solve(ensembleprob,de$Tsit5(),degpu$EnsembleGPUArray(),trajectories=10000,saveat=0.01)
