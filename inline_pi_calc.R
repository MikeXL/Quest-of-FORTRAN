library(inline)
code <- "
C monte carlo estimate Pi
C
        k = 0
        do 10 i=1,1000000
          x = rand()
          y = rand()
          if(x**2 + y**2 .lt. 1.0) k=k+1
 10     continue
        pi(1) = real(k) / 1000000 * 4
"
mc_pi <- cfunction(sigature(pi="numeric"), code, convention=".Fortran")
mc_pi(pi)
