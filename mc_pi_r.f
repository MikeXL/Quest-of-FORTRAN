C monte carlo estimate Pi
C always double precision for R 
C compile using R CMD SHLIB mc_pi_r.f
C23456789
      subroutine mc_pi_r(pi)
        double precision pi
        k = 0
        do 10 i=1,1000000
          x = rand()
          y = rand()
          if(x**2 + y**2 .lt. 1.0) k=k+1
 10     continue
        pi = real(k) / 1000000 * 4
      end subroutine mc_pi_r
