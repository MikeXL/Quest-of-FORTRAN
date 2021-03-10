C23456789
C v0.0.1
C compile
C     f95 -shared -fPIC -o mc_pi.so mc_pi.f
C calling in R
C     dyn.load("~/a.out")
C     r = .Fortran("pso", gbest=rep(0, 4), x, y)
C sanity check on yhat
C     yhat = x %*% r$gbest
C     plot(yhat, pch=(y*2+6), col=(y*2+6))
C     abline(h=0, col="gray", lty=3)
C     legend("topleft", legend=c(0, 1), pch=c(6, 8), col=c(6, 8), bty="n")
C
C compute nn, return mse
C all hardcoded for this version
C   4 - 1
        function ann(w, x, y, yhat)
            double precision w(4, 1), x(100, 4), y(100,1), yhat(100,1)
            yhat = matmul(x, w)
            ann = sum((y-yhat)**2)/size(y)
        end function
C particle swarm optim
        subroutine pso(gbest, x, y)
        parameter (iter=1000, w=2.66, c1=.8, c2=.8, npar=12, nd=4)
        double precision par(npar, nd), pbest(npar, nd), gbest(nd)
        double precision r1(npar, nd), r2(npar, nd)
        double precision x(100, 4), y(100,1), yhat(100,1)
C initialize particles
        call random_number(par)
        call random_number(pbest)
        call random_number(gbest)
        do 99 i=1, iter
C update invdividual (personal) best
          do 69 j=1, npar
            parfit = ann(par(j, :), x, y, yhat)
            pbfit  = ann(pbest(j, :), x, y, yhat)
            if (parfit .lt. pbfit) then
              pbest(j, :) = par(j, :)
              kk = j
            end if
69        continue
C update global best
          gbfit  = ann(gbest, x, y, yhat)
          pbfit  = ann(pbest(kk, :), x, y, yhat)
          if ( pbfit  .lt. gbfit) then
            gbest  = pbest(kk, :)
          end if
C update particles
          call random_number(r1)
          call random_number(r2)
          par = w * par +
C cognitive velocity
     +      c1 * r1 * (pbest - par) +
C social velocity
     +      c2 * r2 *( transpose(spread(gbest, 2, npar)) - par)
99       continue
C        print *, gbest
C        print *, ann(gbest, x, y, yhat)
         end subroutine
