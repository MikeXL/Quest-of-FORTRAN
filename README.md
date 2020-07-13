# Quest-of-Fortran

[Here](https://rpubs.com/Mike/fortran_quest) published to rpubs.


#### Free form vs. Fix form

First things first, dated back to punch card days, first six characters
are reserved for label, and important not to exeed column 72

#### Hello world

          PROGRAM CIRCLE_OF_LIFE
          PRINT *, 'CIRCLE OF LIFE'
          END

#### Subroutine instead of function in R

*Function* cannot be called from R, instead *subroutine* can be called
by *.Fortran*, this mechanism is motivated purely for taking advantage
of Fortran numerical computation power. Safe data types are *integer*,
*double precision*, and *logical arguments*.


    C monte carlo estimate Pi
    C R always use double precision 
    C
          subroutine mc_pi_r(pi)
            double precision pi
            k = 0
            do 10 i=1,1000000
              x = rand()
              y = rand()
              if(x**2 + y**2 .lt. 1.0) k=k+1
    10      continue
            pi = real(k) / 1000000 * 4
          end subroutine mc_pi_r
          

Then compile it with *R CMD SHLIB -c mc\_pi\_r.f*

running it in R

      dyn.unload('mc_pi_r.so')
      dyn.load('mc_pi_r.so')
      .Fortran("mc_pi_r", pi=as.double(0))
      .C("mc_pi_r_", pi=as.double(0))

#### Some resources

[F77 tutorial](https://web.stanford.edu/class/me200c/tutorial_77/index.html)  
[make](https://www2.physics.ox.ac.uk/it-services/makefiles-for-beginners)  
[sing](http://www.mit.edu/people/dmredish/wwwMLRF/links/Humor/FORTRAN.html)
