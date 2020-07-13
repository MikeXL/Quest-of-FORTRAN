C simply compile using f77 test.f
C then ./a.out 
      program main
C        print *, test_sum(1.1, 2.2)
         print *, pi_mc()
         pi = 0.0
         call mc_pi_r(pi)
         print *, pi
      end 

C not ideal, 
C suppose to be in separate files, 
C oh bother, test anyway
      function test_sum(a, b)
        real a, b
        test_sum = a + b
        return
      end function test_sum

      subroutine test_sum_r(a, b, answer)
        double precision a, b, answer
        answer = a + b
      end subroutine test_sum_r

C ya know why it is not named mc_pi
      function pi_mc()
        do 10 i=1,1000000
          x = rand()
          y = rand()
          if(x**2 + y**2 .lt. 1) k=k+1
10      continue
        pi_mc = real(k) / 1000000 * 4
      end function pi_mc
      
      subroutine mc_pi_r(pi)
        real pi
        k = 0
        do 20 i=1,1000000
          x = rand()
          y = rand()
          if(x**2 + y**2 .lt. 1) k=k+1
20        continue
        pi = real(k) / 1000000 * 4
      end subroutine mc_pi_r
 
