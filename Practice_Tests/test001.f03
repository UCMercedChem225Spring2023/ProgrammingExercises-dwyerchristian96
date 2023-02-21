      program test001
!
!     This is my first Fortran program. It is a "Hello, World!" program.
!
      implicit none
      integer::year
!
      write(*,*) 'What year is it?'
      read(*,*) year
      write(*,*)
      write(*,*) 'Hello, world!'
      write(*,*) 'It is', year
!
      end program test001
