      program test000
!
!     This is my first Fortran program. It is a "Hello, World!" program.
!     Lecture Notes: StdIn = read(*,*); StdOut = write(*,*)
!
      implicit none
      integer::year
!
      year = 2023
      write(*,*) 'Hello, world!'
      write(*,*) 'It is', year
!
      end program test000
