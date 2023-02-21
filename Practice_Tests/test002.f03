      program test002
!
!     This program asks the user for two numbers. The program calculates the sum of the range of numbers between them.
!
      implicit none
      integer::iStart,iEnd,total,i
      logical::fail=.false.
!
      write(*,*) 'Enter the lower number in your range.'
      read(*,*) iStart
      write(*,*) 'Enter the upper number in your range.'
      read(*,*) iEnd
!
      if(iStart > iEnd) then
        fail=.true.
        write(*,*) 'Invalid iStart and/or iEnd.'
        goto 999
      endIf
!
      total = 0
      do i = iStart,iEnd
        total = total + i
      endDo
      write(*,*) 'Sum(iStart,...,iEnd)=',total
!
  999 if(fail) write(*,*) 'The program FAILED!'
!
      end program test002
