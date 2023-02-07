      Program prgm_01_01
!
!     This program reads a 3x3 matrix from a user-provided input file. After the
!     file is opened and read, it is closed and then printed.
!
!
      implicit none
      integer,parameter::inFileUnitA=10
      integer::errorFlag,i
      real,dimension(3,3)::matrixInA
      character(len=128)::fileNameA
!
!
!     Start by asking the user for the name of the data file.
!
      write(*,*)' What is the name of the input data file?'
      read(*,*) fileNameA
!
!     Open the data file and read matrixInA from that file.
!
      open(unit=inFileUnitA,file=TRIM(fileNameA),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitA,*) matrixInA(1,i),matrixInA(2,i),matrixInA(3,i)
      endDo
      close(inFileUnitA)
!
!     Call the subroutine PrintMatrix to print matrixInA.
!
      call PrintMatrix3x3(matrixInA)
!
  999 continue
      End Program prgm_01_01


      Subroutine PrintMatrix3x3(matrix)
!
!     This subroutine prints a 3x3 real matrix. The output is written to StdOut.
!
      implicit none
      real,dimension(3,3),intent(in)::matrix
      integer::i
!
!     Format statements.
!
 1000 format(3(2x,f5.1))
!
!     Do the printing job.
!
      write(*,*)' Printing Matrix'
!
      ADD CODE HERE
!
!
      return
      End Subroutine PrintMatrix3x3
