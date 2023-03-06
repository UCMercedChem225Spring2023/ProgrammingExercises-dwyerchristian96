      Program prgm_01_03
!
!     This program reads 3x3 matrices from two user-provided input files. After
!     each file is opened and read, it is closed and then printed. The product
!     of the two matrices is calculated and then printed.
!
!     Program Author:  Christian Dwyer, Chemistry and Biochemistry, UC Merced
!     Program Advisor: Hrant Hratchian, Chemistry and Biochemistry, UC Merced
!
      implicit none
      integer,parameter::inFileUnitA=10,inFileUnitB=10
      integer::errorFlag,i,j
      real,dimension(3,3)::matrixInA,matrixInB,matrixAB
      character(len=128)::fileNameA,fileNameB
!
!
!     Ask the user for the name of the 1st data file.
!
      write(*,*)' What is the name of the 1st input data file?'
      read(*,*) fileNameA
!
!     Open the 1st data file and read matrixInA from that file.
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
!     Ask the user for the name of the 2nd data file.
!
      write(*,*)' What is the name of the 2nd input data file?'
      read(*,*) fileNameB
!
!     Open the 2nd data file and read matrixInB from that file.
!
      open(unit=inFileUnitB,file=TRIM(fileNameB),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do j = 1,3
        read(inFileUnitB,*) matrixInB(1,j),matrixInB(2,j),matrixInB(3,j)
      endDo
      close(inFileUnitB)
!
!     Call the subroutine PrintMatrix to print matrixInB.
!
      call PrintMatrix3x3(matrixInB)
!
!     Calculate the product of matrixInA and matrixInB.
!
      write(*,*)' Calculating Matrix Product'
      matrixAB = matmul(transpose(matrixInA),transpose(matrixInB))
!
!     Call the subroutine PrintMatrix to print matrixAB.
!
      call PrintMatrix3x3(transpose(matrixAB))
!
  999 continue
      End Program prgm_01_03


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
      do i = 1,3
        write(*,1000) matrix(1,i),matrix(2,i),matrix(3,i)
      endDo
!
!
      return
      End Subroutine PrintMatrix3x3
