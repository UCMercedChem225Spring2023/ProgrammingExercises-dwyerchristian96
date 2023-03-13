      Program prgm_03_03
!
!     This program reads a file name from the command line, opens that
!     file, and loads a packed form of a symmetric matrix. Then, the packed
!     matrix is expanded assuming a column-wise lower-triangle form and
!     printed. Finally, the matrix is diagonalized using the LAPack routine
!     SSPEV. The eigenvalues and eigenvectors are printed.
!
!     The input file is expected to have the leading dimension (an integer
!     NDim) of the matrix on the first line. The next (NDim*(NDim+1))/2
!     lines each have one real number each given.
!
!     Program Author:  Christian Dwyer, Chemistry and Biochemistry, UC Merced
!     Program Advisor: Hrant Hratchian, Chemistry and Biochemistry, UC Merced
!
!
      Implicit None
      Integer::IIn,IError,NDim,i,j
      Real,Dimension(:),Allocatable::Array_Input,EVals,Temp_Vector
      Real,Dimension(:,:),Allocatable::Matrix,EVecs,Temp_Matrix
      Character(Len=256)::FileName
!
!     Begin by reading the input file name from the command line. Then,
!     open the file and read the input array, Array_Input.
!
      Call Get_Command_Argument(1,FileName)
      Open(Unit=IIn,File=TRIM(FileName),Status='OLD',IOStat=IError)
      If(IError.ne.0) then
        Write(*,*)' Error opening input file.'
        STOP
      endIf
      Read(IIn,*) NDim
      Allocate(Array_Input((NDim*(NDim+1))/2),Matrix(NDim,NDim))
      Allocate(EVals(NDim),EVecs(NDim,NDim),Temp_Vector(3*NDim))
      Allocate(Temp_Matrix(NDim,NDim))
      Do i = 1,((NDim*(NDim+1))/2)
        Read(IIn,*) Array_Input(i)
      endDo
      Close(Unit=IIn)
!
!     Convert Array_Input to Matrix and print the matrix.
!
      Write(*,*)' The matrix loaded (column) lower-triangle packed:'
      Call SymmetricPacked2Matrix_LowerPac(NDim,Array_Input,Matrix)
      Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
      Call SSPEV('V','L',NDim,Array_Input,EVals,EVecs,NDim,Temp_Vector,IError)
      If(IError.ne.0) then
        Write(*,*)' Failure in DSPEV.'
        STOP
      endIf
      Write(*,*)' EVals:'
      Call Print_Matrix_Full_Real(RESHAPE(EVals,(/1,NDim/)),1,NDim)
      Write(*,*)' EVecs:'
      Call Print_Matrix_Full_Real(EVecs,NDim,NDim)
!
      End Program prgm_03_03


      Subroutine SymmetricPacked2Matrix_LowerPac(N,ArrayIn,AMatOut)
!
!     This subroutine accepts an array, ArrayIn, that is (N*(N+1))/2 long.
!     It then converts that form to the N-by-N matrix AMatOut taking
!     ArrayIn to be in lower-packed storage form. Note: The storage mode
!     also assumes the lower-packed storage is packed by columns.
!
      Implicit None
      Integer,Intent(In)::N
      Real,Dimension((N*(N+1))/2),Intent(In)::ArrayIn
      Real,Dimension(N,N),Intent(Out)::AMatOut
      Integer::i,j,k
!
!     Loop through the elements of AMatOut and fill them appropriately from
!     Array_Input.
!
      k = 1
      Do j = 1,N
        Do i = 1,N
          If(i.eq.j) then
            AMatOut(i,i) = ArrayIn(k)
            k = k + 1
          Else If(i.gt.j) then
            AMatOut(i,j) = ArrayIn(k)
            AMatOut(j,i) = ArrayIn(k)
            k = k + 1
          endIf
        endDo
      endDo
!
      Return
      End Subroutine SymmetricPacked2Matrix_LowerPac


      Subroutine Print_Matrix_Full_Real(AMat,M,N)
!
!     This subroutine prints a real matrix that is fully dimension - i.e.,
!     not stored in packed form. AMat is the matrix, which is dimensioned
!     (M,N).
!
!     The output of this routine is sent to unit number 6 (set by the local
!     parameter integer IOut).
!
!
!     Variable Declarations
      implicit none
      integer,intent(in)::M,N
      real,dimension(M,N),intent(in)::AMat
!
!     Local variables
      integer,parameter::IOut=6,NColumns=5
      integer::i,j,IFirst,ILast
!
 1000 Format(1x,A)
 2000 Format(5x,5(7x,I7))
 2010 Format(1x,I7,5F14.6)
!
      Do IFirst = 1,N,NColumns
        ILast = Min(IFirst+NColumns-1,N)
        write(IOut,2000) (i,i=IFirst,ILast)
        Do i = 1,M
          write(IOut,2010) i,(AMat(i,j),j=IFirst,ILast)
        endDo
      endDo
!
      Return
      End Subroutine Print_Matrix_Full_Real
