      Program prgm_03_01
!
!     This program reads a file name from the command line, opens that
!     file, and loads a packed/linearized form of a square matrix. Then,
!     the packed matrix is expanded assuming a row-packed form and printed.
!     Finally, the packed matrix is expanded as a column-packed form and
!     printed.
!
!     The input file is expected to have the leading dimension (an integer
!     NDim) of the matrix on the first line. The next NDim*NDim lines each
!     have one real number each given.
!
      Implicit None
      Integer,Parameter::IIn=10
      Integer::IError,NDim,i,j
      Real,Dimension(:),Allocatable::Array_Input
      Real,Dimension(:,:),Allocatable::Matrix
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
      Allocate(Array_Input(NDim*NDim),Matrix(NDim,NDim))
!
! *************************************************************************
! WRITE CODE HERE TO READ THE ARRAY ELEMENTS FROM THE INPUT FILE.
! *************************************************************************
!
!
!     Convert Array_Input to Matrix and print the matrix.
!
      Write(*,*)' The matrix expanded according to a row-wise ', &
        'linear packed format:'
      Call Packed2Matrix_RowWise(NDim,NDim,Array_Input,Matrix)
      Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
      Write(*,*)' The matrix expanded according to a column-wise ', &
        'linear packed format:'
      Call Packed2Matrix_ColumnWise(NDim,NDim,Array_Input,Matrix)
      Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
!
      End Program prgm_03_01


      Subroutine Packed2Matrix_ColumnWise(M,N,ArrayIn,AMatOut)
!
!     This subroutine accepts an array, ArrayIn, that is M*N long. It then
!     takes those elements and converts them to the M-by-N matrix AMatOut
!     such that the elements in ArrayIn are interpreted as a packed
!     column-wise form of the matrix AMatOut.
!
      Implicit None
      Integer,Intent(In)::M,N
      Real,Dimension(N*M),Intent(In)::ArrayIn
      Real,Dimension(M,N),Intent(Out)::AMatOut
!
      Integer::i,j,k
!
!     Loop through the elements of AMatOut and fill them appropriately from
!     Array_Input.
!
!
! *************************************************************************
! WRITE CODE HERE TO READ THE ARRAY ELEMENTS FROM THE INPUT FILE.
! *************************************************************************
!
!
      Return
      End Subroutine Packed2Matrix_ColumnWise


      Subroutine Packed2Matrix_RowWise(M,N,ArrayIn,AMatOut)
!
!     This subroutine accepts an array, ArrayIn, that is M*N long. It then
!     takes those elements and converts them to the M-by-N matrix AMatOut
!     such that the elements in ArrayIn are interpreted as a packed
!     row-wise form of the matrix AMatOut.
!
      Implicit None
      Integer,Intent(In)::M,N
      Real,Dimension(N*M),Intent(In)::ArrayIn
      Real,Dimension(M,N),Intent(Out)::AMatOut
!
      Integer::i,j,k
!
!     Loop through the elements of AMatOut and fill them appropriately from
!     Array_Input.
!
!
! *************************************************************************
! WRITE CODE HERE TO READ THE ARRAY ELEMENTS FROM THE INPUT FILE.
! *************************************************************************
!
!
      Return
      End Subroutine Packed2Matrix_RowWise
