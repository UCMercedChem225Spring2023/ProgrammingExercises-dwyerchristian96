      program prgm_05_01
!
!     This program solves for the eigenvalues and eigenfunctions for the first
!     five states of a modified one-dimensional particle-in-a-box system. The
!     standard 1D PIB eigenfunctions will be used as the basis set. The user
!     will provide the number of basis functions to use in the calculation.
!
!     This program is written in atomic units.
!
!     Program Author:  Christian Dwyer, Chemistry and Biochemistry, UC Merced
!     Program Advisor: Hrant Hratchian, Chemistry and Biochemistry, UC Merced
!
!     Variable Declarations
      implicit none
      integer::NCmdLineArgs,IError,NDim,i,j,k
      real,external::PIB_1D_Modified_Hamiltonian_Element
      real,dimension(:),allocatable::Vector,EVals,Temp_Vector
      real,dimension(:,:),allocatable::Matrix,EVecs,Temp_Matrix
      real::b=01,m=01,l=01     ! Potential: V(x) = 01*x !
!     real::b=10,m=01,l=01     ! Potential: V(x) = 10*x !
      logical::fail
      character(len=1024)::cmd_buffer
!
!     Format Statements
 9000 format(1x,'Expected 1 command line argument, but found ',i2,'.')
!
!
!     Read in NDim from the command line.
!
      fail = .false.
      NCmdLineArgs = command_argument_count()
      if(NCmdLineArgs.ne.1) then
        write(*,9000) NCmdLineArgs
        fail = .true.
      endIf
      if(fail) goto 999
      call Get_Command_Argument(1,cmd_buffer)
      read(cmd_buffer,*) NDim
!
!     Given the leading dimension, evaluate the Hamiltonian integrals between
!     each unique pair of standard 1D PIB eigenfunctions. Store the values in
!     a column vector that represents a column-wise lower-triangle matrix.
!
      Allocate(Vector((NDim*(NDim+1))/2),Matrix(NDim,NDim))
      Allocate(Temp_Vector(3*NDim),Temp_Matrix(NDim,NDim))
      Allocate(EVals(NDim),EVecs(NDim,NDim))
      k = 1
      Do j = 1,NDim
        Do i = 1,NDim
          If(i.ge.j) then
            Vector(k) = PIB_1D_Modified_Hamiltonian_Element(b,m,l,i,j)
            k = k + 1
          endIf
        endDo
      endDo
!
!     Convert Vector to Matrix and print the matrix. The SSPEV subroutine
!     from the LAPack library is called to calculate the eigenvalues and
!     eigenvectors of the modified particle-in-a-box Hamiltonian matrix.
!
      Write(*,*)' The matrix loaded (column) lower-triangle packed:'
      Call SymmetricPacked2Matrix_LowerPac(NDim,Vector,Matrix)
      Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
      Call SSPEV('V','L',NDim,Vector,EVals,EVecs,NDim,Temp_Vector,IError)
      If(IError.ne.0) then
        Write(*,*)' Failure in DSPEV.'
        STOP
      endIf
      Write(*,*)' EVals:'
      Call Print_Matrix_Full_Real(RESHAPE(EVals,(/1,NDim/)),1,NDim)
      Write(*,*)' EVecs:'
      Call Print_Matrix_Full_Real(EVecs,NDim,NDim)
!
!     The end of the job...
!
  999 continue
      end program prgm_05_01


      real function PIB_1D_Modified_Hamiltonian_Element(b,m,l,n1,n2)
!
!     This function evaluates the Hamiltonian matrix element < n1 | H | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and H is the
!     Hamiltonian operator.
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::b,m,l
      integer,intent(in)::n1,n2
      real::tMatrixElement,vMatrixElement
      real,external::PIB_1D_T_Element,PIB_1D_Modified_V_Element
!
!     Given the input parameters, evaluate the sum of the kinetic and potential
!     energy integrals between particle-in-a-box eigenfunctions n1 and n2.
!
      tMatrixElement = PIB_1D_T_Element(m,l,n1,n2)
      vMatrixElement = PIB_1D_Modified_V_Element(b,l,n1,n2)
      PIB_1D_Modified_Hamiltonian_Element = tMatrixElement + vMatrixElement
!
      end function PIB_1D_Modified_Hamiltonian_Element


      real function PIB_1D_T_Element(m,l,n1,n2)
!
!     This function evaluates the kinetic energy matrix element < n1 | T | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and T is the
!     kinetic energy operator.
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::m,l
      integer,intent(in)::n1,n2
      real(kind=8),parameter::pi=4.D0*datan(1.D0)
!
!     The case where n1=n2 is different than n1\=n2. For this reason, we use an
!     if block to separate the evaluation of the kinetic energy integral for
!     these two different cases.
!
      if(n1.eq.n2) then
        PIB_1D_T_Element = (n1**2*pi**2)/(2*m*l**2)
      else
        PIB_1D_T_Element = 0
      endIf
!
      end function PIB_1D_T_Element


      real function PIB_1D_Modified_V_Element(b,l,n1,n2)
!
!     This function evaluates the potential energy matrix element < n1 | V | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and V is the
!     potential energy operator.
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::b,l
      integer,intent(in)::n1,n2
      real(kind=8),parameter::pi=4.D0*datan(1.D0)
!
!     The case where n1=n2 is different than n1\=n2. For this reason, we use an
!     if block to separate the evaluation of the potential energy integral for
!     these two different cases.
!
      if(n1.eq.n2) then
        PIB_1D_Modified_V_Element = (b*l)/2
      else
        PIB_1D_Modified_V_Element = (4*(-1+(-1)**(n1+n2))*b*l*n1*n2)/&
                                    &(pi**2*(n1**2-n2**2)**2)
      endIf
!
      end function PIB_1D_Modified_V_Element


      Subroutine SymmetricPacked2Matrix_LowerPac(N,ArrayIn,AMatOut)
!
!     This subroutine accepts an array, ArrayIn, that is (N*(N+1))/2 long.
!     It then converts that form to the N-by-N matrix AMatOut taking
!     ArrayIn to be in lower-packed storage form. Note: The storage mode
!     also assumes the lower-packed storage is packed by columns.
!
!
!     Variable Declarations
      implicit none
      integer,intent(in)::N
      real,dimension((N*(N+1))/2),intent(in)::ArrayIn
      real,dimension(N,N),intent(out)::AMatOut
      integer::i,j,k
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
!     Local Variables
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
