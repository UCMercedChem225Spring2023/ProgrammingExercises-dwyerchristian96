      program prgm_02_03
!
!     This program carries out calculation of 1D particle-in-a-box values.
!     Specifically, the user provides a constant, the particle mass, box length,
!     and quantum numbers for two canonical PIB eigenstates.
!
!     This program is written in atomic units.
!
!     Program Author:  Christian Dwyer, Chemistry and Biochemistry, UC Merced
!     Program Advisor: Hrant Hratchian, Chemistry and Biochemistry, UC Merced
!
!     Variable Declarations
      implicit none
      integer::i,NCmdLineArgs
      real::b,m,l,HMatrixElement
      real,external::PIB_1D_Modified_Hamiltonian_Element
      integer::n1,n2
      logical::fail
      character(len=1024)::cmd_buffer
!
!     Format Statements
!
 2000 format(1x,'Hamiltonian matrix element ',I5,',',I5,' is ',F12.5,'.')
 9000 format(1x,'Expected 5 command line arguments, but found ',i2,'.')
!
!
!     Read in b, m, l, n1, and n2 from the command line.
!
      fail = .false.
      NCmdLineArgs = command_argument_count()
      if(NCmdLineArgs.ne.5) then
        write(*,9000) NCmdLineArgs
        fail = .true.
      endIf
      if(fail) goto 999
      call Get_Command_Argument(1,cmd_buffer)
      read(cmd_buffer,*) b
      call Get_Command_Argument(2,cmd_buffer)
      read(cmd_buffer,*) m
      call Get_Command_Argument(3,cmd_buffer)
      read(cmd_buffer,*) l
      call Get_Command_Argument(4,cmd_buffer)
      read(cmd_buffer,*) n1
      call Get_Command_Argument(5,cmd_buffer)
      read(cmd_buffer,*) n2
!
!     Given the input parameters, evaluate the Hamiltonian integral between
!     particle-in-a-box eigenfunctions n1 and n2.
!
      HMatrixElement = PIB_1D_Modified_Hamiltonian_Element(b,m,l,n1,n2)
      write(*,2000) n1,n2,HMatrixElement
!
!     The end of the job...
!
  999 continue
      end program prgm_02_03


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
      real::prefactor
      real(kind=8),parameter::pi=4.D0*datan(1.D0)
!
!     The case where n1=n2 is different than n1\=n2. For this reason, we use an
!     if block to separate the evaluation of the kinetic energy integral for
!     these two different cases.
!
      prefactor = (pi**2)/(2*m*(l**2))
      if(n1.eq.n2) then
        PIB_1D_T_Element = prefactor*(n1**2)
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
        PIB_1D_Modified_V_Element = 1/(pi**2*(n1**2-n2**2)**2)*(2*b*l*(-2*n1*n2+&
                                    &sin(n1*pi)*((n1-n2)*(n1+n2)*n2*pi*cos(n2*pi)+&
                                    &(n1**2+n2**2)*sin(n2*pi))+n1*cos(n1*pi)*(2*n2*&
                                    &cos(n2*pi)+(n2-n1)*(n1+n2)*pi*sin(n2*pi))))
      endIf
!
      end function PIB_1D_Modified_V_Element
