      Program prgm_04_01
!
!     This program carries out matrix-matrix multiplication for two square
!     matrices with a leading dimension given by the user. Each matrix will
!     be filled with random numbers, and the program will report the CPU time
!     required for the matrix multiplication work.
!
!     Program Author:  Christian Dwyer, Chemistry and Biochemistry, UC Merced
!     Program Advisor: Hrant Hratchian, Chemistry and Biochemistry, UC Merced
!
!     Variable Declarations
!
      Implicit None
      Integer::NDim
      Real::Time_Start,Time_End
      Real,Dimension(:,:),Allocatable::Matrix1,Matrix2,Matrix3
      Character(Len=64)::Character_Temp
!
!     Format Statements.
!
 1000 Format(1x,'Dimension=',I6,' Time: ',F10.4,' s.')
!
!     Get the dimensionality of the problem from the command line.
!
      Call Get_Command_Argument(1,Character_Temp)
      Read(Character_Temp,*) NDim
!
!     Allocate memory for Matrix1 and Matrix2. Then fill the matrices with
!     random values.
!
      Allocate(Matrix1(NDim,NDim),Matrix2(NDim,NDim),Matrix3(NDim,NDim))
      Call random_number(Matrix1)
      Call random_number(Matrix2)
!
!     Compute matrix products using the intrinsic MATMUL function.
!
      Call cpu_time(Time_Start)
      Matrix3 = MATMUL(Matrix1,Matrix2)
      Call cpu_time(Time_End)
      Write(*,1000) NDim,Time_End-Time_Start
!
      End Program prgm_04_01