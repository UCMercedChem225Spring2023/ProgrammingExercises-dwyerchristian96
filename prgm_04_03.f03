      Program prgm_04_03
!
!     This program computes the product of a matrix and a vector to yield a
!     new vector with a leading dimension given by the user. Each array will
!     be filled with random numbers, and the program will report the CPU time
!     required for forming the matrix-vector product.
!
!     Program Author:  Christian Dwyer, Chemistry and Biochemistry, UC Merced
!     Program Advisor: Hrant Hratchian, Chemistry and Biochemistry, UC Merced
!
!     Variable Declarations
!
      Implicit None
      Integer::NDim
      Real::Time_Start,Time_End
      Real,Dimension(:,:),Allocatable::Matrix,Vector1,Vector2
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
!     Allocate memory for Matrix and Vector1. Then fill the arrays with
!     random values.
!
      Allocate(Matrix(NDim,NDim),Vector1(NDim,1),Vector2(NDim,1))
      Call random_number(Matrix)
      Call random_number(Vector1)
!
!     Compute matrix-vector products using the intrinsic MATMUL function.
!
      Call cpu_time(Time_Start)
      Vector2 = MATMUL(Matrix,Vector1)
      Call cpu_time(Time_End)
      Write(*,1000) NDim,Time_End-Time_Start
!
      End Program prgm_04_03