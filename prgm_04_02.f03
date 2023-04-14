      Program prgm_04_02
!
!     This program computes the dot product of two vectors with a leading 
!     dimension given by the user. Each vector will be filled with random 
!     numbers, and the program will report the CPU time required for the 
!     dot product work.
!
!     Program Author:  Christian Dwyer, Chemistry and Biochemistry, UC Merced
!     Program Advisor: Hrant Hratchian, Chemistry and Biochemistry, UC Merced
!
!     Variable Declarations
!
      Implicit None
      Integer::NDim
      Real::Time_Start,Time_End,Product
      Real,Dimension(:),Allocatable::Vector1,Vector2
      Character(Len=64)::Character_Temp
!
!     Format Statements.
!
 1000 Format(1x,'Dimension=',I6,' Time: ',F10.8,' s.')
!
!     Get the dimensionality of the problem from the command line.
!
      Call Get_Command_Argument(1,Character_Temp)
      Read(Character_Temp,*) NDim
!
!     Allocate memory for Vector1 and Vector2. Then fill the vectors with
!     random values.
!
      Allocate(Vector1(NDim),Vector2(NDim))
      Call random_number(Vector1)
      Call random_number(Vector2)
!
!     Compute dot products using the intrinsic DOT_PRODUCT function.
!
      Call cpu_time(Time_Start)
      Product = DOT_PRODUCT(Vector1,Vector2)
      Call cpu_time(Time_End)
      Write(*,1000) NDim,Time_End-Time_Start
!
      End Program prgm_04_02