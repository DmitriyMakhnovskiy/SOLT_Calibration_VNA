	
	 SUBROUTINE SOLErrors(SMSOL,SOL,ESOL)
	 INTEGER N,LDA,IPATH
	 PARAMETER(N=3,LDA=3,IPATH=1)
	 DOUBLE COMPLEX SMSOL(3),SOL(3),ESOL(3),A(3,3),B(3)
	 DOUBLE COMPLEX	S11MS,S11MO,S11ML,S11S,S11O,S11L

	 ! Reflection measurements when terminating with the SOL calibration standards.
	 S11MS=SMSOL(1)
	 S11MO=SMSOL(2)
	 S11ML=SMSOL(3)
	 
	 ! Reflection coefficients of the SOL calibration standards.
	 S11S=SOL(1)
	 S11O=SOL(2)
	 S11L=SOL(3)
	 
	 ! Matrices in (6)
	 A(1,1)=(1.0,0.0)
	 A(2,1)=(1.0,0.0)
	 A(3,1)=(1.0,0.0)
	 A(1,2)=S11MS*S11S
	 A(2,2)=S11MO*S11O
	 A(3,2)=S11ML*S11L
	 A(1,3)=S11S
	 A(2,3)=S11O
	 A(3,3)=S11L
	 
	 B(1)=S11MS
	 B(2)=S11MO
	 B(3)=S11ML
	 	 	  
	 ! Solution of the system of linear equations (6)
	 CALL DLSACG(N,A,LDA,B,IPATH,ESOL)
       ! Arguments:
       ! N - Number of equations. (Input)
       ! A - Complex N by N matrix containing the coefficients of the linear system. (Input)
       ! LDA - Leading dimension of A exactly as specified in the dimension statement
	 !       of the calling program. (Input)
       ! B - Complex vector of length N containing the right-hand side
	 !     of the linear system. (Input)
       ! IPATH - Path indicator. (Input) 
       ! IPATH = 1 means the system AX = B is solved. 
       ! IPATH = 2 means the system AHX = B is solved.
       ! ESOL - Complex vector of length N containing the solution to the linear system. (Output)
	 !
	 ! Errors:
	 ! Edf=ESOL(1)
	 ! Esf=ESOL(2)
	 ! Erf=ESOL(3)+ESOL(1)*ESOL(2)
	 
	 ESOL(3)=ESOL(3)+ESOL(1)*ESOL(2)

	 RETURN

	 END
