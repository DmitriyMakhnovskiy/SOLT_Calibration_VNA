	
	 SUBROUTINE SOLTErrors(SMSOLT,SOLT,ESOLT)
	 INTEGER N,LDA,IPATH
	 PARAMETER(N=3,LDA=3,IPATH=1)
	 DOUBLE COMPLEX SMSOLT(10),SOLT(10),ESOLT(10),A(3,3),B(3),X(3)
	 DOUBLE COMPLEX	S11MS,S11MO,S11ML,S11S,S11O,S11L
	 DOUBLE COMPLEX	S22MS,S22MO,S22ML,S22S,S22O,S22L
	 DOUBLE COMPLEX	S11MT,S12MT,S21MT,S22MT,S11T,S12T,S21T,S22T
	 DOUBLE COMPLEX	Edf,Esf,Erf,Edr,Esr,Err,Elf,Etf,Elr,Etr,detT

	 ! Reflection and transmission measurements
	 ! when terminating with the SOLT calibration standards.
	 S11MS=SMSOLT(1)
	 S11MO=SMSOLT(2)
	 S11ML=SMSOLT(3)
	 S22MS=SMSOLT(4)
	 S22MO=SMSOLT(5)
	 S22ML=SMSOLT(6)
	 S11MT=SMSOLT(7)
	 S21MT=SMSOLT(8)
	 S12MT=SMSOLT(9)
	 S22MT=SMSOLT(10)
	 
	 ! Reflection and transmission coefficients
	 ! of the SOLT calibration standards.
	 S11S=SOLT(1)
	 S11O=SOLT(2)
	 S11L=SOLT(3)
	 S22S=SOLT(4)
	 S22O=SOLT(5)
	 S22L=SOLT(6)
	 S11T=SOLT(7)
	 S21T=SOLT(8)
	 S12T=SOLT(9)
	 S22T=SOLT(10)
	 
	 ! Matrices in (12)
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
	 	 	  
	 ! Solution of the system of linear equations (12)
	 CALL DLSACG(N,A,LDA,B,IPATH,X)
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
       ! X - Complex vector of length N containing the solution to the linear system. (Output)
	 !
	 
	 X(3)=X(3)+X(1)*X(2)

	 Edf=X(1)
	 Esf=X(2)
	 Erf=X(3)

	 ESOLT(1)=Edf
	 ESOLT(2)=Esf
	 ESOLT(3)=Erf

 	 ! Matrices in (13)
	 A(1,1)=(1.0,0.0)
	 A(2,1)=(1.0,0.0)
	 A(3,1)=(1.0,0.0)
	 A(1,2)=S22MS*S22S
	 A(2,2)=S22MO*S22O
	 A(3,2)=S22ML*S22L
	 A(1,3)=S22S
	 A(2,3)=S22O
	 A(3,3)=S22L
	 
	 B(1)=S22MS
	 B(2)=S22MO
	 B(3)=S22ML
	 	 	  
	 ! Solution of the system of linear equations (13)
	 CALL DLSACG(N,A,LDA,B,IPATH,X)

	 X(3)=X(3)+X(1)*X(2)
	 
	 Edr=X(1)
	 Esr=X(2)
	 Err=X(3)
	 
	 ESOLT(4)=Edr
	 ESOLT(5)=Esr
	 ESOLT(6)=Err

 	 detT=S11T*S22T-S12T*S21T

	 Elf=S11T*Erf+S11T*(S11MT-Edf)*Esf-(S11MT-Edf)
	 Elf=Elf/((Esf*detT-S22T)*(S11MT-Edf)+Erf*detT)

	 Elr=S22T*Err+S22T*(S22MT-Edr)*Esr-(S22MT-Edr)
	 Elr=Elr/((Esr*detT-S11T)*(S22MT-Edr)+Err*detT)

	 Etf=(S21MT+S21MT*Elf*(Esf*detT-S22T)-S11T*S21MT*Esf)/S21T
	 Etr=(S12MT+S12MT*Elr*(Esr*detT-S11T)-S22T*S12MT*Esr)/S12T

	 ESOLT(7)=Elf
	 ESOLT(8)=Etf
	 ESOLT(9)=Elr
	 ESOLT(10)=Etr

	 RETURN

	 END
