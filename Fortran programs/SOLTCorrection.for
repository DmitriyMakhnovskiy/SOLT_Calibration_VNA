
	 SUBROUTINE SOLTCorrection(ESOLT,SM,SA)
	 INTEGER N,LDA,IPATH
	 PARAMETER(N=4,LDA=4,IPATH=1)
	 DOUBLE COMPLEX	ESOLT(10),SM(4),A(4,4),B(4),SA(4)
	 DOUBLE COMPLEX Edf,Erf,Esf,Elf,Etf,Edr,Err,Esr,Elr,Etr
	 DOUBLE COMPLEX S11M,S22M,S21M,S12M
	 
	 ! Error terms in 12-term error model (Exf and Exr are not used)

 	 Edf=ESOLT(1)
	 Esf=ESOLT(2)
	 Erf=ESOLT(3)
  	 Edr=ESOLT(4)
	 Esr=ESOLT(5)
	 Err=ESOLT(6)
 	 Elf=ESOLT(7)
	 Etf=ESOLT(8)
	 Elr=ESOLT(9)
	 Etr=ESOLT(10)
 
	 ! Measured S-parameters
	 S11M=SM(1)
	 S21M=SM(2)
	 S12M=SM(3)
	 S22M=SM(4)
	 	 
	 ! Matrices in (38)
	 A(1,1)=(0.0,0.0)
	 A(2,1)=S11M*Etf*Esf-Etf*Esf*Edf+Etf*Erf
	 A(3,1)=S22M*Elr-Edr*Elr
	 A(4,1)=S12M*Elr
	 A(1,2)=S11M*Etf-Edf*Etf
	 A(2,2)=(0.0,0.0)
	 A(3,2)=Err+S22M*Esr-Edr*Esr
	 A(4,2)=S12M*Esr
	 A(1,3)=-S21M*Erf
	 A(2,3)=S21M*Erf*Elf
	 A(3,3)=(0.0,0.0)
	 A(4,3)=Etr
	 A(1,4)=-Etf*Erf-S11M*Etf*Esf+Etf*Esf*Edf
	 A(2,4)=(0.0,0.0)
	 A(3,4)=-S22M*Elr*Esr-Elr*Err+Edr*Elr*Esr
	 A(4,4)=-S12M*Elr*Esr

	 B(1)=(0.0,0.0)
	 B(2)=Etf*(S11M-Edf)
	 B(3)=S22M-Edr
	 B(4)=S12M
	 	  
	 ! Solution of the system of linear equations (38)
	 CALL DLSACG(N,A,LDA,B,IPATH,SA)
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
       ! SA - Complex vector of length N containing the solution to the linear system. (Output)
	 !
	 ! Corrected actual S-parameters:
	 ! S11A=SA(1)
	 ! S22A=SA(2)
	 ! S12A=SA(3)
	 ! detA=SA(4)
	 ! S21A=(S11A*S22A-detA)/S12A

	 SA(4)=(SA(1)*SA(2)-SA(4))/SA(3)

	 RETURN

	 END
