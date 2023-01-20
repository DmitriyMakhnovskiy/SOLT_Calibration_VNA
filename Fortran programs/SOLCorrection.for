
	 SUBROUTINE SOLCorrection(ESOL,S11M,S11A)
	 DOUBLE COMPLEX ESOL(3),Edf,Erf,Esf,S11M,S11A

	 Edf=ESOL(1)
	 Esf=ESOL(2)
	 Erf=ESOL(3)
	 
	 S11A=(S11M-Edf)/(Erf+Esf*(S11M-Edf))
	 
	 RETURN

	 END
