	
	 SUBROUTINE Scenario1

	 INTEGER i,NSOL,NMSOL,NM,flag
	 CHARACTER *15 Name

	 DOUBLE PRECISION f(1)
	 
	 DOUBLE PRECISION, ALLOCATABLE :: freqSOL(:),freqMSOL(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11S(:),ImS11S(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11O(:),ImS11O(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11L(:),ImS11L(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11MS(:),ImS11MS(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11MO(:),ImS11MO(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11ML(:),ImS11ML(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11M(:),ImS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: Re(:),Im(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardH(:),ForwardReS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS11M(:),ReverseH(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS11M(:) 

	 DOUBLE COMPLEX unit,cunit
  	 DOUBLE COMPLEX S11M,S11A,Z
	 DOUBLE COMPLEX	ForwardS11M,ForwardS11A
	 DOUBLE COMPLEX	ReverseS11M,ReverseS11A
	 DOUBLE COMPLEX	ForwardZ,ReverseZ
	 DOUBLE COMPLEX SMSOL(3),SOL(3),ESOL(3)
	 PARAMETER(unit=(1.0,0.0),cunit=(0.0,1.0))

	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'The frequency in the files must be in Hz.'
	 WRITE(*,*)''
	 WRITE(*,*)'Please check!'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE
	 	 	 
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Please check that you have copied the following'
	 WRITE(*,*)'CSV files into the calculation folder:'
	 WRITE(*,*)''
	 WRITE(*,*)'S11S.csv'
	 WRITE(*,*)'S11O.csv'
	 WRITE(*,*)'S11L.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S11MS.csv'
	 WRITE(*,*)'S11MO.csv'
	 WRITE(*,*)'S11ML.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S11M.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'All SOL-files must have the same frequency points.'
	 WRITE(*,*)'All MSOL-files must have the same frequency points.'
	 WRITE(*,*)''
	 WRITE(*,*)'When measuring the dispersion, S11M.csv must have'
	 WRITE(*,*)'the same frequency points as MSOLT-files.'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE

	 Name='S11S.csv'
       Call NLine(Name,NSOL)

	 Name='S11MS.csv'
       Call NLine(Name,NMSOL)

	 Name='S11M.csv'
       Call NLine(Name,NM)

	 ALLOCATE(freqSOL(NSOL),freqMSOL(NMSOL))
	 ALLOCATE(ReS11S(NSOL),ImS11S(NSOL))
	 ALLOCATE(ReS11O(NSOL),ImS11O(NSOL))
	 ALLOCATE(ReS11L(NSOL),ImS11L(NSOL))
	 ALLOCATE(ReS11MS(NMSOL),ImS11MS(NMSOL))
	 ALLOCATE(ReS11MO(NMSOL),ImS11MO(NMSOL))
	 ALLOCATE(ReS11ML(NMSOL),ImS11ML(NMSOL))

	 OPEN(10,FILE='S11S.csv')
	 OPEN(20,FILE='S11O.csv')
	 OPEN(30,FILE='S11L.csv')
       DO i=1,NSOL
	    READ(10,*) freqSOL(i),ReS11S(i),ImS11S(i)
	    READ(20,*) freqSOL(i),ReS11O(i),ImS11O(i)
	    READ(30,*) freqSOL(i),ReS11L(i),ImS11L(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)

	 OPEN(10,FILE='S11MS.csv')
	 OPEN(20,FILE='S11MO.csv')
	 OPEN(30,FILE='S11ML.csv')
       DO i=1,NMSOL
	    READ(10,*) freqMSOL(i),ReS11MS(i),ImS11MS(i)
	    READ(20,*) freqMSOL(i),ReS11MO(i),ImS11MO(i)
	    READ(30,*) freqMSOL(i),ReS11ML(i),ImS11ML(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)

	 IF(freqMSOL(1).LT.freqSOL(1)) THEN
	     WRITE(*,*)''
	     WRITE(*,*)'*** Error in the files! ***'
	     WRITE(*,*)'The first frequency in the calibration files'
	     WRITE(*,*)'is larger than the first frequency in'
	     WRITE(*,*)'the measured MSOLT files. Please rebuild.'
	     WRITE(*,*)'The program will be terminated.'
	     WRITE(*,*)''
   	     PAUSE
	     STOP
	 ELSE
	     IF(freqSOL(NSOL).LT.freqMSOL(NMSOL)) THEN
	         WRITE(*,*)''
	         WRITE(*,*)'*** Error in the files! ***'
	         WRITE(*,*)'The last frequency in the calibration files'
	         WRITE(*,*)'is less than the last frequency in'
	         WRITE(*,*)'the measured files. Please rebuild.'
	         WRITE(*,*)'The program will be terminated.'
	         WRITE(*,*)''
   	         PAUSE
	         STOP
	     ELSE
	         CONTINUE
	     END IF
	 END IF

	 flag=0
	 DO WHILE(flag.EQ.0)
	 WRITE(*,*)''
	 WRITE(*,*)'*************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'What did you measure:'
	 WRITE(*,*)''
	 WRITE(*,*)'dispersion of S11? - Enter "d"'
	 WRITE(*,*)''
	 WRITE(*,*)'or'
	 WRITE(*,*)''
	 WRITE(*,*)'field dependence of S11? - Enter "f"'
	 WRITE(*,*)''
	 WRITE(*,*)'*************************************'
	 WRITE(*,*)''
	 READ(*,*) Name

       SELECT CASE (Name)
       
	 CASE ('d','D')

	 ALLOCATE(Re(NMSOL),Im(NMSOL))

      ! CALL DCSIEZ(NDATA,XDATA,FDATA,N,XVEC,VALUE)
      ! Arguments:
      ! NDATA - Number of data points. (Input)
      ! NDATA must be at least 2.
      ! XDATA - Array of length NDATA containing the data point abscissas. (Input)
      ! The data point abscissas must be distinct.
      ! FDATA - Array of length NDATA containing the data point ordinates. (Input)
      ! N - Length of vector XVEC. (Input)
      ! XVEC - Array of length N containing the points at which the spline is to be evaluated. (Input)
      ! VALUE - Array of length N containing the values of the spline at the points in XVEC. (Output)

	 CALL DCSIEZ(NSOL,freqSOL,ReS11S,NMSOL,freqMSOL,Re)
	 CALL DCSIEZ(NSOL,freqSOL,ImS11S,NMSOL,freqMSOL,Im)
	 DEALLOCATE(ReS11S,ImS11S)
	 ALLOCATE(ReS11S(NMSOL),ImS11S(NMSOL))
	 ReS11S=Re
	 ImS11S=Im

	 CALL DCSIEZ(NSOL,freqSOL,ReS11O,NMSOL,freqMSOL,Re)
	 CALL DCSIEZ(NSOL,freqSOL,ImS11O,NMSOL,freqMSOL,Im)
	 DEALLOCATE(ReS11O,ImS11O)
	 ALLOCATE(ReS11O(NMSOL),ImS11O(NMSOL))
	 ReS11O=Re
	 ImS11O=Im

	 CALL DCSIEZ(NSOL,freqSOL,ReS11L,NMSOL,freqMSOL,Re)
	 CALL DCSIEZ(NSOL,freqSOL,ImS11L,NMSOL,freqMSOL,Im)
	 DEALLOCATE(ReS11L,ImS11L)
	 ALLOCATE(ReS11L(NMSOL),ImS11L(NMSOL))
	 ReS11L=Re
	 ImS11L=Im

	 ALLOCATE(ReS11M(NMSOL),ImS11M(NMSOL))
	 OPEN(10,FILE='S11M.csv')
       DO i=1,NMSOL
	    READ(10,*) freqMSOL(i),ReS11M(i),ImS11M(i)
       END DO
	 CLOSE(10)
 
 	 OPEN(10,FILE='S11corrected.csv')
	 OPEN(20,FILE='Zcorrected.csv')
	 
	 DO i=1,NMSOL

		SMSOL(1)=ReS11MS(i)*unit+ImS11MS(i)*cunit
	    SMSOL(2)=ReS11MO(i)*unit+ImS11MO(i)*cunit
	    SMSOL(3)=ReS11ML(i)*unit+ImS11ML(i)*cunit
	 
	    SOL(1)=ReS11S(i)*unit+ImS11S(i)*cunit
	    SOL(2)=ReS11O(i)*unit+ImS11O(i)*cunit
	    SOL(3)=ReS11L(i)*unit+ImS11L(i)*cunit

	    S11M=ReS11M(i)*unit+ImS11M(i)*cunit

	    CALL SOLErrors(SMSOL,SOL,ESOL)
	    CALL SOLCorrection(ESOL,S11M,S11A)

	    Z=50.0*(unit+S11A)/(unit-S11A)

	    WRITE(10,100) freqMSOL(i),REAL(S11A),IMAG(S11A)
100		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(20,200) freqMSOL(i),REAL(Z),IMAG(Z),ABS(Z)
200		FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8)

	 END DO

	 CLOSE(10)
	 CLOSE(20)

	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
       WRITE(*,*)''
	 WRITE(*,*)'Output files: S11corrected.csv and Zcorrected.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'Columns in the output S11 and Z files:'
	 WRITE(*,*)''
	 WRITE(*,*)'f(Hz), Re[S11], Im[S11]'
	 WRITE(*,*)''
	 WRITE(*,*)'f(Hz), Re[Z], Im[Z], |Z)|'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE

	 DEALLOCATE(ReS11M,ImS11M)
	 DEALLOCATE(freqSOL,freqMSOL)
	 DEALLOCATE(ReS11S,ImS11S)
	 DEALLOCATE(ReS11O,ImS11O)
	 DEALLOCATE(ReS11L,ImS11L)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 DEALLOCATE(Re,Im)
	 
	 flag=1         
       
       CASE ('f','F')

       WRITE(*,*)''
	 WRITE(*,*)'Enter the frequency (Hz) at which'
	 WRITE(*,*)'the field dependence of S11 was measured.'
	 READ(*,*)f(1)

	 ALLOCATE(Re(1),Im(1))
	 
	 CALL DCSIEZ(NSOL,freqSOL,ReS11S,1,f,Re)
	 CALL DCSIEZ(NSOL,freqSOL,ImS11S,1,f,Im)
	 DEALLOCATE(ReS11S,ImS11S)
	 ALLOCATE(ReS11S(1),ImS11S(1))
	 ReS11S=Re
	 ImS11S=Im

	 CALL DCSIEZ(NSOL,freqSOL,ReS11O,1,f,Re)
	 CALL DCSIEZ(NSOL,freqSOL,ImS11O,1,f,Im)
	 DEALLOCATE(ReS11O,ImS11O)
	 ALLOCATE(ReS11O(1),ImS11O(1))
	 ReS11O=Re
	 ImS11O=Im

	 CALL DCSIEZ(NSOL,freqSOL,ReS11L,1,f,Re)
	 CALL DCSIEZ(NSOL,freqSOL,ImS11L,1,f,Im)
	 DEALLOCATE(ReS11L,ImS11L)
	 ALLOCATE(ReS11L(1),ImS11L(1))
	 ReS11L=Re
	 ImS11L=Im
	 
	 CALL DCSIEZ(NMSOL,freqMSOL,ReS11MS,1,f,Re)
	 CALL DCSIEZ(NMSOL,freqMSOL,ImS11MS,1,f,Im)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 ALLOCATE(ReS11MS(1),ImS11MS(1))
	 ReS11MS=Re
	 ImS11MS=Im

	 CALL DCSIEZ(NMSOL,freqMSOL,ReS11MO,1,f,Re)
	 CALL DCSIEZ(NMSOL,freqMSOL,ImS11MO,1,f,Im)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 ALLOCATE(ReS11MO(1),ImS11MO(1))
	 ReS11MO=Re
	 ImS11MO=Im

	 CALL DCSIEZ(NMSOL,freqMSOL,ReS11ML,1,f,Re)
	 CALL DCSIEZ(NMSOL,freqMSOL,ImS11ML,1,f,Im)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 ALLOCATE(ReS11ML(1),ImS11ML(1))
	 ReS11ML=Re
	 ImS11ML=Im

 	 ALLOCATE(ForwardH(NM),ReverseH(NM))
 	 ALLOCATE(ForwardReS11M(NM),ForwardImS11M(NM))
	 ALLOCATE(ReverseReS11M(NM),ReverseImS11M(NM))
 	 OPEN(10,FILE='S11M.csv')
       DO i=1,NM
	    READ(10,*) ForwardH(i),ForwardReS11M(i),ForwardImS11M(i)
     *		      ,ReverseH(i),ReverseReS11M(i),ReverseImS11M(i) 
       END DO

 	 OPEN(30,FILE='S11corrected.csv')
	 OPEN(40,FILE='Zcorrected.csv')

	 SMSOL(1)=ReS11MS(1)*unit+ImS11MS(1)*cunit
	 SMSOL(2)=ReS11MO(1)*unit+ImS11MO(1)*cunit
	 SMSOL(3)=ReS11ML(1)*unit+ImS11ML(1)*cunit
	 
	 SOL(1)=ReS11S(1)*unit+ImS11S(1)*cunit
	 SOL(2)=ReS11O(1)*unit+ImS11O(1)*cunit
	 SOL(3)=ReS11L(1)*unit+ImS11L(1)*cunit

	 CALL SOLErrors(SMSOL,SOL,ESOL)

	 DO i=1,NM

	    ForwardS11M=ForwardReS11M(i)*unit+ForwardImS11M(i)*cunit
	    ReverseS11M=ReverseReS11M(i)*unit+ReverseImS11M(i)*cunit

		CALL SOLCorrection(ESOL,ForwardS11M,ForwardS11A)
		CALL SOLCorrection(ESOL,ReverseS11M,ReverseS11A)

	    ForwardZ=50.0*(unit+ForwardS11A)/(unit-ForwardS11A)
	    ReverseZ=50.0*(unit+ReverseS11A)/(unit-ReverseS11A)

	    WRITE(30,300) ForwardH(i),REAL(ForwardS11A),IMAG(ForwardS11A)
     *		         ,ReverseH(i),REAL(ReverseS11A),IMAG(ReverseS11A)
300		FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *           ,',',F20.8)

	    WRITE(40,400) ForwardH(i),REAL(ForwardZ),IMAG(ForwardZ)
     *		          ,ABS(ForwardZ),ReverseH(i),REAL(ReverseZ)
     *                  ,IMAG(ReverseZ),ABS(ReverseZ)
400		FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *           ,',',F20.8,',',F20.8,',',F20.8)

	 END DO

	 CLOSE(30)
	 CLOSE(40)

	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Output files: S11corrected.csv and Zcorrected.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'Columns in the output S11 and Z files:'
	 WRITE(*,*)''
	 WRITE(*,*)'H(-->), Re[S11], Im[S11], H(<--), Re[S11], Im[S11]'
	 WRITE(*,*)''
	 WRITE(*,*)'H(-->), Re[Z], Im[Z], |Z|, H(<--), Re[Z], Im[Z], |Z|'
	 WRITE(*,*)''
	 WRITE(*,*)'where'
	 WRITE(*,*)''
	 WRITE(*,*)'H(-->) is the forward magnetic field (arb. units)'
	 WRITE(*,*)'H(<--) is the reverse magnetic field (arb. units)'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE
 
	 DEALLOCATE(freqSOL,freqMSOL)
	 DEALLOCATE(ReS11S,ImS11S)
	 DEALLOCATE(ReS11O,ImS11O)
	 DEALLOCATE(ReS11L,ImS11L)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 DEALLOCATE(Re,Im)
	 DEALLOCATE(ForwardH,ReverseH)
 	 DEALLOCATE(ForwardReS11M,ForwardImS11M)
	 DEALLOCATE(ReverseReS11M,ReverseImS11M)

	 flag=1
	 
       CASE DEFAULT
       
	 WRITE (*, *)'Command not recognized. Please try again.'
	 flag=0
	 PAUSE
      
	 END SELECT
	 
	 END DO

	 RETURN

	 END
