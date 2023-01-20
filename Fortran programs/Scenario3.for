	
	 SUBROUTINE Scenario3

	 INTEGER i,NSOLT,NMSOLT,NM,flag,flagTHRU
	 CHARACTER *15 Name

	 DOUBLE PRECISION f(1)
	 
	 DOUBLE PRECISION, ALLOCATABLE :: freqSOLT(:),freqMSOLT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11S(:),ImS11S(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11O(:),ImS11O(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11L(:),ImS11L(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22S(:),ImS22S(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22O(:),ImS22O(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22L(:),ImS22L(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11MS(:),ImS11MS(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11MO(:),ImS11MO(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11ML(:),ImS11ML(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22MS(:),ImS22MS(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22MO(:),ImS22MO(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22ML(:),ImS22ML(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11T(:),ImS11T(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS21T(:),ImS21T(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS12T(:),ImS12T(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22T(:),ImS22T(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11MT(:),ImS11MT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS21MT(:),ImS21MT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS12MT(:),ImS12MT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22MT(:),ImS22MT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11M(:),ImS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS21M(:),ImS21M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS12M(:),ImS12M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22M(:),ImS22M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: Re(:),Im(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardH(:),ReverseH(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardReS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS11M(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardReS21M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS21M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS21M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS21M(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardReS12M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS12M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS12M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS12M(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardReS22M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS22M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS22M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS22M(:)
	 DOUBLE COMPLEX unit,cunit
  	 DOUBLE COMPLEX SM(4),SA(4),Z
	 DOUBLE COMPLEX	ForwardSM(4),ForwardSA(4)
	 DOUBLE COMPLEX	ReverseSM(4),ReverseSA(4)
	 DOUBLE COMPLEX	ForwardZ,ReverseZ
	 DOUBLE COMPLEX SMSOLT(10),SOLT(10),ESOLT(10)
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
	 
	 flag=0
	 DO WHILE(flag.EQ.0)

	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'When calibrating with THRU, will you use an adapter'
	 WRITE(*,*)'to connect the cables together? '
	 WRITE(*,*)''
	 WRITE(*,*)'Please enter Y/N.'
 	 WRITE(*,*)''
 	 WRITE(*,*)'Yes - the cables are of the same sex, and hence'
	 WRITE(*,*)'an adapter is required to connect them together.'
	 WRITE(*,*)''
 	 WRITE(*,*)'No - the cables are of different sexes, and hence'
 	 WRITE(*,*)'they can be connected together without adapters.'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 READ(*,*) Name

	 SELECT CASE(Name)

	 CASE('Y','y')
	 flagTHRU=1
	 flag=1

	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Please check that you have copied the following'
	 WRITE(*,*)'CSV files into the calculation folder (24 in total):'
	 WRITE(*,*)''
	 WRITE(*,*)'S11S.csv'
	 WRITE(*,*)'S11O.csv'
	 WRITE(*,*)'S11L.csv'
	 WRITE(*,*)'S11MS.csv'
	 WRITE(*,*)'S11MO.csv'
	 WRITE(*,*)'S11ML.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S22S.csv'
	 WRITE(*,*)'S22O.csv'
	 WRITE(*,*)'S22L.csv'
	 WRITE(*,*)'S22MS.csv'
	 WRITE(*,*)'S22MO.csv'
	 WRITE(*,*)'S22ML.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S11T.csv'
	 WRITE(*,*)'S21T.csv'
	 WRITE(*,*)'S12T.csv'
	 WRITE(*,*)'S22T.csv'
	 WRITE(*,*)'S11MT.csv'
	 WRITE(*,*)'S21MT.csv'
	 WRITE(*,*)'S12MT.csv'
	 WRITE(*,*)'S22MT.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S11M.csv'
	 WRITE(*,*)'S21M.csv'
	 WRITE(*,*)'S12M.csv'
	 WRITE(*,*)'S22M.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'All SOLT-files must have the same frequency points.'
	 WRITE(*,*)'All MSOLT-files must have the same frequency points.'
	 WRITE(*,*)''
	 WRITE(*,*)'When measuring the dispersion, all M-files must have'
	 WRITE(*,*)'the same frequency points as MSOLT-files.'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE

	 CASE('N','n')
	 flagTHRU=0
	 flag=1

	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Please check that you have copied the following'
	 WRITE(*,*)'CSV files into the calculation folder (20 in total):'
	 WRITE(*,*)''
	 WRITE(*,*)'S11S.csv'
	 WRITE(*,*)'S11O.csv'
	 WRITE(*,*)'S11L.csv'
	 WRITE(*,*)'S11MS.csv'
	 WRITE(*,*)'S11MO.csv'
	 WRITE(*,*)'S11ML.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S22S.csv'
	 WRITE(*,*)'S22O.csv'
	 WRITE(*,*)'S22L.csv'
	 WRITE(*,*)'S22MS.csv'
	 WRITE(*,*)'S22MO.csv'
	 WRITE(*,*)'S22ML.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S11MT.csv'
	 WRITE(*,*)'S21MT.csv'
	 WRITE(*,*)'S12MT.csv'
	 WRITE(*,*)'S22MT.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S11M.csv'
	 WRITE(*,*)'S21M.csv'
	 WRITE(*,*)'S12M.csv'
	 WRITE(*,*)'S22M.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'All SOL-files must have the same frequency points.'
	 WRITE(*,*)''
	 WRITE(*,*)'All MSOLT-files must have the same frequency points.'
	 WRITE(*,*)''
	 WRITE(*,*)'When measuring the dispersion, S**M.csv files'
	 WRITE(*,*)'must have the same frequency points as MSOLT-files.'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE

	 CASE DEFAULT
	 WRITE (*, *)'Command not recognized. Please try again.'
	 flag=0
	 PAUSE

	 END SELECT

	 END DO

	 Name='S11S.csv'
       Call NLine(Name,NSOLT)

	 Name='S11MS.csv'
       Call NLine(Name,NMSOLT)

	 Name='S11M.csv'
       Call NLine(Name,NM)

	 ALLOCATE(freqSOLT(NSOLT),freqMSOLT(NMSOLT))

	 ALLOCATE(ReS11S(NSOLT),ImS11S(NSOLT))
	 ALLOCATE(ReS11O(NSOLT),ImS11O(NSOLT))
	 ALLOCATE(ReS11L(NSOLT),ImS11L(NSOLT))
	 ALLOCATE(ReS11MS(NMSOLT),ImS11MS(NMSOLT))
	 ALLOCATE(ReS11MO(NMSOLT),ImS11MO(NMSOLT))
	 ALLOCATE(ReS11ML(NMSOLT),ImS11ML(NMSOLT))

	 ALLOCATE(ReS22S(NSOLT),ImS22S(NSOLT))
	 ALLOCATE(ReS22O(NSOLT),ImS22O(NSOLT))
	 ALLOCATE(ReS22L(NSOLT),ImS22L(NSOLT))
	 ALLOCATE(ReS22MS(NMSOLT),ImS22MS(NMSOLT))
	 ALLOCATE(ReS22MO(NMSOLT),ImS22MO(NMSOLT))
	 ALLOCATE(ReS22ML(NMSOLT),ImS22ML(NMSOLT))

	 ALLOCATE(ReS11MT(NMSOLT),ImS11MT(NMSOLT))
	 ALLOCATE(ReS21MT(NMSOLT),ImS21MT(NMSOLT))
	 ALLOCATE(ReS12MT(NMSOLT),ImS12MT(NMSOLT))
	 ALLOCATE(ReS22MT(NMSOLT),ImS22MT(NMSOLT))
	
	 OPEN(10,FILE='S11S.csv')
	 OPEN(20,FILE='S11O.csv')
	 OPEN(30,FILE='S11L.csv')
       DO i=1,NSOLT
	    READ(10,*) freqSOLT(i),ReS11S(i),ImS11S(i)
	    READ(20,*) freqSOLT(i),ReS11O(i),ImS11O(i)
	    READ(30,*) freqSOLT(i),ReS11L(i),ImS11L(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)

	 OPEN(10,FILE='S11MS.csv')
	 OPEN(20,FILE='S11MO.csv')
	 OPEN(30,FILE='S11ML.csv')
       DO i=1,NMSOLT
	    READ(10,*) freqMSOLT(i),ReS11MS(i),ImS11MS(i)
	    READ(20,*) freqMSOLT(i),ReS11MO(i),ImS11MO(i)
	    READ(30,*) freqMSOLT(i),ReS11ML(i),ImS11ML(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)

	 IF(freqMSOLT(1).LT.freqSOLT(1)) THEN
	     WRITE(*,*)''
	     WRITE(*,*)'*** Error in the files! ***'
	     WRITE(*,*)'The first frequency in the calibration files'
	     WRITE(*,*)'is larger than the first frequency in'
	     WRITE(*,*)'the measured files. Please rebuild.'
	     WRITE(*,*)'The program will be terminated.'
	     WRITE(*,*)''
   	     PAUSE
	     STOP
	 ELSE
	     IF(freqSOLT(NSOLT).LT.freqMSOLT(NMSOLT)) THEN
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

	 OPEN(10,FILE='S22S.csv')
	 OPEN(20,FILE='S22O.csv')
	 OPEN(30,FILE='S22L.csv')
       DO i=1,NSOLT
	    READ(10,*) freqSOLT(i),ReS22S(i),ImS22S(i)
	    READ(20,*) freqSOLT(i),ReS22O(i),ImS22O(i)
	    READ(30,*) freqSOLT(i),ReS22L(i),ImS22L(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)

	 OPEN(10,FILE='S22MS.csv')
	 OPEN(20,FILE='S22MO.csv')
	 OPEN(30,FILE='S22ML.csv')
       DO i=1,NMSOLT
	    READ(10,*) freqMSOLT(i),ReS22MS(i),ImS22MS(i)
	    READ(20,*) freqMSOLT(i),ReS22MO(i),ImS22MO(i)
	    READ(30,*) freqMSOLT(i),ReS22ML(i),ImS22ML(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)

	 IF(flagTHRU.EQ.1) THEN

	 ALLOCATE(ReS11T(NSOLT),ImS11T(NSOLT))
	 ALLOCATE(ReS21T(NSOLT),ImS21T(NSOLT))
	 ALLOCATE(ReS12T(NSOLT),ImS12T(NSOLT))
	 ALLOCATE(ReS22T(NSOLT),ImS22T(NSOLT))

	 OPEN(10,FILE='S11T.csv')
	 OPEN(20,FILE='S21T.csv')
	 OPEN(30,FILE='S12T.csv')
	 OPEN(40,FILE='S22T.csv')
       DO i=1,NSOLT
	    READ(10,*) freqSOLT(i),ReS11T(i),ImS11T(i)
	    READ(20,*) freqSOLT(i),ReS21T(i),ImS21T(i)
	    READ(30,*) freqSOLT(i),ReS12T(i),ImS12T(i)
	    READ(40,*) freqSOLT(i),ReS22T(i),ImS22T(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)
	 CLOSE(40)

	 ELSE

	 ALLOCATE(ReS11T(NMSOLT),ImS11T(NMSOLT))
	 ALLOCATE(ReS21T(NMSOLT),ImS21T(NMSOLT))
	 ALLOCATE(ReS12T(NMSOLT),ImS12T(NMSOLT))
	 ALLOCATE(ReS22T(NMSOLT),ImS22T(NMSOLT))

       DO i=1,NMSOLT
	    ReS11T(i)=0.0
		ImS11T(i)=0.0
	    ReS21T(i)=1.0
		ImS21T(i)=0.0
	    ReS12T(i)=1.0
		ImS12T(i)=0.0
	    ReS22T(i)=0.0
		ImS22T(i)=0.0
       END DO

	 END IF

	 OPEN(10,FILE='S11MT.csv')
	 OPEN(20,FILE='S21MT.csv')
	 OPEN(30,FILE='S12MT.csv')
	 OPEN(40,FILE='S22MT.csv')
       DO i=1,NMSOLT
	    READ(10,*) freqMSOLT(i),ReS11MT(i),ImS11MT(i)
	    READ(20,*) freqMSOLT(i),ReS21MT(i),ImS21MT(i)
	    READ(30,*) freqMSOLT(i),ReS12MT(i),ImS12MT(i)
	    READ(40,*) freqMSOLT(i),ReS22MT(i),ImS22MT(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)
	 CLOSE(40)
       
	 flag=0
	 DO WHILE(flag.EQ.0)
	 WRITE(*,*)''
	 WRITE(*,*)'***********************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'What did you measure:'
	 WRITE(*,*)''
	 WRITE(*,*)'dispersion of S-parameters? - Enter "d"'
	 WRITE(*,*)''
	 WRITE(*,*)'or'
	 WRITE(*,*)''
	 WRITE(*,*)'field dependence of S-parameters? - Enter "f"'
	 WRITE(*,*)''
	 WRITE(*,*)'***********************************************'
	 WRITE(*,*)''
	 READ(*,*) Name

       SELECT CASE (Name)
       
	 CASE ('d','D')

	 ALLOCATE(Re(NMSOLT),Im(NMSOLT))

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

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS11S,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS11S,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS11S,ImS11S)
	 ALLOCATE(ReS11S(NMSOLT),ImS11S(NMSOLT))
	 ReS11S=Re
	 ImS11S=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS11O,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS11O,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS11O,ImS11O)
	 ALLOCATE(ReS11O(NMSOLT),ImS11O(NMSOLT))
	 ReS11O=Re
	 ImS11O=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS11L,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS11L,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS11L,ImS11L)
	 ALLOCATE(ReS11L(NMSOLT),ImS11L(NMSOLT))
	 ReS11L=Re
	 ImS11L=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS22S,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS22S,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS22S,ImS22S)
	 ALLOCATE(ReS22S(NMSOLT),ImS22S(NMSOLT))
	 ReS22S=Re
	 ImS22S=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS22O,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS22O,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS22O,ImS22O)
	 ALLOCATE(ReS22O(NMSOLT),ImS22O(NMSOLT))
	 ReS22O=Re
	 ImS22O=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS22L,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS22L,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS22L,ImS22L)
	 ALLOCATE(ReS22L(NMSOLT),ImS22L(NMSOLT))
	 ReS22L=Re
	 ImS22L=Im

	 IF(flagTHRU.EQ.1) THEN

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS11T,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS11T,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS11T,ImS11T)
	 ALLOCATE(ReS11T(NMSOLT),ImS11T(NMSOLT))
	 ReS11T=Re
	 ImS11T=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS21T,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS21T,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS21T,ImS21T)
	 ALLOCATE(ReS21T(NMSOLT),ImS21T(NMSOLT))
	 ReS21T=Re
	 ImS21T=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS12T,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS12T,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS12T,ImS12T)
	 ALLOCATE(ReS12T(NMSOLT),ImS12T(NMSOLT))
	 ReS12T=Re
	 ImS12T=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS22T,NMSOLT,freqMSOLT,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS22T,NMSOLT,freqMSOLT,Im)
	 DEALLOCATE(ReS22T,ImS22T)
	 ALLOCATE(ReS22T(NMSOLT),ImS22T(NMSOLT))
	 ReS22T=Re
	 ImS22T=Im

	 ELSE

	 CONTINUE

	 END IF

	 ALLOCATE(ReS11M(NMSOLT),ImS11M(NMSOLT))
	 ALLOCATE(ReS21M(NMSOLT),ImS21M(NMSOLT))
	 ALLOCATE(ReS12M(NMSOLT),ImS12M(NMSOLT))
	 ALLOCATE(ReS22M(NMSOLT),ImS22M(NMSOLT))
	 OPEN(10,FILE='S11M.csv')
	 OPEN(20,FILE='S21M.csv')
	 OPEN(30,FILE='S12M.csv')
	 OPEN(40,FILE='S22M.csv')
       DO i=1,NMSOLT
	    READ(10,*) freqMSOLT(i),ReS11M(i),ImS11M(i)
	    READ(20,*) freqMSOLT(i),ReS21M(i),ImS21M(i)
	    READ(30,*) freqMSOLT(i),ReS12M(i),ImS12M(i)
	    READ(40,*) freqMSOLT(i),ReS22M(i),ImS22M(i)
       END DO
	 CLOSE(10)
	 CLOSE(20)
	 CLOSE(30)
	 CLOSE(40)
 
 	 OPEN(10,FILE='S11corrected.csv')
 	 OPEN(20,FILE='S22corrected.csv')
 	 OPEN(30,FILE='S12corrected.csv')
 	 OPEN(40,FILE='S21corrected.csv')
	 OPEN(50,FILE='Zcorrected.csv')
	 
	 DO i=1,NMSOLT

		SMSOLT(1)=ReS11MS(i)*unit+ImS11MS(i)*cunit
	    SMSOLT(2)=ReS11MO(i)*unit+ImS11MO(i)*cunit
	    SMSOLT(3)=ReS11ML(i)*unit+ImS11ML(i)*cunit
	    SMSOLT(4)=ReS22MS(i)*unit+ImS22MS(i)*cunit
	    SMSOLT(5)=ReS22MO(i)*unit+ImS22MO(i)*cunit
	    SMSOLT(6)=ReS22ML(i)*unit+ImS22ML(i)*cunit
	    SMSOLT(7)=ReS11MT(i)*unit+ImS11MT(i)*cunit
	    SMSOLT(8)=ReS21MT(i)*unit+ImS21MT(i)*cunit
	    SMSOLT(9)=ReS12MT(i)*unit+ImS12MT(i)*cunit
	    SMSOLT(10)=ReS22MT(i)*unit+ImS22MT(i)*cunit

		SOLT(1)=ReS11S(i)*unit+ImS11S(i)*cunit
    	    SOLT(2)=ReS11O(i)*unit+ImS11O(i)*cunit
	    SOLT(3)=ReS11L(i)*unit+ImS11L(i)*cunit
	    SOLT(4)=ReS22S(i)*unit+ImS22S(i)*cunit
	    SOLT(5)=ReS22O(i)*unit+ImS22O(i)*cunit
	    SOLT(6)=ReS22L(i)*unit+ImS22L(i)*cunit
	    SOLT(7)=ReS11T(i)*unit+ImS11T(i)*cunit
	    SOLT(8)=ReS21T(i)*unit+ImS21T(i)*cunit
	    SOLT(9)=ReS12T(i)*unit+ImS12T(i)*cunit
	    SOLT(10)=ReS22T(i)*unit+ImS22T(i)*cunit

	    SM(1)=ReS11M(i)*unit+ImS11M(i)*cunit
	    SM(2)=ReS21M(i)*unit+ImS21M(i)*cunit
	    SM(3)=ReS12M(i)*unit+ImS12M(i)*cunit
	    SM(4)=ReS22M(i)*unit+ImS22M(i)*cunit

	    CALL SOLTErrors(SMSOLT,SOLT,ESOLT)
	    CALL SOLTCorrection(ESOLT,SM,SA)

	    Z=100.0*(unit-SA(4))/SA(4)

	    WRITE(10,100) freqMSOLT(i),REAL(SA(1)),IMAG(SA(1))
100		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(20,200) freqMSOLT(i),REAL(SA(2)),IMAG(SA(2))
200		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(30,300) freqMSOLT(i),REAL(SA(3)),IMAG(SA(3))
300		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(40,400) freqMSOLT(i),REAL(SA(4)),IMAG(SA(4))
400		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(50,500) freqMSOLT(i),REAL(Z),IMAG(Z),ABS(Z)
500		FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8)

	 END DO

	 CLOSE(10)
	 CLOSE(20)
	 CLOSE(30)
	 CLOSE(40)
	 CLOSE(50)

	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Output files:'
	 WRITE(*,*)''
	 WRITE(*,*)'S11corrected.csv'
	 WRITE(*,*)'S21corrected.csv'
	 WRITE(*,*)'S12corrected.csv'
	 WRITE(*,*)'S22corrected.csv'
	 WRITE(*,*)'Zcorrected.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'The impedance was calculated from S21-parameter.'
	 WRITE(*,*)''
	 WRITE(*,*)'Columns in the output S-parameter and Z files:'
	 WRITE(*,*)''
	 WRITE(*,*)'f(Hz), Re[S], Im[S]'
	 WRITE(*,*)''
	 WRITE(*,*)'f(Hz), Re[Z], Im[Z], |Z)|'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE
	 
	 DEALLOCATE(ReS11M,ImS11M)
	 DEALLOCATE(ReS21M,ImS21M)
	 DEALLOCATE(ReS12M,ImS12M)
	 DEALLOCATE(ReS22M,ImS22M)
	 DEALLOCATE(freqSOLT,freqMSOLT)
	 DEALLOCATE(ReS11S,ImS11S)
	 DEALLOCATE(ReS11O,ImS11O)
	 DEALLOCATE(ReS11L,ImS11L)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 DEALLOCATE(Re,Im)
	 DEALLOCATE(ReS11MT,ImS11MT)
	 DEALLOCATE(ReS21MT,ImS21MT)
	 DEALLOCATE(ReS12MT,ImS12MT)
	 DEALLOCATE(ReS22MT,ImS22MT)
	 DEALLOCATE(ReS11T,ImS11T)
	 DEALLOCATE(ReS21T,ImS21T)
	 DEALLOCATE(ReS12T,ImS12T)
	 DEALLOCATE(ReS22T,ImS22T)
	 
	 flag=1         
       
       CASE ('f','F')

       WRITE(*,*)''
	 WRITE(*,*)'Enter the frequency (Hz) at which'
	 WRITE(*,*)'the field dependencies of S-parameters were measured.'
	 READ(*,*)f(1)

	 ALLOCATE(Re(1),Im(1))
	 
	 CALL DCSIEZ(NSOLT,freqSOLT,ReS11S,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS11S,1,f,Im)
	 DEALLOCATE(ReS11S,ImS11S)
	 ALLOCATE(ReS11S(1),ImS11S(1))
	 ReS11S=Re
	 ImS11S=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS11O,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS11O,1,f,Im)
	 DEALLOCATE(ReS11O,ImS11O)
	 ALLOCATE(ReS11O(1),ImS11O(1))
	 ReS11O=Re
	 ImS11O=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS11L,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS11L,1,f,Im)
	 DEALLOCATE(ReS11L,ImS11L)
	 ALLOCATE(ReS11L(1),ImS11L(1))
	 ReS11L=Re
	 ImS11L=Im
	 
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS11MS,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS11MS,1,f,Im)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 ALLOCATE(ReS11MS(1),ImS11MS(1))
	 ReS11MS=Re
	 ImS11MS=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS11MO,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS11MO,1,f,Im)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 ALLOCATE(ReS11MO(1),ImS11MO(1))
	 ReS11MO=Re
	 ImS11MO=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS11ML,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS11ML,1,f,Im)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 ALLOCATE(ReS11ML(1),ImS11ML(1))
	 ReS11ML=Re
	 ImS11ML=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS22S,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS22S,1,f,Im)
	 DEALLOCATE(ReS22S,ImS22S)
	 ALLOCATE(ReS22S(1),ImS22S(1))
	 ReS22S=Re
	 ImS22S=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS22O,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS22O,1,f,Im)
	 DEALLOCATE(ReS22O,ImS22O)
	 ALLOCATE(ReS22O(1),ImS22O(1))
	 ReS22O=Re
	 ImS22O=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS22L,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS22L,1,f,Im)
	 DEALLOCATE(ReS22L,ImS22L)
	 ALLOCATE(ReS22L(1),ImS22L(1))
	 ReS22L=Re
	 ImS22L=Im
	 
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS22MS,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS22MS,1,f,Im)
	 DEALLOCATE(ReS22MS,ImS22MS)
	 ALLOCATE(ReS22MS(1),ImS22MS(1))
	 ReS22MS=Re
	 ImS22MS=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS22MO,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS22MO,1,f,Im)
	 DEALLOCATE(ReS22MO,ImS22MO)
	 ALLOCATE(ReS22MO(1),ImS22MO(1))
	 ReS22MO=Re
	 ImS22MO=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS22ML,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS22ML,1,f,Im)
	 DEALLOCATE(ReS22ML,ImS22ML)
	 ALLOCATE(ReS22ML(1),ImS22ML(1))
	 ReS22ML=Re
	 ImS22ML=Im
	 
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS11MT,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS11MT,1,f,Im)
	 DEALLOCATE(ReS11MT,ImS11MT)
	 ALLOCATE(ReS11MT(1),ImS11MT(1))
	 ReS11MT=Re
	 ImS11MT=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS21MT,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS21MT,1,f,Im)
	 DEALLOCATE(ReS21MT,ImS21MT)
	 ALLOCATE(ReS21MT(1),ImS21MT(1))
	 ReS21MT=Re
	 ImS21MT=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS12MT,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS12MT,1,f,Im)
	 DEALLOCATE(ReS12MT,ImS12MT)
	 ALLOCATE(ReS12MT(1),ImS12MT(1))
	 ReS12MT=Re
	 ImS12MT=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS22MT,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS22MT,1,f,Im)
	 DEALLOCATE(ReS22MT,ImS22MT)
	 ALLOCATE(ReS22MT(1),ImS22MT(1))
	 ReS22MT=Re
	 ImS22MT=Im

	 IF(flagTHRU.EQ.1) THEN

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS11T,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS11T,1,f,Im)
	 DEALLOCATE(ReS11T,ImS11T)
	 ALLOCATE(ReS11T(1),ImS11T(1))
	 ReS11T=Re
	 ImS11T=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS21T,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS21T,1,f,Im)
	 DEALLOCATE(ReS21T,ImS21T)
	 ALLOCATE(ReS21T(1),ImS21T(1))
	 ReS21T=Re
	 ImS21T=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS12T,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS12T,1,f,Im)
	 DEALLOCATE(ReS12T,ImS12T)
	 ALLOCATE(ReS12T(1),ImS12T(1))
	 ReS12T=Re
	 ImS12T=Im

	 CALL DCSIEZ(NSOLT,freqSOLT,ReS22T,1,f,Re)
	 CALL DCSIEZ(NSOLT,freqSOLT,ImS22T,1,f,Im)
	 DEALLOCATE(ReS22T,ImS22T)
	 ALLOCATE(ReS22T(1),ImS22T(1))
	 ReS22T=Re
	 ImS22T=Im

	 ELSE

	 DEALLOCATE(ReS11T,ImS11T)
	 ALLOCATE(ReS11T(1),ImS11T(1))
	 ReS11T(1)=0.0
	 ImS11T(1)=0.0

	 DEALLOCATE(ReS21T,ImS21T)
	 ALLOCATE(ReS21T(1),ImS21T(1))
	 ReS21T(1)=1.0
	 ImS21T(1)=0.0

	 DEALLOCATE(ReS12T,ImS12T)
	 ALLOCATE(ReS12T(1),ImS12T(1))
	 ReS12T(1)=1.0
	 ImS12T(1)=0.0

	 DEALLOCATE(ReS22T,ImS22T)
	 ALLOCATE(ReS22T(1),ImS22T(1))
	 ReS22T(1)=0.0
	 ImS22T(1)=0.0

	 END IF

 	 ALLOCATE(ForwardH(NM),ReverseH(NM))
 	 ALLOCATE(ForwardReS11M(NM),ForwardImS11M(NM))
	 ALLOCATE(ReverseReS11M(NM),ReverseImS11M(NM))
 	 ALLOCATE(ForwardReS21M(NM),ForwardImS21M(NM))
	 ALLOCATE(ReverseReS21M(NM),ReverseImS21M(NM))
 	 ALLOCATE(ForwardReS12M(NM),ForwardImS12M(NM))
	 ALLOCATE(ReverseReS12M(NM),ReverseImS12M(NM))
 	 ALLOCATE(ForwardReS22M(NM),ForwardImS22M(NM))
	 ALLOCATE(ReverseReS22M(NM),ReverseImS22M(NM))

 	 OPEN(10,FILE='S11M.csv')
 	 OPEN(20,FILE='S21M.csv')
 	 OPEN(30,FILE='S12M.csv')
 	 OPEN(40,FILE='S22M.csv')
       DO i=1,NM
	    READ(10,*) ForwardH(i),ForwardReS11M(i),ForwardImS11M(i)
     *		      ,ReverseH(i),ReverseReS11M(i),ReverseImS11M(i) 
	    READ(20,*) ForwardH(i),ForwardReS21M(i),ForwardImS21M(i)
     *		      ,ReverseH(i),ReverseReS21M(i),ReverseImS21M(i) 
	    READ(30,*) ForwardH(i),ForwardReS12M(i),ForwardImS12M(i)
     *		      ,ReverseH(i),ReverseReS12M(i),ReverseImS12M(i) 
	    READ(40,*) ForwardH(i),ForwardReS22M(i),ForwardImS22M(i)
     *		      ,ReverseH(i),ReverseReS22M(i),ReverseImS22M(i) 
       END DO
	 CLOSE(10)
	 CLOSE(20)
	 CLOSE(30)
	 CLOSE(40)

 	 OPEN(60,FILE='S11corrected.csv')
 	 OPEN(70,FILE='S22corrected.csv')
 	 OPEN(80,FILE='S12corrected.csv')
 	 OPEN(90,FILE='S21corrected.csv')
	 OPEN(100,FILE='Zcorrected.csv')

	 SMSOLT(1)=ReS11MS(1)*unit+ImS11MS(1)*cunit
	 SMSOLT(2)=ReS11MO(1)*unit+ImS11MO(1)*cunit
	 SMSOLT(3)=ReS11ML(1)*unit+ImS11ML(1)*cunit
	 SMSOLT(4)=ReS22MS(1)*unit+ImS22MS(1)*cunit
	 SMSOLT(5)=ReS22MO(1)*unit+ImS22MO(1)*cunit
	 SMSOLT(6)=ReS22ML(1)*unit+ImS22ML(1)*cunit
	 SMSOLT(7)=ReS11MT(1)*unit+ImS11MT(1)*cunit
	 SMSOLT(8)=ReS21MT(1)*unit+ImS21MT(1)*cunit
	 SMSOLT(9)=ReS12MT(1)*unit+ImS12MT(1)*cunit
	 SMSOLT(10)=ReS22MT(1)*unit+ImS22MT(1)*cunit

	 SOLT(1)=ReS11S(1)*unit+ImS11S(1)*cunit
	 SOLT(2)=ReS11O(1)*unit+ImS11O(1)*cunit
	 SOLT(3)=ReS11L(1)*unit+ImS11L(1)*cunit
	 SOLT(4)=ReS22S(1)*unit+ImS22S(1)*cunit
	 SOLT(5)=ReS22O(1)*unit+ImS22O(1)*cunit
	 SOLT(6)=ReS22L(1)*unit+ImS22L(1)*cunit
	 SOLT(7)=ReS11T(1)*unit+ImS11T(1)*cunit
	 SOLT(8)=ReS21T(1)*unit+ImS21T(1)*cunit
	 SOLT(9)=ReS12T(1)*unit+ImS12T(1)*cunit
	 SOLT(10)=ReS22T(1)*unit+ImS22T(1)*cunit

	 CALL SOLTErrors(SMSOLT,SOLT,ESOLT)

	 DO i=1,NM

	    ForwardSM(1)=ForwardReS11M(i)*unit+ForwardImS11M(i)*cunit
	    ForwardSM(2)=ForwardReS21M(i)*unit+ForwardImS21M(i)*cunit
	    ForwardSM(3)=ForwardReS12M(i)*unit+ForwardImS12M(i)*cunit
	    ForwardSM(4)=ForwardReS22M(i)*unit+ForwardImS22M(i)*cunit

	    ReverseSM(1)=ReverseReS11M(i)*unit+ReverseImS11M(i)*cunit
	    ReverseSM(2)=ReverseReS21M(i)*unit+ReverseImS21M(i)*cunit
	    ReverseSM(3)=ReverseReS12M(i)*unit+ReverseImS12M(i)*cunit
	    ReverseSM(4)=ReverseReS22M(i)*unit+ReverseImS22M(i)*cunit

		CALL SOLTCorrection(ESOLT,ForwardSM,ForwardSA)
		CALL SOLTCorrection(ESOLT,ReverseSM,ReverseSA)

	    ForwardZ=100.0*(unit-ForwardSA(4))/ForwardSA(4)
	    ReverseZ=100.0*(unit-ReverseSA(4))/ReverseSA(4)

	   WRITE(60,600) ForwardH(i),REAL(ForwardSA(1)),IMAG(ForwardSA(1))
     *		        ,ReverseH(i),REAL(ReverseSA(1)),IMAG(ReverseSA(1))
600	   FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *                ,',',F20.8)

	   WRITE(70,700) ForwardH(i),REAL(ForwardSA(2)),IMAG(ForwardSA(2))
     *		        ,ReverseH(i),REAL(ReverseSA(2)),IMAG(ReverseSA(2))
700	   FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *                ,',',F20.8)

	   WRITE(80,800) ForwardH(i),REAL(ForwardSA(3)),IMAG(ForwardSA(3))
     *		        ,ReverseH(i),REAL(ReverseSA(3)),IMAG(ReverseSA(3))
800	   FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *                ,',',F20.8)

	   WRITE(90,900) ForwardH(i),REAL(ForwardSA(4)),IMAG(ForwardSA(4))
     *		        ,ReverseH(i),REAL(ReverseSA(4)),IMAG(ReverseSA(4))
900	   FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *                ,',',F20.8)

	    WRITE(100,1000) ForwardH(i),REAL(ForwardZ),IMAG(ForwardZ)
     *		          ,ABS(ForwardZ),ReverseH(i),REAL(ReverseZ)
     *                  ,IMAG(ReverseZ),ABS(ReverseZ)
1000		FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *           ,',',F20.8,',',F20.8,',',F20.8)

	 END DO

	 CLOSE(60)
	 CLOSE(70)
	 CLOSE(80)
	 CLOSE(90)
	 CLOSE(100)

	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Output files:'
	 WRITE(*,*)''
	 WRITE(*,*)'S11corrected.csv'
	 WRITE(*,*)'S21corrected.csv'
	 WRITE(*,*)'S12corrected.csv'
	 WRITE(*,*)'S22corrected.csv'
	 WRITE(*,*)'Zcorrected.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'The impedance was calculated from S21-parameter.'
	 WRITE(*,*)''
	 WRITE(*,*)'Columns of the output S-parameter and Z files:'
	 WRITE(*,*)''
	 WRITE(*,*)'H(-->), Re[S], Im[S], H(<--), Re[S], Im[S]'
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

	 DEALLOCATE(freqSOLT,freqMSOLT)
	 DEALLOCATE(ReS11S,ImS11S)
	 DEALLOCATE(ReS11O,ImS11O)
	 DEALLOCATE(ReS11L,ImS11L)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 DEALLOCATE(ReS22S,ImS22S)
	 DEALLOCATE(ReS22O,ImS22O)
	 DEALLOCATE(ReS22L,ImS22L)
	 DEALLOCATE(ReS22MS,ImS22MS)
	 DEALLOCATE(ReS22MO,ImS22MO)
	 DEALLOCATE(ReS22ML,ImS22ML)
 	 DEALLOCATE(ReS11T,ImS11T)
	 DEALLOCATE(ReS21T,ImS21T)
	 DEALLOCATE(ReS12T,ImS12T)
	 DEALLOCATE(ReS22T,ImS22T)
	 DEALLOCATE(ReS11MT,ImS11MT)
	 DEALLOCATE(ReS21MT,ImS21MT)
	 DEALLOCATE(ReS12MT,ImS12MT)
	 DEALLOCATE(ReS22MT,ImS22MT)
 	 DEALLOCATE(Re,Im)
 	 DEALLOCATE(ForwardH,ReverseH)
 	 DEALLOCATE(ForwardReS11M,ForwardImS11M)
	 DEALLOCATE(ReverseReS11M,ReverseImS11M)
 	 DEALLOCATE(ForwardReS21M,ForwardImS21M)
	 DEALLOCATE(ReverseReS21M,ReverseImS21M)
 	 DEALLOCATE(ForwardReS12M,ForwardImS12M)
	 DEALLOCATE(ReverseReS12M,ReverseImS12M)
 	 DEALLOCATE(ForwardReS22M,ForwardImS22M)
	 DEALLOCATE(ReverseReS22M,ReverseImS22M)

	 flag=1
	 
       CASE DEFAULT
       
	 WRITE (*, *)'Command not recognized. Please try again.'
	 flag=0
	 PAUSE
      
	 END SELECT
	 
	 END DO

	 RETURN

	 END
