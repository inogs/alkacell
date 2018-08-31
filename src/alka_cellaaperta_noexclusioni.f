	program alka_cellaaperta
c modified by CGP 
c for experimental data from open cell titrantion  
c
* Driver routine for the evaluation of the total alkalinity (AT) and
* total dissolved inorganic carbon (CT) from potentiometric titration
* data. This routine calls LMDIF1 from MINPACK to adjust the values
* of F (E0), AT, CT, and K1 so as to minimize the sum-of-squares of
* residuals in the concentration of total hydrogen (defined relative
* to the alkalinity equivalence point).
*
* Written by Andrew G. Dickson (last modified February 1994)
	IMPLICIT NONE
* Parameter:
* MaxPts - maximum number of titration data points
* (In routines MAIN, SetUP and FCN)
	INTEGER MaxPts
	PARAMETER (MaxPts = 100)
* Common block /TITN/:
* C - concentration of titrant acid (mol/kg)
* W0 - mass of sample titrated (g)
* W - array containing titrant amounts (g)
* H - array containing [H'] = 10**((E0 - E)/K)
* based on the initial E0 estimate
* BT - total boron (mol/kg-soln)
* ST - total sulfate (mol/kg-soln)
* FT - total fluoride (mol/kg-soln)
* PT - total phosphate (mol/kg-soln)
* SiT - total silicate (mol/kg-soln)
*
* K1 - [H][HCO3]/[H2CO3]
* K2 - [H][CO3]/[HCO3]
* KB - [H][BO2]/[HBO2]
* K1P - [H][H2PO4]/[H3PO4]
* K2P - [H][HPO4]/[H2PO4]
* K3P - [H][PO4]/[HPO4]
* KSI - [H][SiO(OH)3]/[Si(OH)4]
* KS - [h][SO4]/[HSO4]
* KF - [H][F]/[HF]
* KW - [H][OH]
* Z - pH scale conversion factor [H] = [h](1 + ST/KS)
	DOUBLE PRECISION C, W0, W(MaxPts), H(MaxPts),
     + BT, ST, FT, PT, SIT,
     + K1, K2, KB, K1P, K2P, K3P, KSi, KW, KS, KF, Z
	COMMON /TITN/ C, W0, W, H,
     + BT, ST, FT, PT, SIT,
     + K1, K2, KB, K1P, K2P, K3P, KSi, KW, KS, KF, Z
* Common block /EVAL/:
* ICOUNT(1) - total number of function evaluations
* ICOUNT(2) - total number of Jacobian calculations
	INTEGER ICOUNT(2)
	COMMON / EVAL/ ICOUNT
* Additional variables used by subroutine Input
	CHARACTER Fname*100, Header*80
	DOUBLE PRECISION S, T, V0, DAcid, V(MaxPts), E(MaxPts)
* Additional variables used by subroutine SetUp
	DOUBLE PRECISION KNernst, E0, alka1

 
* MINPACK function
	DOUBLE PRECISION DPMPAR
* Additional variables used by subroutine LMDIF1 of MINPACK
	INTEGER NPar, LWA
	PARAMETER (NPar = 2, LWA = MaxPts*NPar + 5*NPar + MaxPts)
	INTEGER NPts, INFO, IWA(NPar)
	DOUBLE PRECISION X(NPar), FVEC(MaxPts), TOL, WA(LWA)
* External function called by MINPACK
	EXTERNAL FCN
* Prompt user for the file name of the input data file
	Fname='C:\AlkaOpenCell\src\input'
c	WRITE (*, '(/ '' Enter name of titration data file): '' $)')
c	READ *, Fname
* Read titration data from file
* Fname - name of file containing the titration data
* Header - sample identifier
* S - salinity of titrated sample
* PT - total phosphate in sample (mol/kg-soln)
* SiT - total silicate in sample (mol/kg-soln)
* T - titration temperature (deg C)
* V0 - volume of titration cell (cm3)
* C - concentration of acid titrant (mol/kg-soln)
* DAcid - density of acid titrant (g/cm3)
* NPts - number of titration points
* V - array of volumes of titrant used (cm3)
* E - array of corresponding e.m.f.s (V)
	CALL Input (MaxPts, FName,
     + Header, S, PT, SiT, T, V0, C, DAcid, NPts, V, E)
* Set up the titration calculation. The data needed for the
* calculation is passed to FCN in common block /TITN/.
* KNernst - Nernst parameter (appropriate to titration)
* E0 - estimate of E0 for pH cell used
c alka1 - first estimation of alka (this should help the covergence
	CALL SetUp (S, T, V0, DAcid, NPts, V, E, KNernst, E0,alka1)
* Inititialize solution vector, X, and ICOUNT
	x(1) = 1.0
	x(2) = alka1
c cgp	x(3) = 2.0
c cgp	x(4) = 1.0
	ICOUNT(1) = 0
	ICOUNT(2) = 0
* Set the desired convergence parameter to sqrt(machine precision)
* DPMPAR - MINPACK function to get machine dependent info.
	TOL = SQRT(DPMPAR(1))
* Use MINPACK routine LMDIF1 to adust X so as to mimize RSS.
* FCN - name of routine used to calculate residuals
* NPts - number of functions (data points)
* Npar - number of parameters adjusted by LMDIF1
* X - array containing solution vector
* X(1) - F = [H]/[H'] )
* X(2) - AT * 1E3 ) scaled to make components
c cgp NO MORE USED 
* X(3) - CT * 1E3 ) of X approx. equal to 1
* X(4) - K1 * 1E6 )
* FVEC - array containg residuals (calculated by FCN)
 
* TOL - tolerance for fitting
* INFO - flag indicating if minimization was successsful
* IWA - )
* WA - ) working areas used by LMDIF1
* LWA - )
	CALL LMDIF1(FCN, NPts, NPar, X, FVEC, TOL, INFO, IWA, WA, LWA)
* Output a table of results on channel 6 (the screen)
	OPEN(UNIT=35, file="C:\AlkaOpenCell\src\output")
	CALL Output (35,
     + INFO, ICOUNT, NPar, X,
     + Header, S, PT, SiT, T, V0, C, DAcid,
     + NPts, V, E, H, FVEC, KNernst, E0)
	CLOSE(35)
	

	END
********************************************************************************
	SUBROUTINE Input (MaxPts, FName,
     + Header, S, PT, SiT, T, M0, C, DAcid, NPts, V, E)
	IMPLICIT NONE
	INTEGER MaxPts, NPts
	CHARACTER Fname*100, Header*80
	DOUBLE PRECISION S, PT, SiT, T, M0, C, DAcid, V(MaxPts), E(MaxPts)
* Subroutine to read in the titration data file
*
* Written by Andrew G. Dickson (last modified February 1994)
*
* Called with:
* MaxPts - maximum number of titration points
* Fname - name of file containing data to be processed
*
* Returns:
* Header - character string identifying sample
* S - salinity of titrated sample
* PT - total phosphate in sample (mol/kg-soln)
* SiT - total silicate in sample (mol/kg-soln)
* T - titration temperature (deg C)
* V0 - volume of sample titrated (cm3)
* C - concentration of acid titrant (mol/kg-soln)
* DAcid - density of the acid titrant (g/cm3)
* V - array of volumes of titrant used (cm3)
* E - array of corresponding e.m.f.s (V)
	INTEGER I
	OPEN (1, FILE = Fname)
* Read in sample information and adjust PT and SiT to mol/kg
	READ (1,'(A80)') Header
	READ (1,*)
	READ (1, '(8X, F8.0)') S
	READ (1, '(8X, F8.0)') PT
	PT = 1D-6 * PT
	READ (1, '(8X, F8.0)') SiT
	SiT = 1D-6 * SiT
* Read in titration information
	READ (1,*)
	READ (1, '(8X, F8.0)') T
c	READ (1, '(8X, F8.0)') V0
	READ (1, '(8X, F8.0)') M0  ! c cgp  read mass no volume 
	READ (1, '(8X, F8.0)') C
	READ (1, '(8X, F8.0)') DAcid

      write(*,*) 'fine costanti'

* Read in volume and e.m.f. data
	READ (1, '(//)')
	DO 200 I = 1, MaxPts
	READ (1, *, END=300) V(I), E(I)
c     '(F6.0, F12.0)'
200   CONTINUE
	WRITE (6, '(1X, ''More than MaxPts (100) data points in file!'')')
	CLOSE (1)
	STOP
300	NPts = I - 1
	CLOSE (1)
* Convert mV to V
	IF (ABS(E(NPts) - E(1)) .GT. 1) THEN
	DO 400 I = 1, NPts
	E(I) = E(I)/1000
400	CONTINUE
	ENDIF
	RETURN
	END
********************************************************************************
	SUBROUTINE SetUp(S,T,M0,DAcid,NPts,V,E,KNernst,E0,alka1)
	IMPLICIT NONE

	INTEGER NPts
	DOUBLE PRECISION S, T, M0, DAcid, V(NPts),E(NPts),KNernst,E0,alka1
* Subroutine to set up calculation ready for MINPACK processing
*
* Written by Andrew G. Dickson (last modified August 1994)
*
* Called with:
* S - salinity of titrated sample
* T - titration temperature (deg C)
c cgp no more V0 - volume of sample titrated (cm3)
c cgp M0 - mass of sample titrated (g)
* DAcid - density of the acid titrant (g/cm3)
* NPts - number of titration points
* V - array of volumes of titrant used (cm3)
* E - Array of corresponding e.m.f.s (V)

*
* Returns:
* KNernst - Nernst parameter (appropriate to titration)
* E0 - Estimate of E0 of pH cell
c alka1 - first estimation of alkalinity
c

* Parameter:
* MaxPts - maximum number of titration data points
* (In routines MAIN, SetUP and FCN)
	INTEGER MaxPts
	PARAMETER (MaxPts = 100)
* Common block /TITN/:
* C - concentration of titrant acid (mol/kg)
* W0 - mass of sample titrated (g)
* W - array containing titrant amounts (g)
* H - array containing [H'] = 10**((E0 - E)/K)
* based on the initial E0 estimate
* BT - total boron (mol/kg-soln)
* ST - total sulfate (mol/kg-soln)
* FT - total fluoride (mol/kg-soln)
* PT - total phosphate (mol/kg-soln)
* SiT - total silicate (mol/kg-soln)
*
 
* K1 - [H][HCO3]/[H2CO3]
* K2 - [H][CO3]/[HCO3]
* KB - [H][BO2]/[HBO2]
* K1P - [H][H2PO4]/[H3PO4]
* K2P - [H][HPO4]/[H2PO4]
* K3P - [H][PO4]/[HPO4]
* KSI - [H][SiO(OH)3]/[Si(OH)4]
* KS - [h][SO4]/[HSO4]
* KF - [H][F]/[HF]
* KW - [H][OH]
* Z - pH scale conversion factor [H] = [h](1 + ST/KS)
	DOUBLE PRECISION C, W0, W(MaxPts), H(MaxPts),
     + BT, ST, FT, PT, SIT,
     + K1, K2, KB, K1P, K2P, K3P, KSi, KW, KS, KF, Z
	COMMON /TITN/ C, W0, W, H,
     + BT, ST, FT, PT, SIT,
     + K1, K2, KB, K1P, K2P, K3P, KSi, KW, KS, KF, Z
	INTEGER I
	DOUBLE PRECISION DensNaCl, DensSW

	IF (S .GE. 5) THEN
* It is a sea water sample with salinity, S:
* Calculate mass of sample titrated (g)
c cgp	W0 = V0 * DensSW (S, T)
	W0 = M0
* Calculate values for the total concentrations
	CALL ConcnsSW (S, BT, ST, FT)
* Calculate values for the equilibrium constants
	CALL ConstsSW (S, T, K1, K2, KB, K1P, K2P, K3P, KSi,
     + KS, KF, KW)
* Calculate pH conversion factor from "free" to "total" scale
	Z = 1 + ST/KS

	ELSE
* It is a NaCl solution with concentration S (mol/kg-soln):
c cgp  -- 6 e' il canale del video
      write(6,'('' Salinity less than 5%o --> stop program'')')
	stop
c TO BE verified
c
c	W0 = V0 * DensNaCl (S, T)
c	BT = 0
c	ST = 0
c	FT = 0
c	CALL ConstsNaCl (S, T, K1, K2, KW)
c	Z = 1
	ENDIF

* Calculate mass of acid titrant at each titration point (g)
	DO 100 I = 1, NPts
	W(I) = V(I) * DACID
100   CONTINUE

* Calculate appropriate Nernst Factor: E = E0 +/- (RT/F)ln[H]
	KNernst = 8.31451 * (273.15 + T) / 96485.309
	IF (E(1) .GT. E(NPts)) KNernst = -KNernst

* Estimate E0 using last two titration points (at low pH)
	CALL EstimE0 (W0,NPts,W,E,C,KNernst,E0,alka1)
 
* Calculate [H] using this initial estimate of E0
	DO 300 I = 1, NPts
	H(I) = EXP((E(I) - E0)/KNernst)
300   CONTINUE
	RETURN
	END

********************************************************************************
	SUBROUTINE Output (NOUT,
     + INFO, ICOUNT, NPar, X,
     + Header, S, PT, SiT, T, M0, C, DAcid,
     + NPts, V, E, H, FVEC, KNernst, E0)
	IMPLICIT NONE
	INTEGER NOUT, INFO, ICOUNT(2), NPar, NPts
	CHARACTER Header*80
	DOUBLE PRECISION X(NPar), S, PT, SiT, T, M0, C, DAcid,
     + V(NPts), E(NPts), H(NPts), FVEC(NPts), KNernst, E0
* Subroutine to print out results as a table
*
* Written by Andrew G. Dickson (last modified February 1994)
*
* Called with:
* NOUT - channel number for printing
* INFO - flag from LMDIF1 indicating if minimization was successful
* NPar - number of parameters being adjusted (4)
* X - array containing final values of adjusted parameters
* Header - sample identifier
* S - salinity of titrated sample
* PT - total phosphate in sample (mol/kg-soln)
* SiT - total silicate in sample (mol/kg-soln)
* T - titration temperature (deg C)
c gcp not more * V0 - volume of sample titrated (cm3)
* M0 - mass of sample titrated (g)
* C - concentration of acid titrant (mol/kg-soln)
* DAcid - density of acid titrant (g/cm3)
* NPts - number of titration points
* V - array of volumes of titrant used (cm3)
* E - array of corresponding e.m.f.s (V)
* FVEC - residuals in total hydrogen ion (mol/kg-soln)
* KNernst - appropriate Nernst factor E = E0 +/- K*ln[H] (V)
* E0 - estimate of E0 (V)
	INTEGER I
	DOUBLE PRECISION ENORM
	WRITE (NOUT, *)
	WRITE (NOUT, '(1X, A80 /)') Header
	WRITE (NOUT, '(1X, ''S = '', F7.3)') S
	WRITE (NOUT, '(1X, ''PT = '', F6.2, ''  micromol/kg'')') 1D6*PT
	WRITE (NOUT, '(1X, ''SiT = '', F6.2, ''  micromol/kg'')') 1D6*SIT
	WRITE (NOUT, *)
	WRITE (NOUT, '(1X, ''T = '', F6.2, '' deg C'')') T
c	WRITE (NOUT, '(1X, ''V0 = '', F7.3, '' cm3'')') V0
	WRITE (NOUT, '(1X, ''M0 = '', F7.3, '' g'')') M0
	WRITE (NOUT, '(1X, ''C = '', F7.5, '' mol/kg'' )') C
	WRITE (NOUT, '(1X, ''DACID = '', F7.5, '' g/cm3'' /)') DACID
	WRITE (NOUT,'('' LMDIF1 exit parameter '', I3)') INFO
	WRITE (NOUT,'('' Function calls '', I5)') ICOUNT(1)
	WRITE (NOUT,'('' Jacobian calls '', I5,/)') ICOUNT(2)
	IF (INFO .GT. 3) WRITE (NOUT, '('' ***LMDIF1 DID NOT CONVERGE!***'')')
	WRITE (NOUT, '(1X, ''E0 = '', F8.6, '' V'')') E0 - KNernst*LOG(X(1))
	WRITE (NOUT, '(1X, ''AT = '', F7.2, ''  micromol/kg'')') 1D3*X(2)
 
c cgp no more this output	WRITE (NOUT, '(1X, ''CT = '', F7.2, ''  mol/kg'')') 1D3*X(3)
c	WRITE (NOUT, '(1X, ''pK1 = '', F6.4 /)') -LOG10(1D-6*X(4))
* Use MINPACK function ENORM to calculate Euclidean Norm
	WRITE (NOUT, '(1X, ''s = '', F5.3, ''  micromol/kg'')')
     + 1D6*SQRT(ENORM(NPts,FVEC)**2/(NPts-NPar))
	WRITE (NOUT, 
     + '(4X,''V/cm3'',6X,''E/V'',6X,''-log[H]'',3X,'' H/( mol/kg)'' /)')
	WRITE (NOUT,'(3X, F6.3, F11.5, F11.4, F12.3)')
     + (V(I), E(I), -LOG10(H(I)*X(1)), 1D6*FVEC(I), I = 1, NPts)
	RETURN
	END

********************************************************************************
	SUBROUTINE FCN (M, N, X, FVEC, IFLAG)
	IMPLICIT NONE
	INTEGER M, N, IFLAG
	DOUBLE PRECISION X(N), FVEC(M)
*
* Subroutine to calculate the vector of residuals, FVEC,
* corresponding to the current values of X
*
* Written by Andrew G. Dickson (last modified February 1994)
*
* Called with:
* NPts - number of titration points
* Npar - number of parameters being adjusted (4)
* X - current estimate of the solution
* IFLAG - set by MINPACK to identify if called for function
* or Jacobian evaluation.
* Returns:
* FVEC - array of residuals in HTOT (mol/kg)
* Parameter:
* MaxPts - maximum number of titration data points
* (In routines MAIN, SetUP and FCN)
	INTEGER MaxPts
	PARAMETER (MaxPts = 100)
* Common block /TITN/:
* C - concentration of titrant acid (mol/kg)
* W0 - mass of sample titrated (g)
* W - array containing titrant amounts (g)
* H - array containing [H'] = 10**((E0 - E)/K)
* based on the initial E0 estimate
* BT - total boron (mol/kg-soln)
* ST - total sulfate (mol/kg-soln)
* FT - total fluoride (mol/kg-soln)
* PT - total phosphate (mol/kg-soln)
* SiT - total silicate (mol/kg-soln)
*
* K1 - [H][HCO3]/[H2CO3]
* K2 - [H][CO3]/[HCO3]
* KB - [H][BO2]/[HBO2]
* K1P - [H][H2PO4]/[H3PO4]
* K2P - [H][HPO4]/[H2PO4]
* K3P - [H][PO4]/[HPO4]
* KSI - [H][SiO(OH)3]/[Si(OH)4]
* KS - [h][SO4]/[HSO4]
* KF - [H][F]/[HF]
* KW - [H][OH]
* Z - pH scale conversion factor [H] = [h](1 + ST/KS)
 
	DOUBLE PRECISION C, W0, W(MaxPts), H(MaxPts),
     + BT, ST, FT, PT, SIT,
     + K1, K2, KB, K1P, K2P, K3P, KSi, KW, KS, KF, Z
	COMMON /TITN/ C, W0, W, H,
     + BT, ST, FT, PT, SIT,
     + K1, K2, KB, K1P, K2P, K3P, KSi, KW, KS, KF, Z
* Common block /EVAL/:
* ICOUNT(1) - total number of function evaluations
* ICOUNT(2) - total number of Jacobian calculations
	INTEGER ICOUNT(2)
	COMMON / EVAL/ ICOUNT

	INTEGER I
	DOUBLE PRECISION F, AT, CT
* Count number of times FCN is called:

	ICOUNT(IFLAG) = ICOUNT(IFLAG) + 1
* Parameters being adjusted:
* F - correction factor for E0
* AT - total alkalinity
* CT - total dissolved inorganic carbon
* K1 - first dissociation constant of carbonic acid
	F = X(1)
	AT = X(2)*1D-3
c cgp	CT = X(3)*1D-3
c cgp	K1 = X(4)*1D-6

* Calculate the residuals at X (mol/kg-soln)
	DO 100 I = 1, M
c	FVEC(I) = AT
c     + - CT*((K1*F*H(I) + 2*K1*K2) /
c     +                       ((F*H(I))**2 + K1*F*H(I) + K1*K2))
c     + - BT/(1 + F*H(I)/KB)
c     + - PT*((K1P*K2P*F*H(I) + 2*K1P*K2P*K3P - (F*H(I))**3) /
c     +               ((F*H(I))**3 + K1P*(F*H(I))**2 +
c     +                           K1P*K2P*F*H(I) + K1P*K2P*K3P))
c     + - SiT/(1 + F*H(I)/KSi)
c     + + ST/(1 + KS*Z/(F*H(I)))
c     + + FT/(1 + KF/(F*H(I)))
c     + + (W0 + W(I))/W0 * (F*H(I)/Z - KW/(F*H(I)))
c     + - (W(I)/W0)*C
c cgp new formula (see formula 13 on SOP3b, july 1, 2008
	FVEC(I) = AT
     + + ST/(1 + KS*Z/(F*H(I)))
     + + FT/(1 + KF/(F*H(I)))
     + + (W0 + W(I))/W0 * (F*H(I)/Z - KW/(F*H(I)))
     + - (W(I)/W0)*C
 

100   CONTINUE

	RETURN
	END
********************************************************************************
	DOUBLE PRECISION FUNCTION DensSW (S, T)
	IMPLICIT NONE
	DOUBLE PRECISION T, S
*
* Function to calculate the density of sea water.
* Based on Millero & Poisson (1981) Deep-Sea Res. 28, 625.
*
* Written by Andrew G. Dickson (last modified February 1994)
*
* Called with:
* S - salinity of sample
* T - titration temperature (Centigrade)
	DOUBLE PRECISION DH2O, A, B, C
 
* Density of SMOW (kg/m3)
	DH2O = 999.842594 + 6.793952D-2 * T - 9.095290D-3 * T**2 +
     + 1.001685D-4 * T**3 - 1.120083D-6 * T**4 + 6.536332D-9 * T**5
* Density of sea water
	A = 8.24493D-1 - 4.0899D-3 * T + 7.6438D-5 * T**2 -
     + 8.2467D-7 * T**3 + 5.3875D-9 * T**4
	B = -5.72466D-3 + 1.0227D-4 * T - 1.6546D-6 * T**2
	C = 4.8314D-4
	DensSW = (DH2O + A*S + B*S**1.5 + C*S**2)/1000
	RETURN
	END

********************************************************************************
	SUBROUTINE ConcnsSW (S, BT, ST, FT)
	DOUBLE PRECISION S, BT, ST, FT
*
* Subroutine to calculate appropriate total concentrations,
* for sea water of salinity, S.
*
* Written by Andrew G. Dickson (last modified August 1994)
*
* Called with:
* S - salinity of sample
*
* Returns:
* BT - total boron (mol/kg-soln)
* ST - total sulfate (mol/kg-soln)
* FT - total fluoride (mol/kg-soln)
* Uppstrom (1974) Deep-Sea Res. 21, 161.
	BT = (0.000232/10.811) * (S/1.80655)
* Morris & Riley (1966) Deep-Sea Res. 13, 699
	ST = (0.1400/96.062) * (S/1.80655)
* Riley (1965) Deep-Sea Res. 12, 219.
	FT = (0.000067/18.998) * (S/1.80655)
	RETURN
	END

********************************************************************************
	SUBROUTINE ConstsSW (S, T,
     + K1, K2, KB, K1P, K2P, K3P, KSi, KS, KF, KW)

	IMPLICIT NONE
	DOUBLE PRECISION T, S,
     + K1, K2, KB, K1P, K2P, K3P, KSi, KS, KF, KW
* Subroutine to calculate values of dissociation constants,
* appropriate to sea water of salinity, S, and temperature, T.
*
* Written by Andrew G. Dickson (last modified August 1994)
*
* Called with:
* S - salinity of sample
* T - titration temperature (deg C)
*
* Returns:
* K1 - [H][HCO3]/[H2CO3]
* K2 - [H][CO3]/[HCO3]
* KB - [H][BO2]/[HBO2]
* K1P - [H][H2PO4]/[H3PO4]
* K2P - [H][HPO4]/[H2PO4]
* K3P - [H][PO4]/[HPO4]
 
* KSI - [H][SiO(OH)3]/[Si(OH)4]
* KS - [h][SO4]/[HSO4]
* KF - [H][F]/[HF]
* KW - [H][OH]
*
	DOUBLE PRECISION TK, IS
	TK = 273.15 + T
	IS = 19.924*S/(1000 - 1.005*S)
* Roy et al. (1993) Mar. Chem. 44, 249
	K1 = exp (
     + -2307.1266/TK + 2.83655 - 1.5529413*LOG(TK) +
     + (-4.0484/TK - 0.20760841)*SQRT(S) + 0.08468345*S -
     + 0.00654208*S**1.5 + log(1 - 0.001005*S)
     + )
	K2 = exp (
     + -3351.6106/TK - 9.226508 - 0.2005743*LOG(TK) +
     + (-23.9722/TK - 0.106901773)*SQRT(S) + 0.1130822*S -
     + 0.00846934*S**1.5 + log(1 - 0.001005*S)
     + )
* Dickson (1990) Deep-Sea Res. 37, 755
	KB = exp (
     + (-8966.90 - 2890.53*SQRT(S) - 77.942*S + 1.728*S**1.5 -
     + 0.0996*S**2)/TK + (148.0248 + 137.1942*SQRT(S) + 1.62142*S) +
     +(-24.4344 - 25.085*SQRT(S) -0.2474*S)*LOG(TK) +0.053105*SQRT(S)*TK
     + )
* Millero (1995) -- composite data (Geochim. Cosmochim Acta 59, 661)
	K1P = exp (
     + -4576.752/TK + 115.525 - 18.453*LOG(TK) +
     + (-106.736/TK + 0.69171)*SQRT(S) + (-0.65643/TK - 0.01844)*S
     + )
	K2P = exp (
     + -8814.715/TK + 172.0883 - 27.927*LOG(TK) +
     + (-160.340/TK + 1.3566)*SQRT(S) + (0.37335/TK - 0.05778)*S
     + )
	K3P = exp (
     + -3070.75/TK - 18.141 +
     + (17.27039/TK + 2.81197)*SQRT(S) + (-44.99486/TK - 0.09984)*S
     + )
* Millero (1995) -- composite data (Geochim. Cosmochim Acta 59, 661)
	KSi = exp (
     + -8904.2/TK + 117.385 - 19.334*LOG(TK) +
     + (-458.79/TK + 3.5913)*SQRT(IS) +
     + (188.74/TK - 1.5998)*IS + (-12.1652/TK + 0.07871)*(IS)**2 +
     + log(1 - 0.001005*S)
     + )
* Millero (1995) -- composite data (Geochim. Cosmochim Acta 59, 661)
	KW = exp (
     + -13847.26/TK + 148.9652 - 23.6521*LOG(TK) +
     + (118.67/TK - 5.977 + 1.0495*LOG(TK))*SQRT(S) - 0.01615*S
     + )
* Dickson (1990) -- free hydrogen ion scale (J. Chem. Thermodynamics 22, 113)
	KS = exp (
     + -4276.1/TK + 141.328 - 23.093*log(TK) +
     + (-13856/TK + 324.57 - 47.986*log(TK)) * sqrt(IS) +
     + (35474/TK - 771.54 + 114.723*log(TK)) * IS -
     + 2698*IS**1.5/TK + 1776*IS**2/TK + log(1 - 0.001005*S)
     + )
 
* Dickson & Riley (1979) -- change pH scale to total (Mar. Chem. 7, 89)
	KF = 1590.2/TK - 12.641 + 1.525 * SQRT(IS) + log(1 - 0.001005*S)
	KF = KF + log(1 + (0.1400/96.062)*(S/1.80655)/KS)
	KF = exp(KF)
	RETURN
	END

********************************************************************************
	DOUBLE PRECISION FUNCTION DensNaCl (CNaCl, T)
	IMPLICIT NONE
	DOUBLE PRECISION T, CNaCl
*
* Function to calculate the density of a sodium cloride solution.
* Based on equation by Lo Surdo et al.
* J. Chem. Thermodynamics 14, 649 (1982).
*
* Written by Andrew G. Dickson (last modified March 1994)
*
* Called with:
* CNaCl - concentration of sodium chloride (mol/kg-soln)
* T - titration temperature (Centigrade)
	DOUBLE PRECISION MNaCl, DH2O, DNaCl
* Calculate the molality of NaCl
	mNaCl = CNaCl/(1 - 0.058443*CNaCl)
* Density of SMOW (kg/m3)
	DH2O = 999.842594 + 6.793952D-2 * T - 9.095290D-3 * T**2 +
     + 1.001685D-4 * T**3 - 1.120083D-6 * T**4 - 6.536332D-9 * T**5
* Density of NaCl (kg/m3)
	DNaCl = DH2O + mNaCl*(46.5655 - 0.2341*t + 3.4128D-3 * T**2
     + - 2.7030D-5 * T**3 + 1.4037D-7 * T**4) +
     + mNaCl**1.5 * (-1.8527 + 5.3956D-2 * T - 6.2635D-4 * T**2) +
     + mNaCl**2 * (-1.6368 - 9.5653D-4 * T + 5.2829D-5 * T**2) +
     + 0.2274 * mNaCl**2.5
	DensNaCl = 1D-3 * DNaCl
	RETURN
	END

********************************************************************************
	SUBROUTINE ConstsNaCl (CNaCl, T, K1, K2, KW)
	IMPLICIT NONE
	DOUBLE PRECISION T, CNaCl, K1, K2, KW
* Subroutine to calculate values of dissociation constants,
* appropriateto a sodium chloride solution of concentration,
* CNaCl, and temperature, T.
*
* Written by Andrew G. Dickson (last modified August 1994)
*
* Called with:
* CNaCl- concentration of NaCl (mol/kg-soln)
* T - titration temperature (deg C)
*
* Returns:
* K1 - [H][HCO3]/[H2CO3]
* K2 - [H][CO3]/[HCO3]
* KW - [H][OH]
 
	DOUBLE PRECISION TK
	TK = 273.15 + T
* At present this only provides constants for
* C(NaCl) = 0.7 mol/kg-soln and t = 25   C.
	IF (ABS(CNaCl - 0.7) .gt. 0.05) THEN
	WRITE (6,'(" ConstsNaCl: C(NaCl) .ne. 0.7")')
	ENDIF
	IF (ABS(T - 25) .gt. 0.1) THEN
	WRITE (6,'(" ConstsNaCl: T .ne. 25 ")')
	ENDIF
	IF ((ABS(CNaCl - 0.7) .gt. 0.05) .or. (ABS(T - 25) .gt. 0.1)) STOP
* Dyrssen & Hansson (1973) Mar. Chem. 1, 137
	K1 = exp(-13.82)
	K2 = exp(-21.97)
	KW = exp(-31.71)
	RETURN
	END

********************************************************************************
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	SUBROUTINE EstimE0 (W0,NPts,W,E,C,KNernst,E0,Atzero)

	IMPLICIT NONE
	DOUBLE PRECISION W0, C, KNernst, E0
      integer NPts, i
      	  
      double precision F1(NPts),W(NPts),E(NPts),Enew(NPts),pHnew(NPts)
      double precision a1(NPts),a2(NPts),a3(NPts),a4(NPts)
	double precision Sw, Sf1, Swf1, Sww, Sw2,a,b
	double precision Wzero, Atzero, mEnew 
c This subroutine estimates an initial value of E0 using a Gran
c function
c pH is calculated to exclude samples outside the range [3 3.5]
c
c Written by G Cossarini (modified Apr 2011)
c
c input
c W0 - mass of sea water sample (g)
c NPts - number of points
c W - mass of titrant  (g)  %%%% these are local variables
c E - e.m.f. (V)
c C - concentration of acid (mol/kg)
c KNernst - appropriate Nernst factor (V)
c
c Returns:
c E0 - estimate of standard potential for pH cell (V)
c Atzero - first estimation of Alkalinity

c compute F1 formula (9)
   
      do 100 i=1, NPts
	  F1(i)= (W0+W(i)) * EXP (E(i) / KNernst)
100   continue

c find W that make F1 ZERO
c solving linear least square for F1=aW+b
      Sw=0
	Sf1=0
	Sww=0
	Swf1=0
	do 101 i=1, NPts
	  Sw=Sw+W(i)
	  Sf1=Sf1+F1(i)
	Sww=Sww+W(i)*W(i)
	Swf1=Swf1+W(i)*F1(i)
101   continue
      Sw2=Sw*Sw
	
	b=(Sww*Sf1-Sw*Swf1 ) / (NPts*Sww-Sw2 )
	a=(NPts*Swf1-Sw*Sf1)/(NPts*Sww-Sw2)
c     0=aW+b
      Wzero=-b/a
c     compute estimation of At in micromol/kg
      Atzero=Wzero/W0*C * 1000 * 1000
	 
c     compute new estimation of E and of pH
	do 102 i=1, NPTs
          a1(i)=(-W0*Atzero/1.0E6+W(i)*C)
          a2(i)=(W0+W(i))
	a3(i)=a1(i)/a2(i)
	a4(i)=log(a3(i))

	   Enew(i)=E(i)-KNernst*LOG( (-W0*Atzero/1.0E6+W(i)*C)/(W0+W(i)) ) 
	   pHnew(i)=-LOG10(EXP( (E(i)-Enew(i))/KNernst ))
102   continue
c     compute the mean of the Enew 
      mEnew=0
	do 103 i=1, NPTs
	  mEnew=mEnew+Enew(i)
103   continue
      mEnew=mEnew/NPTs

      E0=mEnew
c      i=LOG(-C)
	RETURN
	END


 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC+
CCCC MINPACK ROUTINES
C    dpmpar
C    lmdif1
C    lmdif
cccccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision function dpmpar(i)
      integer i
c     **********
c
c     Function dpmpar
c
c     This function provides double precision machine parameters
c     when the appropriate set of data statements is activated (by
c     removing the c from column 1) and all other data statements are
c     rendered inactive. Most of the parameter values were obtained
c     from the corresponding Bell Laboratories Port Library function.
c
c     The function statement is
c
c       double precision function dpmpar(i)
c
c     where
c
c       i is an integer input variable set to 1, 2, or 3 which
c         selects the desired machine parameter. If the machine has
c         t base b digits and its smallest and largest exponents are
c         emin and emax, respectively, then these parameters are
c
c         dpmpar(1) = b**(1 - t), the machine precision,
c
c         dpmpar(2) = b**(emin - 1), the smallest magnitude,
c
c         dpmpar(3) = b**emax*(1 - b**(-t)), the largest magnitude.
c
c     Argonne National Laboratory. MINPACK Project. November 1996.
c     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More'
c
c     **********
      integer mcheps(4)
      integer minmag(4)
      integer maxmag(4)
      double precision dmach(3)
      equivalence (dmach(1),mcheps(1))
      equivalence (dmach(2),minmag(1))
      equivalence (dmach(3),maxmag(1))
c
c     Machine constants for the IBM 360/370 series,
c     the Amdahl 470/V6, the ICL 2900, the Itel AS/6,
c     the Xerox Sigma 5/7/9 and the Sel systems 85/86.
c
c     data mcheps(1),mcheps(2) / z34100000, z00000000 /
c     data minmag(1),minmag(2) / z00100000, z00000000 /
c     data maxmag(1),maxmag(2) / z7fffffff, zffffffff /
c
c     Machine constants for the Honeywell 600/6000 series.
c
c     data mcheps(1),mcheps(2) / o606400000000, o000000000000 /
c     data minmag(1),minmag(2) / o402400000000, o000000000000 /
c     data maxmag(1),maxmag(2) / o376777777777, o777777777777 /
c
c     Machine constants for the CDC 6000/7000 series.
c
c     data mcheps(1) / 15614000000000000000b /
c     data mcheps(2) / 15010000000000000000b /
c
c     data minmag(1) / 00604000000000000000b /
c     data minmag(2) / 00000000000000000000b /
c
c     data maxmag(1) / 37767777777777777777b /
c     data maxmag(2) / 37167777777777777777b /
c
c     Machine constants for the PDP-10 (KA processor).
c
c     data mcheps(1),mcheps(2) / "114400000000, "000000000000 /
c     data minmag(1),minmag(2) / "033400000000, "000000000000 /
c     data maxmag(1),maxmag(2) / "377777777777, "344777777777 /
c
c     Machine constants for the PDP-10 (KI processor).
c
c     data mcheps(1),mcheps(2) / "104400000000, "000000000000 /
c     data minmag(1),minmag(2) / "000400000000, "000000000000 /
c     data maxmag(1),maxmag(2) / "377777777777, "377777777777 /
c
c     Machine constants for the PDP-11. 
c
c     data mcheps(1),mcheps(2) /   9472,      0 /
c     data mcheps(3),mcheps(4) /      0,      0 /
c
c     data minmag(1),minmag(2) /    128,      0 /
c     data minmag(3),minmag(4) /      0,      0 /
c
c     data maxmag(1),maxmag(2) /  32767,     -1 /
c     data maxmag(3),maxmag(4) /     -1,     -1 /
c
c     Machine constants for the Burroughs 6700/7700 systems.
c
c     data mcheps(1) / o1451000000000000 /
c     data mcheps(2) / o0000000000000000 /
c
c     data minmag(1) / o1771000000000000 /
c     data minmag(2) / o7770000000000000 /
c
c     data maxmag(1) / o0777777777777777 /
c     data maxmag(2) / o7777777777777777 /
c
c     Machine constants for the Burroughs 5700 system.
c
c     data mcheps(1) / o1451000000000000 /
c     data mcheps(2) / o0000000000000000 /
c
c     data minmag(1) / o1771000000000000 /
c     data minmag(2) / o0000000000000000 /
c
c     data maxmag(1) / o0777777777777777 /
c     data maxmag(2) / o0007777777777777 /
c
c     Machine constants for the Burroughs 1700 system.
c
c     data mcheps(1) / zcc6800000 /
c     data mcheps(2) / z000000000 /
c
c     data minmag(1) / zc00800000 /
c     data minmag(2) / z000000000 /
c
c     data maxmag(1) / zdffffffff /
c     data maxmag(2) / zfffffffff /
c
c     Machine constants for the Univac 1100 series.
c
c     data mcheps(1),mcheps(2) / o170640000000, o000000000000 /
c     data minmag(1),minmag(2) / o000040000000, o000000000000 /
c     data maxmag(1),maxmag(2) / o377777777777, o777777777777 /
c
c     Machine constants for the Data General Eclipse S/200.
c
c     Note - it may be appropriate to include the following card -
c     static dmach(3)
c
c     data minmag/20k,3*0/,maxmag/77777k,3*177777k/
c     data mcheps/32020k,3*0/
c
c     Machine constants for the Harris 220.
c
c     data mcheps(1),mcheps(2) / '20000000, '00000334 /
c     data minmag(1),minmag(2) / '20000000, '00000201 /
c     data maxmag(1),maxmag(2) / '37777777, '37777577 /
c
c     Machine constants for the Cray-1.
c
c     data mcheps(1) / 0376424000000000000000b /
c     data mcheps(2) / 0000000000000000000000b /
c
c     data minmag(1) / 0200034000000000000000b /
c     data minmag(2) / 0000000000000000000000b /
c
c     data maxmag(1) / 0577777777777777777777b /
c     data maxmag(2) / 0000007777777777777776b /
c
c     Machine constants for the Prime 400.
c
c     data mcheps(1),mcheps(2) / :10000000000, :00000000123 /
c     data minmag(1),minmag(2) / :10000000000, :00000100000 /
c     data maxmag(1),maxmag(2) / :17777777777, :37777677776 /
c
c     Machine constants for the VAX-11.
c
c     data mcheps(1),mcheps(2) /   9472,  0 /
c     data minmag(1),minmag(2) /    128,  0 /
c     data maxmag(1),maxmag(2) / -32769, -1 /
c
c     Machine constants for IEEE machines.
c
      data dmach(1) /2.22044604926d-16/
      data dmach(2) /2.22507385852d-308/
      data dmach(3) /1.79769313485d+308/
c
      dpmpar = dmach(i)
      return
c
c     Last card of function dpmpar.
c
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa)
      integer m,n,info,lwa
      integer iwa(n)
      double precision tol
      double precision x(n),fvec(m),wa(lwa)
      external fcn
c     **********
c
c     subroutine lmdif1
c
c     the purpose of lmdif1 is to minimize the sum of the squares of
c     m nonlinear functions in n variables by a modification of the
c     levenberg-marquardt algorithm. this is done by using the more
c     general least-squares solver lmdif. the user must provide a
c     subroutine which calculates the functions. the jacobian is
c     then calculated by a forward-difference approximation.
c
c     the subroutine statement is
c
c       subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
c
c         subroutine fcn(m,n,x,fvec,iflag)
c         integer m,n,iflag
c         double precision x(n),fvec(m)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ----------
c         return
c         end
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of lmdif1.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.
c
c       fvec is an output array of length m which contains
c         the functions evaluated at the output x.
c
c       tol is a nonnegative input variable. termination occurs
c         when the algorithm estimates either that the relative
c         error in the sum of squares is at most tol or that
c         the relative error between x and the solution is at
c         most tol.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0  improper input parameters.
c
c         info = 1  algorithm estimates that the relative error
c                   in the sum of squares is at most tol.
c
c         info = 2  algorithm estimates that the relative error
c                   between x and the solution is at most tol.
c
c         info = 3  conditions for info = 1 and info = 2 both hold.
c
c         info = 4  fvec is orthogonal to the columns of the
c                   jacobian to machine precision.
c
c         info = 5  number of calls to fcn has reached or
c                   exceeded 200*(n+1).
c
c         info = 6  tol is too small. no further reduction in
c                   the sum of squares is possible.
c
c         info = 7  tol is too small. no further improvement in
c                   the approximate solution x is possible.
c
c       iwa is an integer work array of length n.
c
c       wa is a work array of length lwa.
c
c       lwa is a positive integer input variable not less than
c         m*n+5*n+m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... lmdif
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer maxfev,mode,mp5n,nfev,nprint
      double precision epsfcn,factor,ftol,gtol,xtol,zero
      data factor,zero /1.0d2,0.0d0/
      info = 0
c
c     check the input parameters for errors.
c
      if (n .le. 0 .or. m .lt. n .or. tol .lt. zero
     *    .or. lwa .lt. m*n + 5*n + m) go to 10
c
c     call lmdif.
c
      maxfev = 200*(n + 1)
      ftol = tol
      xtol = tol
      gtol = zero
      epsfcn = zero
      mode = 1
      nprint = 0
      mp5n = m + 5*n
      call lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,wa(1),
     *           mode,factor,nprint,info,nfev,wa(mp5n+1),m,iwa,
     *           wa(n+1),wa(2*n+1),wa(3*n+1),wa(4*n+1),wa(5*n+1))
      if (info .eq. 8) info = 4
   10 continue
      return
c
c     last card of subroutine lmdif1.
c
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
     *                 diag,mode,factor,nprint,info,nfev,fjac,ldfjac,
     *                 ipvt,qtf,wa1,wa2,wa3,wa4)
      integer m,n,maxfev,mode,nprint,info,nfev,ldfjac
      integer ipvt(n)
      double precision ftol,xtol,gtol,epsfcn,factor
      double precision x(n),fvec(m),diag(n),fjac(ldfjac,n),qtf(n),
     *                 wa1(n),wa2(n),wa3(n),wa4(m)
      external fcn
c     **********
c
c     subroutine lmdif
c
c     the purpose of lmdif is to minimize the sum of the squares of
c     m nonlinear functions in n variables by a modification of
c     the levenberg-marquardt algorithm. the user must provide a
c     subroutine which calculates the functions. the jacobian is
c     then calculated by a forward-difference approximation.
c
c     the subroutine statement is
c
c       subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
c                        diag,mode,factor,nprint,info,nfev,fjac,
c                        ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
c
c         subroutine fcn(m,n,x,fvec,iflag)
c         integer m,n,iflag
c         double precision x(n),fvec(m)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ----------
c         return
c         end
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of lmdif.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.
c
c       fvec is an output array of length m which contains
c         the functions evaluated at the output x.
c
c       ftol is a nonnegative input variable. termination
c         occurs when both the actual and predicted relative
c         reductions in the sum of squares are at most ftol.
c         therefore, ftol measures the relative error desired
c         in the sum of squares.
c
c       xtol is a nonnegative input variable. termination
c         occurs when the relative error between two consecutive
c         iterates is at most xtol. therefore, xtol measures the
c         relative error desired in the approximate solution.
c
c       gtol is a nonnegative input variable. termination
c         occurs when the cosine of the angle between fvec and
c         any column of the jacobian is at most gtol in absolute
c         value. therefore, gtol measures the orthogonality
c         desired between the function vector and the columns
c         of the jacobian.
c
c       maxfev is a positive integer input variable. termination
c         occurs when the number of calls to fcn is at least
c         maxfev by the end of an iteration.
c
c       epsfcn is an input variable used in determining a suitable
c         step length for the forward-difference approximation. this
c         approximation assumes that the relative errors in the
c         functions are of the order of epsfcn. if epsfcn is less
c         than the machine precision, it is assumed that the relative
c         errors in the functions are of the order of the machine
c         precision.
c
c       diag is an array of length n. if mode = 1 (see
c         below), diag is internally set. if mode = 2, diag
c         must contain positive entries that serve as
c         multiplicative scale factors for the variables.
c
c       mode is an integer input variable. if mode = 1, the
c         variables will be scaled internally. if mode = 2,
c         the scaling is specified by the input diag. other
c         values of mode are equivalent to mode = 1.
c
c       factor is a positive input variable used in determining the
c         initial step bound. this bound is set to the product of
c         factor and the euclidean norm of diag*x if nonzero, or else
c         to factor itself. in most cases factor should lie in the
c         interval (.1,100.). 100. is a generally recommended value.
c
c       nprint is an integer input variable that enables controlled
c         printing of iterates if it is positive. in this case,
c         fcn is called with iflag = 0 at the beginning of the first
c         iteration and every nprint iterations thereafter and
c         immediately prior to return, with x and fvec available
c         for printing. if nprint is not positive, no special calls
c         of fcn with iflag = 0 are made.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0  improper input parameters.
c
c         info = 1  both actual and predicted relative reductions
c                   in the sum of squares are at most ftol.
c
c         info = 2  relative error between two consecutive iterates
c                   is at most xtol.
c
c         info = 3  conditions for info = 1 and info = 2 both hold.
c
c         info = 4  the cosine of the angle between fvec and any
c                   column of the jacobian is at most gtol in
c                   absolute value.
c
c         info = 5  number of calls to fcn has reached or
c                   exceeded maxfev.
c
c         info = 6  ftol is too small. no further reduction in
c                   the sum of squares is possible.
c
c         info = 7  xtol is too small. no further improvement in
c                   the approximate solution x is possible.
c
c         info = 8  gtol is too small. fvec is orthogonal to the
c                   columns of the jacobian to machine precision.
c
c       nfev is an integer output variable set to the number of
c         calls to fcn.
c
c       fjac is an output m by n array. the upper n by n submatrix
c         of fjac contains an upper triangular matrix r with
c         diagonal elements of nonincreasing magnitude such that
c
c                t     t           t
c               p *(jac *jac)*p = r *r,
c
c         where p is a permutation matrix and jac is the final
c         calculated jacobian. column j of p is column ipvt(j)
c         (see below) of the identity matrix. the lower trapezoidal
c         part of fjac contains information generated during
c         the computation of r.
c
c       ldfjac is a positive integer input variable not less than m
c         which specifies the leading dimension of the array fjac.
c
c       ipvt is an integer output array of length n. ipvt
c         defines a permutation matrix p such that jac*p = q*r,
c         where jac is the final calculated jacobian, q is
c         orthogonal (not stored), and r is upper triangular
c         with diagonal elements of nonincreasing magnitude.
c         column j of p is column ipvt(j) of the identity matrix.
c
c       qtf is an output array of length n which contains
c         the first n elements of the vector (q transpose)*fvec.
c
c       wa1, wa2, and wa3 are work arrays of length n.
c
c       wa4 is a work array of length m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... dpmpar,enorm,fdjac2,lmpar,qrfac
c
c       fortran-supplied ... dabs,dmax1,dmin1,dsqrt,mod
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,iflag,iter,j,l
      double precision actred,delta,dirder,epsmch,fnorm,fnorm1,gnorm,
     *                 one,par,pnorm,prered,p1,p5,p25,p75,p0001,ratio,
     *                 sum,temp,temp1,temp2,xnorm,zero
      double precision dpmpar,enorm
      data one,p1,p5,p25,p75,p0001,zero
     *     /1.0d0,1.0d-1,5.0d-1,2.5d-1,7.5d-1,1.0d-4,0.0d0/
c
c     epsmch is the machine precision.
c
      epsmch = dpmpar(1)
c
      info = 0
      iflag = 0
      nfev = 0
c
c     check the input parameters for errors.
c
      if (n .le. 0 .or. m .lt. n .or. ldfjac .lt. m
     *    .or. ftol .lt. zero .or. xtol .lt. zero .or. gtol .lt. zero
     *    .or. maxfev .le. 0 .or. factor .le. zero) go to 300
      if (mode .ne. 2) go to 20
      do 10 j = 1, n
         if (diag(j) .le. zero) go to 300
   10    continue
   20 continue
c
c     evaluate the function at the starting point
c     and calculate its norm.
c
      iflag = 1
      call fcn(m,n,x,fvec,iflag)
      nfev = 1
      if (iflag .lt. 0) go to 300
      fnorm = enorm(m,fvec)
c
c     initialize levenberg-marquardt parameter and iteration counter.
c
      par = zero
      iter = 1
c
c     beginning of the outer loop.
c
   30 continue
c
c        calculate the jacobian matrix.
c
         iflag = 2
         call fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa4)
         nfev = nfev + n
         if (iflag .lt. 0) go to 300
c
c        if requested, call fcn to enable printing of iterates.
c
         if (nprint .le. 0) go to 40
         iflag = 0
         if (mod(iter-1,nprint) .eq. 0) call fcn(m,n,x,fvec,iflag)
         if (iflag .lt. 0) go to 300
   40    continue
c
c        compute the qr factorization of the jacobian.
c
         call qrfac(m,n,fjac,ldfjac,.true.,ipvt,n,wa1,wa2,wa3)
c
c        on the first iteration and if mode is 1, scale according
c        to the norms of the columns of the initial jacobian.
c
         if (iter .ne. 1) go to 80
         if (mode .eq. 2) go to 60
         do 50 j = 1, n
            diag(j) = wa2(j)
            if (wa2(j) .eq. zero) diag(j) = one
   50       continue
   60    continue
c
c        on the first iteration, calculate the norm of the scaled x
c        and initialize the step bound delta.
c
         do 70 j = 1, n
            wa3(j) = diag(j)*x(j)
   70       continue
         xnorm = enorm(n,wa3)
         delta = factor*xnorm
         if (delta .eq. zero) delta = factor
   80    continue
c
c        form (q transpose)*fvec and store the first n components in
c        qtf.
c
         do 90 i = 1, m
            wa4(i) = fvec(i)
   90       continue
         do 130 j = 1, n
            if (fjac(j,j) .eq. zero) go to 120
            sum = zero
            do 100 i = j, m
               sum = sum + fjac(i,j)*wa4(i)
  100          continue
            temp = -sum/fjac(j,j)
            do 110 i = j, m
               wa4(i) = wa4(i) + fjac(i,j)*temp
  110          continue
  120       continue
            fjac(j,j) = wa1(j)
            qtf(j) = wa4(j)
  130       continue
c
c        compute the norm of the scaled gradient.
c
         gnorm = zero
         if (fnorm .eq. zero) go to 170
         do 160 j = 1, n
            l = ipvt(j)
            if (wa2(l) .eq. zero) go to 150
            sum = zero
            do 140 i = 1, j
               sum = sum + fjac(i,j)*(qtf(i)/fnorm)
  140          continue
            gnorm = dmax1(gnorm,dabs(sum/wa2(l)))
  150       continue
  160       continue
  170    continue
c
c        test for convergence of the gradient norm.
c
         if (gnorm .le. gtol) info = 4
         if (info .ne. 0) go to 300
c
c        rescale if necessary.
c
         if (mode .eq. 2) go to 190
         do 180 j = 1, n
            diag(j) = dmax1(diag(j),wa2(j))
  180       continue
  190    continue
c
c        beginning of the inner loop.
c
  200    continue
c
c           determine the levenberg-marquardt parameter.
c
            call lmpar(n,fjac,ldfjac,ipvt,diag,qtf,delta,par,wa1,wa2,
     *                 wa3,wa4)
c
c           store the direction p and x + p. calculate the norm of p.
c
            do 210 j = 1, n
               wa1(j) = -wa1(j)
               wa2(j) = x(j) + wa1(j)
               wa3(j) = diag(j)*wa1(j)
  210          continue
            pnorm = enorm(n,wa3)
c
c           on the first iteration, adjust the initial step bound.
c
            if (iter .eq. 1) delta = dmin1(delta,pnorm)
c
c           evaluate the function at x + p and calculate its norm.
c
            iflag = 1
            call fcn(m,n,wa2,wa4,iflag)
            nfev = nfev + 1
            if (iflag .lt. 0) go to 300
            fnorm1 = enorm(m,wa4)
c
c           compute the scaled actual reduction.
c
            actred = -one
            if (p1*fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2
c
c           compute the scaled predicted reduction and
c           the scaled directional derivative.
c
            do 230 j = 1, n
               wa3(j) = zero
               l = ipvt(j)
               temp = wa1(l)
               do 220 i = 1, j
                  wa3(i) = wa3(i) + fjac(i,j)*temp
  220             continue
  230          continue
            temp1 = enorm(n,wa3)/fnorm
            temp2 = (dsqrt(par)*pnorm)/fnorm
            prered = temp1**2 + temp2**2/p5
            dirder = -(temp1**2 + temp2**2)
c
c           compute the ratio of the actual to the predicted
c           reduction.
c
            ratio = zero
            if (prered .ne. zero) ratio = actred/prered
c
c           update the step bound.
c
            if (ratio .gt. p25) go to 240
               if (actred .ge. zero) temp = p5
               if (actred .lt. zero)
     *            temp = p5*dirder/(dirder + p5*actred)
               if (p1*fnorm1 .ge. fnorm .or. temp .lt. p1) temp = p1
               delta = temp*dmin1(delta,pnorm/p1)
               par = par/temp
               go to 260
  240       continue
               if (par .ne. zero .and. ratio .lt. p75) go to 250
               delta = pnorm/p5
               par = p5*par
  250          continue
  260       continue
c
c           test for successful iteration.
c
            if (ratio .lt. p0001) go to 290
c
c           successful iteration. update x, fvec, and their norms.
c
            do 270 j = 1, n
               x(j) = wa2(j)
               wa2(j) = diag(j)*x(j)
  270          continue
            do 280 i = 1, m
               fvec(i) = wa4(i)
  280          continue
            xnorm = enorm(n,wa2)
            fnorm = fnorm1
            iter = iter + 1
  290       continue
c
c           tests for convergence.
c
            if (dabs(actred) .le. ftol .and. prered .le. ftol
     *          .and. p5*ratio .le. one) info = 1
            if (delta .le. xtol*xnorm) info = 2
            if (dabs(actred) .le. ftol .and. prered .le. ftol
     *          .and. p5*ratio .le. one .and. info .eq. 2) info = 3
            if (info .ne. 0) go to 300
c
c           tests for termination and stringent tolerances.
c
            if (nfev .ge. maxfev) info = 5
            if (dabs(actred) .le. epsmch .and. prered .le. epsmch
     *          .and. p5*ratio .le. one) info = 6
            if (delta .le. epsmch*xnorm) info = 7
            if (gnorm .le. epsmch) info = 8
            if (info .ne. 0) go to 300
c
c           end of the inner loop. repeat if iteration unsuccessful.
c
            if (ratio .lt. p0001) go to 200
c
c        end of the outer loop.
c
         go to 30
  300 continue
c
c     termination, either normal or user imposed.
c
      if (iflag .lt. 0) info = iflag
      iflag = 0
      if (nprint .gt. 0) call fcn(m,n,x,fvec,iflag)
      return
c
c     last card of subroutine lmdif.
c
      end
cccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision function enorm(n,x)
      integer n
      double precision x(n)
c     **********
c
c     function enorm
c
c     given an n-vector x, this function calculates the
c     euclidean norm of x.
c
c     the euclidean norm is computed by accumulating the sum of
c     squares in three different sums. the sums of squares for the
c     small and large components are scaled so that no overflows
c     occur. non-destructive underflows are permitted. underflows
c     and overflows do not occur in the computation of the unscaled
c     sum of squares for the intermediate components.
c     the definitions of small, intermediate and large components
c     depend on two constants, rdwarf and rgiant. the main
c     restrictions on these constants are that rdwarf**2 not
c     underflow and rgiant**2 not overflow. the constants
c     given here are suitable for every known computer.
c
c     the function statement is
c
c       double precision function enorm(n,x)
c
c     where
c
c       n is a positive integer input variable.
c
c       x is an input array of length n.
c
c     subprograms called
c
c       fortran-supplied ... dabs,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i
      double precision agiant,floatn,one,rdwarf,rgiant,s1,s2,s3,xabs,
     *                 x1max,x3max,zero
      data one,zero,rdwarf,rgiant /1.0d0,0.0d0,3.834d-20,1.304d19/
      s1 = zero
      s2 = zero
      s3 = zero
      x1max = zero
      x3max = zero
      floatn = n
      agiant = rgiant/floatn
      do 90 i = 1, n
         xabs = dabs(x(i))
         if (xabs .gt. rdwarf .and. xabs .lt. agiant) go to 70
            if (xabs .le. rdwarf) go to 30
c
c              sum for large components.
c
               if (xabs .le. x1max) go to 10
                  s1 = one + s1*(x1max/xabs)**2
                  x1max = xabs
                  go to 20
   10          continue
                  s1 = s1 + (xabs/x1max)**2
   20          continue
               go to 60
   30       continue
c
c              sum for small components.
c
               if (xabs .le. x3max) go to 40
                  s3 = one + s3*(x3max/xabs)**2
                  x3max = xabs
                  go to 50
   40          continue
                  if (xabs .ne. zero) s3 = s3 + (xabs/x3max)**2
   50          continue
   60       continue
            go to 80
   70    continue
c
c           sum for intermediate components.
c
            s2 = s2 + xabs**2
   80    continue
   90    continue
c
c     calculation of norm.
c
      if (s1 .eq. zero) go to 100
         enorm = x1max*dsqrt(s1+(s2/x1max)/x1max)
         go to 130
  100 continue
         if (s2 .eq. zero) go to 110
            if (s2 .ge. x3max)
     *         enorm = dsqrt(s2*(one+(x3max/s2)*(x3max*s3)))
            if (s2 .lt. x3max)
     *         enorm = dsqrt(x3max*((s2/x3max)+(x3max*s3)))
            go to 120
  110    continue
            enorm = x3max*dsqrt(s3)
  120    continue
  130 continue
      return
c
c     last card of function enorm.
c
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,wa1,
     *                 wa2)
      integer n,ldr
      integer ipvt(n)
      double precision delta,par
      double precision r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa1(n),
     *                 wa2(n)
c     **********
c
c     subroutine lmpar
c
c     given an m by n matrix a, an n by n nonsingular diagonal
c     matrix d, an m-vector b, and a positive number delta,
c     the problem is to determine a value for the parameter
c     par such that if x solves the system
c
c           a*x = b ,     sqrt(par)*d*x = 0 ,
c
c     in the least squares sense, and dxnorm is the euclidean
c     norm of d*x, then either par is zero and
c
c           (dxnorm-delta) .le. 0.1*delta ,
c
c     or par is positive and
c
c           abs(dxnorm-delta) .le. 0.1*delta .
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then lmpar expects
c     the full upper triangle of r, the permutation matrix p,
c     and the first n components of (q transpose)*b. on output
c     lmpar also provides an upper triangular matrix s such that
c
c            t   t                   t
c           p *(a *a + par*d*d)*p = s *s .
c
c     s is employed within lmpar and may be of separate interest.
c
c     only a few iterations are generally needed for convergence
c     of the algorithm. if, however, the limit of 10 iterations
c     is reached, then the output par will contain the best
c     value obtained so far.
c
c     the subroutine statement is
c
c       subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,
c                        wa1,wa2)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle
c         must contain the full upper triangle of the matrix r.
c         on output the full upper triangle is unaltered, and the
c         strict lower triangle contains the strict upper triangle
c         (transposed) of the upper triangular matrix s.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       delta is a positive input variable which specifies an upper
c         bound on the euclidean norm of d*x.
c
c       par is a nonnegative variable. on input par contains an
c         initial estimate of the levenberg-marquardt parameter.
c         on output par contains the final estimate.
c
c       x is an output array of length n which contains the least
c         squares solution of the system a*x = b, sqrt(par)*d*x = 0,
c         for the output par.
c
c       sdiag is an output array of length n which contains the
c         diagonal elements of the upper triangular matrix s.
c
c       wa1 and wa2 are work arrays of length n.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar,enorm,qrsolv
c
c       fortran-supplied ... dabs,dmax1,dmin1,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,iter,j,jm1,jp1,k,l,nsing
      double precision dxnorm,dwarf,fp,gnorm,parc,parl,paru,p1,p001,
     *                 sum,temp,zero
      double precision dpmpar,enorm
      data p1,p001,zero /1.0d-1,1.0d-3,0.0d0/
c
c     dwarf is the smallest positive magnitude.
c
      dwarf = dpmpar(2)
c
c     compute and store in x the gauss-newton direction. if the
c     jacobian is rank-deficient, obtain a least squares solution.
c
      nsing = n
      do 10 j = 1, n
         wa1(j) = qtb(j)
         if (r(j,j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa1(j) = zero
   10    continue
      if (nsing .lt. 1) go to 50
      do 40 k = 1, nsing
         j = nsing - k + 1
         wa1(j) = wa1(j)/r(j,j)
         temp = wa1(j)
         jm1 = j - 1
         if (jm1 .lt. 1) go to 30
         do 20 i = 1, jm1
            wa1(i) = wa1(i) - r(i,j)*temp
   20       continue
   30    continue
   40    continue
   50 continue
      do 60 j = 1, n
         l = ipvt(j)
         x(l) = wa1(j)
   60    continue
c
c     initialize the iteration counter.
c     evaluate the function at the origin, and test
c     for acceptance of the gauss-newton direction.
c
      iter = 0
      do 70 j = 1, n
         wa2(j) = diag(j)*x(j)
   70    continue
      dxnorm = enorm(n,wa2)
      fp = dxnorm - delta
      if (fp .le. p1*delta) go to 220
c
c     if the jacobian is not rank deficient, the newton
c     step provides a lower bound, parl, for the zero of
c     the function. otherwise set this bound to zero.
c
      parl = zero
      if (nsing .lt. n) go to 120
      do 80 j = 1, n
         l = ipvt(j)
         wa1(j) = diag(l)*(wa2(l)/dxnorm)
   80    continue
      do 110 j = 1, n
         sum = zero
         jm1 = j - 1
         if (jm1 .lt. 1) go to 100
         do 90 i = 1, jm1
            sum = sum + r(i,j)*wa1(i)
   90       continue
  100    continue
         wa1(j) = (wa1(j) - sum)/r(j,j)
  110    continue
      temp = enorm(n,wa1)
      parl = ((fp/delta)/temp)/temp
  120 continue
c
c     calculate an upper bound, paru, for the zero of the function.
c
      do 140 j = 1, n
         sum = zero
         do 130 i = 1, j
            sum = sum + r(i,j)*qtb(i)
  130       continue
         l = ipvt(j)
         wa1(j) = sum/diag(l)
  140    continue
      gnorm = enorm(n,wa1)
      paru = gnorm/delta
      if (paru .eq. zero) paru = dwarf/dmin1(delta,p1)
c
c     if the input par lies outside of the interval (parl,paru),
c     set par to the closer endpoint.
c
      par = dmax1(par,parl)
      par = dmin1(par,paru)
      if (par .eq. zero) par = gnorm/dxnorm
c
c     beginning of an iteration.
c
  150 continue
         iter = iter + 1
c
c        evaluate the function at the current value of par.
c
         if (par .eq. zero) par = dmax1(dwarf,p001*paru)
         temp = dsqrt(par)
         do 160 j = 1, n
            wa1(j) = temp*diag(j)
  160       continue
         call qrsolv(n,r,ldr,ipvt,wa1,qtb,x,sdiag,wa2)
         do 170 j = 1, n
            wa2(j) = diag(j)*x(j)
  170       continue
         dxnorm = enorm(n,wa2)
         temp = fp
         fp = dxnorm - delta
c
c        if the function is small enough, accept the current value
c        of par. also test for the exceptional cases where parl
c        is zero or the number of iterations has reached 10.
c
         if (dabs(fp) .le. p1*delta
     *       .or. parl .eq. zero .and. fp .le. temp
     *            .and. temp .lt. zero .or. iter .eq. 10) go to 220
c
c        compute the newton correction.
c
         do 180 j = 1, n
            l = ipvt(j)
            wa1(j) = diag(l)*(wa2(l)/dxnorm)
  180       continue
         do 210 j = 1, n
            wa1(j) = wa1(j)/sdiag(j)
            temp = wa1(j)
            jp1 = j + 1
            if (n .lt. jp1) go to 200
            do 190 i = jp1, n
               wa1(i) = wa1(i) - r(i,j)*temp
  190          continue
  200       continue
  210       continue
         temp = enorm(n,wa1)
         parc = ((fp/delta)/temp)/temp
c
c        depending on the sign of the function, update parl or paru.
c
         if (fp .gt. zero) parl = dmax1(parl,par)
         if (fp .lt. zero) paru = dmin1(paru,par)
c
c        compute an improved estimate for par.
c
         par = dmax1(parl,par+parc)
c
c        end of an iteration.
c
         go to 150
  220 continue
c
c     termination.
c
      if (iter .eq. 0) par = zero
      return
c
c     last card of subroutine lmpar.
c
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
      integer m,n,ldfjac,iflag
      double precision epsfcn
      double precision x(n),fvec(m),fjac(ldfjac,n),wa(m)
c     **********
c
c     subroutine fdjac2
c
c     this subroutine computes a forward-difference approximation
c     to the m by n jacobian matrix associated with a specified
c     problem of m functions in n variables.
c
c     the subroutine statement is
c
c       subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
c
c         subroutine fcn(m,n,x,fvec,iflag)
c         integer m,n,iflag
c         double precision x(n),fvec(m)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ----------
c         return
c         end
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of fdjac2.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an input array of length n.
c
c       fvec is an input array of length m which must contain the
c         functions evaluated at x.
c
c       fjac is an output m by n array which contains the
c         approximation to the jacobian matrix evaluated at x.
c
c       ldfjac is a positive integer input variable not less than m
c         which specifies the leading dimension of the array fjac.
c
c       iflag is an integer variable which can be used to terminate
c         the execution of fdjac2. see description of fcn.
c
c       epsfcn is an input variable used in determining a suitable
c         step length for the forward-difference approximation. this
c         approximation assumes that the relative errors in the
c         functions are of the order of epsfcn. if epsfcn is less
c         than the machine precision, it is assumed that the relative
c         errors in the functions are of the order of the machine
c         precision.
c
c       wa is a work array of length m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... dpmpar
c
c       fortran-supplied ... dabs,dmax1,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j
      double precision eps,epsmch,h,temp,zero
      double precision dpmpar
      data zero /0.0d0/
c
c     epsmch is the machine precision.
c
      epsmch = dpmpar(1)
c
      eps = dsqrt(dmax1(epsfcn,epsmch))
      do 20 j = 1, n
         temp = x(j)
         h = eps*dabs(temp)
         if (h .eq. zero) h = eps
         x(j) = temp + h
         call fcn(m,n,x,wa,iflag)
         if (iflag .lt. 0) go to 30
         x(j) = temp
         do 10 i = 1, m
            fjac(i,j) = (wa(i) - fvec(i))/h
   10       continue
   20    continue
   30 continue
      return
c
c     last card of subroutine fdjac2.
c
      end 
ccccccccccccccccccccccccccccccccccccccc
      subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
      integer m,n,lda,lipvt
      integer ipvt(lipvt)
      logical pivot
      double precision a(lda,n),rdiag(n),acnorm(n),wa(n)
c     **********
c
c     subroutine qrfac
c
c     this subroutine uses householder transformations with column
c     pivoting (optional) to compute a qr factorization of the
c     m by n matrix a. that is, qrfac determines an orthogonal
c     matrix q, a permutation matrix p, and an upper trapezoidal
c     matrix r with diagonal elements of nonincreasing magnitude,
c     such that a*p = q*r. the householder transformation for
c     column k, k = 1,2,...,min(m,n), is of the form
c
c                           t
c           i - (1/u(k))*u*u
c
c     where u has zeros in the first k-1 positions. the form of
c     this transformation and the method of pivoting first
c     appeared in the corresponding linpack subroutine.
c
c     the subroutine statement is
c
c       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
c
c     where
c
c       m is a positive integer input variable set to the number
c         of rows of a.
c
c       n is a positive integer input variable set to the number
c         of columns of a.
c
c       a is an m by n array. on input a contains the matrix for
c         which the qr factorization is to be computed. on output
c         the strict upper trapezoidal part of a contains the strict
c         upper trapezoidal part of r, and the lower trapezoidal
c         part of a contains a factored form of q (the non-trivial
c         elements of the u vectors described above).
c
c       lda is a positive integer input variable not less than m
c         which specifies the leading dimension of the array a.
c
c       pivot is a logical input variable. if pivot is set true,
c         then column pivoting is enforced. if pivot is set false,
c         then no column pivoting is done.
c
c       ipvt is an integer output array of length lipvt. ipvt
c         defines the permutation matrix p such that a*p = q*r.
c         column j of p is column ipvt(j) of the identity matrix.
c         if pivot is false, ipvt is not referenced.
c
c       lipvt is a positive integer input variable. if pivot is false,
c         then lipvt may be as small as 1. if pivot is true, then
c         lipvt must be at least n.
c
c       rdiag is an output array of length n which contains the
c         diagonal elements of r.
c
c       acnorm is an output array of length n which contains the
c         norms of the corresponding columns of the input matrix a.
c         if this information is not needed, then acnorm can coincide
c         with rdiag.
c
c       wa is a work array of length n. if pivot is false, then wa
c         can coincide with rdiag.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar,enorm
c
c       fortran-supplied ... dmax1,dsqrt,min0
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j,jp1,k,kmax,minmn
      double precision ajnorm,epsmch,one,p05,sum,temp,zero
      double precision dpmpar,enorm
      data one,p05,zero /1.0d0,5.0d-2,0.0d0/
c
c     epsmch is the machine precision.
c
      epsmch = dpmpar(1)
c
c     compute the initial column norms and initialize several arrays.
c
      do 10 j = 1, n
         acnorm(j) = enorm(m,a(1,j))
         rdiag(j) = acnorm(j)
         wa(j) = rdiag(j)
         if (pivot) ipvt(j) = j
   10    continue
c
c     reduce a to r with householder transformations.
c
      minmn = min0(m,n)
      do 110 j = 1, minmn
         if (.not.pivot) go to 40
c
c        bring the column of largest norm into the pivot position.
c
         kmax = j
         do 20 k = j, n
            if (rdiag(k) .gt. rdiag(kmax)) kmax = k
   20       continue
         if (kmax .eq. j) go to 40
         do 30 i = 1, m
            temp = a(i,j)
            a(i,j) = a(i,kmax)
            a(i,kmax) = temp
   30       continue
         rdiag(kmax) = rdiag(j)
         wa(kmax) = wa(j)
         k = ipvt(j)
         ipvt(j) = ipvt(kmax)
         ipvt(kmax) = k
   40    continue
c
c        compute the householder transformation to reduce the
c        j-th column of a to a multiple of the j-th unit vector.
c
         ajnorm = enorm(m-j+1,a(j,j))
         if (ajnorm .eq. zero) go to 100
         if (a(j,j) .lt. zero) ajnorm = -ajnorm
         do 50 i = j, m
            a(i,j) = a(i,j)/ajnorm
   50       continue
         a(j,j) = a(j,j) + one
c
c        apply the transformation to the remaining columns
c        and update the norms.
c
         jp1 = j + 1
         if (n .lt. jp1) go to 100
         do 90 k = jp1, n
            sum = zero
            do 60 i = j, m
               sum = sum + a(i,j)*a(i,k)
   60          continue
            temp = sum/a(j,j)
            do 70 i = j, m
               a(i,k) = a(i,k) - temp*a(i,j)
   70          continue
            if (.not.pivot .or. rdiag(k) .eq. zero) go to 80
            temp = a(j,k)/rdiag(k)
            rdiag(k) = rdiag(k)*dsqrt(dmax1(zero,one-temp**2))
            if (p05*(rdiag(k)/wa(k))**2 .gt. epsmch) go to 80
            rdiag(k) = enorm(m-j,a(jp1,k))
            wa(k) = rdiag(k)
   80       continue
   90       continue
  100    continue
         rdiag(j) = -ajnorm
  110    continue
      return
c
c     last card of subroutine qrfac.
c
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
      integer n,ldr
      integer ipvt(n)
      double precision r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa(n)
c     **********
c
c     subroutine qrsolv
c
c     given an m by n matrix a, an n by n diagonal matrix d,
c     and an m-vector b, the problem is to determine an x which
c     solves the system
c
c           a*x = b ,     d*x = 0 ,
c
c     in the least squares sense.
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then qrsolv expects
c     the full upper triangle of r, the permutation matrix p,
c     and the first n components of (q transpose)*b. the system
c     a*x = b, d*x = 0, is then equivalent to
c
c                  t       t
c           r*z = q *b ,  p *d*p*z = 0 ,
c
c     where x = p*z. if this system does not have full rank,
c     then a least squares solution is obtained. on output qrsolv
c     also provides an upper triangular matrix s such that
c
c            t   t               t
c           p *(a *a + d*d)*p = s *s .
c
c     s is computed within qrsolv and may be of separate interest.
c
c     the subroutine statement is
c
c       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle
c         must contain the full upper triangle of the matrix r.
c         on output the full upper triangle is unaltered, and the
c         strict lower triangle contains the strict upper triangle
c         (transposed) of the upper triangular matrix s.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       x is an output array of length n which contains the least
c         squares solution of the system a*x = b, d*x = 0.
c
c       sdiag is an output array of length n which contains the
c         diagonal elements of the upper triangular matrix s.
c
c       wa is a work array of length n.
c
c     subprograms called
c
c       fortran-supplied ... dabs,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j,jp1,k,kp1,l,nsing
      double precision cos,cotan,p5,p25,qtbpj,sin,sum,tan,temp,zero
      data p5,p25,zero /5.0d-1,2.5d-1,0.0d0/
c
c     copy r and (q transpose)*b to preserve input and initialize s.
c     in particular, save the diagonal elements of r in x.
c
      do 20 j = 1, n
         do 10 i = j, n
            r(i,j) = r(j,i)
   10       continue
         x(j) = r(j,j)
         wa(j) = qtb(j)
   20    continue
c
c     eliminate the diagonal matrix d using a givens rotation.
c
      do 100 j = 1, n
c
c        prepare the row of d to be eliminated, locating the
c        diagonal element using p from the qr factorization.
c
         l = ipvt(j)
         if (diag(l) .eq. zero) go to 90
         do 30 k = j, n
            sdiag(k) = zero
   30       continue
         sdiag(j) = diag(l)
c
c        the transformations to eliminate the row of d
c        modify only a single element of (q transpose)*b
c        beyond the first n, which is initially zero.
c
         qtbpj = zero
         do 80 k = j, n
c
c           determine a givens rotation which eliminates the
c           appropriate element in the current row of d.
c
            if (sdiag(k) .eq. zero) go to 70
            if (dabs(r(k,k)) .ge. dabs(sdiag(k))) go to 40
               cotan = r(k,k)/sdiag(k)
               sin = p5/dsqrt(p25+p25*cotan**2)
               cos = sin*cotan
               go to 50
   40       continue
               tan = sdiag(k)/r(k,k)
               cos = p5/dsqrt(p25+p25*tan**2)
               sin = cos*tan
   50       continue
c
c           compute the modified diagonal element of r and
c           the modified element of ((q transpose)*b,0).
c
            r(k,k) = cos*r(k,k) + sin*sdiag(k)
            temp = cos*wa(k) + sin*qtbpj
            qtbpj = -sin*wa(k) + cos*qtbpj
            wa(k) = temp
c
c           accumulate the tranformation in the row of s.
c
            kp1 = k + 1
            if (n .lt. kp1) go to 70
            do 60 i = kp1, n
               temp = cos*r(i,k) + sin*sdiag(i)
               sdiag(i) = -sin*r(i,k) + cos*sdiag(i)
               r(i,k) = temp
   60          continue
   70       continue
   80       continue
   90    continue
c
c        store the diagonal element of s and restore
c        the corresponding diagonal element of r.
c
         sdiag(j) = r(j,j)
         r(j,j) = x(j)
  100    continue
c
c     solve the triangular system for z. if the system is
c     singular, then obtain a least squares solution.
c
      nsing = n
      do 110 j = 1, n
         if (sdiag(j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa(j) = zero
  110    continue
      if (nsing .lt. 1) go to 150
      do 140 k = 1, nsing
         j = nsing - k + 1
         sum = zero
         jp1 = j + 1
         if (nsing .lt. jp1) go to 130
         do 120 i = jp1, nsing
            sum = sum + r(i,j)*wa(i)
  120       continue
  130    continue
         wa(j) = (wa(j) - sum)/sdiag(j)
  140    continue
  150 continue
c
c     permute the components of z back to components of x.
c
      do 160 j = 1, n
         l = ipvt(j)
         x(l) = wa(j)
  160    continue
      return
c
c     last card of subroutine qrsolv.
c
      end 

