
    (define (problem rcll-production-durative-prob1)
	   (:domain rcll-production-durative)

	(:objects
		R-1 R-2 R-3 - robot
		; If adding R-2 and R-3, also add robot-waiting facts below
		o1 - order
		wp1 - workpiece
		cg1 cg2 cg3 cb1 cb2 cb3 - cap-carrier
		C-BS C-CS1 C-CS2 C-DS C-RS1 C-RS2 - mps
		CYAN - team-color
	)

	(:init
   (mps-type C-BS BS)
(mps-type C-CS1 CS)
(mps-type C-CS2 CS)
(mps-type C-DS DS)
(mps-type C-RS1 RS)
(mps-type C-RS2 RS)
	 (location-free START INPUT)
   (location-free C-BS INPUT)
(location-free C-BS OUTPUT)
(location-free C-CS1 INPUT)
(location-free C-CS1 OUTPUT)
(location-free C-CS2 INPUT)
(location-free C-CS2 OUTPUT)
(location-free C-DS INPUT)
(location-free C-DS OUTPUT)
(location-free C-RS1 INPUT)
(location-free C-RS1 OUTPUT)
(location-free C-RS2 INPUT)
(location-free C-RS2 OUTPUT)

	 (cs-can-perform C-CS1 CS_RETRIEVE)
	 (cs-can-perform C-CS2 CS_RETRIEVE)
	 (cs-free C-CS1)
	 (cs-free C-CS2)
	 ; Additional base number handling static predicates
  
	 (wp-base-color wp1 BASE_NONE)
	 (wp-cap-color wp1 CAP_NONE)
	 (wp-ring1-color wp1 RING_NONE)
	 (wp-ring2-color wp1 RING_NONE)
	 (wp-ring3-color wp1 RING_NONE)
	 (wp-unused wp1)
   (robot-waiting R-1)
(robot-waiting R-2)
(robot-waiting R-3)
	 ;(robot-waiting R-1)
	 ;(robot-waiting R-2)
	 ;(robot-waiting R-3)

	 (mps-state C-BS IDLE)
(mps-state C-CS1 IDLE)
(mps-state C-CS2 IDLE)
(mps-state C-DS IDLE)
(mps-state C-RS1 IDLE)
(mps-state C-RS2 IDLE)
   ;(mps-state C-BS IDLE)
 	 ;(mps-state C-CS1 IDLE)
 	 ;(mps-state C-CS2 IDLE)
	 ;(mps-state C-DS IDLE)
 	 ;(mps-state C-RS1 IDLE)
 	 ;(mps-state C-RS2 IDLE)

	 (wp-cap-color cg1 CAP_GREY)
	 (wp-cap-color cg2 CAP_GREY)
	 (wp-cap-color cg3 CAP_GREY)
	 (wp-on-shelf cg1 C-CS1 LEFT)
	 (wp-on-shelf cg2 C-CS1 MIDDLE)
	 (wp-on-shelf cg3 C-CS1 RIGHT)

	 (wp-cap-color cb1 CAP_BLACK)
	 (wp-cap-color cb2 CAP_BLACK)
	 (wp-cap-color cb3 CAP_BLACK)
	 (wp-on-shelf cb1 C-CS2 LEFT)
	 (wp-on-shelf cb2 C-CS2 MIDDLE)
	 (wp-on-shelf cb3 C-CS2 RIGHT)
   

   
         (order-complexity o1 c0)
         (order-base-color o1 BASE_RED)
         (order-cap-color o1 CAP_GREY)
         (order-gate o1 GATE-2)
      

	 ; C0 order
	 ;(order-complexity o1 c0)
	 ;(order-base-color o1 BASE_BLACK)
	 ;(order-cap-color o1 CAP_GREY)
	 ;(order-gate o1 GATE-1)

	 ; These values are for the static default world
   (= (path-length C-BS INPUT C-BS OUTPUT) 2.820)
(= (path-length C-BS INPUT C-CS1 INPUT) 7.027)
(= (path-length C-BS INPUT C-CS1 OUTPUT) 6.457)
(= (path-length C-BS INPUT C-CS2 INPUT) 3.612)
(= (path-length C-BS INPUT C-CS2 OUTPUT) 1.801)
(= (path-length C-BS INPUT C-DS INPUT) 7.493)
(= (path-length C-BS INPUT C-DS OUTPUT) 5.948)
(= (path-length C-BS INPUT C-RS1 INPUT) 6.643)
(= (path-length C-BS INPUT C-RS1 OUTPUT) 4.172)
(= (path-length C-BS INPUT C-RS2 INPUT) 12.073)
(= (path-length C-BS INPUT C-RS2 OUTPUT) 11.024)
(= (path-length C-BS OUTPUT C-BS INPUT) 2.820)
(= (path-length C-BS OUTPUT C-CS1 INPUT) 6.637)
(= (path-length C-BS OUTPUT C-CS1 OUTPUT) 7.402)
(= (path-length C-BS OUTPUT C-CS2 INPUT) 3.222)
(= (path-length C-BS OUTPUT C-CS2 OUTPUT) 2.746)
(= (path-length C-BS OUTPUT C-DS INPUT) 8.438)
(= (path-length C-BS OUTPUT C-DS OUTPUT) 6.893)
(= (path-length C-BS OUTPUT C-RS1 INPUT) 7.588)
(= (path-length C-BS OUTPUT C-RS1 OUTPUT) 5.118)
(= (path-length C-BS OUTPUT C-RS2 INPUT) 12.957)
(= (path-length C-BS OUTPUT C-RS2 OUTPUT) 11.175)
(= (path-length C-CS1 INPUT C-BS INPUT) 7.027)
(= (path-length C-CS1 INPUT C-BS OUTPUT) 6.637)
(= (path-length C-CS1 INPUT C-CS1 OUTPUT) 3.582)
(= (path-length C-CS1 INPUT C-CS2 INPUT) 3.690)
(= (path-length C-CS1 INPUT C-CS2 OUTPUT) 6.544)
(= (path-length C-CS1 INPUT C-DS INPUT) 5.149)
(= (path-length C-CS1 INPUT C-DS OUTPUT) 7.971)
(= (path-length C-CS1 INPUT C-RS1 INPUT) 5.190)
(= (path-length C-CS1 INPUT C-RS1 OUTPUT) 4.360)
(= (path-length C-CS1 INPUT C-RS2 INPUT) 6.934)
(= (path-length C-CS1 INPUT C-RS2 OUTPUT) 5.152)
(= (path-length C-CS1 OUTPUT C-BS INPUT) 6.457)
(= (path-length C-CS1 OUTPUT C-BS OUTPUT) 7.402)
(= (path-length C-CS1 OUTPUT C-CS1 INPUT) 3.582)
(= (path-length C-CS1 OUTPUT C-CS2 INPUT) 4.998)
(= (path-length C-CS1 OUTPUT C-CS2 OUTPUT) 5.545)
(= (path-length C-CS1 OUTPUT C-DS INPUT) 3.214)
(= (path-length C-CS1 OUTPUT C-DS OUTPUT) 6.035)
(= (path-length C-CS1 OUTPUT C-RS1 INPUT) 3.255)
(= (path-length C-CS1 OUTPUT C-RS1 OUTPUT) 2.425)
(= (path-length C-CS1 OUTPUT C-RS2 INPUT) 5.617)
(= (path-length C-CS1 OUTPUT C-RS2 OUTPUT) 4.568)
(= (path-length C-CS2 INPUT C-BS INPUT) 3.612)
(= (path-length C-CS2 INPUT C-BS OUTPUT) 3.222)
(= (path-length C-CS2 INPUT C-CS1 INPUT) 3.690)
(= (path-length C-CS2 INPUT C-CS1 OUTPUT) 4.998)
(= (path-length C-CS2 INPUT C-CS2 OUTPUT) 3.129)
(= (path-length C-CS2 INPUT C-DS INPUT) 6.211)
(= (path-length C-CS2 INPUT C-DS OUTPUT) 6.600)
(= (path-length C-CS2 INPUT C-RS1 INPUT) 6.252)
(= (path-length C-CS2 INPUT C-RS1 OUTPUT) 4.626)
(= (path-length C-CS2 INPUT C-RS2 INPUT) 10.010)
(= (path-length C-CS2 INPUT C-RS2 OUTPUT) 8.228)
(= (path-length C-CS2 OUTPUT C-BS INPUT) 1.801)
(= (path-length C-CS2 OUTPUT C-BS OUTPUT) 2.746)
(= (path-length C-CS2 OUTPUT C-CS1 INPUT) 6.544)
(= (path-length C-CS2 OUTPUT C-CS1 OUTPUT) 5.545)
(= (path-length C-CS2 OUTPUT C-CS2 INPUT) 3.129)
(= (path-length C-CS2 OUTPUT C-DS INPUT) 6.581)
(= (path-length C-CS2 OUTPUT C-DS OUTPUT) 5.036)
(= (path-length C-CS2 OUTPUT C-RS1 INPUT) 5.731)
(= (path-length C-CS2 OUTPUT C-RS1 OUTPUT) 3.261)
(= (path-length C-CS2 OUTPUT C-RS2 INPUT) 11.161)
(= (path-length C-CS2 OUTPUT C-RS2 OUTPUT) 10.112)
(= (path-length C-DS INPUT C-BS INPUT) 7.493)
(= (path-length C-DS INPUT C-BS OUTPUT) 8.438)
(= (path-length C-DS INPUT C-CS1 INPUT) 5.149)
(= (path-length C-DS INPUT C-CS1 OUTPUT) 3.214)
(= (path-length C-DS INPUT C-CS2 INPUT) 6.211)
(= (path-length C-DS INPUT C-CS2 OUTPUT) 6.581)
(= (path-length C-DS INPUT C-DS OUTPUT) 3.237)
(= (path-length C-DS INPUT C-RS1 INPUT) 1.727)
(= (path-length C-DS INPUT C-RS1 OUTPUT) 3.460)
(= (path-length C-DS INPUT C-RS2 INPUT) 6.847)
(= (path-length C-DS INPUT C-RS2 OUTPUT) 7.781)
(= (path-length C-DS OUTPUT C-BS INPUT) 5.948)
(= (path-length C-DS OUTPUT C-BS OUTPUT) 6.893)
(= (path-length C-DS OUTPUT C-CS1 INPUT) 7.971)
(= (path-length C-DS OUTPUT C-CS1 OUTPUT) 6.035)
(= (path-length C-DS OUTPUT C-CS2 INPUT) 6.600)
(= (path-length C-DS OUTPUT C-CS2 OUTPUT) 5.036)
(= (path-length C-DS OUTPUT C-DS INPUT) 3.237)
(= (path-length C-DS OUTPUT C-RS1 INPUT) 4.145)
(= (path-length C-DS OUTPUT C-RS1 OUTPUT) 4.868)
(= (path-length C-DS OUTPUT C-RS2 INPUT) 9.191)
(= (path-length C-DS OUTPUT C-RS2 OUTPUT) 10.492)
(= (path-length C-RS1 INPUT C-BS INPUT) 6.643)
(= (path-length C-RS1 INPUT C-BS OUTPUT) 7.588)
(= (path-length C-RS1 INPUT C-CS1 INPUT) 5.190)
(= (path-length C-RS1 INPUT C-CS1 OUTPUT) 3.255)
(= (path-length C-RS1 INPUT C-CS2 INPUT) 6.252)
(= (path-length C-RS1 INPUT C-CS2 OUTPUT) 5.731)
(= (path-length C-RS1 INPUT C-DS INPUT) 1.727)
(= (path-length C-RS1 INPUT C-DS OUTPUT) 4.145)
(= (path-length C-RS1 INPUT C-RS1 OUTPUT) 3.501)
(= (path-length C-RS1 INPUT C-RS2 INPUT) 6.888)
(= (path-length C-RS1 INPUT C-RS2 OUTPUT) 7.822)
(= (path-length C-RS1 OUTPUT C-BS INPUT) 4.172)
(= (path-length C-RS1 OUTPUT C-BS OUTPUT) 5.118)
(= (path-length C-RS1 OUTPUT C-CS1 INPUT) 4.360)
(= (path-length C-RS1 OUTPUT C-CS1 OUTPUT) 2.425)
(= (path-length C-RS1 OUTPUT C-CS2 INPUT) 4.626)
(= (path-length C-RS1 OUTPUT C-CS2 OUTPUT) 3.261)
(= (path-length C-RS1 OUTPUT C-DS INPUT) 3.460)
(= (path-length C-RS1 OUTPUT C-DS OUTPUT) 4.868)
(= (path-length C-RS1 OUTPUT C-RS1 INPUT) 3.501)
(= (path-length C-RS1 OUTPUT C-RS2 INPUT) 8.041)
(= (path-length C-RS1 OUTPUT C-RS2 OUTPUT) 6.992)
(= (path-length C-RS2 INPUT C-BS INPUT) 12.073)
(= (path-length C-RS2 INPUT C-BS OUTPUT) 12.957)
(= (path-length C-RS2 INPUT C-CS1 INPUT) 6.934)
(= (path-length C-RS2 INPUT C-CS1 OUTPUT) 5.617)
(= (path-length C-RS2 INPUT C-CS2 INPUT) 10.010)
(= (path-length C-RS2 INPUT C-CS2 OUTPUT) 11.161)
(= (path-length C-RS2 INPUT C-DS INPUT) 6.847)
(= (path-length C-RS2 INPUT C-DS OUTPUT) 9.191)
(= (path-length C-RS2 INPUT C-RS1 INPUT) 6.888)
(= (path-length C-RS2 INPUT C-RS1 OUTPUT) 8.041)
(= (path-length C-RS2 INPUT C-RS2 OUTPUT) 3.254)
(= (path-length C-RS2 OUTPUT C-BS INPUT) 11.024)
(= (path-length C-RS2 OUTPUT C-BS OUTPUT) 11.175)
(= (path-length C-RS2 OUTPUT C-CS1 INPUT) 5.152)
(= (path-length C-RS2 OUTPUT C-CS1 OUTPUT) 4.568)
(= (path-length C-RS2 OUTPUT C-CS2 INPUT) 8.228)
(= (path-length C-RS2 OUTPUT C-CS2 OUTPUT) 10.112)
(= (path-length C-RS2 OUTPUT C-DS INPUT) 7.781)
(= (path-length C-RS2 OUTPUT C-DS OUTPUT) 10.492)
(= (path-length C-RS2 OUTPUT C-RS1 INPUT) 7.822)
(= (path-length C-RS2 OUTPUT C-RS1 OUTPUT) 6.992)
(= (path-length C-RS2 OUTPUT C-RS2 INPUT) 3.254)
(= (path-length START INPUT C-BS INPUT) 2.203)
(= (path-length START INPUT C-BS OUTPUT) 1.813)
(= (path-length START INPUT C-CS1 INPUT) 4.884)
(= (path-length START INPUT C-CS1 OUTPUT) 6.192)
(= (path-length START INPUT C-CS2 INPUT) 1.469)
(= (path-length START INPUT C-CS2 OUTPUT) 1.720)
(= (path-length START INPUT C-DS INPUT) 7.405)
(= (path-length START INPUT C-DS OUTPUT) 5.867)
(= (path-length START INPUT C-RS1 INPUT) 6.562)
(= (path-length START INPUT C-RS1 OUTPUT) 4.091)
(= (path-length START INPUT C-RS2 INPUT) 11.204)
(= (path-length START INPUT C-RS2 OUTPUT) 9.422)

	)

	(:goal (order-fulfilled o1))
)
    