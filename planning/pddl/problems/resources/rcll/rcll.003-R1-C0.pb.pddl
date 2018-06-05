
    (define (problem rcll-production-durative-prob1)
	   (:domain rcll-production-durative)

	(:objects
		R-1 - robot
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
         (order-cap-color o1 CAP_BLACK)
         (order-gate o1 GATE-3)
      

	 ; C0 order
	 ;(order-complexity o1 c0)
	 ;(order-base-color o1 BASE_BLACK)
	 ;(order-cap-color o1 CAP_GREY)
	 ;(order-gate o1 GATE-1)

	 ; These values are for the static default world
   (= (path-length C-BS INPUT C-BS OUTPUT) 2.300)
(= (path-length C-BS INPUT C-CS1 INPUT) 11.131)
(= (path-length C-BS INPUT C-CS1 OUTPUT) 7.829)
(= (path-length C-BS INPUT C-CS2 INPUT) 1.703)
(= (path-length C-BS INPUT C-CS2 OUTPUT) 1.930)
(= (path-length C-BS INPUT C-DS INPUT) 6.806)
(= (path-length C-BS INPUT C-DS OUTPUT) 6.345)
(= (path-length C-BS INPUT C-RS1 INPUT) 6.308)
(= (path-length C-BS INPUT C-RS1 OUTPUT) 5.027)
(= (path-length C-BS INPUT C-RS2 INPUT) 7.259)
(= (path-length C-BS INPUT C-RS2 OUTPUT) 7.800)
(= (path-length C-BS OUTPUT C-BS INPUT) 2.300)
(= (path-length C-BS OUTPUT C-CS1 INPUT) 12.608)
(= (path-length C-BS OUTPUT C-CS1 OUTPUT) 9.820)
(= (path-length C-BS OUTPUT C-CS2 INPUT) 3.877)
(= (path-length C-BS OUTPUT C-CS2 OUTPUT) 4.142)
(= (path-length C-BS OUTPUT C-DS INPUT) 6.496)
(= (path-length C-BS OUTPUT C-DS OUTPUT) 7.538)
(= (path-length C-BS OUTPUT C-RS1 INPUT) 5.398)
(= (path-length C-BS OUTPUT C-RS1 OUTPUT) 4.118)
(= (path-length C-BS OUTPUT C-RS2 INPUT) 8.546)
(= (path-length C-BS OUTPUT C-RS2 OUTPUT) 9.792)
(= (path-length C-CS1 INPUT C-BS INPUT) 11.131)
(= (path-length C-CS1 INPUT C-BS OUTPUT) 12.608)
(= (path-length C-CS1 INPUT C-CS1 OUTPUT) 4.080)
(= (path-length C-CS1 INPUT C-CS2 INPUT) 11.118)
(= (path-length C-CS1 INPUT C-CS2 OUTPUT) 9.588)
(= (path-length C-CS1 INPUT C-DS INPUT) 6.133)
(= (path-length C-CS1 INPUT C-DS OUTPUT) 5.559)
(= (path-length C-CS1 INPUT C-RS1 INPUT) 8.889)
(= (path-length C-CS1 INPUT C-RS1 OUTPUT) 11.018)
(= (path-length C-CS1 INPUT C-RS2 INPUT) 4.341)
(= (path-length C-CS1 INPUT C-RS2 OUTPUT) 4.385)
(= (path-length C-CS1 OUTPUT C-BS INPUT) 7.829)
(= (path-length C-CS1 OUTPUT C-BS OUTPUT) 9.820)
(= (path-length C-CS1 OUTPUT C-CS1 INPUT) 4.080)
(= (path-length C-CS1 OUTPUT C-CS2 INPUT) 8.111)
(= (path-length C-CS1 OUTPUT C-CS2 OUTPUT) 6.286)
(= (path-length C-CS1 OUTPUT C-DS INPUT) 5.680)
(= (path-length C-CS1 OUTPUT C-DS OUTPUT) 4.096)
(= (path-length C-CS1 OUTPUT C-RS1 INPUT) 7.682)
(= (path-length C-CS1 OUTPUT C-RS1 OUTPUT) 9.812)
(= (path-length C-CS1 OUTPUT C-RS2 INPUT) 2.878)
(= (path-length C-CS1 OUTPUT C-RS2 OUTPUT) 1.035)
(= (path-length C-CS2 INPUT C-BS INPUT) 1.703)
(= (path-length C-CS2 INPUT C-BS OUTPUT) 3.877)
(= (path-length C-CS2 INPUT C-CS1 INPUT) 11.118)
(= (path-length C-CS2 INPUT C-CS1 OUTPUT) 8.111)
(= (path-length C-CS2 INPUT C-CS2 OUTPUT) 3.052)
(= (path-length C-CS2 INPUT C-DS INPUT) 6.291)
(= (path-length C-CS2 INPUT C-DS OUTPUT) 5.830)
(= (path-length C-CS2 INPUT C-RS1 INPUT) 5.792)
(= (path-length C-CS2 INPUT C-RS1 OUTPUT) 4.512)
(= (path-length C-CS2 INPUT C-RS2 INPUT) 6.837)
(= (path-length C-CS2 INPUT C-RS2 OUTPUT) 8.083)
(= (path-length C-CS2 OUTPUT C-BS INPUT) 1.930)
(= (path-length C-CS2 OUTPUT C-BS OUTPUT) 4.142)
(= (path-length C-CS2 OUTPUT C-CS1 INPUT) 9.588)
(= (path-length C-CS2 OUTPUT C-CS1 OUTPUT) 6.286)
(= (path-length C-CS2 OUTPUT C-CS2 INPUT) 3.052)
(= (path-length C-CS2 OUTPUT C-DS INPUT) 7.090)
(= (path-length C-CS2 OUTPUT C-DS OUTPUT) 6.041)
(= (path-length C-CS2 OUTPUT C-RS1 INPUT) 7.489)
(= (path-length C-CS2 OUTPUT C-RS1 OUTPUT) 6.376)
(= (path-length C-CS2 OUTPUT C-RS2 INPUT) 5.716)
(= (path-length C-CS2 OUTPUT C-RS2 OUTPUT) 6.257)
(= (path-length C-DS INPUT C-BS INPUT) 6.806)
(= (path-length C-DS INPUT C-BS OUTPUT) 6.496)
(= (path-length C-DS INPUT C-CS1 INPUT) 6.133)
(= (path-length C-DS INPUT C-CS1 OUTPUT) 5.680)
(= (path-length C-DS INPUT C-CS2 INPUT) 6.291)
(= (path-length C-DS INPUT C-CS2 OUTPUT) 7.090)
(= (path-length C-DS INPUT C-DS OUTPUT) 3.157)
(= (path-length C-DS INPUT C-RS1 INPUT) 2.776)
(= (path-length C-DS INPUT C-RS1 OUTPUT) 4.905)
(= (path-length C-DS INPUT C-RS2 INPUT) 2.861)
(= (path-length C-DS INPUT C-RS2 OUTPUT) 5.985)
(= (path-length C-DS OUTPUT C-BS INPUT) 6.345)
(= (path-length C-DS OUTPUT C-BS OUTPUT) 7.538)
(= (path-length C-DS OUTPUT C-CS1 INPUT) 5.559)
(= (path-length C-DS OUTPUT C-CS1 OUTPUT) 4.096)
(= (path-length C-DS OUTPUT C-CS2 INPUT) 5.830)
(= (path-length C-DS OUTPUT C-CS2 OUTPUT) 6.041)
(= (path-length C-DS OUTPUT C-DS INPUT) 3.157)
(= (path-length C-DS OUTPUT C-RS1 INPUT) 3.857)
(= (path-length C-DS OUTPUT C-RS1 OUTPUT) 5.986)
(= (path-length C-DS OUTPUT C-RS2 INPUT) 1.277)
(= (path-length C-DS OUTPUT C-RS2 OUTPUT) 4.401)
(= (path-length C-RS1 INPUT C-BS INPUT) 6.308)
(= (path-length C-RS1 INPUT C-BS OUTPUT) 5.398)
(= (path-length C-RS1 INPUT C-CS1 INPUT) 8.889)
(= (path-length C-RS1 INPUT C-CS1 OUTPUT) 7.682)
(= (path-length C-RS1 INPUT C-CS2 INPUT) 5.792)
(= (path-length C-RS1 INPUT C-CS2 OUTPUT) 7.489)
(= (path-length C-RS1 INPUT C-DS INPUT) 2.776)
(= (path-length C-RS1 INPUT C-DS OUTPUT) 3.857)
(= (path-length C-RS1 INPUT C-RS1 OUTPUT) 2.786)
(= (path-length C-RS1 INPUT C-RS2 INPUT) 4.864)
(= (path-length C-RS1 INPUT C-RS2 OUTPUT) 7.987)
(= (path-length C-RS1 OUTPUT C-BS INPUT) 5.027)
(= (path-length C-RS1 OUTPUT C-BS OUTPUT) 4.118)
(= (path-length C-RS1 OUTPUT C-CS1 INPUT) 11.018)
(= (path-length C-RS1 OUTPUT C-CS1 OUTPUT) 9.811)
(= (path-length C-RS1 OUTPUT C-CS2 INPUT) 4.512)
(= (path-length C-RS1 OUTPUT C-CS2 OUTPUT) 6.376)
(= (path-length C-RS1 OUTPUT C-DS INPUT) 4.905)
(= (path-length C-RS1 OUTPUT C-DS OUTPUT) 5.986)
(= (path-length C-RS1 OUTPUT C-RS1 INPUT) 2.786)
(= (path-length C-RS1 OUTPUT C-RS2 INPUT) 6.993)
(= (path-length C-RS1 OUTPUT C-RS2 OUTPUT) 10.116)
(= (path-length C-RS2 INPUT C-BS INPUT) 7.259)
(= (path-length C-RS2 INPUT C-BS OUTPUT) 8.546)
(= (path-length C-RS2 INPUT C-CS1 INPUT) 4.341)
(= (path-length C-RS2 INPUT C-CS1 OUTPUT) 2.878)
(= (path-length C-RS2 INPUT C-CS2 INPUT) 6.837)
(= (path-length C-RS2 INPUT C-CS2 OUTPUT) 5.716)
(= (path-length C-RS2 INPUT C-DS INPUT) 2.861)
(= (path-length C-RS2 INPUT C-DS OUTPUT) 1.277)
(= (path-length C-RS2 INPUT C-RS1 INPUT) 4.864)
(= (path-length C-RS2 INPUT C-RS1 OUTPUT) 6.993)
(= (path-length C-RS2 INPUT C-RS2 OUTPUT) 3.183)
(= (path-length C-RS2 OUTPUT C-BS INPUT) 7.800)
(= (path-length C-RS2 OUTPUT C-BS OUTPUT) 9.792)
(= (path-length C-RS2 OUTPUT C-CS1 INPUT) 4.385)
(= (path-length C-RS2 OUTPUT C-CS1 OUTPUT) 1.035)
(= (path-length C-RS2 OUTPUT C-CS2 INPUT) 8.083)
(= (path-length C-RS2 OUTPUT C-CS2 OUTPUT) 6.257)
(= (path-length C-RS2 OUTPUT C-DS INPUT) 5.985)
(= (path-length C-RS2 OUTPUT C-DS OUTPUT) 4.401)
(= (path-length C-RS2 OUTPUT C-RS1 INPUT) 7.987)
(= (path-length C-RS2 OUTPUT C-RS1 OUTPUT) 10.116)
(= (path-length C-RS2 OUTPUT C-RS2 INPUT) 3.183)
(= (path-length START INPUT C-BS INPUT) 0.697)
(= (path-length START INPUT C-BS OUTPUT) 2.909)
(= (path-length START INPUT C-CS1 INPUT) 10.524)
(= (path-length START INPUT C-CS1 OUTPUT) 7.222)
(= (path-length START INPUT C-CS2 INPUT) 1.818)
(= (path-length START INPUT C-CS2 OUTPUT) 1.324)
(= (path-length START INPUT C-DS INPUT) 6.921)
(= (path-length START INPUT C-DS OUTPUT) 6.460)
(= (path-length START INPUT C-RS1 INPUT) 6.422)
(= (path-length START INPUT C-RS1 OUTPUT) 5.142)
(= (path-length START INPUT C-RS2 INPUT) 6.652)
(= (path-length START INPUT C-RS2 OUTPUT) 7.193)

	)

	(:goal (order-fulfilled o1))
)
    