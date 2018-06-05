
    (define (problem rcll-production-durative-prob1)
	   (:domain rcll-production-durative)

	(:objects
		R-1 R-2 - robot
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
   (= (path-length C-BS INPUT C-BS OUTPUT) 2.262)
(= (path-length C-BS INPUT C-CS1 INPUT) 12.145)
(= (path-length C-BS INPUT C-CS1 OUTPUT) 9.815)
(= (path-length C-BS INPUT C-CS2 INPUT) 5.549)
(= (path-length C-BS INPUT C-CS2 OUTPUT) 5.654)
(= (path-length C-BS INPUT C-DS INPUT) 7.220)
(= (path-length C-BS INPUT C-DS OUTPUT) 6.524)
(= (path-length C-BS INPUT C-RS1 INPUT) 12.516)
(= (path-length C-BS INPUT C-RS1 OUTPUT) 12.424)
(= (path-length C-BS INPUT C-RS2 INPUT) 7.249)
(= (path-length C-BS INPUT C-RS2 OUTPUT) 4.770)
(= (path-length C-BS OUTPUT C-BS INPUT) 2.262)
(= (path-length C-BS OUTPUT C-CS1 INPUT) 10.344)
(= (path-length C-BS OUTPUT C-CS1 OUTPUT) 8.014)
(= (path-length C-BS OUTPUT C-CS2 INPUT) 3.748)
(= (path-length C-BS OUTPUT C-CS2 OUTPUT) 3.854)
(= (path-length C-BS OUTPUT C-DS INPUT) 6.809)
(= (path-length C-BS OUTPUT C-DS OUTPUT) 4.723)
(= (path-length C-BS OUTPUT C-RS1 INPUT) 10.716)
(= (path-length C-BS OUTPUT C-RS1 OUTPUT) 10.623)
(= (path-length C-BS OUTPUT C-RS2 INPUT) 5.449)
(= (path-length C-BS OUTPUT C-RS2 OUTPUT) 2.969)
(= (path-length C-CS1 INPUT C-BS INPUT) 12.145)
(= (path-length C-CS1 INPUT C-BS OUTPUT) 10.344)
(= (path-length C-CS1 INPUT C-CS1 OUTPUT) 4.811)
(= (path-length C-CS1 INPUT C-CS2 INPUT) 7.704)
(= (path-length C-CS1 INPUT C-CS2 OUTPUT) 8.693)
(= (path-length C-CS1 INPUT C-DS INPUT) 5.713)
(= (path-length C-CS1 INPUT C-DS OUTPUT) 6.700)
(= (path-length C-CS1 INPUT C-RS1 INPUT) 2.662)
(= (path-length C-CS1 INPUT C-RS1 OUTPUT) 2.412)
(= (path-length C-CS1 INPUT C-RS2 INPUT) 6.415)
(= (path-length C-CS1 INPUT C-RS2 OUTPUT) 8.711)
(= (path-length C-CS1 OUTPUT C-BS INPUT) 9.815)
(= (path-length C-CS1 OUTPUT C-BS OUTPUT) 8.014)
(= (path-length C-CS1 OUTPUT C-CS1 INPUT) 4.811)
(= (path-length C-CS1 OUTPUT C-CS2 INPUT) 5.662)
(= (path-length C-CS1 OUTPUT C-CS2 OUTPUT) 6.738)
(= (path-length C-CS1 OUTPUT C-DS INPUT) 5.624)
(= (path-length C-CS1 OUTPUT C-DS OUTPUT) 4.370)
(= (path-length C-CS1 OUTPUT C-RS1 INPUT) 5.267)
(= (path-length C-CS1 OUTPUT C-RS1 OUTPUT) 5.090)
(= (path-length C-CS1 OUTPUT C-RS2 INPUT) 4.085)
(= (path-length C-CS1 OUTPUT C-RS2 OUTPUT) 6.669)
(= (path-length C-CS2 INPUT C-BS INPUT) 5.549)
(= (path-length C-CS2 INPUT C-BS OUTPUT) 3.748)
(= (path-length C-CS2 INPUT C-CS1 INPUT) 7.704)
(= (path-length C-CS2 INPUT C-CS1 OUTPUT) 5.662)
(= (path-length C-CS2 INPUT C-CS2 OUTPUT) 1.839)
(= (path-length C-CS2 INPUT C-DS INPUT) 5.729)
(= (path-length C-CS2 INPUT C-DS OUTPUT) 4.476)
(= (path-length C-CS2 INPUT C-RS1 INPUT) 7.346)
(= (path-length C-CS2 INPUT C-RS1 OUTPUT) 8.271)
(= (path-length C-CS2 INPUT C-RS2 INPUT) 2.079)
(= (path-length C-CS2 INPUT C-RS2 OUTPUT) 1.385)
(= (path-length C-CS2 OUTPUT C-BS INPUT) 5.654)
(= (path-length C-CS2 OUTPUT C-BS OUTPUT) 3.854)
(= (path-length C-CS2 OUTPUT C-CS1 INPUT) 8.693)
(= (path-length C-CS2 OUTPUT C-CS1 OUTPUT) 6.738)
(= (path-length C-CS2 OUTPUT C-CS2 INPUT) 1.839)
(= (path-length C-CS2 OUTPUT C-DS INPUT) 6.903)
(= (path-length C-CS2 OUTPUT C-DS OUTPUT) 5.489)
(= (path-length C-CS2 OUTPUT C-RS1 INPUT) 8.335)
(= (path-length C-CS2 OUTPUT C-RS1 OUTPUT) 9.446)
(= (path-length C-CS2 OUTPUT C-RS2 INPUT) 3.253)
(= (path-length C-CS2 OUTPUT C-RS2 OUTPUT) 2.174)
(= (path-length C-DS INPUT C-BS INPUT) 7.220)
(= (path-length C-DS INPUT C-BS OUTPUT) 6.809)
(= (path-length C-DS INPUT C-CS1 INPUT) 5.713)
(= (path-length C-DS INPUT C-CS1 OUTPUT) 5.624)
(= (path-length C-DS INPUT C-CS2 INPUT) 5.729)
(= (path-length C-DS INPUT C-CS2 OUTPUT) 6.903)
(= (path-length C-DS INPUT C-DS OUTPUT) 4.315)
(= (path-length C-DS INPUT C-RS1 INPUT) 8.049)
(= (path-length C-DS INPUT C-RS1 OUTPUT) 5.992)
(= (path-length C-DS INPUT C-RS2 INPUT) 4.152)
(= (path-length C-DS INPUT C-RS2 OUTPUT) 6.690)
(= (path-length C-DS OUTPUT C-BS INPUT) 6.524)
(= (path-length C-DS OUTPUT C-BS OUTPUT) 4.723)
(= (path-length C-DS OUTPUT C-CS1 INPUT) 6.700)
(= (path-length C-DS OUTPUT C-CS1 OUTPUT) 4.370)
(= (path-length C-DS OUTPUT C-CS2 INPUT) 4.476)
(= (path-length C-DS OUTPUT C-CS2 OUTPUT) 5.489)
(= (path-length C-DS OUTPUT C-DS INPUT) 4.315)
(= (path-length C-DS OUTPUT C-RS1 INPUT) 8.617)
(= (path-length C-DS OUTPUT C-RS1 OUTPUT) 6.979)
(= (path-length C-DS OUTPUT C-RS2 INPUT) 2.898)
(= (path-length C-DS OUTPUT C-RS2 OUTPUT) 4.604)
(= (path-length C-RS1 INPUT C-BS INPUT) 12.516)
(= (path-length C-RS1 INPUT C-BS OUTPUT) 10.716)
(= (path-length C-RS1 INPUT C-CS1 INPUT) 2.662)
(= (path-length C-RS1 INPUT C-CS1 OUTPUT) 5.267)
(= (path-length C-RS1 INPUT C-CS2 INPUT) 7.346)
(= (path-length C-RS1 INPUT C-CS2 OUTPUT) 8.335)
(= (path-length C-RS1 INPUT C-DS INPUT) 8.049)
(= (path-length C-RS1 INPUT C-DS OUTPUT) 8.617)
(= (path-length C-RS1 INPUT C-RS1 OUTPUT) 3.085)
(= (path-length C-RS1 INPUT C-RS2 INPUT) 7.422)
(= (path-length C-RS1 INPUT C-RS2 OUTPUT) 8.353)
(= (path-length C-RS1 OUTPUT C-BS INPUT) 12.424)
(= (path-length C-RS1 OUTPUT C-BS OUTPUT) 10.623)
(= (path-length C-RS1 OUTPUT C-CS1 INPUT) 2.412)
(= (path-length C-RS1 OUTPUT C-CS1 OUTPUT) 5.090)
(= (path-length C-RS1 OUTPUT C-CS2 INPUT) 8.271)
(= (path-length C-RS1 OUTPUT C-CS2 OUTPUT) 9.446)
(= (path-length C-RS1 OUTPUT C-DS INPUT) 5.992)
(= (path-length C-RS1 OUTPUT C-DS OUTPUT) 6.979)
(= (path-length C-RS1 OUTPUT C-RS1 INPUT) 3.085)
(= (path-length C-RS1 OUTPUT C-RS2 INPUT) 6.694)
(= (path-length C-RS1 OUTPUT C-RS2 OUTPUT) 9.279)
(= (path-length C-RS2 INPUT C-BS INPUT) 7.249)
(= (path-length C-RS2 INPUT C-BS OUTPUT) 5.449)
(= (path-length C-RS2 INPUT C-CS1 INPUT) 6.415)
(= (path-length C-RS2 INPUT C-CS1 OUTPUT) 4.085)
(= (path-length C-RS2 INPUT C-CS2 INPUT) 2.079)
(= (path-length C-RS2 INPUT C-CS2 OUTPUT) 3.253)
(= (path-length C-RS2 INPUT C-DS INPUT) 4.152)
(= (path-length C-RS2 INPUT C-DS OUTPUT) 2.898)
(= (path-length C-RS2 INPUT C-RS1 INPUT) 7.422)
(= (path-length C-RS2 INPUT C-RS1 OUTPUT) 6.694)
(= (path-length C-RS2 INPUT C-RS2 OUTPUT) 3.086)
(= (path-length C-RS2 OUTPUT C-BS INPUT) 4.770)
(= (path-length C-RS2 OUTPUT C-BS OUTPUT) 2.969)
(= (path-length C-RS2 OUTPUT C-CS1 INPUT) 8.711)
(= (path-length C-RS2 OUTPUT C-CS1 OUTPUT) 6.669)
(= (path-length C-RS2 OUTPUT C-CS2 INPUT) 1.385)
(= (path-length C-RS2 OUTPUT C-CS2 OUTPUT) 2.174)
(= (path-length C-RS2 OUTPUT C-DS INPUT) 6.690)
(= (path-length C-RS2 OUTPUT C-DS OUTPUT) 4.604)
(= (path-length C-RS2 OUTPUT C-RS1 INPUT) 8.353)
(= (path-length C-RS2 OUTPUT C-RS1 OUTPUT) 9.279)
(= (path-length C-RS2 OUTPUT C-RS2 INPUT) 3.086)
(= (path-length START INPUT C-BS INPUT) 2.417)
(= (path-length START INPUT C-BS OUTPUT) 0.616)
(= (path-length START INPUT C-CS1 INPUT) 10.499)
(= (path-length START INPUT C-CS1 OUTPUT) 8.169)
(= (path-length START INPUT C-CS2 INPUT) 3.903)
(= (path-length START INPUT C-CS2 OUTPUT) 4.009)
(= (path-length START INPUT C-DS INPUT) 6.964)
(= (path-length START INPUT C-DS OUTPUT) 4.878)
(= (path-length START INPUT C-RS1 INPUT) 10.871)
(= (path-length START INPUT C-RS1 OUTPUT) 10.778)
(= (path-length START INPUT C-RS2 INPUT) 5.604)
(= (path-length START INPUT C-RS2 OUTPUT) 3.124)

	)

	(:goal (order-fulfilled o1))
)
    