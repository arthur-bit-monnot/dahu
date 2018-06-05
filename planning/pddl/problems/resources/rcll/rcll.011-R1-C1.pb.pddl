
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
  (rs-sub THREE TWO ONE)
	 (rs-sub THREE ONE TWO)
	 (rs-sub THREE ZERO THREE)
	 (rs-sub TWO TWO ZERO)
	 (rs-sub TWO ONE ONE)
	 (rs-sub TWO ZERO TWO)
	 (rs-sub ONE ONE ZERO)
	 (rs-sub ONE ZERO ONE)
	 (rs-sub ZERO ZERO ZERO)
	 (rs-inc ZERO ONE)
	 (rs-inc ONE TWO)
	 (rs-inc TWO THREE)
	 (rs-filled-with C-RS1 ZERO)
	 (rs-filled-with C-RS2 ZERO)
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
   (rs-ring-spec C-RS1 RING_GREEN ZERO)
	 (rs-ring-spec C-RS1 RING_YELLOW ZERO)
	 (rs-ring-spec C-RS2 RING_BLUE ONE)
	 (rs-ring-spec C-RS2 RING_ORANGE TWO)

   
         ; C1 order
	       (order-complexity o1 c1)
	       (order-base-color o1 BASE_BLACK)
	       (order-ring1-color o1 RING_YELLOW)
	       (order-cap-color o1 CAP_BLACK)
	       (order-gate o1 GATE-3)
       

	 ; C0 order
	 ;(order-complexity o1 c0)
	 ;(order-base-color o1 BASE_BLACK)
	 ;(order-cap-color o1 CAP_GREY)
	 ;(order-gate o1 GATE-1)

	 ; These values are for the static default world
   (= (path-length C-BS INPUT C-BS OUTPUT) 3.594)
(= (path-length C-BS INPUT C-CS1 INPUT) 11.214)
(= (path-length C-BS INPUT C-CS1 OUTPUT) 9.941)
(= (path-length C-BS INPUT C-CS2 INPUT) 8.599)
(= (path-length C-BS INPUT C-CS2 OUTPUT) 6.412)
(= (path-length C-BS INPUT C-DS INPUT) 8.441)
(= (path-length C-BS INPUT C-DS OUTPUT) 7.320)
(= (path-length C-BS INPUT C-RS1 INPUT) 9.373)
(= (path-length C-BS INPUT C-RS1 OUTPUT) 10.415)
(= (path-length C-BS INPUT C-RS2 INPUT) 6.540)
(= (path-length C-BS INPUT C-RS2 OUTPUT) 6.061)
(= (path-length C-BS OUTPUT C-BS INPUT) 3.594)
(= (path-length C-BS OUTPUT C-CS1 INPUT) 9.022)
(= (path-length C-BS OUTPUT C-CS1 OUTPUT) 7.749)
(= (path-length C-BS OUTPUT C-CS2 INPUT) 6.408)
(= (path-length C-BS OUTPUT C-CS2 OUTPUT) 5.166)
(= (path-length C-BS OUTPUT C-DS INPUT) 7.194)
(= (path-length C-BS OUTPUT C-DS OUTPUT) 6.074)
(= (path-length C-BS OUTPUT C-RS1 INPUT) 7.181)
(= (path-length C-BS OUTPUT C-RS1 OUTPUT) 8.224)
(= (path-length C-BS OUTPUT C-RS2 INPUT) 5.294)
(= (path-length C-BS OUTPUT C-RS2 OUTPUT) 4.815)
(= (path-length C-CS1 INPUT C-BS INPUT) 11.214)
(= (path-length C-CS1 INPUT C-BS OUTPUT) 9.022)
(= (path-length C-CS1 INPUT C-CS1 OUTPUT) 4.567)
(= (path-length C-CS1 INPUT C-CS2 INPUT) 4.928)
(= (path-length C-CS1 INPUT C-CS2 OUTPUT) 6.922)
(= (path-length C-CS1 INPUT C-DS INPUT) 6.148)
(= (path-length C-CS1 INPUT C-DS OUTPUT) 5.514)
(= (path-length C-CS1 INPUT C-RS1 INPUT) 4.000)
(= (path-length C-CS1 INPUT C-RS1 OUTPUT) 5.157)
(= (path-length C-CS1 INPUT C-RS2 INPUT) 6.669)
(= (path-length C-CS1 INPUT C-RS2 OUTPUT) 9.142)
(= (path-length C-CS1 OUTPUT C-BS INPUT) 9.941)
(= (path-length C-CS1 OUTPUT C-BS OUTPUT) 7.749)
(= (path-length C-CS1 OUTPUT C-CS1 INPUT) 4.567)
(= (path-length C-CS1 OUTPUT C-CS2 INPUT) 5.157)
(= (path-length C-CS1 OUTPUT C-CS2 OUTPUT) 7.331)
(= (path-length C-CS1 OUTPUT C-DS INPUT) 7.550)
(= (path-length C-CS1 OUTPUT C-DS OUTPUT) 6.916)
(= (path-length C-CS1 OUTPUT C-RS1 INPUT) 1.425)
(= (path-length C-CS1 OUTPUT C-RS1 OUTPUT) 1.812)
(= (path-length C-CS1 OUTPUT C-RS2 INPUT) 7.460)
(= (path-length C-CS1 OUTPUT C-RS2 OUTPUT) 10.146)
(= (path-length C-CS2 INPUT C-BS INPUT) 8.599)
(= (path-length C-CS2 INPUT C-BS OUTPUT) 6.408)
(= (path-length C-CS2 INPUT C-CS1 INPUT) 4.928)
(= (path-length C-CS2 INPUT C-CS1 OUTPUT) 5.157)
(= (path-length C-CS2 INPUT C-CS2 OUTPUT) 3.578)
(= (path-length C-CS2 INPUT C-DS INPUT) 2.805)
(= (path-length C-CS2 INPUT C-DS OUTPUT) 2.171)
(= (path-length C-CS2 INPUT C-RS1 INPUT) 4.589)
(= (path-length C-CS2 INPUT C-RS1 OUTPUT) 5.632)
(= (path-length C-CS2 INPUT C-RS2 INPUT) 3.326)
(= (path-length C-CS2 INPUT C-RS2 OUTPUT) 5.799)
(= (path-length C-CS2 OUTPUT C-BS INPUT) 6.412)
(= (path-length C-CS2 OUTPUT C-BS OUTPUT) 5.166)
(= (path-length C-CS2 OUTPUT C-CS1 INPUT) 6.922)
(= (path-length C-CS2 OUTPUT C-CS1 OUTPUT) 7.331)
(= (path-length C-CS2 OUTPUT C-CS2 INPUT) 3.578)
(= (path-length C-CS2 OUTPUT C-DS INPUT) 2.916)
(= (path-length C-CS2 OUTPUT C-DS OUTPUT) 1.795)
(= (path-length C-CS2 OUTPUT C-RS1 INPUT) 6.764)
(= (path-length C-CS2 OUTPUT C-RS1 OUTPUT) 7.806)
(= (path-length C-CS2 OUTPUT C-RS2 INPUT) 1.016)
(= (path-length C-CS2 OUTPUT C-RS2 OUTPUT) 3.702)
(= (path-length C-DS INPUT C-BS INPUT) 8.441)
(= (path-length C-DS INPUT C-BS OUTPUT) 7.194)
(= (path-length C-DS INPUT C-CS1 INPUT) 6.148)
(= (path-length C-DS INPUT C-CS1 OUTPUT) 7.550)
(= (path-length C-DS INPUT C-CS2 INPUT) 2.805)
(= (path-length C-DS INPUT C-CS2 OUTPUT) 2.916)
(= (path-length C-DS INPUT C-DS OUTPUT) 3.223)
(= (path-length C-DS INPUT C-RS1 INPUT) 6.982)
(= (path-length C-DS INPUT C-RS1 OUTPUT) 8.025)
(= (path-length C-DS INPUT C-RS2 INPUT) 2.664)
(= (path-length C-DS INPUT C-RS2 OUTPUT) 3.160)
(= (path-length C-DS OUTPUT C-BS INPUT) 7.320)
(= (path-length C-DS OUTPUT C-BS OUTPUT) 6.074)
(= (path-length C-DS OUTPUT C-CS1 INPUT) 5.514)
(= (path-length C-DS OUTPUT C-CS1 OUTPUT) 6.916)
(= (path-length C-DS OUTPUT C-CS2 INPUT) 2.171)
(= (path-length C-DS OUTPUT C-CS2 OUTPUT) 1.795)
(= (path-length C-DS OUTPUT C-DS INPUT) 3.223)
(= (path-length C-DS OUTPUT C-RS1 INPUT) 6.348)
(= (path-length C-DS OUTPUT C-RS1 OUTPUT) 7.391)
(= (path-length C-DS OUTPUT C-RS2 INPUT) 1.543)
(= (path-length C-DS OUTPUT C-RS2 OUTPUT) 4.016)
(= (path-length C-RS1 INPUT C-BS INPUT) 9.373)
(= (path-length C-RS1 INPUT C-BS OUTPUT) 7.181)
(= (path-length C-RS1 INPUT C-CS1 INPUT) 4.000)
(= (path-length C-RS1 INPUT C-CS1 OUTPUT) 1.425)
(= (path-length C-RS1 INPUT C-CS2 INPUT) 4.589)
(= (path-length C-RS1 INPUT C-CS2 OUTPUT) 6.764)
(= (path-length C-RS1 INPUT C-DS INPUT) 6.982)
(= (path-length C-RS1 INPUT C-DS OUTPUT) 6.348)
(= (path-length C-RS1 INPUT C-RS1 OUTPUT) 2.987)
(= (path-length C-RS1 INPUT C-RS2 INPUT) 6.892)
(= (path-length C-RS1 INPUT C-RS2 OUTPUT) 9.578)
(= (path-length C-RS1 OUTPUT C-BS INPUT) 10.415)
(= (path-length C-RS1 OUTPUT C-BS OUTPUT) 8.224)
(= (path-length C-RS1 OUTPUT C-CS1 INPUT) 5.157)
(= (path-length C-RS1 OUTPUT C-CS1 OUTPUT) 1.812)
(= (path-length C-RS1 OUTPUT C-CS2 INPUT) 5.632)
(= (path-length C-RS1 OUTPUT C-CS2 OUTPUT) 7.806)
(= (path-length C-RS1 OUTPUT C-DS INPUT) 8.025)
(= (path-length C-RS1 OUTPUT C-DS OUTPUT) 7.391)
(= (path-length C-RS1 OUTPUT C-RS1 INPUT) 2.987)
(= (path-length C-RS1 OUTPUT C-RS2 INPUT) 7.935)
(= (path-length C-RS1 OUTPUT C-RS2 OUTPUT) 10.621)
(= (path-length C-RS2 INPUT C-BS INPUT) 6.540)
(= (path-length C-RS2 INPUT C-BS OUTPUT) 5.294)
(= (path-length C-RS2 INPUT C-CS1 INPUT) 6.669)
(= (path-length C-RS2 INPUT C-CS1 OUTPUT) 7.460)
(= (path-length C-RS2 INPUT C-CS2 INPUT) 3.326)
(= (path-length C-RS2 INPUT C-CS2 OUTPUT) 1.016)
(= (path-length C-RS2 INPUT C-DS INPUT) 2.664)
(= (path-length C-RS2 INPUT C-DS OUTPUT) 1.543)
(= (path-length C-RS2 INPUT C-RS1 INPUT) 6.892)
(= (path-length C-RS2 INPUT C-RS1 OUTPUT) 7.935)
(= (path-length C-RS2 INPUT C-RS2 OUTPUT) 3.449)
(= (path-length C-RS2 OUTPUT C-BS INPUT) 6.061)
(= (path-length C-RS2 OUTPUT C-BS OUTPUT) 4.815)
(= (path-length C-RS2 OUTPUT C-CS1 INPUT) 9.142)
(= (path-length C-RS2 OUTPUT C-CS1 OUTPUT) 10.146)
(= (path-length C-RS2 OUTPUT C-CS2 INPUT) 5.799)
(= (path-length C-RS2 OUTPUT C-CS2 OUTPUT) 3.702)
(= (path-length C-RS2 OUTPUT C-DS INPUT) 3.160)
(= (path-length C-RS2 OUTPUT C-DS OUTPUT) 4.016)
(= (path-length C-RS2 OUTPUT C-RS1 INPUT) 9.578)
(= (path-length C-RS2 OUTPUT C-RS1 OUTPUT) 10.621)
(= (path-length C-RS2 OUTPUT C-RS2 INPUT) 3.449)
(= (path-length START INPUT C-BS INPUT) 2.936)
(= (path-length START INPUT C-BS OUTPUT) 1.415)
(= (path-length START INPUT C-CS1 INPUT) 8.353)
(= (path-length START INPUT C-CS1 OUTPUT) 7.079)
(= (path-length START INPUT C-CS2 INPUT) 5.738)
(= (path-length START INPUT C-CS2 OUTPUT) 5.465)
(= (path-length START INPUT C-DS INPUT) 7.494)
(= (path-length START INPUT C-DS OUTPUT) 6.373)
(= (path-length START INPUT C-RS1 INPUT) 6.512)
(= (path-length START INPUT C-RS1 OUTPUT) 7.554)
(= (path-length START INPUT C-RS2 INPUT) 5.594)
(= (path-length START INPUT C-RS2 OUTPUT) 5.872)

	)

	(:goal (order-fulfilled o1))
)
    