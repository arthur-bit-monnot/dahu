
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
   (rs-ring-spec C-RS1 RING_GREEN ZERO)
	 (rs-ring-spec C-RS1 RING_YELLOW ZERO)
	 (rs-ring-spec C-RS2 RING_BLUE ONE)
	 (rs-ring-spec C-RS2 RING_ORANGE TWO)

   
         ; C1 order
	       (order-complexity o1 c1)
	       (order-base-color o1 BASE_BLACK)
	       (order-ring1-color o1 RING_GREEN)
	       (order-cap-color o1 CAP_GREY)
	       (order-gate o1 GATE-3)
       

	 ; C0 order
	 ;(order-complexity o1 c0)
	 ;(order-base-color o1 BASE_BLACK)
	 ;(order-cap-color o1 CAP_GREY)
	 ;(order-gate o1 GATE-1)

	 ; These values are for the static default world
   (= (path-length C-BS INPUT C-BS OUTPUT) 2.476)
(= (path-length C-BS INPUT C-CS1 INPUT) 3.380)
(= (path-length C-BS INPUT C-CS1 OUTPUT) 5.700)
(= (path-length C-BS INPUT C-CS2 INPUT) 10.304)
(= (path-length C-BS INPUT C-CS2 OUTPUT) 10.258)
(= (path-length C-BS INPUT C-DS INPUT) 5.205)
(= (path-length C-BS INPUT C-DS OUTPUT) 7.266)
(= (path-length C-BS INPUT C-RS1 INPUT) 3.574)
(= (path-length C-BS INPUT C-RS1 OUTPUT) 4.523)
(= (path-length C-BS INPUT C-RS2 INPUT) 10.710)
(= (path-length C-BS INPUT C-RS2 OUTPUT) 10.344)
(= (path-length C-BS OUTPUT C-BS INPUT) 2.476)
(= (path-length C-BS OUTPUT C-CS1 INPUT) 3.765)
(= (path-length C-BS OUTPUT C-CS1 OUTPUT) 6.086)
(= (path-length C-BS OUTPUT C-CS2 INPUT) 10.690)
(= (path-length C-BS OUTPUT C-CS2 OUTPUT) 10.643)
(= (path-length C-BS OUTPUT C-DS INPUT) 5.591)
(= (path-length C-BS OUTPUT C-DS OUTPUT) 7.651)
(= (path-length C-BS OUTPUT C-RS1 INPUT) 3.960)
(= (path-length C-BS OUTPUT C-RS1 OUTPUT) 4.908)
(= (path-length C-BS OUTPUT C-RS2 INPUT) 11.095)
(= (path-length C-BS OUTPUT C-RS2 OUTPUT) 10.730)
(= (path-length C-CS1 INPUT C-BS INPUT) 3.380)
(= (path-length C-CS1 INPUT C-BS OUTPUT) 3.765)
(= (path-length C-CS1 INPUT C-CS1 OUTPUT) 3.177)
(= (path-length C-CS1 INPUT C-CS2 INPUT) 7.781)
(= (path-length C-CS1 INPUT C-CS2 OUTPUT) 7.734)
(= (path-length C-CS1 INPUT C-DS INPUT) 4.809)
(= (path-length C-CS1 INPUT C-DS OUTPUT) 6.480)
(= (path-length C-CS1 INPUT C-RS1 INPUT) 2.504)
(= (path-length C-CS1 INPUT C-RS1 OUTPUT) 1.999)
(= (path-length C-CS1 INPUT C-RS2 INPUT) 8.817)
(= (path-length C-CS1 INPUT C-RS2 OUTPUT) 7.821)
(= (path-length C-CS1 OUTPUT C-BS INPUT) 5.700)
(= (path-length C-CS1 OUTPUT C-BS OUTPUT) 6.086)
(= (path-length C-CS1 OUTPUT C-CS1 INPUT) 3.177)
(= (path-length C-CS1 OUTPUT C-CS2 INPUT) 5.977)
(= (path-length C-CS1 OUTPUT C-CS2 OUTPUT) 6.440)
(= (path-length C-CS1 OUTPUT C-DS INPUT) 4.714)
(= (path-length C-CS1 OUTPUT C-DS OUTPUT) 3.833)
(= (path-length C-CS1 OUTPUT C-RS1 INPUT) 3.480)
(= (path-length C-CS1 OUTPUT C-RS1 OUTPUT) 1.496)
(= (path-length C-CS1 OUTPUT C-RS2 INPUT) 6.171)
(= (path-length C-CS1 OUTPUT C-RS2 OUTPUT) 6.017)
(= (path-length C-CS2 INPUT C-BS INPUT) 10.304)
(= (path-length C-CS2 INPUT C-BS OUTPUT) 10.690)
(= (path-length C-CS2 INPUT C-CS1 INPUT) 7.781)
(= (path-length C-CS2 INPUT C-CS1 OUTPUT) 5.977)
(= (path-length C-CS2 INPUT C-CS2 OUTPUT) 4.539)
(= (path-length C-CS2 INPUT C-DS INPUT) 7.862)
(= (path-length C-CS2 INPUT C-DS OUTPUT) 6.818)
(= (path-length C-CS2 INPUT C-RS1 INPUT) 7.916)
(= (path-length C-CS2 INPUT C-RS1 OUTPUT) 6.100)
(= (path-length C-CS2 INPUT C-RS2 INPUT) 4.875)
(= (path-length C-CS2 INPUT C-RS2 OUTPUT) 1.729)
(= (path-length C-CS2 OUTPUT C-BS INPUT) 10.258)
(= (path-length C-CS2 OUTPUT C-BS OUTPUT) 10.643)
(= (path-length C-CS2 OUTPUT C-CS1 INPUT) 7.734)
(= (path-length C-CS2 OUTPUT C-CS1 OUTPUT) 6.440)
(= (path-length C-CS2 OUTPUT C-CS2 INPUT) 4.539)
(= (path-length C-CS2 OUTPUT C-DS INPUT) 10.192)
(= (path-length C-CS2 OUTPUT C-DS OUTPUT) 9.148)
(= (path-length C-CS2 OUTPUT C-RS1 INPUT) 7.869)
(= (path-length C-CS2 OUTPUT C-RS1 OUTPUT) 6.053)
(= (path-length C-CS2 OUTPUT C-RS2 INPUT) 5.402)
(= (path-length C-CS2 OUTPUT C-RS2 OUTPUT) 4.429)
(= (path-length C-DS INPUT C-BS INPUT) 5.205)
(= (path-length C-DS INPUT C-BS OUTPUT) 5.591)
(= (path-length C-DS INPUT C-CS1 INPUT) 4.809)
(= (path-length C-DS INPUT C-CS1 OUTPUT) 4.714)
(= (path-length C-DS INPUT C-CS2 INPUT) 7.862)
(= (path-length C-DS INPUT C-CS2 OUTPUT) 10.192)
(= (path-length C-DS INPUT C-DS OUTPUT) 3.223)
(= (path-length C-DS INPUT C-RS1 INPUT) 5.003)
(= (path-length C-DS INPUT C-RS1 OUTPUT) 5.679)
(= (path-length C-DS INPUT C-RS2 INPUT) 6.877)
(= (path-length C-DS INPUT C-RS2 OUTPUT) 7.902)
(= (path-length C-DS OUTPUT C-BS INPUT) 7.266)
(= (path-length C-DS OUTPUT C-BS OUTPUT) 7.651)
(= (path-length C-DS OUTPUT C-CS1 INPUT) 6.480)
(= (path-length C-DS OUTPUT C-CS1 OUTPUT) 3.833)
(= (path-length C-DS OUTPUT C-CS2 INPUT) 6.818)
(= (path-length C-DS OUTPUT C-CS2 OUTPUT) 9.148)
(= (path-length C-DS OUTPUT C-DS INPUT) 3.223)
(= (path-length C-DS OUTPUT C-RS1 INPUT) 6.783)
(= (path-length C-DS OUTPUT C-RS1 OUTPUT) 4.798)
(= (path-length C-DS OUTPUT C-RS2 INPUT) 3.779)
(= (path-length C-DS OUTPUT C-RS2 OUTPUT) 6.857)
(= (path-length C-RS1 INPUT C-BS INPUT) 3.574)
(= (path-length C-RS1 INPUT C-BS OUTPUT) 3.960)
(= (path-length C-RS1 INPUT C-CS1 INPUT) 2.504)
(= (path-length C-RS1 INPUT C-CS1 OUTPUT) 3.480)
(= (path-length C-RS1 INPUT C-CS2 INPUT) 7.916)
(= (path-length C-RS1 INPUT C-CS2 OUTPUT) 7.869)
(= (path-length C-RS1 INPUT C-DS INPUT) 5.003)
(= (path-length C-RS1 INPUT C-DS OUTPUT) 6.783)
(= (path-length C-RS1 INPUT C-RS1 OUTPUT) 3.026)
(= (path-length C-RS1 INPUT C-RS2 INPUT) 9.121)
(= (path-length C-RS1 INPUT C-RS2 OUTPUT) 7.956)
(= (path-length C-RS1 OUTPUT C-BS INPUT) 4.523)
(= (path-length C-RS1 OUTPUT C-BS OUTPUT) 4.908)
(= (path-length C-RS1 OUTPUT C-CS1 INPUT) 1.999)
(= (path-length C-RS1 OUTPUT C-CS1 OUTPUT) 1.496)
(= (path-length C-RS1 OUTPUT C-CS2 INPUT) 6.100)
(= (path-length C-RS1 OUTPUT C-CS2 OUTPUT) 6.053)
(= (path-length C-RS1 OUTPUT C-DS INPUT) 5.679)
(= (path-length C-RS1 OUTPUT C-DS OUTPUT) 4.798)
(= (path-length C-RS1 OUTPUT C-RS1 INPUT) 3.026)
(= (path-length C-RS1 OUTPUT C-RS2 INPUT) 7.136)
(= (path-length C-RS1 OUTPUT C-RS2 OUTPUT) 6.140)
(= (path-length C-RS2 INPUT C-BS INPUT) 10.710)
(= (path-length C-RS2 INPUT C-BS OUTPUT) 11.095)
(= (path-length C-RS2 INPUT C-CS1 INPUT) 8.817)
(= (path-length C-RS2 INPUT C-CS1 OUTPUT) 6.171)
(= (path-length C-RS2 INPUT C-CS2 INPUT) 4.875)
(= (path-length C-RS2 INPUT C-CS2 OUTPUT) 5.402)
(= (path-length C-RS2 INPUT C-DS INPUT) 6.877)
(= (path-length C-RS2 INPUT C-DS OUTPUT) 3.779)
(= (path-length C-RS2 INPUT C-RS1 INPUT) 9.121)
(= (path-length C-RS2 INPUT C-RS1 OUTPUT) 7.136)
(= (path-length C-RS2 INPUT C-RS2 OUTPUT) 4.764)
(= (path-length C-RS2 OUTPUT C-BS INPUT) 10.344)
(= (path-length C-RS2 OUTPUT C-BS OUTPUT) 10.730)
(= (path-length C-RS2 OUTPUT C-CS1 INPUT) 7.821)
(= (path-length C-RS2 OUTPUT C-CS1 OUTPUT) 6.017)
(= (path-length C-RS2 OUTPUT C-CS2 INPUT) 1.729)
(= (path-length C-RS2 OUTPUT C-CS2 OUTPUT) 4.429)
(= (path-length C-RS2 OUTPUT C-DS INPUT) 7.902)
(= (path-length C-RS2 OUTPUT C-DS OUTPUT) 6.857)
(= (path-length C-RS2 OUTPUT C-RS1 INPUT) 7.956)
(= (path-length C-RS2 OUTPUT C-RS1 OUTPUT) 6.140)
(= (path-length C-RS2 OUTPUT C-RS2 INPUT) 4.764)
(= (path-length START INPUT C-BS INPUT) 1.379)
(= (path-length START INPUT C-BS OUTPUT) 3.202)
(= (path-length START INPUT C-CS1 INPUT) 2.759)
(= (path-length START INPUT C-CS1 OUTPUT) 5.079)
(= (path-length START INPUT C-CS2 INPUT) 9.683)
(= (path-length START INPUT C-CS2 OUTPUT) 9.636)
(= (path-length START INPUT C-DS INPUT) 4.584)
(= (path-length START INPUT C-DS OUTPUT) 6.644)
(= (path-length START INPUT C-RS1 INPUT) 2.953)
(= (path-length START INPUT C-RS1 OUTPUT) 3.901)
(= (path-length START INPUT C-RS2 INPUT) 10.088)
(= (path-length START INPUT C-RS2 OUTPUT) 9.723)

	)

	(:goal (order-fulfilled o1))
)
    