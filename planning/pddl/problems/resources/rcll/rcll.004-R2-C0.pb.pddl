
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
         (order-base-color o1 BASE_BLACK)
         (order-cap-color o1 CAP_GREY)
         (order-gate o1 GATE-2)
      

	 ; C0 order
	 ;(order-complexity o1 c0)
	 ;(order-base-color o1 BASE_BLACK)
	 ;(order-cap-color o1 CAP_GREY)
	 ;(order-gate o1 GATE-1)

	 ; These values are for the static default world
   (= (path-length C-BS INPUT C-BS OUTPUT) 2.823)
(= (path-length C-BS INPUT C-CS1 INPUT) 8.295)
(= (path-length C-BS INPUT C-CS1 OUTPUT) 10.770)
(= (path-length C-BS INPUT C-CS2 INPUT) 1.734)
(= (path-length C-BS INPUT C-CS2 OUTPUT) 1.268)
(= (path-length C-BS INPUT C-DS INPUT) 5.518)
(= (path-length C-BS INPUT C-DS OUTPUT) 7.312)
(= (path-length C-BS INPUT C-RS1 INPUT) 10.354)
(= (path-length C-BS INPUT C-RS1 OUTPUT) 12.078)
(= (path-length C-BS INPUT C-RS2 INPUT) 5.771)
(= (path-length C-BS INPUT C-RS2 OUTPUT) 3.694)
(= (path-length C-BS OUTPUT C-BS INPUT) 2.823)
(= (path-length C-BS OUTPUT C-CS1 INPUT) 8.891)
(= (path-length C-BS OUTPUT C-CS1 OUTPUT) 11.366)
(= (path-length C-BS OUTPUT C-CS2 INPUT) 2.540)
(= (path-length C-BS OUTPUT C-CS2 OUTPUT) 3.483)
(= (path-length C-BS OUTPUT C-DS INPUT) 6.113)
(= (path-length C-BS OUTPUT C-DS OUTPUT) 7.908)
(= (path-length C-BS OUTPUT C-RS1 INPUT) 10.949)
(= (path-length C-BS OUTPUT C-RS1 OUTPUT) 12.673)
(= (path-length C-BS OUTPUT C-RS2 INPUT) 6.367)
(= (path-length C-BS OUTPUT C-RS2 OUTPUT) 4.290)
(= (path-length C-CS1 INPUT C-BS INPUT) 8.295)
(= (path-length C-CS1 INPUT C-BS OUTPUT) 8.891)
(= (path-length C-CS1 INPUT C-CS1 OUTPUT) 4.777)
(= (path-length C-CS1 INPUT C-CS2 INPUT) 9.581)
(= (path-length C-CS1 INPUT C-CS2 OUTPUT) 7.806)
(= (path-length C-CS1 INPUT C-DS INPUT) 5.772)
(= (path-length C-CS1 INPUT C-DS OUTPUT) 4.359)
(= (path-length C-CS1 INPUT C-RS1 INPUT) 4.360)
(= (path-length C-CS1 INPUT C-RS1 OUTPUT) 5.958)
(= (path-length C-CS1 INPUT C-RS2 INPUT) 4.589)
(= (path-length C-CS1 INPUT C-RS2 OUTPUT) 5.589)
(= (path-length C-CS1 OUTPUT C-BS INPUT) 10.770)
(= (path-length C-CS1 OUTPUT C-BS OUTPUT) 11.366)
(= (path-length C-CS1 OUTPUT C-CS1 INPUT) 4.777)
(= (path-length C-CS1 OUTPUT C-CS2 INPUT) 11.814)
(= (path-length C-CS1 OUTPUT C-CS2 OUTPUT) 10.281)
(= (path-length C-CS1 OUTPUT C-DS INPUT) 8.210)
(= (path-length C-CS1 OUTPUT C-DS OUTPUT) 5.185)
(= (path-length C-CS1 OUTPUT C-RS1 INPUT) 2.128)
(= (path-length C-CS1 OUTPUT C-RS1 OUTPUT) 2.907)
(= (path-length C-CS1 OUTPUT C-RS2 INPUT) 7.064)
(= (path-length C-CS1 OUTPUT C-RS2 OUTPUT) 8.064)
(= (path-length C-CS2 INPUT C-BS INPUT) 1.734)
(= (path-length C-CS2 INPUT C-BS OUTPUT) 2.540)
(= (path-length C-CS2 INPUT C-CS1 INPUT) 9.581)
(= (path-length C-CS2 INPUT C-CS1 OUTPUT) 11.814)
(= (path-length C-CS2 INPUT C-CS2 OUTPUT) 2.554)
(= (path-length C-CS2 INPUT C-DS INPUT) 5.454)
(= (path-length C-CS2 INPUT C-DS OUTPUT) 7.656)
(= (path-length C-CS2 INPUT C-RS1 INPUT) 11.398)
(= (path-length C-CS2 INPUT C-RS1 OUTPUT) 13.363)
(= (path-length C-CS2 INPUT C-RS2 INPUT) 7.057)
(= (path-length C-CS2 INPUT C-RS2 OUTPUT) 4.980)
(= (path-length C-CS2 OUTPUT C-BS INPUT) 1.268)
(= (path-length C-CS2 OUTPUT C-BS OUTPUT) 3.483)
(= (path-length C-CS2 OUTPUT C-CS1 INPUT) 7.806)
(= (path-length C-CS2 OUTPUT C-CS1 OUTPUT) 10.281)
(= (path-length C-CS2 OUTPUT C-CS2 INPUT) 2.554)
(= (path-length C-CS2 OUTPUT C-DS INPUT) 5.029)
(= (path-length C-CS2 OUTPUT C-DS OUTPUT) 6.823)
(= (path-length C-CS2 OUTPUT C-RS1 INPUT) 9.864)
(= (path-length C-CS2 OUTPUT C-RS1 OUTPUT) 11.589)
(= (path-length C-CS2 OUTPUT C-RS2 INPUT) 5.282)
(= (path-length C-CS2 OUTPUT C-RS2 OUTPUT) 3.205)
(= (path-length C-DS INPUT C-BS INPUT) 5.518)
(= (path-length C-DS INPUT C-BS OUTPUT) 6.113)
(= (path-length C-DS INPUT C-CS1 INPUT) 5.772)
(= (path-length C-DS INPUT C-CS1 OUTPUT) 8.210)
(= (path-length C-DS INPUT C-CS2 INPUT) 5.454)
(= (path-length C-DS INPUT C-CS2 OUTPUT) 5.029)
(= (path-length C-DS INPUT C-DS OUTPUT) 4.051)
(= (path-length C-DS INPUT C-RS1 INPUT) 7.793)
(= (path-length C-DS INPUT C-RS1 OUTPUT) 10.076)
(= (path-length C-DS INPUT C-RS2 INPUT) 5.019)
(= (path-length C-DS INPUT C-RS2 OUTPUT) 2.812)
(= (path-length C-DS OUTPUT C-BS INPUT) 7.312)
(= (path-length C-DS OUTPUT C-BS OUTPUT) 7.908)
(= (path-length C-DS OUTPUT C-CS1 INPUT) 4.359)
(= (path-length C-DS OUTPUT C-CS1 OUTPUT) 5.185)
(= (path-length C-DS OUTPUT C-CS2 INPUT) 7.656)
(= (path-length C-DS OUTPUT C-CS2 OUTPUT) 6.823)
(= (path-length C-DS OUTPUT C-DS INPUT) 4.051)
(= (path-length C-DS OUTPUT C-RS1 INPUT) 4.768)
(= (path-length C-DS OUTPUT C-RS1 OUTPUT) 7.051)
(= (path-length C-DS OUTPUT C-RS2 INPUT) 3.606)
(= (path-length C-DS OUTPUT C-RS2 OUTPUT) 4.606)
(= (path-length C-RS1 INPUT C-BS INPUT) 10.354)
(= (path-length C-RS1 INPUT C-BS OUTPUT) 10.949)
(= (path-length C-RS1 INPUT C-CS1 INPUT) 4.360)
(= (path-length C-RS1 INPUT C-CS1 OUTPUT) 2.128)
(= (path-length C-RS1 INPUT C-CS2 INPUT) 11.398)
(= (path-length C-RS1 INPUT C-CS2 OUTPUT) 9.864)
(= (path-length C-RS1 INPUT C-DS INPUT) 7.793)
(= (path-length C-RS1 INPUT C-DS OUTPUT) 4.768)
(= (path-length C-RS1 INPUT C-RS1 OUTPUT) 2.848)
(= (path-length C-RS1 INPUT C-RS2 INPUT) 6.647)
(= (path-length C-RS1 INPUT C-RS2 OUTPUT) 7.647)
(= (path-length C-RS1 OUTPUT C-BS INPUT) 12.078)
(= (path-length C-RS1 OUTPUT C-BS OUTPUT) 12.673)
(= (path-length C-RS1 OUTPUT C-CS1 INPUT) 5.958)
(= (path-length C-RS1 OUTPUT C-CS1 OUTPUT) 2.907)
(= (path-length C-RS1 OUTPUT C-CS2 INPUT) 13.363)
(= (path-length C-RS1 OUTPUT C-CS2 OUTPUT) 11.589)
(= (path-length C-RS1 OUTPUT C-DS INPUT) 10.076)
(= (path-length C-RS1 OUTPUT C-DS OUTPUT) 7.051)
(= (path-length C-RS1 OUTPUT C-RS1 INPUT) 2.848)
(= (path-length C-RS1 OUTPUT C-RS2 INPUT) 8.046)
(= (path-length C-RS1 OUTPUT C-RS2 OUTPUT) 9.930)
(= (path-length C-RS2 INPUT C-BS INPUT) 5.771)
(= (path-length C-RS2 INPUT C-BS OUTPUT) 6.367)
(= (path-length C-RS2 INPUT C-CS1 INPUT) 4.589)
(= (path-length C-RS2 INPUT C-CS1 OUTPUT) 7.064)
(= (path-length C-RS2 INPUT C-CS2 INPUT) 7.057)
(= (path-length C-RS2 INPUT C-CS2 OUTPUT) 5.282)
(= (path-length C-RS2 INPUT C-DS INPUT) 5.019)
(= (path-length C-RS2 INPUT C-DS OUTPUT) 3.606)
(= (path-length C-RS2 INPUT C-RS1 INPUT) 6.647)
(= (path-length C-RS2 INPUT C-RS1 OUTPUT) 8.046)
(= (path-length C-RS2 INPUT C-RS2 OUTPUT) 4.836)
(= (path-length C-RS2 OUTPUT C-BS INPUT) 3.694)
(= (path-length C-RS2 OUTPUT C-BS OUTPUT) 4.290)
(= (path-length C-RS2 OUTPUT C-CS1 INPUT) 5.589)
(= (path-length C-RS2 OUTPUT C-CS1 OUTPUT) 8.064)
(= (path-length C-RS2 OUTPUT C-CS2 INPUT) 4.980)
(= (path-length C-RS2 OUTPUT C-CS2 OUTPUT) 3.205)
(= (path-length C-RS2 OUTPUT C-DS INPUT) 2.812)
(= (path-length C-RS2 OUTPUT C-DS OUTPUT) 4.606)
(= (path-length C-RS2 OUTPUT C-RS1 INPUT) 7.647)
(= (path-length C-RS2 OUTPUT C-RS1 OUTPUT) 9.930)
(= (path-length C-RS2 OUTPUT C-RS2 INPUT) 4.836)
(= (path-length START INPUT C-BS INPUT) 2.729)
(= (path-length START INPUT C-BS OUTPUT) 2.131)
(= (path-length START INPUT C-CS1 INPUT) 7.647)
(= (path-length START INPUT C-CS1 OUTPUT) 10.122)
(= (path-length START INPUT C-CS2 INPUT) 4.014)
(= (path-length START INPUT C-CS2 OUTPUT) 2.240)
(= (path-length START INPUT C-DS INPUT) 4.869)
(= (path-length START INPUT C-DS OUTPUT) 6.664)
(= (path-length START INPUT C-RS1 INPUT) 9.705)
(= (path-length START INPUT C-RS1 OUTPUT) 11.430)
(= (path-length START INPUT C-RS2 INPUT) 5.123)
(= (path-length START INPUT C-RS2 OUTPUT) 3.046)

	)

	(:goal (order-fulfilled o1))
)
    