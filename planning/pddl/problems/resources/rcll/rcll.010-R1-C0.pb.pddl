
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
         (order-base-color o1 BASE_BLACK)
         (order-cap-color o1 CAP_BLACK)
         (order-gate o1 GATE-3)
      

	 ; C0 order
	 ;(order-complexity o1 c0)
	 ;(order-base-color o1 BASE_BLACK)
	 ;(order-cap-color o1 CAP_GREY)
	 ;(order-gate o1 GATE-1)

	 ; These values are for the static default world
   (= (path-length C-BS INPUT C-BS OUTPUT) 2.727)
(= (path-length C-BS INPUT C-CS1 INPUT) 5.590)
(= (path-length C-BS INPUT C-CS1 OUTPUT) 5.757)
(= (path-length C-BS INPUT C-CS2 INPUT) 6.314)
(= (path-length C-BS INPUT C-CS2 OUTPUT) 7.379)
(= (path-length C-BS INPUT C-DS INPUT) 5.438)
(= (path-length C-BS INPUT C-DS OUTPUT) 7.995)
(= (path-length C-BS INPUT C-RS1 INPUT) 0.724)
(= (path-length C-BS INPUT C-RS1 OUTPUT) 3.576)
(= (path-length C-BS INPUT C-RS2 INPUT) 11.349)
(= (path-length C-BS INPUT C-RS2 OUTPUT) 10.372)
(= (path-length C-BS OUTPUT C-BS INPUT) 2.727)
(= (path-length C-BS OUTPUT C-CS1 INPUT) 5.841)
(= (path-length C-BS OUTPUT C-CS1 OUTPUT) 6.878)
(= (path-length C-BS OUTPUT C-CS2 INPUT) 6.565)
(= (path-length C-BS OUTPUT C-CS2 OUTPUT) 7.630)
(= (path-length C-BS OUTPUT C-DS INPUT) 5.690)
(= (path-length C-BS OUTPUT C-DS OUTPUT) 8.246)
(= (path-length C-BS OUTPUT C-RS1 INPUT) 2.063)
(= (path-length C-BS OUTPUT C-RS1 OUTPUT) 4.697)
(= (path-length C-BS OUTPUT C-RS2 INPUT) 11.600)
(= (path-length C-BS OUTPUT C-RS2 OUTPUT) 10.623)
(= (path-length C-CS1 INPUT C-BS INPUT) 5.590)
(= (path-length C-CS1 INPUT C-BS OUTPUT) 5.841)
(= (path-length C-CS1 INPUT C-CS1 OUTPUT) 3.275)
(= (path-length C-CS1 INPUT C-CS2 INPUT) 5.881)
(= (path-length C-CS1 INPUT C-CS2 OUTPUT) 7.919)
(= (path-length C-CS1 INPUT C-DS INPUT) 0.736)
(= (path-length C-CS1 INPUT C-DS OUTPUT) 2.844)
(= (path-length C-CS1 INPUT C-RS1 INPUT) 5.859)
(= (path-length C-CS1 INPUT C-RS1 OUTPUT) 4.197)
(= (path-length C-CS1 INPUT C-RS2 INPUT) 9.704)
(= (path-length C-CS1 INPUT C-RS2 OUTPUT) 8.707)
(= (path-length C-CS1 OUTPUT C-BS INPUT) 5.757)
(= (path-length C-CS1 OUTPUT C-BS OUTPUT) 6.878)
(= (path-length C-CS1 OUTPUT C-CS1 INPUT) 3.275)
(= (path-length C-CS1 OUTPUT C-CS2 INPUT) 8.079)
(= (path-length C-CS1 OUTPUT C-CS2 OUTPUT) 9.775)
(= (path-length C-CS1 OUTPUT C-DS INPUT) 3.572)
(= (path-length C-CS1 OUTPUT C-DS OUTPUT) 4.622)
(= (path-length C-CS1 OUTPUT C-RS1 INPUT) 5.092)
(= (path-length C-CS1 OUTPUT C-RS1 OUTPUT) 4.859)
(= (path-length C-CS1 OUTPUT C-RS2 INPUT) 11.902)
(= (path-length C-CS1 OUTPUT C-RS2 OUTPUT) 10.485)
(= (path-length C-CS2 INPUT C-BS INPUT) 6.314)
(= (path-length C-CS2 INPUT C-BS OUTPUT) 6.565)
(= (path-length C-CS2 INPUT C-CS1 INPUT) 5.881)
(= (path-length C-CS2 INPUT C-CS1 OUTPUT) 8.079)
(= (path-length C-CS2 INPUT C-CS2 OUTPUT) 2.585)
(= (path-length C-CS2 INPUT C-DS INPUT) 5.729)
(= (path-length C-CS2 INPUT C-DS OUTPUT) 4.566)
(= (path-length C-CS2 INPUT C-RS1 INPUT) 6.583)
(= (path-length C-CS2 INPUT C-RS1 OUTPUT) 6.133)
(= (path-length C-CS2 INPUT C-RS2 INPUT) 7.491)
(= (path-length C-CS2 INPUT C-RS2 OUTPUT) 6.514)
(= (path-length C-CS2 OUTPUT C-BS INPUT) 7.379)
(= (path-length C-CS2 OUTPUT C-BS OUTPUT) 7.630)
(= (path-length C-CS2 OUTPUT C-CS1 INPUT) 7.919)
(= (path-length C-CS2 OUTPUT C-CS1 OUTPUT) 9.775)
(= (path-length C-CS2 OUTPUT C-CS2 INPUT) 2.585)
(= (path-length C-CS2 OUTPUT C-DS INPUT) 7.768)
(= (path-length C-CS2 OUTPUT C-DS OUTPUT) 6.604)
(= (path-length C-CS2 OUTPUT C-RS1 INPUT) 7.649)
(= (path-length C-CS2 OUTPUT C-RS1 OUTPUT) 7.830)
(= (path-length C-CS2 OUTPUT C-RS2 INPUT) 5.608)
(= (path-length C-CS2 OUTPUT C-RS2 OUTPUT) 4.630)
(= (path-length C-DS INPUT C-BS INPUT) 5.438)
(= (path-length C-DS INPUT C-BS OUTPUT) 5.690)
(= (path-length C-DS INPUT C-CS1 INPUT) 0.736)
(= (path-length C-DS INPUT C-CS1 OUTPUT) 3.572)
(= (path-length C-DS INPUT C-CS2 INPUT) 5.729)
(= (path-length C-DS INPUT C-CS2 OUTPUT) 7.768)
(= (path-length C-DS INPUT C-DS OUTPUT) 3.141)
(= (path-length C-DS INPUT C-RS1 INPUT) 5.708)
(= (path-length C-DS INPUT C-RS1 OUTPUT) 4.046)
(= (path-length C-DS INPUT C-RS2 INPUT) 9.553)
(= (path-length C-DS INPUT C-RS2 OUTPUT) 8.576)
(= (path-length C-DS OUTPUT C-BS INPUT) 7.995)
(= (path-length C-DS OUTPUT C-BS OUTPUT) 8.246)
(= (path-length C-DS OUTPUT C-CS1 INPUT) 2.844)
(= (path-length C-DS OUTPUT C-CS1 OUTPUT) 4.622)
(= (path-length C-DS OUTPUT C-CS2 INPUT) 4.566)
(= (path-length C-DS OUTPUT C-CS2 OUTPUT) 6.604)
(= (path-length C-DS OUTPUT C-DS INPUT) 3.141)
(= (path-length C-DS OUTPUT C-RS1 INPUT) 8.264)
(= (path-length C-DS OUTPUT C-RS1 OUTPUT) 6.602)
(= (path-length C-DS OUTPUT C-RS2 INPUT) 7.750)
(= (path-length C-DS OUTPUT C-RS2 OUTPUT) 6.314)
(= (path-length C-RS1 INPUT C-BS INPUT) 0.724)
(= (path-length C-RS1 INPUT C-BS OUTPUT) 2.063)
(= (path-length C-RS1 INPUT C-CS1 INPUT) 5.859)
(= (path-length C-RS1 INPUT C-CS1 OUTPUT) 5.092)
(= (path-length C-RS1 INPUT C-CS2 INPUT) 6.583)
(= (path-length C-RS1 INPUT C-CS2 OUTPUT) 7.649)
(= (path-length C-RS1 INPUT C-DS INPUT) 5.708)
(= (path-length C-RS1 INPUT C-DS OUTPUT) 8.264)
(= (path-length C-RS1 INPUT C-RS1 OUTPUT) 2.911)
(= (path-length C-RS1 INPUT C-RS2 INPUT) 11.618)
(= (path-length C-RS1 INPUT C-RS2 OUTPUT) 10.641)
(= (path-length C-RS1 OUTPUT C-BS INPUT) 3.576)
(= (path-length C-RS1 OUTPUT C-BS OUTPUT) 4.697)
(= (path-length C-RS1 OUTPUT C-CS1 INPUT) 4.197)
(= (path-length C-RS1 OUTPUT C-CS1 OUTPUT) 4.859)
(= (path-length C-RS1 OUTPUT C-CS2 INPUT) 6.133)
(= (path-length C-RS1 OUTPUT C-CS2 OUTPUT) 7.830)
(= (path-length C-RS1 OUTPUT C-DS INPUT) 4.046)
(= (path-length C-RS1 OUTPUT C-DS OUTPUT) 6.602)
(= (path-length C-RS1 OUTPUT C-RS1 INPUT) 2.911)
(= (path-length C-RS1 OUTPUT C-RS2 INPUT) 9.957)
(= (path-length C-RS1 OUTPUT C-RS2 OUTPUT) 8.980)
(= (path-length C-RS2 INPUT C-BS INPUT) 11.349)
(= (path-length C-RS2 INPUT C-BS OUTPUT) 11.600)
(= (path-length C-RS2 INPUT C-CS1 INPUT) 9.704)
(= (path-length C-RS2 INPUT C-CS1 OUTPUT) 11.902)
(= (path-length C-RS2 INPUT C-CS2 INPUT) 7.491)
(= (path-length C-RS2 INPUT C-CS2 OUTPUT) 5.608)
(= (path-length C-RS2 INPUT C-DS INPUT) 9.553)
(= (path-length C-RS2 INPUT C-DS OUTPUT) 7.750)
(= (path-length C-RS2 INPUT C-RS1 INPUT) 11.618)
(= (path-length C-RS2 INPUT C-RS1 OUTPUT) 9.957)
(= (path-length C-RS2 INPUT C-RS2 OUTPUT) 3.639)
(= (path-length C-RS2 OUTPUT C-BS INPUT) 10.372)
(= (path-length C-RS2 OUTPUT C-BS OUTPUT) 10.623)
(= (path-length C-RS2 OUTPUT C-CS1 INPUT) 8.707)
(= (path-length C-RS2 OUTPUT C-CS1 OUTPUT) 10.485)
(= (path-length C-RS2 OUTPUT C-CS2 INPUT) 6.514)
(= (path-length C-RS2 OUTPUT C-CS2 OUTPUT) 4.630)
(= (path-length C-RS2 OUTPUT C-DS INPUT) 8.576)
(= (path-length C-RS2 OUTPUT C-DS OUTPUT) 6.314)
(= (path-length C-RS2 OUTPUT C-RS1 INPUT) 10.641)
(= (path-length C-RS2 OUTPUT C-RS1 OUTPUT) 8.980)
(= (path-length C-RS2 OUTPUT C-RS2 INPUT) 3.639)
(= (path-length START INPUT C-BS INPUT) 2.828)
(= (path-length START INPUT C-BS OUTPUT) 2.038)
(= (path-length START INPUT C-CS1 INPUT) 4.705)
(= (path-length START INPUT C-CS1 OUTPUT) 6.066)
(= (path-length START INPUT C-CS2 INPUT) 5.430)
(= (path-length START INPUT C-CS2 OUTPUT) 6.495)
(= (path-length START INPUT C-DS INPUT) 4.554)
(= (path-length START INPUT C-DS OUTPUT) 7.110)
(= (path-length START INPUT C-RS1 INPUT) 3.097)
(= (path-length START INPUT C-RS1 OUTPUT) 4.120)
(= (path-length START INPUT C-RS2 INPUT) 10.465)
(= (path-length START INPUT C-RS2 OUTPUT) 9.487)

	)

	(:goal (order-fulfilled o1))
)
    