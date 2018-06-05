
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
         (order-cap-color o1 CAP_GREY)
         (order-gate o1 GATE-3)
      

	 ; C0 order
	 ;(order-complexity o1 c0)
	 ;(order-base-color o1 BASE_BLACK)
	 ;(order-cap-color o1 CAP_GREY)
	 ;(order-gate o1 GATE-1)

	 ; These values are for the static default world
   (= (path-length C-BS INPUT C-BS OUTPUT) 2.851)
(= (path-length C-BS INPUT C-CS1 INPUT) 9.902)
(= (path-length C-BS INPUT C-CS1 OUTPUT) 7.140)
(= (path-length C-BS INPUT C-CS2 INPUT) 2.920)
(= (path-length C-BS INPUT C-CS2 OUTPUT) 6.618)
(= (path-length C-BS INPUT C-DS INPUT) 7.973)
(= (path-length C-BS INPUT C-DS OUTPUT) 6.608)
(= (path-length C-BS INPUT C-RS1 INPUT) 12.179)
(= (path-length C-BS INPUT C-RS1 OUTPUT) 12.629)
(= (path-length C-BS INPUT C-RS2 INPUT) 2.608)
(= (path-length C-BS INPUT C-RS2 OUTPUT) 3.114)
(= (path-length C-BS OUTPUT C-BS INPUT) 2.851)
(= (path-length C-BS OUTPUT C-CS1 INPUT) 8.689)
(= (path-length C-BS OUTPUT C-CS1 OUTPUT) 5.927)
(= (path-length C-BS OUTPUT C-CS2 INPUT) 3.443)
(= (path-length C-BS OUTPUT C-CS2 OUTPUT) 5.435)
(= (path-length C-BS OUTPUT C-DS INPUT) 6.760)
(= (path-length C-BS OUTPUT C-DS OUTPUT) 6.481)
(= (path-length C-BS OUTPUT C-RS1 INPUT) 10.966)
(= (path-length C-BS OUTPUT C-RS1 OUTPUT) 11.416)
(= (path-length C-BS OUTPUT C-RS2 INPUT) 5.094)
(= (path-length C-BS OUTPUT C-RS2 OUTPUT) 3.637)
(= (path-length C-CS1 INPUT C-BS INPUT) 9.902)
(= (path-length C-CS1 INPUT C-BS OUTPUT) 8.689)
(= (path-length C-CS1 INPUT C-CS1 OUTPUT) 4.902)
(= (path-length C-CS1 INPUT C-CS2 INPUT) 8.929)
(= (path-length C-CS1 INPUT C-CS2 OUTPUT) 6.257)
(= (path-length C-CS1 INPUT C-DS INPUT) 4.488)
(= (path-length C-CS1 INPUT C-DS OUTPUT) 7.025)
(= (path-length C-CS1 INPUT C-RS1 INPUT) 5.407)
(= (path-length C-CS1 INPUT C-RS1 OUTPUT) 5.445)
(= (path-length C-CS1 INPUT C-RS2 INPUT) 10.340)
(= (path-length C-CS1 INPUT C-RS2 OUTPUT) 8.903)
(= (path-length C-CS1 OUTPUT C-BS INPUT) 7.140)
(= (path-length C-CS1 OUTPUT C-BS OUTPUT) 5.927)
(= (path-length C-CS1 OUTPUT C-CS1 INPUT) 4.902)
(= (path-length C-CS1 OUTPUT C-CS2 INPUT) 6.167)
(= (path-length C-CS1 OUTPUT C-CS2 OUTPUT) 4.703)
(= (path-length C-CS1 OUTPUT C-DS INPUT) 2.972)
(= (path-length C-CS1 OUTPUT C-DS OUTPUT) 5.509)
(= (path-length C-CS1 OUTPUT C-RS1 INPUT) 7.178)
(= (path-length C-CS1 OUTPUT C-RS1 OUTPUT) 7.628)
(= (path-length C-CS1 OUTPUT C-RS2 INPUT) 7.818)
(= (path-length C-CS1 OUTPUT C-RS2 OUTPUT) 6.361)
(= (path-length C-CS2 INPUT C-BS INPUT) 2.920)
(= (path-length C-CS2 INPUT C-BS OUTPUT) 3.443)
(= (path-length C-CS2 INPUT C-CS1 INPUT) 8.929)
(= (path-length C-CS2 INPUT C-CS1 OUTPUT) 6.167)
(= (path-length C-CS2 INPUT C-CS2 OUTPUT) 4.187)
(= (path-length C-CS2 INPUT C-DS INPUT) 6.860)
(= (path-length C-CS2 INPUT C-DS OUTPUT) 4.177)
(= (path-length C-CS2 INPUT C-RS1 INPUT) 10.735)
(= (path-length C-CS2 INPUT C-RS1 OUTPUT) 11.185)
(= (path-length C-CS2 INPUT C-RS2 INPUT) 3.571)
(= (path-length C-CS2 INPUT C-RS2 OUTPUT) 0.683)
(= (path-length C-CS2 OUTPUT C-BS INPUT) 6.618)
(= (path-length C-CS2 OUTPUT C-BS OUTPUT) 5.435)
(= (path-length C-CS2 OUTPUT C-CS1 INPUT) 6.257)
(= (path-length C-CS2 OUTPUT C-CS1 OUTPUT) 4.703)
(= (path-length C-CS2 OUTPUT C-CS2 INPUT) 4.187)
(= (path-length C-CS2 OUTPUT C-DS INPUT) 4.327)
(= (path-length C-CS2 OUTPUT C-DS OUTPUT) 1.918)
(= (path-length C-CS2 OUTPUT C-RS1 INPUT) 8.476)
(= (path-length C-CS2 OUTPUT C-RS1 OUTPUT) 8.926)
(= (path-length C-CS2 OUTPUT C-RS2 INPUT) 4.955)
(= (path-length C-CS2 OUTPUT C-RS2 OUTPUT) 3.519)
(= (path-length C-DS INPUT C-BS INPUT) 7.973)
(= (path-length C-DS INPUT C-BS OUTPUT) 6.760)
(= (path-length C-DS INPUT C-CS1 INPUT) 4.488)
(= (path-length C-DS INPUT C-CS1 OUTPUT) 2.972)
(= (path-length C-DS INPUT C-CS2 INPUT) 6.860)
(= (path-length C-DS INPUT C-CS2 OUTPUT) 4.327)
(= (path-length C-DS INPUT C-DS OUTPUT) 3.084)
(= (path-length C-DS INPUT C-RS1 INPUT) 5.862)
(= (path-length C-DS INPUT C-RS1 OUTPUT) 6.312)
(= (path-length C-DS INPUT C-RS2 INPUT) 7.628)
(= (path-length C-DS INPUT C-RS2 OUTPUT) 6.192)
(= (path-length C-DS OUTPUT C-BS INPUT) 6.608)
(= (path-length C-DS OUTPUT C-BS OUTPUT) 6.481)
(= (path-length C-DS OUTPUT C-CS1 INPUT) 7.025)
(= (path-length C-DS OUTPUT C-CS1 OUTPUT) 5.509)
(= (path-length C-DS OUTPUT C-CS2 INPUT) 4.177)
(= (path-length C-DS OUTPUT C-CS2 OUTPUT) 1.918)
(= (path-length C-DS OUTPUT C-DS INPUT) 3.084)
(= (path-length C-DS OUTPUT C-RS1 INPUT) 6.958)
(= (path-length C-DS OUTPUT C-RS1 OUTPUT) 7.408)
(= (path-length C-DS OUTPUT C-RS2 INPUT) 4.945)
(= (path-length C-DS OUTPUT C-RS2 OUTPUT) 3.509)
(= (path-length C-RS1 INPUT C-BS INPUT) 12.179)
(= (path-length C-RS1 INPUT C-BS OUTPUT) 10.966)
(= (path-length C-RS1 INPUT C-CS1 INPUT) 5.407)
(= (path-length C-RS1 INPUT C-CS1 OUTPUT) 7.178)
(= (path-length C-RS1 INPUT C-CS2 INPUT) 10.735)
(= (path-length C-RS1 INPUT C-CS2 OUTPUT) 8.476)
(= (path-length C-RS1 INPUT C-DS INPUT) 5.862)
(= (path-length C-RS1 INPUT C-DS OUTPUT) 6.958)
(= (path-length C-RS1 INPUT C-RS1 OUTPUT) 2.976)
(= (path-length C-RS1 INPUT C-RS2 INPUT) 11.503)
(= (path-length C-RS1 INPUT C-RS2 OUTPUT) 10.067)
(= (path-length C-RS1 OUTPUT C-BS INPUT) 12.629)
(= (path-length C-RS1 OUTPUT C-BS OUTPUT) 11.416)
(= (path-length C-RS1 OUTPUT C-CS1 INPUT) 5.445)
(= (path-length C-RS1 OUTPUT C-CS1 OUTPUT) 7.628)
(= (path-length C-RS1 OUTPUT C-CS2 INPUT) 11.185)
(= (path-length C-RS1 OUTPUT C-CS2 OUTPUT) 8.926)
(= (path-length C-RS1 OUTPUT C-DS INPUT) 6.312)
(= (path-length C-RS1 OUTPUT C-DS OUTPUT) 7.408)
(= (path-length C-RS1 OUTPUT C-RS1 INPUT) 2.976)
(= (path-length C-RS1 OUTPUT C-RS2 INPUT) 11.953)
(= (path-length C-RS1 OUTPUT C-RS2 OUTPUT) 10.517)
(= (path-length C-RS2 INPUT C-BS INPUT) 2.608)
(= (path-length C-RS2 INPUT C-BS OUTPUT) 5.094)
(= (path-length C-RS2 INPUT C-CS1 INPUT) 10.340)
(= (path-length C-RS2 INPUT C-CS1 OUTPUT) 7.818)
(= (path-length C-RS2 INPUT C-CS2 INPUT) 3.571)
(= (path-length C-RS2 INPUT C-CS2 OUTPUT) 4.955)
(= (path-length C-RS2 INPUT C-DS INPUT) 7.628)
(= (path-length C-RS2 INPUT C-DS OUTPUT) 4.945)
(= (path-length C-RS2 INPUT C-RS1 INPUT) 11.503)
(= (path-length C-RS2 INPUT C-RS1 OUTPUT) 11.953)
(= (path-length C-RS2 INPUT C-RS2 OUTPUT) 2.903)
(= (path-length C-RS2 OUTPUT C-BS INPUT) 3.114)
(= (path-length C-RS2 OUTPUT C-BS OUTPUT) 3.637)
(= (path-length C-RS2 OUTPUT C-CS1 INPUT) 8.903)
(= (path-length C-RS2 OUTPUT C-CS1 OUTPUT) 6.361)
(= (path-length C-RS2 OUTPUT C-CS2 INPUT) 0.683)
(= (path-length C-RS2 OUTPUT C-CS2 OUTPUT) 3.519)
(= (path-length C-RS2 OUTPUT C-DS INPUT) 6.192)
(= (path-length C-RS2 OUTPUT C-DS OUTPUT) 3.509)
(= (path-length C-RS2 OUTPUT C-RS1 INPUT) 10.067)
(= (path-length C-RS2 OUTPUT C-RS1 OUTPUT) 10.517)
(= (path-length C-RS2 OUTPUT C-RS2 INPUT) 2.903)
(= (path-length START INPUT C-BS INPUT) 3.737)
(= (path-length START INPUT C-BS OUTPUT) 1.461)
(= (path-length START INPUT C-CS1 INPUT) 8.150)
(= (path-length START INPUT C-CS1 OUTPUT) 5.388)
(= (path-length START INPUT C-CS2 INPUT) 3.275)
(= (path-length START INPUT C-CS2 OUTPUT) 4.896)
(= (path-length START INPUT C-DS INPUT) 6.221)
(= (path-length START INPUT C-DS OUTPUT) 5.942)
(= (path-length START INPUT C-RS1 INPUT) 10.427)
(= (path-length START INPUT C-RS1 OUTPUT) 10.877)
(= (path-length START INPUT C-RS2 INPUT) 4.926)
(= (path-length START INPUT C-RS2 OUTPUT) 3.469)

	)

	(:goal (order-fulfilled o1))
)
    