;****************************************************************************
;  rcll_domain_production.pddl: RoboCup Logistics League Production Model
;
;  Created: Fri Feb 24 23:20:38 2017
;  Copyright  2017  Tim Niemueller [www.niemueller.de]
;****************************************************************************

;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU Library General Public License for more details.
;
;  Read the full text in the LICENSE.GPL file in the doc directory.

(define (domain rcll-production-durative)
	(:requirements :strips :typing :durative-actions :numeric-fluents)

	(:types
		robot - object
		team-color - object
		location - object
		mps - location
		mps-typename - object
		mps-statename - object
		mps-side - object
		base-color - object
		product-base-color - base-color
		cap-color - object
		product-cap-color - cap-color
		ring-color - object
		product-ring-color - ring-color
		ds-gate - object
		cs-operation - object
		cs-statename - object
		order - object
    order-complexity-value - object
		workpiece - object
		cap-carrier - workpiece
		shelf-spot - object
		ring-num - object
	)

	(:constants
		START - location
		BS CS DS RS - mps-typename
		IDLE BROKEN PREPARED PROCESSING PROCESSED WAIT-IDLE READY-AT-OUTPUT DOWN - mps-statename
		INPUT OUTPUT - mps-side
		BASE_NONE - base-color
		BASE_RED BASE_BLACK BASE_SILVER - product-base-color
		CAP_NONE - cap-color
		CAP_BLACK CAP_GREY - product-cap-color
		GATE-1 GATE-2 GATE-3 - ds-gate
		RING_NONE - ring-color
		RING_BLUE RING_GREEN RING_ORANGE RING_YELLOW - product-ring-color
		CS_RETRIEVE CS_MOUNT - cs-operation
		C0 C1 C2 C3 - order-complexity-value
		LEFT MIDDLE RIGHT - shelf-spot
		ZERO ONE TWO THREE - ring-num
	)

	(:predicates
		(at ?r - robot ?m - location ?side - mps-side)
		(holding ?r - robot ?wp - workpiece)
		(can-hold ?r - robot)
		(entered-field ?r - robot)
		(location-free ?l - location ?side - mps-side)
		(robot-waiting ?r - robot)
		(mps-type ?m - mps ?t - mps-typename)
		(mps-state ?m - mps ?s - mps-statename)
		(bs-prepared-color ?m - mps ?col - product-base-color)
		(bs-prepared-side ?m - mps ?side - mps-side)
		(rs-ring-spec ?m - mps ?r - product-ring-color ?rn - ring-num)
		(cs-can-perform ?m - mps ?op - cs-operation)
		(cs-prepared-for ?m - mps ?op - cs-operation)
		(cs-buffered ?m - mps ?col - cap-color)
		(cs-free ?m - mps)
		(rs-prepared-color ?m - mps ?col - product-ring-color)
		(rs-filled-with ?m - mps ?n - ring-num)
		(ds-prepared-gate ?m - mps ?g - ds-gate)
		; These must be static predicates stating the legal ring-num operations
		(rs-sub ?minuend ?subtrahend ?difference - ring-num)
		(rs-inc ?summand ?sum - ring-num)
		(order-complexity ?ord - order ?com - order-complexity-value)
		(order-base-color ?ord - order ?col - product-base-color)
		(order-ring1-color ?ord - order ?col - product-ring-color)
		(order-ring2-color ?ord - order ?col - product-ring-color)
		(order-ring3-color ?ord - order ?col - product-ring-color)
		(order-cap-color ?ord - order ?col - product-cap-color)
		(order-fulfilled ?ord - order)
		(order-gate ?ord - order ?gate - ds-gate)
		(wp-unused ?wp - workpiece)
		(wp-usable ?wp - workpiece)
		(wp-at ?wp - workpiece ?m - mps ?side - mps-side)
		(wp-base-color ?wp - workpiece ?col - base-color)
		(wp-ring1-color ?wp - workpiece ?col - ring-color)
		(wp-ring2-color ?wp - workpiece ?col - ring-color)
		(wp-ring3-color ?wp - workpiece ?col - ring-color)
		(wp-cap-color ?wp - workpiece ?col - cap-color)
		(wp-on-shelf ?wp - workpiece ?m - mps ?spot - shelf-spot)
	)

	(:functions
		(path-length ?from - location ?from-side - mps-side ?to - location ?to-side - mps-side)
		(order-delivery-begin ?ord - order)
		(order-delivery-end   ?ord - order)
	)

	(:action fulfill-orderX
		:parameters (
			?ord - order
			; ?wp - workpiece
			; ?m - mps 
			; ?g - ds-gate
		    ; ?basecol - product-base-color
			; ?capcol - product-cap-color
		    ?ring1col - product-ring-color
			)

		:precondition (and ;(wp-at ?wp ?m INPUT) (wp-usable ?wp)
											;  (mps-type ?m DS) (mps-state ?m PROCESSING) (ds-prepared-gate ?m ?g)
											;   (order-complexity ?ord C1) (order-gate ?ord ?g)
											; (order-base-color ?ord ?basecol) (wp-base-color ?wp ?basecol)
											(order-ring1-color ?ord ?ring1col);
											;   (wp-ring1-color ?wp ?ring1col)
											;  (order-cap-color ?ord ?capcol) 
											 ;(wp-cap-color ?wp ?capcol)
											)
		:effect (and 
			;(order-fulfilled ?ord) 
			; (not (wp-at ?wp ?m INPUT)) 
			; (not (ds-prepared-gate ?m ?g))
			; (not (wp-base-color ?wp ?basecol)) 
			; (not (wp-cap-color ?wp ?capcol))
			)
	)


