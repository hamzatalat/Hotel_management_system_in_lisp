(load "Guest.lisp")
(load "room.lisp")
(load "booking.lisp")
(defun dateToDayNumber (day month)
	(cond(
		(or (< month 1) (> month 12) (< day 1) (> day 31))
			(return-from dateToDayNumber 0)
		)
   )
   
	(cond(
		(= month 1)
			(return-from dateToDayNumber day)
		)
	)
   (cond(
		(= month 2)
			(return-from dateToDayNumber (+ 31 day))
		)
	)
	(cond(
		(= month 3)
			(return-from dateToDayNumber (+ 59 day))
		)
	)
	(cond(
		(= month 4)
			(return-from dateToDayNumber (+ 90 day))
		)
	)
	(cond(
		(= month 5)
			(return-from dateToDayNumber (+ 120 day))
		)
	)
	(cond(
		(= month 6)
			(return-from dateToDayNumber (+ 151 day))
		)
	)
	(cond(
		(= month 7)
			(return-from dateToDayNumber (+ 181 day))
		)
	)
	(cond(
		(= month 8)
			(return-from dateToDayNumber (+ 212 day))
		)
	)
	(cond(
		(= month 9)
			(return-from dateToDayNumber (+ 243 day))
		)
	)
	(cond(
		(= month 10)
			(return-from dateToDayNumber (+ 273 day))
		)
	)
	(cond(
		(= month 11)
			(return-from dateToDayNumber (+ 304 day))
		)
	)
	(return-from dateToDayNumber (+ 334 day))
)
(defun dayNumberToMonth (dayNumber)
	(cond(
		(or (< dayNumber 1) (> dayNumber 365))
			(return-from dayNumberToMonth 0)
		)
   )
   
	(cond(
		(<= dayNumber 31)
			(return-from dayNumberToMonth 1) ;Jan
		)
	)
   (cond(
		(<= dayNumber 59)
			(return-from dayNumberToMonth 2)
		)
	)
	(cond(
		(<= dayNumber 90)
			(return-from dayNumberToMonth 3)
		)
	)
	(cond(
		(<= dayNumber 120)
			(return-from dayNumberToMonth 4)
		)
	)
	(cond(
		(<= dayNumber 151)
			(return-from dayNumberToMonth 5)
		)
	)
	(cond(
		(<= dayNumber 181)
			(return-from dayNumberToMonth 6)
		)
	)
	(cond(
		(<= dayNumber 212)
			(return-from dayNumberToMonth 7)
		)
	)
	(cond(
		(<= dayNumber 243)
			(return-from dayNumberToMonth 8)
		)
	)
	(cond(
		(<= dayNumber 273)
			(return-from dayNumberToMonth 9)
		)
	)
	(cond(
		(<= dayNumber 304)
			(return-from dayNumberToMonth 10)
		)
	)
	(cond(
		(<= dayNumber 334)
			(return-from dayNumberToMonth 11)
		)
	)
	(return-from dayNumberToMonth 12)
)
(defun dayNumberToDayofMonth(dayNumber)
	(cond(
		(or (< dayNumber 1) (> dayNumber 365))
			(return-from dayNumberToDayofMonth 0)
		)
   )
   
	(cond(
			(<= dayNumber 31)
			(return-from dayNumberToDayofMonth dayNumber)
		 ) ;Jan
	)
   (cond(
		(<= dayNumber 59)
			(return-from dayNumberToDayofMonth (- dayNumber 31))
		)
	)
	(cond(
		(<= dayNumber 90)
			(return-from dayNumberToDayofMonth (- dayNumber 59))
		)
	)
	(cond(
		(<= dayNumber 120)
			(return-from dayNumberToDayofMonth (- dayNumber 90))
		)
	)
	(cond(
		(<= dayNumber 151)
			(return-from dayNumberToDayofMonth (- dayNumber 120))
		)
	)
	(cond(
		(<= dayNumber 181)
			(return-from dayNumberToDayofMonth (- dayNumber 151))
		)
	)
	(cond(
		(<= dayNumber 212)
			(return-from dayNumberToDayofMonth (- dayNumber 181))
		)
	)
	(cond(
		(<= dayNumber 243)
			(return-from dayNumberToDayofMonth (- dayNumber 212))
		)
	)
	(cond(
		(<= dayNumber 273)
			(return-from dayNumberToDayofMonth (- dayNumber 243))
		)
	)
	(cond(
		(<= dayNumber 304)
			(return-from dayNumberToDayofMonth (- dayNumber 273))
		)
	)
	(cond(
		(<= dayNumber 334)
			(return-from dayNumberToDayofMonth (- dayNumber 304))
		)
	)
	(return-from dayNumberToDayofMonth (- dayNumber 334))

)
(setf guest-id 0)
(setq bookings (make-array 100 :fill-pointer 0 :adjustable t))
(setq rooms (make-array 100 :fill-pointer 0 :adjustable t))
(setq guests (make-array 100 :fill-pointer 0 :adjustable t))

(defun add-guest(name-of-guest)
	(setf guest-id (+ guest-id 1))
	
	(setf item (make-instance 'guest))
	(setf (guest-id item) guest-id)
	(setf (guest-name item) name-of-guest)
	
	(vector-push-extend item guests)
)
(defun add-room (room-num room-capacity)
	(setf temp-room (make-instance 'room))
	(setf (room-number temp-room) room-num)
	(setf (room-capacity temp-room) room-capacity)
	
	(setq current (make-array 100 :fill-pointer 0 :adjustable t))
	(setf already-present 0)
	(loop for x across rooms
		do(
			cond(
					(= room-num (room-number x))
					(setf already-present 1)
				)
			)
	)
	(cond(
			(= already-present 1)
			(format t "A room with same number already exists.~%")
		)
	)
	(cond(
			(/= already-present 1)
			(vector-push-extend temp-room rooms)
		)
	)
)
(defun add-booking (g r ng sd sm ed em)
	(setf guest-present 0)
	(setf room-present 0)
	(setf already-booked 0)
	(setf count-check 0)
	(setf s (dateToDayNumber sm sd))
	(setf e (dateToDayNumber em ed))
	
	(loop for x across guests
	   do
	   (cond(
				(= (guest-id x) g)
				(setf guest-present 1)
				(format t "======================~%")
				(break)
			)
		)
	)
	(loop for x across rooms
	   do
	   (cond(
				(= (room-number x) r)
				(setf room-present 1)
				(cond(
					(>= (room-capacity x) ng)
					(setf count-check 1)
					)
				)
				(break)
			)
		)
	)
	(loop for x across bookings
	   do
	   (cond(
				(= (room-number x) r)
				(cond(
						(and (>= s (start x)) (<= s (end x)))
						(format t "s:~%")
						(format t "Start: ~d~%" (start x))
						(format t "s:~%")
						(format t "End: ~d~%" (end x))
						(setf already-booked 1)
						(break)
					)
				)
				(cond(
						(and (>= e (start x)) (<= e (end x)))
						(format t "e:~%")
						(format t "Start: ~d~%" (start x))
						(format t "e:~%")
						(format t "End: ~d~%" (end x))
						(setf already-booked 1)
						(break)
					)
				)
			)
		)
	)
	(cond(
			(and (= already-booked 0) (= room-present 1) (= guest-present 1) (= count-check 1))
						
			(setf item (make-instance 'booking))
			(setf (booking-start-date item) s)
			(setf (booking-end-date item) e)
			(setf (booking-room-number item) r)
			(setf (booking-guest-id item) g)
			(setf (booking-people-count item) ng)
			
			(vector-push-extend item bookings)
			(format t "Booking added successfully.~%")
		)
	)
	(cond(
			(= room-present 0)
			(format t "Room not present.~%")
			(return-from add-booking nil)
		)
	)
	(cond(
			(= guest-present 0)
			(format t "Guest not found.~%")
			(return-from add-booking nil)
		)
	)
	(cond(
			(= already-booked 1)
			(format t "Already Booked.~%")
			(return-from add-booking nil)
		)
	)
	(cond(
			(= count-check 0)
			(format t "More People than room capacity.~%")
			(return-from add-booking nil)
		)
	)
)
(defun view-booking (type id)
	(cond(
			(= type 1)
			(setf checked 0)
			(loop for x across bookings
				do(
					cond(
							(= id (booking-guest-id x))
							(setf already-present 1)
							(format t "-----------------------------------~%")
							(format t "Guest ID: ~d~%" (booking-guest-id x))
							(format t "Room Number: ~d~%" (booking-room-number x))
							(format t "Number Of People: ~d~%" (booking-people-count x))
							(format t "Start Date: ~d ~d~%" 
				(dayNumberToDayofMonth (booking-start-date x)) 
				(dayNumberToMonth (booking-start-date x)))
							(format t "End Date: ~d ~d~%" 
				(dayNumberToDayofMonth (booking-end-date x)) 
				(dayNumberToMonth (booking-end-date x)))
							(format t "-----------------------------------~%")
							(setf checked 1)
						)
				)
			)
			(cond(
					(= checked 0)
					(format t "------------NO RECORD FOUND--------------~%")
				)
			)
		)
	)
	;Not Sure Below
	(cond(
			(= type 2)
			(setf checked 0)
			(loop for x across bookings
				do(
					cond(
							(= id (booking-room-number x))
							(setf already-present 1)
							(format t "-----------------------------------~%")
							(format t "Guest ID: ~d~%" (booking-guest-id x))
							(format t "Room Number: ~d~%" (booking-room-number x))
							(format t "Number Of People: ~d~%" (booking-people-count x))
							(format t "Start Date: ~d ~d~%" 
				(dayNumberToDayofMonth (booking-start-date x)) 
				(dayNumberToMonth (booking-start-date x)))
							(format t "End Date: ~d ~d~%" 
				(dayNumberToDayofMonth (booking-end-date x)) 
				(dayNumberToMonth (booking-end-date x)))
							(format t "-----------------------------------~%")
						)
						
					(format t "~d ~d" id (booking-room-id x))
				(setf checked 1)
				)
			)
		)
	)
)