(deftemplate Goal
	(slot id (type NUMBER))
	(slot row)
	(slot column)
	(slot part-type)
	(slot pallet)
)
(deftemplate S1
	(slot row)
	(slot column)
	(slot part-type)
)
(deftemplate WorkingTable
	(slot row)
	(slot column)
	(slot part-type)
	(slot check-result)
)
(deftemplate PalletCompletion
	(slot pallet (type NUMBER))
	(slot completed)
	(slot available) 
)

(deftemplate MachineTool
	(slot program (type NUMBER))
	(slot part-to-be-transformed)
	(slot transformed-part)
)

(deffacts Conveyor
	(free-pallet 1 0) ;nr-pallet 1-full 0-empty
	(free-pallet 2 0)
	(free-pallet 3 0)
	(free-pallet 4 0)
	(free-pallet 5 0)
	(free-pallet 6 0)
	(free-pallet 7 0)
	(free-pallet 8 0)
	(free-pallet 9 0)
	(free-pallet 10 0)
)
(deffacts robots
	(robot IRB1400 free)
	(robot IRB2400 free)
)

(deffacts MT
;programs from MT
	(MachineTool (program 1) (part-to-be-transformed A) (transformed-part B))
	(MachineTool (program 2) (part-to-be-transformed A) (transformed-part C))
	(MachineTool (program 3) (part-to-be-transformed A) (transformed-part D))
	(MachineTool (program 4) (part-to-be-transformed A) (transformed-part E))
	(MachineTool (program 5) (part-to-be-transformed A) (transformed-part F))
	(MachineTool (program 6) (part-to-be-transformed B) (transformed-part C))
	(MachineTool (program 7) (part-to-be-transformed C) (transformed-part D))
	(MachineTool (program 8) (part-to-be-transformed D) (transformed-part E))
	(MachineTool (program 9) (part-to-be-transformed E) (transformed-part F))
)

(deffacts WT
;WT state at the be
	(WorkingTable (row 1) (column 1) (part-type A) (check-result incorrect))
	(WorkingTable (row 1) (column 2) (part-type B) (check-result correct))
	(WorkingTable (row 1) (column 3) (part-type F) (check-result correct))
	(WorkingTable (row 2) (column 1) (part-type D) (check-result correct))
	(WorkingTable (row 2) (column 2) (part-type E) (check-result correct))
	(WorkingTable (row 2) (column 3) (part-type F) (check-result correct))
	(WorkingTable (row 3) (column 1) (part-type A) (check-result correct))
	(WorkingTable (row 3) (column 2) (part-type B) (check-result correct))
	(WorkingTable (row 3) (column 3) (part-type C) (check-result correct))
)

(deffacts SD1
;6 rows and 9 columns
;S1
	(S1 (row 1) (column 1) (part-type A))
	(S1 (row 1) (column 2) (part-type C))
	(S1 (row 1) (column 3) (part-type B))
	(S1 (row 1) (column 4) (part-type A))
	(S1 (row 1) (column 5) (part-type C))
	(S1 (row 1) (column 6) (part-type B))
	(S1 (row 1) (column 7) (part-type C))
	(S1 (row 1) (column 8) (part-type E))
	(S1 (row 1) (column 9) (part-type D))
	(S1 (row 2) (column 1) (part-type B))
	(S1 (row 2) (column 2) (part-type D))
	(S1 (row 2) (column 3) (part-type C))
	(S1 (row 2) (column 4) (part-type B))
	(S1 (row 2) (column 5) (part-type D))
	(S1 (row 2) (column 6) (part-type C))
	(S1 (row 3) (column 1) (part-type C))
	(S1 (row 3) (column 2) (part-type E))
	(S1 (row 3) (column 3) (part-type A))
	(S1 (row 3) (column 4) (part-type C))
	(S1 (row 3) (column 5) (part-type B))
	(S1 (row 3) (column 6) (part-type B))
	(S1 (row 4) (column 1) (part-type D))
	(S1 (row 4) (column 2) (part-type A))
	(S1 (row 4) (column 3) (part-type A))
	(S1 (row 5) (column 1) (part-type D))
	(S1 (row 5) (column 2) (part-type C))
	(S1 (row 5) (column 3) (part-type B))
	(S1 (row 5) (column 4) (part-type E))
	(S1 (row 5) (column 5) (part-type A))
	(S1 (row 5) (column 6) (part-type C))
	(S1 (row 5) (column 7) (part-type B))
	(S1 (row 5) (column 8) (part-type D))
	(S1 (row 6) (column 2) (part-type C))
	(S1 (row 6) (column 3) (part-type A))
	(S1 (row 6) (column 4) (part-type A))
	(S1 (row 6) (column 5) (part-type E))
	(S1 (row 6) (column 6) (part-type D))
	(S1 (row 6) (column 7) (part-type C))
	(S1 (row 6) (column 8) (part-type B))
	(S1 (row 6) (column 9) (part-type A))
)

(deffacts ManufacturingGoal
;5 rows and 3 columns without the main element
;S2
	(Goal (id 1) (row 1) (column 1) (part-type B) (pallet 1))
	(Goal (id 1) (row 1) (column 2) (part-type C) (pallet 1))
	(Goal (id 1) (row 1) (column 3) (part-type D) (pallet 1))
	(Goal (id 1) (row 2) (column 1) (part-type E) (pallet 1))
	(Goal (id 1) (row 2) (column 2) (part-type F) (pallet 1))
	(Goal (id 1) (row 2) (column 3) (part-type B) (pallet 1))
	(Goal (id 1) (row 3) (column 1) (part-type C) (pallet 1))
	(Goal (id 1) (row 3) (column 3) (part-type B) (pallet 1))
	(Goal (id 1) (row 4) (column 1) (part-type B) (pallet 1))
	(Goal (id 1) (row 4) (column 2) (part-type F) (pallet 1))
	(Goal (id 1) (row 4) (column 3) (part-type C) (pallet 1))
	(Goal (id 1) (row 5) (column 1) (part-type B) (pallet 1))
	(Goal (id 1) (row 5) (column 2) (part-type F) (pallet 1))
	(Goal (id 1) (row 5) (column 3) (part-type F) (pallet 1))
	(Goal (id 1) (row 1) (column 1) (part-type C) (pallet 2))
	(Goal (id 1) (row 1) (column 2) (part-type B) (pallet 2))
	(Goal (id 1) (row 1) (column 3) (part-type C) (pallet 2))
	(Goal (id 1) (row 2) (column 1) (part-type E) (pallet 2))
	(Goal (id 1) (row 2) (column 2) (part-type E) (pallet 2))
	(Goal (id 1) (row 2) (column 3) (part-type E) (pallet 2))
	(Goal (id 1) (row 3) (column 1) (part-type E) (pallet 2))
	(Goal (id 1) (row 3) (column 3) (part-type D) (pallet 2))
	(Goal (id 1) (row 4) (column 1) (part-type B) (pallet 2))
	(Goal (id 1) (row 4) (column 2) (part-type D) (pallet 2))
	(Goal (id 1) (row 4) (column 3) (part-type D) (pallet 2))
	(Goal (id 1) (row 5) (column 1) (part-type D) (pallet 2))
	(Goal (id 1) (row 5) (column 2) (part-type B) (pallet 2))
	(Goal (id 1) (row 5) (column 3) (part-type C) (pallet 2))
	(Goal (id 1) (row 1) (column 1) (part-type D) (pallet 3))
	(Goal (id 1) (row 1) (column 2) (part-type E) (pallet 3))
	(Goal (id 1) (row 1) (column 3) (part-type B) (pallet 3))
	(Goal (id 1) (row 2) (column 1) (part-type B) (pallet 3))
	(Goal (id 1) (row 2) (column 2) (part-type B) (pallet 3))
	(Goal (id 1) (row 2) (column 3) (part-type C) (pallet 3))
	(Goal (id 1) (row 3) (column 1) (part-type C) (pallet 3))
	(Goal (id 1) (row 4) (column 1) (part-type D) (pallet 3))
	(Goal (id 1) (row 4) (column 3) (part-type E) (pallet 3))
	(Goal (id 1) (row 5) (column 1) (part-type E) (pallet 3))
	(Goal (id 1) (row 1) (column 1) (part-type E) (pallet 4))
	(Goal (id 1) (row 1) (column 2) (part-type E) (pallet 4))
	(Goal (id 1) (row 1) (column 3) (part-type E) (pallet 4))
	(Goal (id 1) (row 2) (column 1) (part-type F) (pallet 4))
	(Goal (id 1) (row 2) (column 2) (part-type F) (pallet 4))
	(Goal (id 1) (row 2) (column 3) (part-type F) (pallet 4))
	(Goal (id 1) (row 4) (column 1) (part-type F) (pallet 4))
	(Goal (id 1) (row 4) (column 2) (part-type B) (pallet 4))
	(Goal (id 1) (row 4) (column 3) (part-type D) (pallet 4))
	(Goal (id 1) (row 5) (column 1) (part-type C) (pallet 4))
)
(deffacts PalletCompletion
    (PalletCompletion (pallet 1) (completed false) (available true)) ;available is used for the top pallet 
	(PalletCompletion (pallet 2) (completed false) (available false))
	(PalletCompletion (pallet 3) (completed false) (available false))
	(PalletCompletion (pallet 4) (completed false) (available false))
)

(defrule remove-incorrect-parts
  ?incorrectPart <- (WorkingTable (row ?r) (column ?c) (part-type ?pt) (check-result incorrect))
  ?robot-action <- (robot IRB2400 free)
  =>
  (retract ?incorrectPart ?robot-action)
  (printout t "The IRB2400 robot place the part: " ?pt " in scrap box." crlf)
  (assert (robot IRB2400 free))
  (assert (WorkingTable (row ?r) (column ?c) (part-type null) (check-result null)))
)

;robotul IRB2400 se duce sa preia piesa de pe WT
(defrule retrieve-part-from-WT 
	?robot-action <- (robot IRB2400 free)
	(Goal (pallet ?p) (part-type ?part))
	(PalletCompletion (pallet ?p) (completed false) (available true))
	?wt <- (WorkingTable (row ?r) (column ?c) (part-type ?part) (check-result correct))
    =>
	(printout t "The IRB2400 robot moves to retrieve the part " ?part " from Working Table." crlf)
	(retract ?robot-action ?wt)
    (assert (robot IRB2400 retrieve-part-from-WT ?part))
	(assert (WorkingTable (row ?r) (column ?c) (part-type null) (check-result null)))
)

(defrule retrieve-part-from-WT-to-place-on-conv (declare (salience -10))
	?robot-action <- (robot IRB2400 free)
	(Goal (pallet ?p) (part-type ?part2))
	(MachineTool (program ?) (part-to-be-transformed ?part1) (transformed-part ?part2))
	(not (S1 (row ?r) (column ?c) (part-type ?part1|?part2)))
	?wt <- (WorkingTable (row ?r) (column ?c) (part-type ?part1) (check-result correct))
    =>
	(printout t "The IRB2400 robot moves to retrieve the part " ?part1 " from Working Table." crlf)
	(retract ?robot-action ?wt)
    (assert (robot IRB2400 retrieve-part-from-WT-to-place-on-conv  ?part1))
	(assert (WorkingTable (row ?r) (column ?c) (part-type null) (check-result null)))
)

(defrule place-on-conv-irb-2400 (declare (salience -10))
	?conveyorPallet <- (free-pallet ?nr 0)
	?robot-action <- (robot IRB2400 retrieve-part-from-WT-to-place-on-conv ?part)
	=>
	(printout t "The IRB1400 robot place the part " ?part " on pallet " ?nr " of the conveyor. " crlf)
	(retract ?robot-action ?conveyorPallet)
	(assert (robot IRB2400 free))
	(assert (free-pallet ?nr 1 ?part))
)

(defrule return-to-home-2400
    ?robot-action <- (robot IRB2400 retrieve-part-from-conv|retrieve-part-from-WT ?part)
    =>
	(printout t "The IRB2400 robot returns with the part in the home position." crlf)
    (retract ?robot-action)
    (assert (robot IRB2400 return-to-home ?part))
)

(defrule place-on-pallet
    ?robot-action <- (robot IRB2400 return-to-home ?part)
    ?goal <- (Goal (pallet ?p) (row ?r) (column ?c) (part-type ?part))
	(PalletCompletion (pallet ?p) (completed false) (available true))
    =>
	(printout t "The IRB2400 robot place the part " ?part ", on " ?r " row and " ?c " column "  crlf)
    (retract ?goal ?robot-action)
	;(assert (robot IRB2400 free))
)

;robotul IRB1400 muta piesa de pe S1 pe conveior
(defrule retrieve-part-from-S1
    ?robot-action <- (robot IRB1400 free)
    ?s1 <- (S1 (row ?r) (column ?c) (part-type ?part))
	(Goal (pallet ?p) (part-type ?part))
	(PalletCompletion (pallet ?p) (completed false) (available true))
	(not (WorkingTable (part-type ?part)))
    =>
    (retract ?robot-action ?s1)
    (printout t "The IRB1400 robot moves to retrieve the part " ?part " from S1"  crlf)
    (assert (robot IRB1400 retrieve-part-from-S1 ?part))
	(assert (S1 (row ?r) (column ?c) (part-type null)))
)

(defrule return-to-home-1400 
    ?robot-action <- (robot IRB1400 retrieve-part-from-S1 ?part)
    =>
    (retract ?robot-action)
    (assert (robot IRB1400 return-to-home ?part))
    (printout t "The IRB1400 robot returns with the part in the home position." crlf)
)

(defrule place-on-conv-irb1400
	?conveyorPallet <- (free-pallet ?nr 0)
	?robot-action <- (robot IRB1400 return-to-home|retrieve-part-from-MT ?part)
	=>
	(retract ?robot-action ?conveyorPallet)
    (printout t "The IRB1400 robot place the part " ?part " on pallet " ?nr " of the conveyor. " crlf)
	(assert (robot IRB1400 free))
	(assert (free-pallet ?nr 1 ?part))
)

(defrule retrieve-part-from-conv
	?robot-action <- (robot IRB2400 free)
	?conveyorPallet <- (free-pallet ?nr 1 ?part)
	(Goal (pallet ?p) (part-type ?part))
	(PalletCompletion (pallet ?p) (completed false) (available true))
	=>
	(retract ?robot-action ?conveyorPallet)
	(printout t "The IRB2400 robot pick up the part " ?part " from conveyor. " crlf)
	(assert (robot IRB2400 retrieve-part-from-conv ?part))
	(assert (free-pallet ?nr 0))
)

(defrule retrieve-part-from-conv-irb1400
	?robot-action <- (robot IRB1400 free)
	?conveyorPallet <- (free-pallet ?nr 1 ?part)
	=>
	(retract ?robot-action ?conveyorPallet)
	(printout t "The IRB1400 robot pick up the part " ?part " from conveyor. " crlf)
	(assert (robot IRB1400 retrieve-part-from-conv-irb1400 ?part))
	(assert (free-pallet ?nr 0))
)

(defrule place-to-WT
	?robot-action <- (robot IRB2400 return-to-home ?part)
	?wt <- (WorkingTable (row ?r) (column ?c) (part-type null) (check-result null))
	(Goal (pallet ?p) (part-type ?part))
	(PalletCompletion (pallet ?p) (completed false) (available true))
	=>
	(retract ?robot-action ?wt)
	(printout t "The IRB2400 robot place the part " ?part " on WT " ?r " and the column " ?c crlf)
	(assert (robot IRB2400 free))
	(assert (WorkingTable (row ?r) (column ?c) (part-type ?part) (check-result correct)))
)

(defrule retrieve-part-from-S1-to-MT
    ?robot-action <- (robot IRB1400 free)
    ?s1 <- (S1 (row ?r) (column ?c) (part-type ?part))
	(MachineTool (program ?) (part-to-be-transformed ?part) (transformed-part ?part2))
	(Goal (pallet ?p) (part-type ?part2))
	(PalletCompletion (pallet ?p) (completed false) (available true))
	(not (WorkingTable (part-type ?part2)))
	(not (S1 (part-type ?part2)))
    =>
    (retract ?robot-action ?s1)
    (printout t "The IRB1400 robot moves to retrieve the part " ?part " from S1 and places it on MT. "  crlf)
    (assert (robot IRB1400 retrieve-part-from-S1-to-MT ?part))
	(assert (S1 (row ?r) (column ?c) (part-type null)))
)

(defrule machine-tool-process
	?robot-action <- (robot IRB1400 retrieve-part-from-S1-to-MT|retrieve-part-from-conv-irb1400 ?part)
	(MachineTool (program ?) (part-to-be-transformed ?part) (transformed-part ?part2))
	(Goal (pallet ?p) (part-type ?part2))
	(PalletCompletion (pallet ?p) (completed false) (available true))
	(not (WorkingTable (part-type ?part2)))
	(not (S1 (part-type ?part2)))
	=>
	(retract ?robot-action)
	(printout t "The " ?part " part has been processed to " ?part2 " part. " crlf)
	(assert (robot IRB1400 free))
	(assert (mt done ?part2))
)

(defrule retrieve-part-from-MT
	?robot-action <- (robot IRB1400 free)
	?mt-done <- (mt done ?part2)
	=>
	(retract ?robot-action ?mt-done)
    (printout t "The IRB1400 robot pick up the part. " ?part2 crlf)
	(assert (robot IRB1400 retrieve-part-from-MT ?part2))
)

(defrule check-pallet-full
    ?pc <- (PalletCompletion (pallet ?p) (completed false) (available true))
    ?pcnext <- (PalletCompletion (pallet ?nextp&:(= (+ ?p 1) ?nextp)) (completed false) (available false))
    (not (Goal (pallet ?p)))
	=>
    (printout t "The pallet " ?p " has been completed. " crlf)
    (retract ?pc)
    (assert (PalletCompletion (pallet ?p) (completed true) (available false)))
    (assert (PalletCompletion (pallet ?nextp) (completed false) (available true)))
)

(defrule finish
	(not (Goal))
	=>
	(printout t "Objective completed!" crlf)
)
;(assert (WorkingTable (row 3) (column 2) (part-type B) (check-result correct)))