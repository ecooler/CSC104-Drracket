;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname CSC104.2017W.Project.I) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CSC104 2017 Winter, Project I.
;
; A Program to Model and Visualize the Spread of an Infection.


; Do all the tasks below marked by a “★”.

(require picturing-programs)

; These are some values that affect how the disease spread.
; Experiment with them once you have the program working, by changing one or more of the
;  numbers and running the program again.
;
; VIRULENCE: a positive number for how likely disease spreads from person to person.
; DURATION : a non-negative number for how many days an infection lasts.
; IMMUNITY : a non-negative number for how many days immunity lasts after infection ends.
(define VIRULENCE 20)
(define DURATION 10)
(define IMMUNITY 10)

; ★ AFTER YOU HAVE IMPLEMENTED THE REST OF THE PROGRAM, run it with various values
;    of VIRULENCE, DURATION and IMMUNITY.
;
;   Try to find values of those parameters that produce a delicate balance of the spread:
;    values where running it a few times leads to to visibly different end results due
;    to the randomness.
;   Also try large and small values and view the results.
;   Write a couple of paragraphs here summarizing your findings:
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;



; WIDTH : number of dots wide the simulation image is.
; HEIGHT: number of dots high the simulation image is.
(define WIDTH 100)
(define HEIGHT 100)


; take : list number → list
(check-expect (take (list 1 2 3 4 5) 3) (list 1 2 3))
; Produce prefix of length n of L.
(define (take L n)
  (cond [(zero? n) (list)]
        [else (list* (first L) (take (rest L) (- n 1)))]))

; drop : list number → list
(check-expect (drop (list 1 2 3 4 5) 3) (list 4 5))
(check-expect (drop (list 1 2 3) 3) (list))
; Produce suffix of L after removing n elements.
(define (drop L n)
  (cond [(<= (length L) n) (list)]
        [(zero? n) L]
        [else (drop (rest L) (- n 1))]))

; left-cycle : list number → list
(check-expect (left-cycle (list 1 2 3 4) 1) (list 2 3 4 1))
(check-expect (left-cycle (list 1 2 3 4) 2) (list 3 4 1 2))
;
; ★ Write a full design check-expect for left-cycle:



; ★ Fix the body of left-cycle based on your full design check-expect.
; HINT: use take and drop.
; Produce a list with the first n elements of L after the others.
(define (left-cycle L n)
  L)


; right-cycle : list number → list
(check-expect (right-cycle (list 1 2 3 4) 1) (list 4 1 2 3))
(check-expect (right-cycle (list 1 2 3 4) 2) (list 3 4 1 2))
; Produce a list with the last n elements of L before the others.
(define (right-cycle L n)
  (reverse (left-cycle (reverse L) n)))

; Infection Status.
;
; A person is either:
;
;   Infected, with a certain number of days of infection left.
;     This is represented by a number which is the *negative* of the number
;      of days of infection left, and shown as a red dot.
;
;   Immune, with a certain number of days of immunity left.
;     This is represented by a *positive* number which is the number of days
;      of immunity left, and shown as a green dot.
;
;   Infectable.
;     This is represented by zero, and shown as a blue dot.

; INITIAL-STATUSES is the list of initial infection statuses of everyone.
(define INITIAL-STATUSES
  (local [(define DOTS (* WIDTH HEIGHT))
          (define DOTS/2 (quotient DOTS 2))]
    (append (make-list DOTS/2 0)
            (list (- DURATION)) ; One infected person.
            (make-list (- DOTS (+ 1 DOTS/2)) 0))))


; infect : integer integer integer integer integer → integer
(check-expect (<= (infect 0 -1 2 -3 4) 0) #true)
(check-expect (infect -1  0 -2  3 -4) -1)
(check-expect (infect  2  0  3 -4  5)  2)
(check-expect (infect  2  0  3 -4  5)  2)
(check-expect (infect  0  1  0  2  0)  0)
; Due to the randomness, a check-expect that tests that an infectable subject can be
;  infected is a bit tricky to do properly. Here is an approach that should work
;  with high probability.
(define (try-to-infect unused-parameter)
  (infect 0 1 0 -1 -2))
(check-expect (apply min (map try-to-infect (range 0 10000 1)))
              (- DURATION))


; ★ Fix the body of infect.
; Produce the number representing an infection of DURATION days if:
;   the subject number represents infectable, and
;   neighbour-0 or neighbour-1 or neighbour-2 or neighbour-3 represents infected, and
;   VIRULENCE is more than this randomly chosen number: (random 100).
; Otherwise: produce the number representing the subject's current status.
(define (infect subject neighbour-0 neighbour-1 neighbour-2 neighbour-3)
  (cond [#false 0] ; Change this clause's condition and result.
        [else subject]))


; update-status : integer → integer
(check-expect (update-status 15) 14)
(check-expect (update-status -1) IMMUNITY)
(check-expect (update-status 0) 0)
(check-expect (update-status -4) -3)

; ★ Fix the body of update-status.
; Produce updated duration status of infection and immunity from current infection status.
; The status remains infectable if it is infectable.
; If the status represents one day of infection, the result is IMMUNITY days of immunity
; Otherwise the result represents one less day of infection or immunity.
(define (update-status status)
  status)


; update-statuses : list-of-numbers → list-of-numbers
(check-expect (update-statuses (list 5 0 -7 0 4 0)) (list 4 0 -6 0 3 0))
; ★ Write a full design check-expect for update-statuses.



; ★ Fix the body of update-statuses.
; Produce the list of updated duration of infection and immunity form a list of statuses.
(define (update-statuses statuses)
  statuses)


; day-tick : list-of-numbers → list-of-numbers
; Produce the next day's infection status for each subject from their neighbours.
(define (day-tick statuses)
  (update-statuses (map infect
                        statuses
                        (left-cycle statuses 1)
                        (right-cycle statuses 1)
                        (left-cycle statuses WIDTH)
                        (right-cycle statuses WIDTH))))


; status-color : integer → color
(check-expect (status-color 0) (make-color 0 0 255))
(check-expect (status-color 5) (make-color 0 255 0))
(check-expect (status-color -7) (make-color 255 0 0))

; ★ Fix the body of status-color according to the check-expects.
; Produce a colour corresponding to numerical infection status.
(define (status-color status)
  (make-color 0 0 0))

; draw-statuses : list-of-numbers → image
; Produce rectangular image depicting statuses.
(define (draw-statuses statuses)
  (color-list->bitmap (map status-color statuses) WIDTH HEIGHT))

; Start the universe!
; This will produce a blue background, representing a population of infectable subjects,
;  with a red dot at the edge representing an infected person. That infection will likely
;  spread, turning some subjects red, then green as they recover and become immune for a while,
;  then back to blue.
;
; ★ Uncomment this:
;(big-bang INITIAL-STATUSES
;          (on-tick day-tick)
;          (on-draw draw-statuses))