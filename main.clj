(use 'clojure.test)

(defn get-month [date]
;gets the month of a given date and parses it based off a slash or a dash
  (let [date (clojure.string/split date #"[-|/]")]
    (Integer. (get date 0)))
)

(defn get-day [date]
;gets the day of a given date and parses it by a slash or a dash
  (let [date (clojure.string/split date #"[-|/]")]
    (Integer. (get date 1)))
)

(defn get-year [date]
;gets the year of a given date and parses it based off a slash or a dash
  (let [date (clojure.string/split date #"[-|/]")]
    (Integer. (get date 2)))
)

(defn is-leap-year [year]
;conditional that tests if it is a leap year or not
  (cond
    (zero? (mod (Integer. year) 400)) true
    (zero? (mod (Integer. year) 100)) false
    (zero? (mod (Integer. year) 4)) true
    :default false
  )
)

(defn map-of-months [month]
;returns a map of the the correct calendar based off if it is a leapyear or not
  (let [theMap {1 "January" 2 "February" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" 8 "August" 9 "September" 10 "October" 11 "November" 12 "December"}]
  (get theMap month))
  
)

(defn max-day [year]
  (cond
  (true?(is-leap-year year)) (31 29 31 30 31 30 31 31 30 31 30 31)
  :else (31 28 31 30 31 30 31 31 30 31 30 31)
  )
)

(defn is-max-month [month day year]
  (if (= month 12)
  true
  false
  )
)

(defn is-max-day [month day year]
  (cond
  (true?(and(= month 1) (= day 31))) true
  (true?(and(= month 3) (= day 31))) true
  (true?(and(= month 5) (= day 31))) true
  (true?(and(= month 7) (= day 31))) true
  (true?(and(= month 8) (= day 31))) true
  (true?(and(= month 10) (= day 31))) true
  (true?(and(= month 12) (= day 31))) true
  (true?(and(= month 4) (= day 30))) true
  (true?(and(= month 6) (= day 30))) true
  (true?(and(= month 9) (= day 30))) true
  (true?(and(= month 11) (= day 30))) true
  (true?(and(and(= month 2) (= day 28))(not(is-leap-year year)))) true
  (true?(and(and(= month 2) (= day 29))(is-leap-year year))) true
  :default false
  )
)



(defn advance-day [month day year]
  ;if its the last day of the month then return 1
  (cond
    (true?(is-max-day month day year)) 1
    :else (+ day 1)
  )
  ;else increment day by 1
)

(defn advance-month [month day year]
  ;if its december return 1
  (if (is-max-day month day year)
  (cond
  (true?(= month 12)) 1
  :else (+ month 1)
  )
  (int month)
  )

  ;if its not increment by 1

)

(defn advance-year [month day year]
  (if(and(is-max-month month day year) (is-max-day month day year))
  (+ year 1)
  (int year)
  ;inrement year by 1
  )

)


(defn advance-date [month day year]
  (let[next-day (advance-day month day year)
  next-month(advance-month month day year)
  next-year(advance-year month day year)]

  (cond
  (true?(and(< next-month 10)(< next-day 10)))(def final-date(str "0" next-month "/0" next-day "/" next-year))
  (< next-month 10)(def final-date(str "0" next-month "/" next-day "/" next-year))
  (< next-day 10)(def final-date(str next-month "/0" next-day "/" next-year))
  :else (def final-date(str next-month "/" next-day "/" next-year))
  )
  final-date
  )
)

(defn get-dates [date]
  (let[day (get-day date)
    month (get-month date)
    year (get-year date)]
      (lazy-seq
      (cons date (get-dates (advance-date month day year)))
      ))
)

(defn get-dates-long [date]
  (let [day (get-day date)
    month (get-month date)
    year(get-year date)]
    
    (if (< day 10)
    (def dateLong (str (map-of-months month)" 0" day ", " year))
    (def dateLong (str (map-of-months month)" " day ", " year))
    )
      (lazy-seq
        (cons dateLong (get-dates-long (advance-date month day year)))))
)

;(println(take 5(get-dates "02/28/2001")))
;(println(take 5(get-dates "02/28/2000")))
;(println(nth(get-dates "12/25/2001")23))
;(println (nth (get-dates-long "02/20/2001")20))
;(println (nth (get-dates-long "02/20/2000")20))
;(println (take 3 (get-dates-long "01/17/1999")))

(deftest get-dates-test
  (is (= '("02/28/2001" "03/01/2001" "03/02/2001" "03/03/2001" "03/04/2001")(take 5(get-dates "02/28/2001"))))
  (is (= '("02/28/2000" "02/29/2000" "03/01/2000" "03/02/2000" "03/03/2000")(take 5(get-dates "02/28/2000"))))
  (is (= "01/17/2002" (nth (get-dates "12/25/2001") 23)))
)

(deftest get-dates-long-test
  (is(= "March 12, 2001" (nth (get-dates-long "02/20/2001")20)))
  (is(= "March 11, 2000" (nth (get-dates-long "02/20/2000")20)))
  (is(= '("January 17, 1999" "January 18, 1999" "January 19, 1999") (take 3 (get-dates-long "01/17/1999"))))
)

(run-tests)

(println)
(println "---------------------------- 30 base 10 birthdays ---------------------------")
(println)
(println(take 30(take-nth 1000(get-dates-long "01/17/2019"))))

