(ns test.cards.rebirth
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.pprint :refer :all]
            [clojure.test :refer :all]))


; (deftest rebirth
;   "Account Siphon - Use ability"
;   (do-game
;     (new-game (default-corp) (default-runner [(qty "Rebirth" 3)]))
;     (take-credits state :corp)
;     (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    
;     (println (get-in (get-runner) [:identity :title]))
;     (println (:identity (get-runner)))
;     (load-card "Leela Patel: Trained Pragmatist")

;     (core/disable-identity state :runner)
;     (swap! state update-in [:runner :identity] 
;         (fn [x] (load-card "Whizzard: Master Gamer")))
;     (core/enable-identity state :runner)

;     (println (get-in (get-runner) [:identity :title]))
;     (println (:identity (get-runner)))

;     (is (= 5 (:credit (get-runner))))
;     (card-ability state :runner (:identity (get-runner)) 0)
;     (is (= 6 (:credit (get-runner))))

;     ))

; (deftest rebirth-inline
;   "Account Siphon - Use ability"
;   (do-game
;     (new-game (default-corp) (default-runner [(qty "Magnum Opus" 3)]))
;     (take-credits state :corp)
;     (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    
;     (println (get-in (get-runner) [:identity :title]))
;     (println (:identity (get-runner)))
;     (load-card "Leela Patel: Trained Pragmatist")

;     (core/disable-identity state :runner)
;     (swap! state update-in [:runner :identity] 
;         (fn [x] (load-card "Kate \"Mac\" McCaffrey: Digital Tinker")))
;     (core/enable-identity state :runner)

;     (println (get-in (get-runner) [:identity :title]))
;     (println (:identity (get-runner)))

;     (is (= 5 (:credit (get-runner))))
;     (play-from-hand state :runner "Magnum Opus")
;     (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")

;     ))

(defn swappable-id [name]
    (assoc (load-card name) :cid (game.utils/make-cid) :zone [:swappable-identities]))

(deftest rebirth
  "Rebirth"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Magnum Opus" 1) (qty "Rebirth" 1)]))
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    
    (println (get-in (get-runner) [:identity :title]))
    ; (println (:identity (get-runner)))

    

    (let [get-prompt (fn [] (first (#(get-in @state [:runner :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))
          kate "Kate \"Mac\" McCaffrey: Digital Tinker"
          kit "Rielle \"Kit\" Peddler: Transhuman"
          ids [(swappable-id kate) (swappable-id kit)]]

          ; (pprint kate-card)
          ; (pprint (:identity (get-runner)))

          (swap! state update-in [:runner :swappable-identities] 
              (fn [x] ids))

          (play-from-hand state :runner "Rebirth")
          (is (= (prompt-names) [kate,kit,nil]))
          ; (println kate-card)

          ; (core/card-init state :runner kate-card)
          ; (find-card kate (:swappable-identities (get-runner)))
          (prompt-choice :runner (first ids))
          ; (core/resolve-prompt state :runner {:choice kate-card})
          (println "selected choice" (get-prompt))


    (println "New Identity" (get-in (get-runner) [:identity :title]))
    ; (println (:identity (get-runner)))

    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :runner "Magnum Opus")
    (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")

    )))


(deftest rebirth-whizzard
  "Rebirth"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Magnum Opus" 1) (qty "Rebirth" 1)]))
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    
    (println (get-in (get-runner) [:identity :title]))
    ; (println (:identity (get-runner)))

    

    (let [get-prompt (fn [] (first (#(get-in @state [:runner :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))
          whizzard "Whizzard: Master Gamer"
          ids [(swappable-id whizzard)]]

          ; (pprint kate-card)
          ; (pprint (:identity (get-runner)))

          (swap! state update-in [:runner :swappable-identities] 
              (fn [x] ids))

          (play-from-hand state :runner "Rebirth")
          (is (= (prompt-names) [whizzard,nil]))
          ; (println kate-card)

          ; (core/card-init state :runner kate-card)
          ; (find-card kate (:swappable-identities (get-runner)))
          (prompt-choice :runner (first ids))
          ; (core/resolve-prompt state :runner {:choice kate-card})
          (println "selected choice" (get-prompt))


    (println "New Identity" (get-in (get-runner) [:identity :title]))
    ; (println (:identity (get-runner)))

    (is (= 5 (:credit (get-runner))))
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 11 (:credit (get-corp))) "Corp has 8 credits")

    (is (= 8 (:credit (get-runner))))
    (println "Rebirth Whizzard test")
    (pprint (:identity (get-runner)))
    (card-ability state :runner (:identity (get-runner)) 0)
    (is (= 9 (:credit (get-runner))))
    ; (is (= 6 (:credit (get-runner))) "Whizzard gained a credit")

    )))

(deftest regular-whizzard
  "Whizz"
  (do-game
    (new-game (default-corp) 
        (make-deck "Whizzard: Master Gamer" [(qty "Sure Gamble" 1)]))
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    
    (println "Regular Whizzard test")
    (pprint (:identity (get-runner)))
    (card-ability state :runner (:identity (get-runner)) 0)
    (is (= 6 (:credit (get-runner))) "Whizzard took a credit")))
