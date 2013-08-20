#lang racket

(require framework/splash)
(require racket/gui)


(start-splash (string->path-element "splash.png")
              "CA"	 	 	 	 
              400)
(set-splash-progress-bar?! #t)
(sleep/yield 1)
(add-splash-icon (read-bitmap "voltmeter.png") 0 100)
(sleep/yield 0.1)
(add-splash-icon (read-bitmap "battery.png") 65 115)
(sleep/yield 0.1)
(add-splash-icon (read-bitmap "earth2.png") 105 115)
(sleep/yield 0.1)
(add-splash-icon (read-bitmap "resistance1.png") 150 115)
(sleep/yield 0.1)
(add-splash-icon (read-bitmap "play.png") 250 115)

(sleep/yield 1)
(close-splash)
(require "pasteboard.rkt")