;; The solution of exercise 2.16
;; Explain, in general, why equivalent algebraic expressions may lead to
;; different answers. Can you devise an interval-arithmetic package that
;; does not have this shortcoming, or is this task impossible? (Warning:
;; This problem is very difficult.)
;;
;; -------- (above from SICP)
;;
;;      ___     ___       ______       ___    ___
;;     |   \   |   |     /      \     |   |  |   |
;;     |    \  |   |    /   __   \    |   |  |   |
;;     |     \ |   |   |   |  |   |   |   |  |   |
;;     |      \|   |   |   |  |   |   |   |  |   |
;;     |   |\      |   |   |  |   |   |   |  |   |
;;     |   | \     |   |   |__|   |   |___|  |___|
;;     |   |  \    |    \        /    /   \  /   \
;;     |___|   \___|     \______/     \___/  \___/
;;

(defun tell-me-the-answer ()
  (format t "
       ___     ___       ______       ___    ___
      |   \\   |   |     /      \\     |   |  |   |
      |    \\  |   |    /   __   \\    |   |  |   |
      |     \\ |   |   |   |  |   |   |   |  |   |
      |      \\|   |   |   |  |   |   |   |  |   |
      |   |\\      |   |   |  |   |   |   |  |   |
      |   | \\     |   |   |__|   |   |___|  |___|
      |   |  \\    |    \\        /    /   \\  /   \\
      |___|   \\___|     \\______/     \\___/  \\___/

  "))
