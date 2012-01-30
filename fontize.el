;; Based on work by Ola Bini. Refactored and cut down by Jonas Arvidsson.

(defun fontize-change-font (new-font)
  (message (concat "setting " new-font))
  (set-default-font new-font t)
  (set-frame-font new-font t))

(defun fontize-change-font-size (step)
  (let* ((splitted (split-string (cdr (assoc 'font (frame-parameters))) "-"))
         (new-size (+ (string-to-number (nth 7 splitted)) step))
         (new-font (concat (nth 0 splitted) "-"
                           (nth 1 splitted) "-"
                           (nth 2 splitted) "-"
                           (nth 3 splitted) "-"
                           (nth 4 splitted) "-"
                           (nth 5 splitted) "-"
                           (nth 6 splitted) "-"
                           (number-to-string new-size) "-*-"
                           (nth 9 splitted) "-"
                           (nth 10 splitted) "-"
                           (nth 11 splitted) "-*-"
                           (nth 13 splitted))))
    (if (string= (car (last splitted)) "ISO8859")
        (setq new-font (concat new-font "-1")))
    (fontize-change-font new-font)))

(defvar *fontize-current-font-index* 0)

(defconst *fontize-font-ring* '(
                                "-Adobe-Courier-Medium-R-Normal--14-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Medium-R-Normal--16-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Medium-R-Normal--18-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Medium-R-Normal--20-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Medium-R-Normal--24-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Medium-R-Normal--28-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Medium-R-Normal--30-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Bold-R-Normal--14-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Bold-R-Normal--16-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Bold-R-Normal--20-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Bold-R-Normal--22-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Bold-R-Normal--24-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Bold-R-Normal--28-*-100-100-M-*-ISO8859-1"
                                "-Adobe-Courier-Bold-R-Normal--30-*-100-100-M-*-ISO8859-1"
                                "-Misc-Fixed-Medium-R-SemiCondensed--12-*-75-75-C-*-ISO8859-1"
                                "-Misc-Fixed-Medium-R-SemiCondensed--13-*-75-75-C-*-ISO8859-1"
                                "-Misc-Fixed-Medium-R-Normal--13-*-75-75-C-*-ISO8859-1"
                                "-Misc-Fixed-Medium-R-Normal--14-*-75-75-C-*-ISO8859-1"
                                "-Misc-Fixed-Medium-R-Normal--15-*-75-75-C-*-ISO8859-1"
                                "-Misc-Fixed-Medium-R-Normal--17-*-75-75-C-*-ISO8859-1"
                                "-Misc-Fixed-Medium-R-Normal--20-*-75-75-C-*-ISO8859-1"
                                "-Schumacher-Clean-Medium-R-Normal--13-*-75-75-C-*-ISO8859-1"
                                "-B&H-LucidaTypewriter-Medium-R-Normal-Sans-14-*-100-100-M-*-ISO8859-1"
                                "-B&H-LucidaTypewriter-Medium-R-Normal-Sans-16-*-100-100-M-*-ISO8859-1"
                                "-B&H-LucidaTypewriter-Medium-R-Normal-Sans-18-*-100-100-M-*-ISO8859-1"
                                "-B&H-LucidaTypewriter-Bold-R-Normal-Sans-20-*-100-100-M-*-ISO8859-1"
                                "-B&H-LucidaTypewriter-Bold-R-Normal-Sans-24-*-100-100-M-*-ISO8859-1"
                                "-B&H-LucidaTypewriter-Bold-R-Normal-Sans-30-*-100-100-M-*-ISO8859-1"
                                ))

(defun fontize-font-spin (step)
  (let ((len (length *fontize-font-ring*))
        (ix (+ *fontize-current-font-index* step)))
    (if (= ix -1)
        (setq ix (- len 1)))
    (if (= ix len)
        (setq ix 0))
    (setq *fontize-current-font-index* ix)
    (fontize-change-font (nth ix *fontize-font-ring*))))

;; Public interface

(defun inc-font-size ()
  (interactive)
  (fontize-change-font-size 1)
  )

(defun dec-font-size ()
  (interactive)
  (fontize-change-font-size -1)
  )

(defun font-next ()
  (interactive)
  (fontize-font-spin 1))

(defun font-prev ()
  (interactive)
  (fontize-font-spin -1))

(provide 'fontize)
