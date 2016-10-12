;;; powerline.el --- fancy statusline

;; Name: Emacs Powerline
;; Author: Nicolas Rougier and Chen Yuan
;; Version: 1.2
;; Keywords: statusline
;; Repository: https://github.com/yuanotes/powerline (not maintained)
;; Alternative: https://github.com/milkypostman/powerline by Donald Curtis

;;; Commentary:

;; This package simply provides a minor mode for fancifying the status line.

;;; Changelog:

;; v1.0 - Nicolas Rougier posted to wiki (http://www.emacswiki.org/emacs/PowerLine)
;; v1.1 - Guard clause around the powerline output, so that if
;;        powerline tries to output something unexpected, it won't
;;        just fail and flail-barf.  (JonathanArkell)
;; v1.2 - Fixed the Guard Clause to not just sit there and message like mad
;;        When a list is encountered, it is interpreted as a mode line. Fixes
;;        problems with shell mode and nXhtml mode.

;;; Code:

(defvar powerline-c1-primary "#383838")
(defvar powerline-c2-primary "#666666")
(defvar powerline-fg-primary "#bababa")
(defvar powerline-c1-secondary "#383838")
(defvar powerline-c2-secondary "#666666")
(defvar powerline-fg-secondary "#bababa")

(defvar powerline/default-height 130)

(defun powerline-primary-window () (eq (get-buffer-window) powerline-current-window))
(defun powerline-c1 () (if (powerline-primary-window) powerline-c1-primary powerline-c1-secondary))
(defun powerline-c2 () (if (powerline-primary-window) powerline-c2-primary powerline-c2-secondary))
(defun powerline-fg () (if (powerline-primary-window) powerline-fg-primary powerline-fg-secondary))

(defvar theme-powerline-color-alist
  '((whiteboard     (:primary ("#bbbbbb" "#d7d7d7" "#2a2a2a") :secondary ("#bbbbbb" "#d7d7d7" "#2a2a2a")))
    (atom-one-dark  (:primary ("#3E4451" "#5C6370" "#ABB2BF") :secondary ("#3E4451" "#5C6370" "#ABB2BF")))
    (darktooth      (:primary ("#222222" "#222222" "#FDF3C3") :secondary ("#403935" "#403935" "#988975")))
    (niflheim       (:primary ("#222222" "#2a2a2a" "#bababa") :secondary ("#222222" "#2a2a2a" "#bababa")))
    (aurora         (:primary ("#455a64" "#2B3B40" "#CDD3D3") :secondary ("#232A2F" "#232A2F" "#556D79")))
    (forest-blue    (:primary ("#0e5994" "#203439" "#d3cbc4") :secondary ("#203439" "#203439" "#203439")))
    (eink           (:primary ("#DDDDD8" "#DDDDD8" "#383838") :secondary ("#DDDDD8" "#DDDDD8" "#DDDDD8")))
    (ujelly         (:primary ("#000000" "#000000" "#ffffff") :secondary ("#000000" "#000000" "#ffffff")))
    (spacemacs-dark (:primary ("#6c3163" "#292B2E" "#b2b2b2") :secondary ("#292B2E" "#292B2E" "#292B2E")))
    (solarized-dark (:primary ("#657b83" "#073642" "#073642") :secondary ("#002b36" "#002b36" "#586e75")))
    (gruvbox        (:primary ("#3c3836" "#282828" "#f4e8ba") :secondary ("#504945" "#282828" "#a89984")))
    (material       (:primary ("#1c1f26" "#1c1f26" "#ffffff") :secondary ("#1c1f26" "#1c1f26" "#a7adba")))
    (monokai        (:primary ("#363731" "#272822" "#E5DDB7") :secondary ("#272822" "#272822" "#75715E")))
    (darkokai       (:primary ("#ab7eff" "#242728" "#3D4345") :secondary ("#242728" "#242728" "#5D6365")))
    (suscolors      (:primary ("#5faf5f" "#262626" "#262626") :secondary ("#262626" "#262626" "#949494")))
    (wombat         (:primary ("#444444" "#343434" "#CCC9C0") :secondary ("#444444" "#343434" "#99968b")))))

(defun update-powerline (&rest args)
  "Update the extra powerline colours based on a mapping to theme."
  (interactive)
  (let* ((theme (car custom-enabled-themes))
         (primary-alist (plist-get (cadr (assoc theme theme-powerline-color-alist)) :primary))
         (secondary-alist (plist-get (cadr (assoc theme theme-powerline-color-alist)) :secondary)))
    (if (and primary-alist secondary-alist)
        (setq powerline-c1-primary (car primary-alist)
              powerline-c2-primary (cadr primary-alist)
              powerline-fg-primary (caddr primary-alist)
              powerline-c1-secondary (car secondary-alist)
              powerline-c2-secondary (cadr secondary-alist)
              powerline-fg-secondary (caddr secondary-alist))
      (setq powerline-fg-primary "white"
            powerline-fg-secondary "white"))))

(defun colon-xpm
  (color1 color2)
  "Return an XPM left arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"10 26 2 1\",
\". c %s\",
\"  c %s\",
\"          \",
\"          \",
\"          \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"          \",
\"          \",
\"          \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun colon-alt-xpm
  (color1 color2)
  "Return an XPM left arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"10 26 2 1\",
\"  c %s\",
\". c %s\",
\"          \",
\"          \",
\"          \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"          \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"   ....   \",
\"          \",
\"          \",
\"          \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-left-xpm
  (color1 color2)
  "Return an XPM left arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"14 26 2 1\",
\". c %s\",
\"  c %s\",
\".             \",
\"..            \",
\"...           \",
\"....          \",
\".....         \",
\"......        \",
\".......       \",
\"........      \",
\".........     \",
\"..........    \",
\"...........   \",
\"............  \",
\"............. \",
\"............. \",
\"............  \",
\"...........   \",
\"..........    \",
\".........     \",
\"........      \",
\".......       \",
\"......        \",
\".....         \",
\"....          \",
\"...           \",
\"..            \",
\".             \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-right-xpm
  (color1 color2)
  "Return an XPM right arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"14 26 2 1\",
\". c %s\",
\"   c %s\",
\"             .\",
\"            ..\",
\"           ...\",
\"          ....\",
\"         .....\",
\"        ......\",
\"       .......\",
\"      ........\",
\"     .........\",
\"    ..........\",
\"   ...........\",
\"  ............\",
\" .............\",
\" .............\",
\"  ............\",
\"   ...........\",
\"    ..........\",
\"     .........\",
\"      ........\",
\"       .......\",
\"        ......\",
\"         .....\",
\"          ....\",
\"           ...\",
\"            ..\",
\"             .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-right-xpm
  (color1 color2)
  "Return an XPM right curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_right[] = {
\"12 26 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"        ....\",
\"      ......\",
\"      ......\",
\"      ......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"    ........\",
\"    ........\",
\"    ........\",
\"    ........\",
\"     .......\",
\"     .......\",
\"     .......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"        ....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-left-xpm
  (color1 color2)
  "Return an XPM left curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"12 26 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\"....        \",
\"......      \",
\"......      \",
\"......      \",
\".......     \",
\".......     \",
\".......     \",
\"........    \",
\"........    \",
\"........    \",
\"........    \",
\".......     \",
\".......     \",
\".......     \",
\"......      \",
\"......      \",
\"......      \",
\"....        \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun gradient-color-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'gradient-color-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))

(defun gradient-color-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun gradient-xpm
  (c1 c2)
  "Return an XPM gradient string representing."
  (let* ((backup-color
          (if (eq (get-buffer-window) powerline-current-window)
            (face-attribute 'mode-line :background)
            (face-attribute 'mode-line-inactive :background)))
        (c1 (or c1 backup-color))
        (c2 (or c2 backup-color)))
    (create-image
    (format "/* XPM */
static char * gradient_left[] = {
/* columns rows colours chars-per-pixel */
\"12 26 12 1\",
\"a c %s\",
\"b c %s\",
\"c c %s\",
\"d c %s\",
\"e c %s\",
\"f c %s\",
\"g c %s\",
\"h c %s\",
\"i c %s\",
\"j c %s\",
\"k c %s\",
\"l c %s\",
/* pixels */
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\",
\"abcdefghijkl\"};"
            c1
            (gradient-color-blend (color-values c2) (color-values c1) 0.1)
            (gradient-color-blend (color-values c2) (color-values c1) 0.2)
            (gradient-color-blend (color-values c2) (color-values c1) 0.3)
            (gradient-color-blend (color-values c2) (color-values c1) 0.4)
            (gradient-color-blend (color-values c2) (color-values c1) 0.5)
            (gradient-color-blend (color-values c2) (color-values c1) 0.6)
            (gradient-color-blend (color-values c2) (color-values c1) 0.7)
            (gradient-color-blend (color-values c2) (color-values c1) 0.8)
            (gradient-color-blend (color-values c2) (color-values c1) 0.9)
            (gradient-color-blend (color-values c2) (color-values c1) 1.0)
            c2)
    'xpm t :ascent 'center)))

(defun slash-left-xpm
  (color1 color2)
  "Return an XPM left curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"14 26 2 1\",
\". c %s\",
\"  c %s\",
\"............. \",
\"............. \",
\"............  \",
\"............  \",
\"...........   \",
\"...........   \",
\"..........    \",
\"..........    \",
\".........     \",
\".........     \",
\"........      \",
\"........      \",
\".......       \",
\".......       \",
\"......        \",
\"......        \",
\".....         \",
\".....         \",
\"....          \",
\"....          \",
\"...           \",
\"...           \",
\"..            \",
\"..            \",
\".             \",
\".             \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun slash-right-xpm
  (color1 color2)
  "Return an XPM left curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"14 26 2 1\",
\". c %s\",
\"  c %s\",
\".             \",
\".             \",
\"..            \",
\"..            \",
\"...           \",
\"...           \",
\"....          \",
\"....          \",
\".....         \",
\".....         \",
\"......        \",
\"......        \",
\".......       \",
\".......       \",
\"........      \",
\"........      \",
\".........     \",
\".........     \",
\"..........    \",
\"..........    \",
\"...........   \",
\"...........   \",
\"............  \",
\"............  \",
\"............. \",
\"............. \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun make-xpm
  (name color1 color2 data)
  "Return an XPM image for lol data"
  (create-image
   (concat
    (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (if color1 color1 "None")
            (if color2 color2 "None"))
    (let ((len  (length data))
          (idx  0))
      (apply 'concat
             (mapcar #'(lambda (dl)
                        (setq idx (+ idx 1))
                        (concat
                         "\""
                         (concat
                          (mapcar #'(lambda (d)
                                     (if (eq d 0)
                                         (string-to-char " ")
                                       (string-to-char ".")))
                                  dl))
                         (if (eq idx len)
                             "\"};"
                           "\",\n")))
                     data))))
   'xpm t :ascent 'center))

(defun half-xpm
  (color1 color2)
  (make-xpm "half" color1 color2
            (make-list 18
                       (append (make-list 6 0)
                               (make-list 6 1)))))

(defun percent-xpm
  (pmax pmin we ws width color1 color2)
  (let* ((fs   (if (eq pmin ws)
                   0
                 (round (* 17 (/ (float ws) (float pmax))))))
         (fe   (if (eq pmax we)
                   17
                 (round (* 17 (/ (float we) (float pmax))))))
         (o    nil)
         (i    0))
    (while (< i 18)
      (setq o (cons
               (if (and (<= fs i)
                        (<= i fe))
                   (append (list 0) (make-list width 1) (list 0))
                 (append (list 0) (make-list width 0) (list 0)))
               o))
      (setq i (+ i 1)))
    (make-xpm "percent" color1 color2 (reverse o))))


;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of the given function."
  (let ((table-sym (gensym))
  (val-sym (gensym))
  (args-sym (gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
   (if ,val-sym
       ,val-sym
     (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))

(memoize 'arrow-left-xpm)
(memoize 'arrow-right-xpm)
(memoize 'curve-left-xpm)
(memoize 'curve-right-xpm)
(memoize 'slash-left-xpm)
(memoize 'slash-right-xpm)
(memoize 'gradient-xpm)
(memoize 'colon-xpm)
(memoize 'colon-alt-xpm)

(defun powerline-set-style ()
  "Set the style of the powerline separator"
  (interactive)
  (let* ((styles
          '(("arrow" arrow-left-xpm arrow-right-xpm)
            ("curve" curve-left-xpm curve-right-xpm)
            ("bolts" colon-xpm colon-alt-xpm)
            ("slash-/\\" slash-left-xpm slash-right-xpm)
            ("slash-//" slash-left-xpm slash-left-xpm)
            ("slash-\\/" slash-right-xpm slash-left-xpm)
            ("slash-\\\\" slash-right-xpm slash-right-xpm)
            ("gradient" gradient-xpm gradient-xpm)))
         (result (assoc (completing-read "Style: " styles) styles)))
    (defalias 'right-xpm (caddr result))
    (defalias 'left-xpm (cadr result))))

(defalias 'right-xpm 'slash-right-xpm)
(defalias 'left-xpm  'slash-left-xpm)

(defvar powerline-minor-modes nil)
(defvar powerline-arrow-shape 'arrow)
(defun powerline-make-face
  (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "powerline-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)2
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                  :box nil))
          (set-face-attribute cface nil
                            :foreground (powerline-fg)
                            :background bg
                            :box nil))
        cface)
    nil))

(defun powerline-make-left
  (string color1 &optional color2 localmap)
  (let ((plface (powerline-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display
                     (left-xpm color1 color2)
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq powerline-arrow-shape 'arrow)
                                            (force-mode-line-update)))) ""))))



(defun powerline-make-right
    (string color2 &optional color1 localmap)
  (let ((plface (powerline-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
         (propertize " " 'display
                     (right-xpm color1 color2)
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq powerline-arrow-shape 'arrow)
                                            (force-mode-line-update))))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))

(defun powerline-make-fill (color)
  ;; justify right by filling with spaces to right fringe, 20 should be calculated
  (let ((plface (powerline-make-face color))
        (amount (- (window-total-width)
                   (+ (- 37 (* (face-attribute 'default :height) 0.11238))
                      (if (eq (get-buffer-window) powerline-current-window)
                          (+ (length (-powerline-get-weather "%(weather)"))
                             (length (-powerline-get-weather "%(sunrise)  %(sunset)"))
                             (if (and (boundp 'yahoo-weather-info) yahoo-weather-mode) 4 0))
                        0)
                      (length (-powerline-get-temp))))))
    (propertize " " 'display `((space :align-to ,amount)) 'face plface)))

(defvar powerline/render-center? t)
(defvar powerline/colour-flycheck? nil)
(defvar powerline/upgrades nil)

(defun powerline/count-upgrades ()
  (let ((buf (current-buffer)))
    (package-list-packages-no-fetch)
    (with-current-buffer "*Packages*"
      (setq powerline/upgrades (length (package-menu--find-upgrades))))
    (switch-to-buffer buf)))
(advice-add 'package-menu-execute :after 'powerline/count-upgrades)
(defun powerline-package-updates ()
  (let ((num (or powerline/upgrades (powerline/count-upgrades))))
    (when (> num 0)
      (concat
       (propertize " ·" 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2)))
       (propertize
        (concat
         (propertize (format "   %s" (all-the-icons-octicon "package"))
                     'face `(:family "github-octicons" :foreground ,(powerline-fg) :background ,(powerline-c2) :height 1.2)
                     'display '(raise -0.1))
         (propertize (format " %d updates " num)
                     'face `(:foreground ,(powerline-fg) :background ,(powerline-c2) :height 0.9)))
        'help-echo "Open Packages Menu"
        'mouse-face '(:box 1)
        'local-map (make-mode-line-mouse-map
                    'mouse-1 (lambda () (interactive) (package-list-packages))))))))

(defun powerline-make-text
  (string color &optional fg localmap)
  (let ((plface (powerline-make-face color)))
    (if string
        (if localmap
            (propertize string 'face plface 'mouse-face plface 'local-map localmap)
          (propertize string 'face plface))
      "")))

(defun powerline-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (powerline-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (powerline-make-left   string color1 color2 localmap))
        ((eq side 'left)               (powerline-make-left   string color1 color1 localmap))
        ((eq side 'right)              (powerline-make-right  string color1 color1 localmap))
        ((eq side 'donttouch)          (powerline-make-right  string color1 color1 localmap))
        (t                             (powerline-make-text   string color1 localmap))))

(defmacro defpowerline (name string)
  "Macro to create a powerline chunk."
  `(defun ,(intern (concat "powerline-" (symbol-name name)))
       (side color1 &optional color2)
     (powerline-make
      side
      (let ((result ,string))
        (cond ((listp result)
               (format-mode-line result))
              ((not (or (stringp result)
                        (null result)))
               (progn " ERR"))
              (t
               result)))
      color1 color2)))



(defun powerline-mouse (click-group click-type string)
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                    nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

(defpowerline arrow       "")

(defvar powerline-buffer-size-suffix t)
(defun powerline-buffer-size (&rest args)
  (propertize (format-mode-line " %I                                          ")
              'face `(:height 0.9 :foreground ,(powerline-fg) :background ,(powerline-c1))))

(defun powerline-process (&rest args)
  (let ((icon (all-the-icons-icon-for-buffer)))
    (concat
     (when (or (symbolp icon) ;; This implies it's the major mode
               mode-line-process)
       (propertize
        (format-mode-line " %m")
        'face `(:height 0.8 :foreground ,(powerline-fg) :background ,(powerline-c1))
        'display '(raise 0.0)))
     (when mode-line-process
       (propertize (format-mode-line mode-line-process)
                   'face `(:height 0.8 :foreground ,(powerline-fg) :background ,(powerline-c1)))))))

(defpowerline row         "%4l")
(defpowerline column      "%3c")
(defpowerline percent     "%6p")
(defun powerline-row-col ()
  (propertize  (format-mode-line " %l:%c")
               'face `(:foreground ,(powerline-fg) :background ,(powerline-c1) :height 0.9)))

(defun powerline-region-info ()
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                   'face `(:foreground ,(powerline-fg) :background ,(powerline-c1) :family "github-octicons")
                   'display '(raise -0.0))
       (propertize (format " (%s, %s)" words chars)
                   'face `(:foreground ,(powerline-fg) :background ,(powerline-c1) :height 0.9))))))

(defun powerline-project-id (&rest args)
  (if (and (fboundp 'projectile-project-name)
           (projectile-project-name))
      (format " | %s |"
              (propertize (format "%s" (concat (projectile-project-name) ))
                          'face '(:height 0.8)
                          'help-echo "Switch Project"
                          'mouse-face '(:box 1)
                          'local-map (make-mode-line-mouse-map
                                      'mouse-1 (lambda () (interactive) (projectile-switch-project)))))
    (propertize " | × |" 'face '(:height 0.8))))

(defun powerline-buffer-id (&rest args)
  (if (and (fboundp 'projectile-project-root))
      (let* ((buf (or (buffer-file-name) (buffer-name)))
             (proj (ignore-errors (projectile-project-root)) )
             (name (if (buffer-file-name)
                       (or (cadr (split-string buf proj))
                           (format-mode-line "%b"))
                     (format-mode-line "%b"))))
        (propertize (format "  %s" name)
                    'face `(:height 0.8)
                    'help-echo (format "Major-mode: `%s`" major-mode)))
    (propertize (format-mode-line "  %b") 'face '(:height 0.8))))

(defun powerline-flycheck-status (&rest args)
  (let* ((text
          (pcase flycheck-last-status-change
            (`finished (if flycheck-current-errors
                           (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                          (+ (or .warning 0) (or .error 0)))))
                             (format "✖ %s Issue%s" count (if (eq 1 count) "" "s")))
                         "✔ No Issues"))
            (`running     "⟲ Running")
            (`no-checker  "⚠ No Checker")
            (`not-checked "✖ Disabled")
            (`errored     "⚠ Error")
            (`interrupted "⛔ Interrupted")
            (`suspicious  "")))
         (fg (cond
              ((not powerline/colour-flycheck?) (powerline-fg))
              ((string-match "Disabled" text) (powerline-fg))
              ((string-match "Running" text) (powerline-fg))
              ((string-match "⚠" text) (face-attribute 'warning :foreground))
              ((string-match "✖" text) (face-attribute 'error :foreground))
              (t (face-attribute 'success :foreground)))))
    (concat
     (when (and
            vc-mode
            (eq (get-buffer-window) powerline-current-window)
            powerline/render-center?)
       (propertize " ·" 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2))))
     (propertize (format " %s" text)
                 'face `(:height 0.9 :foreground ,fg :background ,(powerline-c2))
                 'help-echo "Show Flycheck Errors"
                 ;; 'display '(raise 0.1)
                 'mouse-face '(:box 1)
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))))

(defun -powerline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-alltheicon "git"))
                 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2) :height 1.2)
                 'display '(raise -0.1))
     (propertize " · " 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2)))
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2) :height 1.3 :family "github-octicons")
                 'display '(raise -0.1))
     (propertize (format " %s" branch)
                 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2) :height 0.9)))))

(defun -powerline-svn-vc ()
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud"))
                 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2) :height 1.2)
                 'display '(raise -0.1))
     (propertize (format " · %s" revision)
                 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2) :height 0.9)))))

(defun powerline-icon-vc ()
  (when vc-mode
    (cond ((string-match "Git[:-]" vc-mode) (-powerline-github-vc))
         ((string-match "SVN-" vc-mode) (-powerline-svn-vc))
         (t (propertize (format "%s" vc-mode) 'face `(:foreground ,(powerline-fg) :background ,(powerline-c2)))))))

(defpowerline gap "")

(defun powerline-time ()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (concat
     (propertize (format-time-string " %H:%M ")
                 'face `(:height 0.9 :foreground ,(powerline-fg) :background ,(powerline-c1)))
     (propertize (format "%s " icon)
                 'face `(:height 1.0 :family "Weather Icons" :foreground ,(powerline-fg) :background ,(powerline-c1))
                 'display '(raise -0.0))
     (propertize " · " 'face `(:height 0.9 :foreground ,(powerline-fg) :background ,(powerline-c1))))))

(defun -powerline-get-temp ()
  (let ((temp (-powerline-get-weather " %(temperature) ")))
    (unless (string= "" temp) (format "%s°C" (round (string-to-number temp))))))

(defun -powerline-get-weather (format)
  (if (and (boundp 'yahoo-weather-info)
           yahoo-weather-mode)
      (let* ((weather (yahoo-weather-info-format yahoo-weather-info format))
             (icon (all-the-icons-icon-for-weather (downcase weather)))
             (family (if (> (length icon) 2)
                         (face-attribute 'default :family)
                       "Weather Icons")))
        (propertize (format " %s " icon)
                    'help-echo weather
                    'face `( :height 1.0 :family ,family
                             :foreground ,(powerline-fg)
                             :background ,(powerline-c2))
                    'display '(raise 0.1)))
  ""))

(defun powerline-weather () (-powerline-get-weather "%(weather)"))
(defun powerline-suntime ()
  (if (and (boundp 'yahoo-weather-info)
           yahoo-weather-mode)

      (concat
              (propertize (format "%s "(yahoo-weather-info-format yahoo-weather-info "%(sunrise-time)"))
                          'face `(:foreground ,(powerline-fg) :background ,(powerline-c2)))
              (propertize (format "%s  " (all-the-icons-wicon "sunrise" :height 0.5 :v-adjust -0.1))
                          'face `(:height 1.1 :family "Weather Icons" :foreground ,(powerline-fg) :background ,(powerline-c2)))
              (propertize (format "%s "(yahoo-weather-info-format yahoo-weather-info "%(sunset-time)"))
                          'face `(:foreground ,(powerline-fg) :background ,(powerline-c2)))
              (propertize (format "%s "(all-the-icons-wicon "sunset" :height 0.5 :v-adjust -0.1))
                          'face `(:height 1.1 :family "Weather Icons" :foreground ,(powerline-fg) :background ,(powerline-c2))))
    ""))

(defpowerline temperature (-powerline-get-temp))
(defpowerline eb-indicator (eyebrowse-mode-line-indicator))

(defun powerline-window-number (&rest args)
  (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
              'face `(:height ,(/ (* 0.90 powerline/default-height) 100.0))
              'display '(raise 0.0)))

(defvar powerline-current-window nil)
(defun update-current-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq powerline-current-window (selected-window))))
(add-function :before pre-redisplay-function 'update-current-window)

(defun powerline-boop ()
  (when (fboundp 'boop-format-results)
    (let ((s (boop-format-results)))
      (add-face-text-property 0 (length s) `(:background ,(powerline-c2)) nil s) s)))

(defun -count-notifications (pattern notification-char)
  (when (boundp 'slack-ims)
    (let ((result
          (-reduce '+ (-map 'string-to-number (-non-nil
                                               (-map (lambda (it)
                                                       (with-temp-buffer
                                                         (insert (format "%s" it))
                                                         (goto-char (point-min))
                                                         (when (search-forward-regexp pattern (point-max) t)
                                                           (match-string 1)))) slack-ims))))))
     (when (> result 0) notification-char))))

(defpowerline new-im-notifications (-count-notifications "[0-9]+ \\([0-9]+\\) (.*?)" "✩"))
(defpowerline new-channel-notifications (-count-notifications "(.*?) \\([0-9]+\\) [0-9]+ nil" "✧"))

(defun powerline-mode-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format " %s"
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))

(defun powerline-mode-default ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (when (symbolp icon) ;; This implies it's the major mode
      (propertize
       (format-mode-line " %m")
       'face `(:height 0.8 :foreground ,(powerline-fg) :background ,(powerline-c1))
       'display '(raise 0.1)))))

(defun powerline-modified ()
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (propertize (apply (cadr result) (cddr result))
                'face `(:family ,(funcall (car result))))))

(setq-default
 mode-line-format
 (list "%e"
       '(:eval (concat

                (powerline-make-text " "  nil)
                (powerline-modified)
                ;(powerline-window-number  'left  nil  )
                (powerline-project-id     'left  nil  )
                (powerline-mode-icon)

                (powerline-buffer-id)
                (powerline-gap 'left nil (powerline-c1))

                (powerline-process)

                (powerline-row-col)
                (powerline-region-info)
                (powerline-gap 'left (powerline-c1) (powerline-c2))

                (if (and (eq (get-buffer-window) powerline-current-window)
                         powerline/render-center?)
                    (concat
                     (powerline-icon-vc)
                     (powerline-flycheck-status)
                     ;(powerline-package-updates)
                     (powerline-make-fill             (powerline-c2)  )
                     (powerline-suntime)
                     (when (and (boundp 'yahoo-weather-info)
                                yahoo-weather-mode)
                       (powerline-make-text " · " (powerline-c2)))
                     (powerline-weather)
                     (powerline-temperature    'right  (powerline-c2)  )
                     (powerline-boop))
                  (concat
                   (powerline-flycheck-status)
                   (powerline-make-fill                (powerline-c2)  )))

                (powerline-gap  'right (powerline-c1) (powerline-c2))

                (powerline-time)
                (powerline-buffer-size    'left   nil  )))))

(provide 'powerline)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
;;; powerline.el ends here
