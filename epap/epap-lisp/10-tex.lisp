;;
;; Copyright (C) 2021  Restless Hypermedia, Inc.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;

(in-package :epap)

(defun latex-preamble ()
  (format t "
\\documentclass[14pt,twocolumn]{extarticle}
\\usepackage[
  paperwidth=209.66mm,paperheight=157.25mm,
  margin=0.8cm,includefoot]{geometry}
\\usepackage[
  width=209.66mm,height=157.25mm,center,frame,noinfo]{crop}
\\usepackage{parskip}
\\usepackage{ebgaramond}
\\usepackage[sc]{titlesec}
\\begin{document}
"))

(defun latex-postamble ()
  (format t "
\\end{document}"))

(defun into-temporary-file (f)
  (uiop:with-temporary-file (:stream stream :pathname pathname
                             :prefix "epap" :type "tex" :keep t)
    (let ((*standard-output* stream))
      (funcall f))
    :close-stream pathname))

(defun $ (&rest command)
  (uiop:run-program
   (mapcar #'namestring command) :output :interactive))

(defun $$ (&rest command)
  (uiop:run-program
   (mapcar #'namestring command) :output '(:string :stripped t)))

(defmacro cd (directory &body body)
  `(uiop:with-current-directory (,directory) ,@body))

(defun call-with-latex-to-png (f)
  (let ((pathname (into-temporary-file f)))
    (cd pathname
      ($ "latex" pathname)
      ($ "dvipng" "-D" "226.785" (pathname-name pathname))
      (png:decode-file
       (make-pathname
        :name (format nil "~A1" (pathname-name pathname))
        :type "png"
        :defaults pathname)))))

(defun call-with-latex-to-pdf (f)
    (let ((pathname (into-temporary-file f)))
    (cd pathname
      ($ "pdflatex" pathname)
      (make-pathname
       :type "pdf"
       :defaults pathname))))

(defun call-with-latex-to-pngs (f)
  (let ((pathname (into-temporary-file f)))
    (cd pathname
      ($ "latex" pathname)
      ($ "dvipng" "-D" "226.785" (pathname-name pathname)))
    pathname))

(defmacro wrap-latex (&body body)
  `(progn 
     (latex-preamble)
     (prog1 (progn ,@body)
       (latex-postamble))))

(defmacro latex-png (&body body)
  `(call-with-latex-to-png (lambda ()
                             (latex-preamble)
                             ,@body
                             (latex-postamble))))

(defun display-image (image)
  (destructuring-bind (height width channels) (array-dimensions image)
    (declare (ignore channels))
    (let ((*local-framebuffer*
            (make-array (list *display-height* *display-width*)
                        :element-type 'bit)))
      (loop for y from 0 below (min *display-height* height)
            do (loop for x from 0 below (min *display-width* width)
                     do (setf (sbit *local-framebuffer* y x)
                              (if (< (aref image y x 0) 200) 1 0))))
      (write-whole-framebuffer)
      (refresh))))

(defmacro with-display (&body body)
  `(progn
     (start-display)
     (initialize-blank-display)
     (unwind-protect (progn ,@body)
       (format t "epap: putting display to sleep~%")
       (enter-sleep-mode))))

(defun save-image (path image)
  (with-open-file (output path :element-type '(unsigned-byte 8)
                          :direction :output :if-exists :supersede)
    (png:encode image output)))

(defun real-latex-demo ()
  (for-real
    (start-display)
    (initialize-blank-display)))

(defparameter *chapman-prompt*
  "Various religions, philosophies, and systems claim to have answers. Some are complicated, and they all seem quite different. When you strip away the details, though, there are only a half dozen fundamental answers. Each is appealing in its own way, but also problematic. Understanding clearly what is right and wrong about each approach can resolve the underlying problem.")

(defun read-from-emacs (prompt)
  (swank::read-from-minibuffer-in-emacs prompt))

(defmacro display-latex (&body document)
  `(display-image (latex-png ,@document)))

(defmacro save-latex (path &body document)
  `(save-image ,path (latex-png ,@document)))

(defun mv (a b)
  ($ "mv" a b))

(defun format-aquinas ()
  (wrap-latex
    (format t "~{\\hspace{0pt}\\vfill{\\LARGE ~a}~%\\vfill\\hspace{0pt}\\newpage~%~}" *meekaale-aquinas*)))

(defparameter *christopher-alexander*
  "
\\section*{the family}

\\textbf{The nuclear family is not by itself a viable social form.}

\Huge{
Until a few years ago, human society was based on the extended family:
a family of at least three generations, with parents, children,
grandparents, uncles, aunts, and cousins, all living together in a
single or loosely knit multiple household. But today people move
hundreds of miles to marry, to find education, and to work. Under
these circumstances the only family units which are left are those
units called nuclear families: father, mother, and children. And many
of these are broken down even further by divorce and separation.

Unfortunately, it seems very likely that the nuclear family is not a
viable social form. It is too small. Each person in a nuclear family
is too tightly linked to other members of the family; any one
relationship which goes sour, even for a few hours, becomes critical;
people cannot simply turn away toward uncles, aunts, grandchildren,
cousins, brothers. Instead, each difficulty twists the family unit
into ever tighter spirals of discomfort; the children become prey to
all kinds of dependencies and oedipal neuroses; the parents are so
dependent on each other that they are finally forced to separate.

Philip Slater describes this situation for American families and finds
in the adults of the family, especially the women, a terrible,
brooding sense of deprivation. There are simply not enough people
around, not enough communal action, to give the ordinary experience
around the home any depth or richness.\\footnote{Philip E. Slater,
\\emph{The Pursuit of Loneliness}, Boston: Beacon Press, 1970, p. 67,
and throughout.}

It seems essential that the people in a household have at least a
dozen people round them, so that they can find the comfort and
relationships they need to sustain them during their ups and
downs. Since the old extended family, based on blood ties, seems to be
gone---at least for the moment---this can only happen if small
families, couples, and single people join together in voluntary
``families'' of ten or so.

Physically, the setting for a large voluntary family must provide for
a balance of privacy and communality. Each small family, each person,
each couple, needs a private realm, almost a private household of
their own, according to their territorial need. In the movement to
build communes, it is our experience that groups have not taken this
need for privacy seriously enough. It has been shrugged off, as
something to overcome. But it is a deep and basic need; and if the
setting does not let each person and each small household regulate
itself on this dimension, it is sure to cause trouble. We propose,
therefore, that individuals, couples, people young and old---each
subgroup---have its own legally independent household---in some cases,
physically separate households and cottages, at least separate rooms,
suites, and floors.

The private realms are then set off against the common space and the
common functions. The most vital commons are the kitchen, the place to
sit down and eat, and a garden. Common meals, at least several nights
a week, seem to play the biggest role in binding the group. The meals,
and taking time at the cooking, provide the kind of casual meeting
time when everything else can be comfortably discussed: the child care
arrange\\-ments, maintenance, projects---see \\textsc{communal
eating (147)}.

This would suggest, then, a large family room---farmhouse kitchen,
right at the heart of the site---at the main crossroads, where
everyone would tend to meet toward the end of the day. Again,
according to the style of the family, this might be a separate
building, with workshop and gardens, or one wing of a house, or the
entire first floor of a two or three story building.
}")

(defun latex-demo ()
  (with-display
    (display-image (latex-png (format t *christopher-alexander*)))
    ))

(defun babble-2 (prompt)
  (with-display
    (loop
      do
         (display-latex
           (format t "\\textsl{~a} ~a"
                   prompt (openai:completions prompt :tokens 200)))
      until (not (swank:y-or-n-p-in-emacs "Continue?")))))

(defun babble ()
  (start-display)
  (initialize-blank-display)
  (loop with lines = () do
    (let* ((prompt
             (swank::read-from-minibuffer-in-emacs "GPT-3: ")))
      (unless prompt (return))
      (push (format nil "\\textsc{Prompt.} ~a" prompt) lines)
      (let ((babble (openai:answer-question prompt 20)))
        (push (format nil "\\textsc{Answer.} ~a"
                      (ppcre:regex-replace-all "&" babble "\\\\&")) lines))
      (display-image
       (latex-png
         (loop for line in (reverse lines) do
           (format t "~a~%~%" line))))
      (unless (swank:y-or-n-p-in-emacs "Continue?")
        (return))))
  (goodnight))

;; (initialize-blank-display)

