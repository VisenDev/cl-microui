
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-concat (&rest symbols)
    (intern (format nil "~{~a~}" symbols))
    ))

(defmacro defenum (name fields)
  (assert (not (find 'count fields)) (name) "Cannot use count as an enum field name in enum ~a" name)
  (setq fields (append fields '(count)))
  (loop
    :for field in fields    
    :for i from 0
    :for fieldname = (symbol-concat '+ name '- field '+)
    :collect `(defparameter ,fieldname (the fixnum ,i)) into result
    :finally
       (progn
         (push `(deftype ,name () 'fixnum) result)
         (return `(progn ,@result))
         )
    )
  )

(defenum clip (part all))
(defenum command (jump clip rect text icon max))
(defenum color-id (
   text
   border
   windowbg
   titlebg
   titletext
   panelbg
   button
   buttonhover
   buttonfocus
   base
   basehover
   basefocus
   scrollbase
   scrollthumb
   max
   ))
(defenum icon (
   close
   check
   collapsed
   expanded
   max
   ))

(deftype id () 'fixnum)
(defstruct vec2
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  )
(defstruct rect
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (w 0 :type fixnum)
  (h 0 :type fixnum)
  )
(defstruct color
  (r 0 :type character)
  (b 0 :type character)
  (g 0 :type character)
  (a 255 :type character)  
  )

(define-symbol-macro no-default (error "must be initialized"))
(defstruct poolitem
  (id no-default :type id)
  (lastupdate no-default :type fixnum)
  )
(defstruct base-command
  (type no-default :type fixnum)
  (size no-default :type fixnum)
  )
(defstruct jump-command
  (base no-default :type base-command)
  (dst)
  )
(defstruct clip-command
  (base no-default :type base-command)
  (rect no-default :type rect)
  )
(defstruct rect-command
  (base no-default :type base-command)
  (rect no-default :type rect)
  (color no-default :type color)
  )
(deftype font () 't)
(defstruct text-command
  (base no-default :type base-command)
  (font no-default :type font)
  (pos no-default :type vec2)
  (str "" :type string)
  )
(defstruct icon-command
  (base no-default :type base-command)
  (rect no-default :type rect)
  (id no-default :type id)
  (color no-default :type color)
  )
(deftype command ()
  '(or
    base-command
    jump-command
    clip-command
    rect-command
    text-command
    icon-command)
  )
(defstruct layout
  (body no-default :type rect)
  (next no-default :type rect)
  (position no-default :type vec2)
  (size no-default :type vec2)
  (max no-default :type vec2)
  (widths no-default :type (array fixnum))
  (items no-default :type fixnum)
  (item-index no-default :type fixnum)
  (next-row no-default :type fixnum)
  (next-type no-default :type fixnum)
  (indent no-default :type fixnum)
  )
(defstruct container
  (commands no-default :type (array command))
  (rect no-default :type rect)
  (body no-default :type rect)
  (content-size no-default :type vec2)
  (scroll no-default :type vec2)
  (zindex no-default :type fixnum)
  (open no-default :type fixnum)
  )
(defstruct style
  (font no-default :type font)
  (size no-default :type vec2)
  (padding no-default :type fixnum)
  (spacing no-default :type fixnum)
  (indent no-default :type fixnum)
  (title-height no-default :type fixnum)
  (scrollbar-size no-default :type fixnum)
  (thumb-size no-default :type fixnum)
  (colors no-default :type (array color))
  )
(defstruct context
  (text-width no-default
   :type (function (font string fixnum) fixnum))
  (text-height no-default
   :type (function (font) fixnum))
  (draw-frame no-default
   :type (function (context rect color-id) nil))
  (style no-default :type style)
  (hover no-default :type id)
  (focus no-default :type id)
  (last-id no-default :type id)
  (last-rect no-default :type rect)
  (last-zindex no-default :type fixnum)
  (updated-focus no-default :type fixnum)
  (frame no-default :type fixnum)
  (hover-root)
  (next-hover-root)
  (scroll-target)
  (number-edit-buf)
  (number-edit)

                                        ;stacks
  (command-list)
  (root-list)
  (container-stack)
  (clip-stack)
  (id-stack)
  (layout-stack)

                                        ;state pools
  (container-pool)
  (containers)
  (treenode-pool)

                                        ;input state
  (mouse-pos (make-vec2) :type vec2)
  (last-mouse-pos (make-vec2) :type vec2)
  (mouse-default (make-vec2) :type vec2)
  (scroll-delta (make-vec2) :type vec2)
  (mouse-down)
  (mouse-pressed)
  (key-down)
  (key-pressed)
  (input-text)
  )



;;;; microui.c

(defmacro unused (x)
  `(declare (ignore ,x)))

(defmacro expect (x)
  `(assert (not (null ,x))))

(defparameter +unclipped-rect+
  (make-rect :x 0 :y 0 :w #x1000000 :h #x1000000))
