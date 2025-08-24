
(define-condition todo (error)
  ((msg :initarg :msg :type string :reader todo-msg))
  (:report (lambda (condition stream)
             (format stream "TODO: ~a" (todo-msg condition))
             )
   )
  )
(defun todo (msg)
  (signal 'todo :msg msg)
  )

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
(deftype clip () '(member part all))
(deftype command () '(member jump clip rect text icon max))
(deftype u8 () '(integer 0 255))
(defstruct
    (color
     (:constructor create-color (r g b &optional a)))
  (r 0 :type u8)
  (b 0 :type u8)
  (g 0 :type u8)
  (a 255 :type u8)  
  )
(defstruct color-theme
  (text (create-color 230 230 230) :type color)
  (border (create-color 25 25 25) :type color)
  (window-bg (create-color 50 50 50) :type color)
  (title-bg (create-color 25 25 25) :type color)
  (title-text (create-color 240 240 240) :type color)
  (panel-bg (create-color 0 0 0 0) :type color)
  (button (create-color 75 75 75) :type color)
  (button-hover (create-color 95 95 95) :type color)
  (button-focus (create-color 115 115 115) :type color)
  (base (create-color 30 30 30) :type color)
  (base-hover (create-color 35 35 35) :type color)
  (base-focus (create-color 40 40 40) :type color)
  (scroll-base (create-color 43 43 43) :type color)
  (scroll-thumb (create-color 30 30 30) :type color)
  )
(deftype icon () '(member close check collapsed expanded))
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
  (colors no-default :type color-theme)
  )


(declaim (ftype (function (context rect symbol) nil) draw-frame))
(defun draw-frame (ctx rect color-id)
  (let*
      ((style (context-style ctx))
       (colors (style-colors style))
       (color (funcall (symbol-function color-id) colors))
       )
  (draw-rect ctx rect color)
    (when
        (member
         color-id
         '(color-theme-scroll-base
           color-theme-scroll-thumb
           color-theme-title-bg)
         )
      (return-from draw-frame)
      )
    (let* ((border (color-theme-border colors))
          (a (color-a border))
          )
      (unless (= 0 a)
        (draw-box ctx (expand-rect rect 1) border)
        )
      )
    )
  )

(defstruct context
  (text-width no-default
   :type (function (font string fixnum) fixnum))
  (text-height no-default
   :type (function (font) fixnum))
  (draw-frame #'draw-frame
   :type (function (context rect symbol) nil))
  (style (make-style) :type style)
  (hover 0 :type id)
  (focus 0 :type id)
  (last-id 0 :type id)
  (last-rect 0 :type rect)
  (last-zindex 0 :type fixnum)
  (updated-focus 0 :type fixnum)
  (frame 0 :type fixnum)
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

(defparameter +default-style+
  (make-style
   :font nil
   :size (make-vec2 :x 68 :y 10)
   :padding 5
   :spacing 4
   :indent 24
   :title-height 24
   :scrollbar-size 12
   :thumb-size 8
   :colors (make-color-theme)
   ))

(declaim (ftype (function (rect fixnum) rect) expand-rect))
(defun expand-rect (r n)
  (make-rect :x (- (rect-x r) n)
             :y (- (rect-y r) n)
             :w (+ (rect-w r) (* n 2))
             :h (+ (rect-h r) (* n 2))
             )
  )
(declaim (ftype (function (rect rect) rect) intersect-rects))
(defun intersect-rects (r1 r2)
  (let*
      ((x1 (max (rect-x r1) (rect-x r2)))
       (y1 (max (rect-y r1) (rect-y r2)))
       (x2 (min (+ (rect-x r1) (rect-w r1))
                (+ (rect-x r2) (rect-w r2))
                )
           )
       (y2 (min (+ (rect-y r1) (rect-h r1))
                (+ (rect-y r2) (rect-h r2))
                )
           )
       )
    (when (< x2 x1)
      (setf x2 x1)
      )
    (when (< y2 y1)
      (setf y2 y1)
      )
    (make-rect :x x1 :y y1 :w (- x2 x1) :h (- y2 y1))
    )
  )

(declaim (ftype (function (rect vec2) boolean) rect-overlaps-vec2))
(defun rect-overlaps-vec2 (rect vec2)
  (let* ((rx (rect-x rect))
         (ry (rect-y rect))
         (rw (rect-w rect))
         (rh (rect-h rect))
         (vx (vec2-x vec2))
         (vy (vec2-y vec2))
         )
    (and
     (>= vx rx)
     (< vx (+ rx rw))
     (>= vy ry)
     (< vy (+ ry rh))
     )
    )
  )

(declaim (ftype (function (context) nil) begin))
(defun begin (ctx)
  (expect (context-text-width ctx))
  (expect (context-text-height ctx))
  ;ctx->command_list.idx = 0;
  ;ctx->root_list.idx = 0;
  ;ctx->scroll_target = NULL;
  ;ctx->hover_root = ctx->next_hover_root;
  ;ctx->next_hover_root = NULL;
  ;ctx->mouse_delta.x = ctx->mouse_pos.x - ctx->last_mouse_pos.x;
  ;ctx->mouse_delta.y = ctx->mouse_pos.y - ctx->last_mouse_pos.y;
                                        ;ctx->frame++;
  (todo "finish this function")
  )

