
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


(declaim (ftype (function (ctx rect symbol) nil) draw-frame))
(defun draw-frame (ctx rect color-id)
  (let*
      ((style (ctx-style ctx))
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

(defconstant +default-stack-size+ 64)
(defun make-stack ()
  (make-array +default-stack-size+ :fill-pointer 0)
  )

(defstruct ctx
  (text-width no-default
   :type (function (font string fixnum) fixnum))
  (text-height no-default
   :type (function (font) fixnum))
  (draw-frame #'draw-frame
   :type (function (ctx rect symbol) nil))
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
  (command-list (make-stack))
  (root-list (make-stack))
  (container-stack (make-stack))
  (clip-stack (make-stack))
  (id-stack (make-stack))
  (layout-stack (make-stack))

                                        ;state pools
  (container-pool)
  (containers)
  (treenode-pool)

                                        ;input state
  (mouse-pos (make-vec2) :type vec2)
  (last-mouse-pos (make-vec2) :type vec2)
  (mouse-delta (make-vec2) :type vec2)
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

(defmacro reset-stack (stack-location)
  `(setf (fill-pointer ,stack-location) 0)
  )

(declaim (ftype (function (ctx) nil) begin))
(defun begin (ctx)
  (expect (ctx-text-width ctx))
  (expect (ctx-text-height ctx))
  (reset-stack (ctx-command-list ctx))
  (reset-stack (ctx-root-list ctx))
  (setf (ctx-scroll-target ctx) nil)
  (setf (ctx-hover-root ctx) (ctx-next-hover-root ctx))
  (setf (ctx-hover-root ctx) nil)
  (symbol-macrolet
      ((x (vec2-x (ctx-mouse-pos ctx)))
       (y (vec2-y (ctx-mouse-pos ctx)))
       (lx (vec2-x (ctx-last-mouse-pos ctx)))
       (ly (vec2-y (ctx-last-mouse-pos ctx)))
       (dx (vec2-x (ctx-mouse-delta ctx)))
       (dy (vec2-y (ctx-mouse-delta ctx)))
       )
    (setf dx (- x lx))
    (setf dy (- y ly))
    (incf (ctx-frame ctx))
    )
  )

(defmacro assert-stack-empty (stack-location)
  "Asserts that the fill pointer of a stack is empty"
  `(assert (= 0 (fill-pointer ,stack-location)))
  )

(defun not-nil (obj)
  "returns true when an object is not nil"
  (not (null obj))
  )
(defun =0 (obj)
  "returns true when an object is equal to 0"
  (= 0 obj))

(declaim (ftype (function (container container) boolean)))
(defun container-compare-zindexp (a b)
  "compares two containers based on their zindex, used for sort"
  (< (container-zindex a) (container-zindex b))
  )

(declaim (ftype (function (ctx) nil) end))
(defun end (ctx)
  (assert-stack-empty (ctx-container-stack ctx))
  (assert-stack-empty (ctx-clip-stack ctx))
  (assert-stack-empty (ctx-id-stack ctx))
  (assert-stack-empty (ctx-layout-stack ctx))

  (when (not-nil (ctx-scroll-target ctx))
    (setf
     (vec2-x (ctx-scroll-target ctx))
     (vec2-x (ctx-scroll-delta ctx))
     )
    (setf
     (vec2-y (ctx-scroll-target ctx))
     (vec2-y (ctx-scroll-delta ctx))
     )
    )
  (when (=0 (ctx-updated-focus ctx))
    (setf (ctx-focus ctx) 0)
    )
  (setf (ctx-updated-focus ctx) 0)
  (when (and
         (ctx-mouse-pressed ctx)
         (ctx-next-hover-root ctx)
         (< (container-zindex (ctx-next-hover-root ctx))
            (ctx-last-zindex ctx))
         (>= 0 (container-zindex (ctx-next-hover-root ctx)))
         )
    (bring-to-front ctx (ctx-next-hover-root ctx))
    )

  ;; reset input state
  (setf (ctx-key-pressed ctx) 0)
  (reset-stack (ctx-input-text ctx))
  (setf (ctx-mouse-pressed ctx) 0)
  (setf (ctx-scroll-delta ctx) (make-vec2))
  (setf (ctx-last-mouse-pos ctx) (ctx-mouse-pos ctx))

  ;; sort root containers by zindex
  (setf (ctx-root-list ctx)
        (sort (ctx-root-list ctx) #'container-compare-zindexp))

  ;; TODO figure out what this is and what it does
  ;/* set root container jump commands */
  ;for (i = 0; i < n; i++) {
  ;  mu_Container *cnt = ctx->root_list.items[i];
  ;  /* if this is the first container then make the first command jump to it.
  ;  ** otherwise set the previous container's tail to jump to this one */
  ;  if (i == 0) {
  ;    mu_Command *cmd = (mu_Command*) ctx->command_list.items;
  ;    cmd->jump.dst = (char*) cnt->head + sizeof(mu_JumpCommand);
  ;  } else {
  ;    mu_Container *prev = ctx->root_list.items[i - 1];
  ;    prev->tail->jump.dst = (char*) cnt->head + sizeof(mu_JumpCommand);
  ;  }
  ;  /* make the last container's tail jump to the end of command list */
  ;  if (i == n - 1) {
  ;    cnt->tail->jump.dst = ctx->command_list.items + ctx->command_list.idx;
  ;  }
                                        ;}
  )

(declaim (ftype (function (ctx id) nil) set-focus))
(defun set-focus (ctx id)
  (setf (ctx-focus ctx) id)
  (setf (ctx-updated-focus ctx) 1)
  )

(defconstant +hash-initial+ 2166136261)
; TODO implement this
;static void hash(mu_Id *hash, const void *data, int size) {
;  const unsigned char *p = data;
;  while (size--) {
;    *hash = (*hash ^ *p++) * 16777619;
;  }
;}

; TODO implement this
;mu_Id mu_get_id(mu_Context *ctx, const void *data, int size) {
;  int idx = ctx->id_stack.idx;
;  mu_Id res = (idx > 0) ? ctx->id_stack.items[idx - 1] : HASH_INITIAL;
;  hash(&res, data, size);
;  ctx->last_id = res;
;  return res;
;}

(defun check-clip (ctx rect)
  (

                              

    
  


