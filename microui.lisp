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
    )
  )

;;;; ========== VECTOR ============

(defun make-vector (type)
  (make-array 8 :adjustable t :fill-pointer 0 :element-type type)
  )

;;;; ========== ENUMS ============
(deftype clip () '(member :part :all))
(deftype command () '(member :jump :clip :rect :text :icon :max))
(deftype icon () '(member :close :check :collapsed :expanded))

;;;; ========== COLORS ===========
(deftype u8 () '(integer 0 255))
(defstruct
    (color
     (:constructor create-color (&optional r g b a)))
  (r 0 :type u8)
  (b 0 :type u8)
  (g 0 :type u8)
  (a 255 :type u8)  
  )
(deftype color-id ()
  '(member :text :border :window-bg :title-bg :title-text
    :panel-bg :button :button-hover :button-focus :base
    :base-hover :base-focus :scroll-base :scroll-thumb)
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
(declaim (ftype (function (color-theme color-id) color) theme-get))
(defun theme-get (theme id)
  (funcall (symbol-function (symbol-concat 'color-theme- id))
           theme)
  )

                  
;;;; ============ RECT / VEC =============
(deftype id () 'fixnum)
(defstruct (vec2 (:constructor make-vec2 (&optional x y))
                 )
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  )
(defstruct (rect (:constructor make-rect (&optional x y w h))
                 )
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (w 0 :type fixnum)
  (h 0 :type fixnum)
  )

;;;; =========== COMMANDS ==================
(define-symbol-macro no-default (error "must be initialized"))
(defstruct poolitem
  (id 0 :type id)
  (lastupdate 0 :type fixnum)
  )
(defstruct jump-command
  (dst)
  )
(defstruct clip-command
  (rect (make-rect) :type rect)
  )
(defstruct rect-command
  (rect (make-rect 10 10 10 10) :type rect)
  (color nil :type color)
  )
(deftype font () 't)
(defstruct text-command
  (font nil :type font)
  (pos (make-vec2 0 0) :type vec2)
  (str "" :type string)
  )
(defstruct icon-command
  (rect no-default :type rect)
  (id no-default :type id)
  (color no-default :type color)
  )
(deftype command ()
  '(or
    jump-command
    clip-command
    rect-command
    text-command
    icon-command)
  )
(defstruct layout
  (body (make-rect 100 100 100 100) :type rect)
  (next (make-rect 1000 100 100 100) :type rect)
  (position (make-vec2) :type vec2)
  (size (make-vec2 100 100) :type vec2)
  (max (make-vec2 400 400) :type vec2)
  (widths (make-vector 'fixnum) :type (vector fixnum))
  (items 0 :type fixnum)
  (item-index 0 :type fixnum)
  (next-row 0 :type fixnum)
  (next-type 0 :type fixnum)
  (indent 0 :type fixnum)
  )
(defstruct container
  (commands (make-vector 'command) :type (array command))
  (rect (make-rect) :type rect)
  (body (make-rect) :type rect)
  (content-size (make-vec2 100 100) :type vec2)
  (scroll (make-vec2) :type vec2)
  (zindex 0 :type fixnum)
  (open 0 :type fixnum)
  )
(defstruct style
  (font nil :type font)
  (size (make-vec2 68 10) :type vec2)
  (padding 5 :type fixnum)
  (spacing 4 :type fixnum)
  (indent 24 :type fixnum)
  (title-height 24 :type fixnum)
  (scrollbar-size 12 :type fixnum)
  (thumb-size 8 :type fixnum)
  (colors (make-color-theme) :type color-theme)
  )

;;;; ========== STACK ==========
(defconstant +default-stack-size+ 64)
(deftype stack (type) `(vector ,type ,+default-stack-size+))
(defun make-stack (&optional (element-type t))
  (make-array +default-stack-size+
              :fill-pointer 0
              :element-type element-type)
  )
(defmacro stack-reset (stack-location)
  `(setf (fill-pointer ,stack-location) 0)
  )
(defmacro assert-stack-empty (stack-location)
  "Asserts that the fill pointer of a stack is empty"
  `(assert (= 0 (fill-pointer ,stack-location)))
  )


;;;; ========== CTX ===========
(defstruct ctx
  (text-width nil
   :type (function (font string fixnum) fixnum))
  (text-height nil
   :type (function (font) fixnum))
  (draw-frame nil
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
  (mouse-down nil :type boolean)
  (mouse-pressed nil)
  (key-down nil)
  (key-pressed nil)
  (input-text nil)
  )


;;;; ============ RECT UTILS =============

(defmacro unused (x)
  `(declare (ignore ,x)))

(defparameter +unclipped-rect+
  (make-rect 0 0 #x1000000 #x1000000))

(declaim (ftype (function (rect fixnum) rect) expand-rect))
(defun expand-rect (r n)
  (make-rect (- (rect-x r) n)
             (- (rect-y r) n)
             (+ (rect-w r) (* n 2))
             (+ (rect-h r) (* n 2))
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
    (make-rect x1 y1 (- x2 x1) (- y2 y1))
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


;;;; ================= FORWARD DECLARATIONS ==============
(declaim (ftype (function (ctx rect color) nil) draw-rect))
(declaim (ftype (function (ctx rect color) nil) draw-box))

;;;; ================= CORE IMPLEMENTATION ===============
(declaim (ftype (function (ctx rect keyword) nil) draw-frame))
(defun draw-frame (ctx rect color-id)
  (let*
      ((style (ctx-style ctx))
       (colors (style-colors style))
       (color (funcall (symbol-function color-id) colors))
       )
    (draw-rect ctx rect color)
    (unless (member color-id '(:scroll-base :scroll-thumb :title-bg))
      (let* ((border (color-theme-border colors))
             (a (color-a border))
             )
        (unless (= 0 a)
          (draw-box ctx (expand-rect rect 1) border)
          )
        )
      )
    )
  )


(declaim (ftype (function (ctx) nil) begin))
(defun begin (ctx)
  (assert (ctx-text-width ctx))
  (assert (ctx-text-height ctx))
  (stack-reset (ctx-command-list ctx))
  (stack-reset (ctx-root-list ctx))
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



(defun not-nil (obj)
  (not (null obj))
  )

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
  (when (= 0 (ctx-updated-focus ctx))
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
  (stack-reset (ctx-input-text ctx))
  (setf (ctx-mouse-pressed ctx) 0)
  (setf (ctx-scroll-delta ctx) (make-vec2))
  (setf (ctx-last-mouse-pos ctx) (ctx-mouse-pos ctx))

  ;; sort root containers by zindex
  (setf (ctx-root-list ctx)
        (sort (ctx-root-list ctx) #'container-compare-zindexp))

  (loop
    :for i :from 0 :below (fill-pointer (ctx-root-list ctx))
    :for cnt = (aref (ctx-root-list ctx) i)
    :when (= i 0)
      :for cmd = (ctx-command-list ctx)
      :do (setf (jump-command-dst cmd) 
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
(declaim (ftype (function (id t) id) hash))
(defun hash (hash-seed-id data)

  )

(declaim (ftype (function (array) boolean) stack-emptyp))
(defun stack-emptyp (stk)
  (= 0 (fill-pointer stk))
  )

(defun id-stack-top (ctx)
  (let* ((stk (ctx-id-stack ctx))
         )

    (if (stack-emptyp stk)
        +hash-initial+
        (vector-top


(defun get-id (ctx obj)
  (let*
      ((idx (fill-pointer (ctx-id-stack ctx)))
       (res (i
  

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

                              

    
  


