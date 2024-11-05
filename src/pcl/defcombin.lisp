;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")



;; #### NOTE: those two functions rather belong to documentation.lisp, but
;; this file is loaded before. That's not very satisfactory. Perhaps there's a
;; way to reorganize documentation code (load the core early, spread the rest
;; in the relevant files)?
;; -- didier

(defun random-documentation (name type)
  (cdr (assoc type (info :random-documentation :stuff name))))

(defun (setf random-documentation) (new-value name type)
  (let ((pair (assoc type (info :random-documentation :stuff name)))
        (new-value (possibly-base-stringize new-value)))
    (if pair
        (setf (cdr pair) new-value)
        (push (cons type new-value)
              (info :random-documentation :stuff name))))
  new-value)




;; ========================
;; Method Combination Types
;; ========================

;; #### NOTE: one could be tempted to put this in defs.lisp, alongside with
;; the METHOD-COMBINATION hierarchy, but it's preferable to keep it here for
;; the following reasons.
;; 1. defs.lisp has some limitations due to the fact that it's used during
;;    bootstrap (there is no :documentation option to DEFCLASS, I don't think
;;    VALIDATE-SUPERCLASS is available, etc.).
;; 2. the bootstrap code creates two (early) specific method combination
;;    objects (STANDARD and (OR :MOST-SPECIFIC-FIRST)) which are stored
;;    respectively in *STANDARD-METHOD-COMBINATION* and
;;    *OR-METHOD-COMBINATION*. These objects are created before the full
;;    method combination architecture is in place, but they're enough to
;;    bootstrap the whole thing. So we can leave them be until this file is
;;    loaded, and convert them to the final machinery eventually.
;; -- didier

(defclass method-combination-type (standard-class)
  ()
  (:documentation "Metaclass for all method combination types."))

;; #### WARNING: trying to hijack the NAME slot in anonymous class metaobjects
;; (that is, classes that are not meant to be registered globally) is
;; dangerous, especially during bootstrap. I've seen very strange errors
;; occurring when trying to do so. Hence the TYPE-NAME slot below.
;; -- didier
(defclass standard-method-combination-type (method-combination-type)
  ((type-name :initarg :type-name :reader method-combination-type-name)
   (lambda-list :initform nil :initarg :lambda-list
                :reader method-combination-type-lambda-list)
   ;; A reader without "type" in the name seems more readable to me.
   (%constructor :reader method-combination-%constructor)
   (%cache :initform (make-hash-table :test #'equal)
           :reader method-combination-type-%cache))
  (:documentation "Metaclass for standard method combination types.
It is the base class for short and long method combination types metaclasses.
This only class directly implemented as this class is the standard method
combination class."))

(defmethod validate-superclass
    ((class standard-method-combination-type) (superclass standard-class))
  "Validate the creation of subclasses of METHOD-COMBINATION implemented as
STANDARD-METHOD-COMBINATION-TYPE."
  t)


(defclass short-method-combination-type (standard-method-combination-type)
  ((lambda-list :initform '(&optional (order :most-specific-first)))
   (operator :initarg :operator
             :reader short-method-combination-type-operator)
   (identity-with-one-argument
    :initarg :identity-with-one-argument
    :reader short-method-combination-type-identity-with-one-argument))
  (:documentation "Metaclass for short method combination types."))


(defclass long-method-combination-type (standard-method-combination-type)
  ((%args-lambda-list :initform nil :initarg :args-lambda-list
                      :reader long-method-combination-type-%args-lambda-list)
   (%function :initarg :function
              :reader long-method-combination-type-%function))
  (:documentation "Metaclass for long method combination types."))


(defglobal **method-combination-types** (make-hash-table :test 'eq)
  "The global method combination types hash table.
This hash table maps names to method combination types.")

;; #### NOTE: not SETF method here. We wouldn't want it to be public.
;; -- didier
(defun find-method-combination-type (name &optional (errorp t))
  "Find a NAMEd method combination type.
If ERRORP (the default), throw an error if no such method combination type is
found. Otherwise, return NIL."
  (or (gethash name **method-combination-types**)
      (when errorp
        (error "There is no method combination type named ~A." name))))

(defmethod find-method-combination
    ((generic-function generic-function) name options)
  "Find a method combination object for type NAME and options.
If no method combination type exists by that NAME, return NIL.
Otherwise, a (potentially new) method combination object is returned.
The GENERIC-FUNCTION argument is ignored."
  (let ((type (find-method-combination-type name nil)))
    (when type
      (or (gethash options (method-combination-type-%cache type))
          (setf (gethash options (method-combination-type-%cache type))
                (funcall (method-combination-%constructor type)
                  options))))))

(defun load-defcombin
    (name new documentation &aux (old (find-method-combination-type name nil)))
  "Register NEW method combination type under NAME with DOCUMENTATION.
This function takes care of any potential redefinition of an existing method
combination type."
  (when old
    (setf (slot-value new '%cache) (method-combination-type-%cache old))
    (maphash (lambda (options combination)
               (declare (ignore options))
               (change-class combination new))
             (method-combination-type-%cache new)))
  (setf (gethash name **method-combination-types**) new)
  (setf (random-documentation name 'method-combination) documentation)
  name)




;; ===================
;; Method Combinations
;; ===================

;; This section completes the minimal instalment of the method combinations
;; hierarchy that was elaborated in defs.lisp.

(defmethod update-generic-function-for-redefined-method-combination
    ((function generic-function)
     (previous standard-method-combination)
     (current standard-method-combination))
  "Flush the effective method cache and reinitialize FUNCTION."
  (flush-effective-method-cache function)
  (reinitialize-instance function))

(defmethod update-instance-for-different-class :after
    ((previous standard-method-combination)
     (current standard-method-combination)
     &key &allow-other-keys)
  "Inform every function using CURRENT method combination that it has changed."
  (map-hashset
   (lambda (gf)
     (update-generic-function-for-redefined-method-combination
      gf previous current))
   (method-combination-%generic-functions current)))


;; -----------------------------------
;; Method combination pseudo-accessors
;; -----------------------------------

;; These provide direct access to properties that belong to the method
;; combination type rather to the method combination itself.

;; #### TODO: some potentially usefull information is missing (like the
;; generic-function-symbol of the long form), or, maybe some of those are
;; actually useless. I need to verify which stuff is actually used (like, in
;; compute-effective-method etc.).
;; -- didier

(defmethod method-combination-type-name
    ((combination standard-method-combination))
  "Return method COMBINATION's type name."
  (method-combination-type-name (class-of combination)))

(defmethod method-combination-lambda-list
    ((combination standard-method-combination))
  "Return method COMBINATION's lambda-list."
  (method-combination-type-lambda-list (class-of combination)))

(defmethod short-method-combination-operator
    ((combination short-method-combination))
  "Return short method COMBINATION's operator."
  (short-method-combination-type-operator (class-of combination)))

(defmethod short-method-combination-identity-with-one-argument
    ((combination short-method-combination))
  "Return short method COMBINATION's identity-with-one-argument."
  (short-method-combination-type-identity-with-one-argument
   (class-of combination)))

(defmethod long-method-combination-%args-lambda-list
    ((combination long-method-combination))
  "Return long method COMBINATION's args-lambda-list."
  (long-method-combination-type-%args-lambda-list (class-of combination)))


;; ---------------------------
;; standard method combination
;; ---------------------------

;; #### WARNING: we work with CLOS layer 1 (the macro level) below because
;; it's much simpler to create the specialization of COMPUTE-PRIMARY-METHODS
;; this way. The unfortunate side effect is that the class below is defined
;; globally, which I don't really want (all other concrete method combination
;; classes are anonymous; even the built-in short ones).
;; -- didier
(defclass standard-standard-method-combination (standard-method-combination)
  ()
  (:metaclass standard-method-combination-type)
  (:documentation "The standard method combination."))

(setf (random-documentation 'standard 'method-combination)
      "The standard method combination.")

(defmethod compute-primary-methods
    ((gf generic-function)
     (combin standard-standard-method-combination)
     applicable-methods)
  (remove-if #'method-qualifiers applicable-methods))



;; -------------------------
;; short method combinations
;; -------------------------

;; Short method combinations all follow the same rule for computing the
;; effective method. So, we just implement that rule once. Each short method
;; combination object just reads the parameters out of the object and runs the
;; same rule.

(defmethod initialize-instance :before
    ((instance short-method-combination)
     &key options &allow-other-keys
     &aux (name (method-combination-type-name instance)))
  "Check the validity of OPTIONS for a short method combination INSTANCE."
  (when (cdr options)
    (method-combination-error
     "Illegal options to the ~S short method combination.~%~
      Short method combinations accept a single ORDER argument."
     name))
  (unless (member (car options) '(:most-specific-first :most-specific-last))
    (method-combination-error
     "Illegal ORDER option to the ~S short method combination.~%~
      ORDER must be either :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST."
     name)))

(defun load-short-defcombin
    (name operator identity-with-one-argument documentation
     mc-class mct-spec
     source-location
     &aux (mc-class (find-class mc-class))
          (mct-class (find-class (if (symbolp mct-spec)
                                   mct-spec
                                   (car mct-spec)))))
  "Register a new short method combination type under NAME."
  (unless (subtypep mc-class 'short-method-combination)
    (method-combination-error
     "Invalid method combination class: ~A.~%~
      When defining a method combination type in short form, the provided~%~
      method combination class must be a subclass of SHORT-METHOD-COMBINATION."
     mc-class))
  (unless (subtypep mct-class 'short-method-combination-type)
    (method-combination-error
     "Invalid method combination type class: ~A.~%~
      When defining a method combination type in short form, the provided~%~
      method combination type class must be a subclass of
      SHORT-METHOD-COMBINATION-TYPE."
     mct-class))
  ;; #### NOTE: we can't change-class class metaobjects, so we need to
  ;; recreate a brand new one.
  (let ((new (apply #'make-instance mct-class
                    'source source-location
                    :direct-superclasses (list mc-class)
                    :documentation documentation
                    :type-name name
                    :operator operator
                    :identity-with-one-argument identity-with-one-argument
                    (when (consp mct-spec) (cdr mct-spec)))))
    (setf (slot-value new '%constructor)
          (lambda (options)
            (funcall #'make-instance
              ;; #### NOTE: in principle, short method combinations would only
              ;; have at most two different instances, because the only
              ;; possible choice for options is :MOST-SPECIFIC-FIRST or
              ;; :MOST-SPECIFIC-LAST. However, thanks to the line below, the
              ;; caches will keep track of the options provided by the
              ;; programmer. It's nice because it's informative. As a result,
              ;; if the method combination is used without any argument, or
              ;; explicitly with :MOST-SPECIFIC-FIRST, we will end up with 2
              ;; different yet identical instances (so possibly 3 in total if
              ;; :MOST-SPECIFIC-LAST appears as well). Not such a big deal.
              new :options (or options '(:most-specific-first)))))
    (load-defcombin name new documentation)))

(defmethod invalid-qualifiers
    ((gf generic-function) (combin short-method-combination) method)
  (let* ((qualifiers (method-qualifiers method))
         (qualifier (first qualifiers))
         (type-name (method-combination-type-name combin))
         (why (cond
                ((null qualifiers)
                 "has no qualifiers")
                ((cdr qualifiers)
                 "has too many qualifiers")
                (t
                 (aver (not (short-method-combination-qualifier-p
                             type-name qualifier)))
                 "has an invalid qualifier"))))
    (invalid-method-error
     method
     "~@<The method ~S on ~S ~A.~
      ~@:_~@:_~
      The method combination type ~S was defined with the short form ~
      of DEFINE-METHOD-COMBINATION and so requires all methods have ~
      either ~{the single qualifier ~S~^ or ~}.~@:>"
     method gf why type-name (short-method-combination-qualifiers type-name))))

(defmethod compute-primary-methods ((gf generic-function)
                                    (combin short-method-combination)
                                    applicable-methods)
  (let ((type-name (method-combination-type-name combin)))
    (remove-if-not (lambda (m) (let ((qs (method-qualifiers m)))
                                 (and (eql (car qs) type-name)
                                      (null (cdr qs)))))
                   applicable-methods)))



;; ------------------------
;; Long method combinations
;; ------------------------

(defun expand-long-defcombin (form)
  (let ((type-name (cadr form))
        (lambda-list (caddr form))
        (method-group-specifiers-presentp (cdddr form))
        (method-group-specifiers (cadddr form))
        (body (cddddr form))
        (args-option ())
        (gf-var nil)
        (mc-class 'long-method-combination)
        (mct-spec '(long-method-combination-type)))
    (unless method-group-specifiers-presentp
      (%program-error
       "~@<The long form of ~S requires a list of method group specifiers.~:@>"
       'define-method-combination))
    (when (and (consp (car body)) (eq (caar body) :arguments))
      (setq args-option (cdr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :generic-function))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (%program-error
         "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
         :generic-function 'define-method-combination))
      (setq gf-var (cadr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :method-combination-class))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (%program-error
         "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
         :method-combination-class 'define-method-combination))
      (setq mc-class (cadr (pop body))))
    (when (and (consp (car body))
               (eq (caar body) :method-combination-type-class))
      (setq mct-spec (cdr (pop body))))
    (multiple-value-bind (documentation function)
        (make-long-method-combination-function
         type-name lambda-list method-group-specifiers args-option gf-var
         body)
      `(load-long-defcombin
        ',type-name ',documentation #',function ',lambda-list
        ',args-option ',mc-class ',mct-spec (sb-c:source-location)))))

(defun load-long-defcombin
    (name documentation function lambda-list args-lambda-list
     mc-class mct-spec
     source-location
     &aux (mc-class (find-class mc-class))
          (mct-class (find-class (car mct-spec))))
  (unless (subtypep mc-class 'long-method-combination)
    (method-combination-error
     "Invalid method combination class: ~A.~%~
      When defining a method combination type in long form, the provided~%~
      method combination class must be a subclass of LONG-METHOD-COMBINATION."
     mc-class))
  (unless (subtypep mct-class 'long-method-combination-type)
    (method-combination-error
     "Invalid method combination type class: ~A.~%~
      When defining a method combination type in long form, the provided~%~
      method combination type class must be a subclass of
      LONG-METHOD-COMBINATION-TYPE."
     mct-class))
  ;; #### NOTE: we can't change-class class metaobjects, so we need to
  ;; recreate a brand new one.
  ;; -- didier
  (let ((new (apply #'make-instance mct-class
                    'source source-location
                    :direct-superclasses (list mc-class)
                    :documentation documentation
                    :type-name name
                    :lambda-list lambda-list
                    :args-lambda-list args-lambda-list
                    :function function
                    (cdr mct-spec))))
    (setf (slot-value new '%constructor)
          (lambda (options) (funcall #'make-instance new :options options)))
    (load-defcombin name new documentation)))

(defmethod compute-effective-method
    ((function generic-function)
     (combination long-method-combination)
     applicable-methods)
  "Call the long method COMBINATION type's specific function."
  (funcall (long-method-combination-type-%function (class-of combination))
    function combination applicable-methods))

(defun make-long-method-combination-function
       (type-name ll method-group-specifiers args-option gf-var body)
  (declare (ignore type-name))
  (multiple-value-bind (real-body declarations documentation)
      (parse-body body t)
    (let ((wrapped-body
            (wrap-method-group-specifier-bindings method-group-specifiers
                                                  declarations
                                                  real-body)))
      (when gf-var
        (push `(,gf-var .generic-function.) (cadr wrapped-body)))

      (when args-option
        (setq wrapped-body (deal-with-args-option wrapped-body args-option)))

      (when ll
        (setq wrapped-body
              `(apply #'(lambda ,ll ,wrapped-body)
                      (method-combination-options .method-combination.))))

      (values
        documentation
        `(lambda (.generic-function. .method-combination. .applicable-methods.)
           (declare (ignorable .generic-function.
                     .method-combination. .applicable-methods.))
           (block .long-method-combination-function. ,wrapped-body))))))

(define-condition long-method-combination-error
    (reference-condition simple-error)
  ()
  (:default-initargs
   :references '((:ansi-cl :macro define-method-combination))))

;;; NOTE:
;;;
;;; The semantics of long form method combination in the presence of
;;; multiple methods with the same specializers in the same method
;;; group are unclear by the spec: a portion of the standard implies
;;; that an error should be signalled, and another is more lenient.
(defun group-cond-clause (name tests specializer-cache order-matters-test)
  (let ((maybe-error-clause
          `(if (and ,order-matters-test
                    (equal ,specializer-cache .specializers.)
                    (not (null .specializers.)))
               (return-from .long-method-combination-function.
                 '(error 'long-method-combination-error
                   :format-control "More than one method of type ~S ~
                                       with the same specializers."
                   :format-arguments (list ',name)))
               (setq ,specializer-cache .specializers.))))
    `((or ,@tests)
      ,maybe-error-clause
      (push .method. ,name))))

(defun wrap-method-group-specifier-bindings
    (method-group-specifiers declarations real-body)
  (let (names specializer-caches cond-clauses required-checks order-vars order-cleanups)
    (let ((nspecifiers (length method-group-specifiers)))
      (dolist (method-group-specifier method-group-specifiers
               (push `(t (return-from .long-method-combination-function.
                           `(invalid-method-error , .method.
                             "~@<is applicable, but does not belong ~
                              to any method group~@:>")))
                     cond-clauses))
        (multiple-value-bind (name tests description order required)
            (parse-method-group-specifier method-group-specifier)
          (declare (ignore description))
          (let* ((specializer-cache (gensym))
                 (order-var (gensym "O"))
                 (order-constantp (constantp order))
                 (order-value (and order-constantp (constant-form-value order))))
            (push name names)
            (push specializer-cache specializer-caches)
            (unless order-constantp
              (push `(,order-var ,order) order-vars))
            (let ((order-matters-test
                    (cond
                      ;; It is reasonable to allow a single method
                      ;; group of * to bypass all rules, as this is
                      ;; explicitly stated in the standard.
                      ((and (eq (cadr method-group-specifier) '*)
                            (= nspecifiers 1))
                       nil)
                      ;; an :ORDER value known at compile-time to be
                      ;; NIL (an SBCL extension) also bypasses the
                      ;; ordering checks.  (Other :ORDER values do
                      ;; not.)
                      (order-constantp (not (eql order-value nil)))
                      ;; otherwise, check the ORDER value at
                      ;; method-combination time, bypassing ordering
                      ;; checks if it is NIL.
                      (t `(not (eql ,order-var nil))))))
              (push (group-cond-clause name tests specializer-cache order-matters-test)
                    cond-clauses))
            (when required
              (push `(when (null ,name)
                      (return-from .long-method-combination-function.
                        '(error 'long-method-combination-error
                          :format-control "No ~S methods."
                          :format-arguments (list ',name))))
                    required-checks))
            (cond
              ((and order-constantp (eq order-value :most-specific-first))
               (push `(setq ,name (nreverse ,name)) order-cleanups))
              ((and order-constantp
                    (or (null order-value) (eq order-value :most-specific-last))))
              (t (push `(ecase ,order-var
                          (:most-specific-first (setq ,name (nreverse ,name)))
                          ((nil :most-specific-last)))
                       order-cleanups))))))
      `(let (,@(nreverse names) ,@specializer-caches ,@order-vars)
        (declare (ignorable ,@specializer-caches))
        ,@declarations
        (dolist (.method. .applicable-methods.)
          (let ((.qualifiers. (method-qualifiers .method.))
                (.specializers. (method-specializers .method.)))
            (declare (ignorable .qualifiers. .specializers.))
            (cond ,@(nreverse cond-clauses))))
        ,@(nreverse required-checks)
        ,@(nreverse order-cleanups)
        ,@real-body))))

(defun parse-method-group-specifier (method-group-specifier)
  (unless (symbolp (car method-group-specifier))
    (%program-error "~@<Method group specifiers in the long form of ~S ~
                     must begin with a symbol.~:@>" 'define-method-combination))
  (let* ((name (pop method-group-specifier))
         (patterns ())
         (tests
           (let (collect)
             (block collect-tests
               (loop
                 (if (or (null method-group-specifier)
                         (memq (car method-group-specifier)
                               '(:description :order :required)))
                     (return-from collect-tests t)
                     (let ((pattern (pop method-group-specifier)))
                       (push pattern patterns)
                       (push (parse-qualifier-pattern name pattern)
                             collect)))))
             (nreverse collect))))
    (when (null patterns)
      (%program-error "~@<Method group specifiers in the long form of ~S ~
                       must have at least one qualifier pattern or predicate.~@:>"
                      'define-method-combination))
    (values name
            tests
            (getf method-group-specifier :description
                  (make-default-method-group-description patterns))
            (getf method-group-specifier :order :most-specific-first)
            (getf method-group-specifier :required nil))))

(defun parse-qualifier-pattern (name pattern)
  (cond ((eq pattern '()) `(null .qualifiers.))
        ((eq pattern '*) t)
        ((symbolp pattern) `(,pattern .qualifiers.))
        ((listp pattern) `(qualifier-check-runtime ',pattern .qualifiers.))
        (t (error "In the method group specifier ~S,~%~
                   ~S isn't a valid qualifier pattern."
                  name pattern))))

(defun qualifier-check-runtime (pattern qualifiers)
  (loop (cond ((and (null pattern) (null qualifiers))
               (return t))
              ((eq pattern '*) (return t))
              ((and pattern qualifiers
                    (or (eq (car pattern) '*)
                        (eq (car pattern) (car qualifiers))))
               (pop pattern)
               (pop qualifiers))
              (t (return nil)))))

(defun make-default-method-group-description (patterns)
  (if (cdr patterns)
      (format nil
              "methods matching one of the patterns: ~{~S, ~} ~S"
              (butlast patterns) (car (last patterns)))
      (format nil
              "methods matching the pattern: ~S"
              (car patterns))))

;;; This baby is a complete mess. I can't believe we put it in this
;;; way. No doubt this is a large part of what drives MLY crazy.
;;;
;;; At runtime (when the effective-method is run), we bind an intercept
;;; lambda-list to the arguments to the generic function.
;;;
;;; At compute-effective-method time, the symbols in the :arguments
;;; option are bound to the symbols in the intercept lambda list.
(defun deal-with-args-option (wrapped-body args-lambda-list)
  (binding* (((llks required optional rest key aux env whole)
              (parse-lambda-list
               args-lambda-list
               :context "a define-method-combination arguments lambda list"
               :accept (lambda-list-keyword-mask '(&allow-other-keys &aux &key &optional &rest &whole)))))
    (check-lambda-list-names llks required optional rest key aux env whole
                             :context "a define-method-combination arguments lambda list"
                             :signal-via #'%program-error)
    (let (intercept-rebindings)
      (flet ((intercept (sym) (push `(,sym ',sym) intercept-rebindings)))
        (when whole (intercept (car whole)))
        (dolist (arg required)
          (intercept arg))
        (dolist (arg optional)
          (multiple-value-bind (name default suppliedp)
              (parse-optional-arg-spec arg)
            (declare (ignore default))
            (intercept name)
            (when suppliedp (intercept (car suppliedp)))))
        (when rest (intercept (car rest)))
        (dolist (arg key)
          (multiple-value-bind (keyword name default suppliedp)
              (parse-key-arg-spec arg)
            (declare (ignore keyword default))
            (intercept name)
            (when suppliedp (intercept (car suppliedp)))))
        (dolist (arg aux)
          (intercept (if (consp arg) (car arg) arg)))
        ;; cosmetic only
        (setq intercept-rebindings (nreverse intercept-rebindings)))
      ;; This assumes that the head of WRAPPED-BODY is a let, and it
      ;; injects let-bindings of the form (ARG 'SYM) for all variables
      ;; of the argument-lambda-list; SYM is a gensym.
      (aver (memq (first wrapped-body) '(let let*)))
      (setf (second wrapped-body)
            (append intercept-rebindings (second wrapped-body)))
      ;; Be sure to fill out the args lambda list so that it can be too
      ;; short if it wants to.
      (unless (or (memq '&rest args-lambda-list)
                  (memq '&allow-other-keys args-lambda-list))
        (let ((aux (memq '&aux args-lambda-list)))
          (setq args-lambda-list
                (append (ldiff args-lambda-list aux)
                        (if (memq '&key args-lambda-list)
                            '(&allow-other-keys)
                            '(&rest .ignore.))
                        aux))))
      ;; .GENERIC-FUNCTION. is bound to the generic function in the
      ;; method combination function, and .GF-ARGS* is bound to the
      ;; generic function arguments in effective method functions
      ;; created for generic functions having a method combination that
      ;; uses :ARGUMENTS.
      ;;
      ;; The DESTRUCTURING-BIND binds the parameters of the
      ;; ARGS-LAMBDA-LIST to actual generic function arguments.  Because
      ;; ARGS-LAMBDA-LIST may be shorter or longer than the generic
      ;; function's lambda list, which is only known at run time, this
      ;; destructuring has to be done on a slighly modified list of
      ;; actual arguments, from which values might be stripped or added.
      ;;
      ;; Using one of the variable names in the body inserts a symbol
      ;; into the effective method, and running the effective method
      ;; produces the value of actual argument that is bound to the
      ;; symbol.
      `(let ((inner-result. ,wrapped-body)
             (gf-lambda-list (generic-function-lambda-list .generic-function.)))
         `(destructuring-bind ,',args-lambda-list
              ;; FIXME: we know enough (generic function lambda list,
              ;; args lambda list) at generate-effective-method-time
              ;; that we could partially evaluate this frobber, to
              ;; inline specific argument list manipulation rather
              ;; than the generic code currently contained in
              ;; FROB-COMBINED-METHOD-ARGS.
              (frob-combined-method-args
               .gf-args. ',gf-lambda-list
               ,',(length required) ,',(length optional))
            ,,(when (memq '.ignore. args-lambda-list)
                ''(declare (ignore .ignore.)))
            ;; If there is a &WHOLE in the args-lambda-list, let
            ;; it result in the actual arguments of the generic-function
            ;; not the frobbed list.
            ,,(when whole
                ``(setq ,',(car whole) .gf-args.))
            ,inner-result.)))))

;;; Partition VALUES into three sections: required, optional, and the
;;; rest, according to required, optional, and other parameters in
;;; LAMBDA-LIST.  Make the required and optional sections NREQ and
;;; NOPT elements long by discarding values or adding NILs, except
;;; don't extend the optional section when there are no more VALUES.
;;; Value is the concatenated list of required and optional sections,
;;; and what is left as rest from VALUES.
(defun frob-combined-method-args (values lambda-list nreq nopt)
  (loop with section = 'required
        for arg in lambda-list
        if (memq arg lambda-list-keywords) do
          (setq section arg)
          (unless (eq section '&optional)
            (loop-finish))
        else if (eq section 'required)
          count t into nr
          and collect (pop values) into required
        else if (and values (eq section '&optional))
          count t into no
          and collect (pop values) into optional
        finally
          (flet ((frob (list n m lengthenp)
                   (cond ((> n m) (butlast list (- n m)))
                         ((and (< n m) lengthenp) (nconc list (make-list (- m n))))
                         (t list))))
            (return (nconc (frob required nr nreq t)
                           (frob optional no nopt values)
                           values)))))




;; ===========
;; Entry Point
;; ===========

;; FIXME: according to ANSI 3.4.10 this is supposed to allow &WHOLE in the
;; long syntax. But it clearly does not, because if you write (&WHOLE v) then
;; you get (LAMBDA (&WHOLE V ...) ...) which is illegal

;; #### NOTE: according to Section 1.6 Language Extensions of the standard,
;; extending DEFINE-METHOD-COMBINATION with the :method-combination-class and
;; :method-combination-type-class options is conformant (it does not alter the
;; behavior of conforming code, and it is not explicitly prohibited).
;; -- didier

(defmacro define-method-combination (&whole form name . args)
  (declare (ignore args))
  (check-designator name 'define-method-combination)
  `(progn
     (with-single-package-locked-error
         (:symbol ',name "defining ~A as a method combination"))
     ,(if (and (cddr form)
               (listp (caddr form)))
        (expand-long-defcombin form)
        (let* ((type-name (cadr form))
               (doc (getf (cddr form) :documentation (make-unbound-marker)))
               (ioa (getf (cddr form) :identity-with-one-argument nil))
               (operator (getf (cddr form) :operator type-name))
               (mc-class (getf (cddr form) :method-combination-class
                               'short-method-combination))
               (mct-class (getf (cddr form) :method-combination-type-class
                                'short-method-combination-type)))
          (unless (or (unbound-marker-p doc) (stringp doc))
            (%program-error
             "~@<~S argument to the short form of ~S must be a string.~:@>"
             :documentation 'define-method-combination))
          `(load-short-defcombin ',type-name ',operator ',ioa
                                 ,(unless (unbound-marker-p doc) doc)
                                 ',mc-class ',mct-class
                                 (sb-c:source-location))))))




;; ========================
;; Infrastructure Injection
;; ========================

;; #### NOTE: here, we need to take care of converting the two early method
;; combinations that were defined in the bootstrap phase (STANDARD and OR).
;; -- didier

(defun substitute-method-combination (new old)
  "Substitute NEW for OLD method combination.
OLD is an early method combination object (either the STANDARD or the OR one).
NEW is the corresponding full-blown object in the complete infrastructure.
This function transfers the generic functions cache from the old to the new
object, and updates all such generic functions to point to the new method
combination object."
  (setf (slot-value new '%generic-functions)
        (method-combination-%generic-functions old))
  (map-hashset (lambda (gf)
                 (setf (generic-function-method-combination gf) new))
               (method-combination-%generic-functions new)))


;; ---------------------------
;; Standard method combination
;; ---------------------------

(let* ((class (find-class 'standard-standard-method-combination))
       (instance (make-instance class)))
  (setf (slot-value class 'type-name) 'standard)
  (setf (slot-value class '%constructor)
        (lambda (options)
          (when options
            (method-combination-error
             "The standard method combination accepts no options."))
          instance))
  (setf (gethash nil (method-combination-type-%cache class)) instance)
  (setf (gethash 'standard **method-combination-types**) class)
  (substitute-method-combination instance *standard-method-combination*)
  (setq *standard-method-combination* instance))


;; ------------------------------------
;; Built-in (short) method combinations
;; ------------------------------------

;;; The built-in method combination types as taken from page 1-31 of 88-002R.

(define-method-combination +      :identity-with-one-argument t)
(define-method-combination and    :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list   :identity-with-one-argument nil)
(define-method-combination max    :identity-with-one-argument t)
(define-method-combination min    :identity-with-one-argument t)
(define-method-combination nconc  :identity-with-one-argument t)
(define-method-combination progn  :identity-with-one-argument t)
(define-method-combination or     :identity-with-one-argument t)

(let* ((or-class (find-method-combination-type 'or))
       (or-instance (funcall (method-combination-%constructor or-class)
                      '(:most-specific-first))))
  (setf (gethash '(:most-specific-first)
                 (method-combination-type-%cache or-class))
        or-instance)
  (substitute-method-combination or-instance *or-method-combination*)
  (setq *or-method-combination* or-instance))
