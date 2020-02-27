#lang racket

(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)

(require racket/set)
(require racket/undefined)

(provide tla)

(define (by-decreasing-length x y) (> (string-length x) (string-length y)))

(define tla-symbols #hash(("/\\"          . "∧")  ("\\/"          . "∨")  ("=>"        . "⇒")
                          ("~"            . "¬")  ("<=>"          . "≡")  ("=="        . "≜")
                          ("\\in"         . "∈")  ("\\notin"      . "∉")  ("/="        . "≠")
                          ("<<"           . "⟨")  (">>"           . "⟩")  ("[]"        . "☐")
                          ("<"            . "<")  (">"            . ">")  ("<>"        . "◇")
                          ("=<"           . "≤")  (">="           . "≥")  ("~>"        . "⇝")
                          ("\\ll"         . "⟪")  ("\\gg"         . "⟫")  #;("-+->"     . "?")
                          ("\\prec"       . "≺")  ("\\succ"       . "≻")  ("|->"       . "↦")
                          ("\\preceq"     . "≼")  ("\\succeq"     . "≽")  ("\\div"     . "÷")
                          ("\\subseteq"   . "⊆")  ("\\supseteq"   . "⊇")  ("\\cdot"    . "·")
                          ("\\subset"     . "⊂")  ("\\supset"     . "⊃")  ("\\o"       . "◦")
                          ("\\sqsubset"   . "⊏")  ("\\sqsupset"   . "⊐")  ("\\bullet"  . "•")
                          ("\\sqsubseteq" . "⊑")  ("\\sqsupseteq" . "⊒")  ("\\star"    . "⋆")
                          ("|-"           . "⊢")  ("-|"           . "⊣")  ("\\bigcirc" . "◯")
                          ("|="           . "⊨")  #;("=|"         . "?")  ("\\sim"     . "∼")
                          ("->"           . "→")  ("<-"           . "←")  ("\\simeq"  . "≃")
                          ("\\cap"        . "∩")  ("\\cup"        . "∪")  ("\\asymp"   . "≍")
                          ("\\sqcap"      . "⊓")  ("\\sqcup"      . "⊔")  ("\\approx"  . "≈")
                          ("(+)"          . "⊕")  ("\\uplus"      . "⨄")  ("\\cong"    . "≅")
                          ("(-)"          . "⊖")  ("\\times"      . "×")  ("\\doteq"   . "≐")
                          ("\\E"          . "∃")  ("\\A"          . "∀")  ("\\"        . "\\")
                                                  ))

(define tla-keywords '("MODULE"
                       "EXTENDS"
                       "CONSTANT" "CONSTANTS"
                       "VARIABLE" "VARIABLES"
                       "ASSUME"
                       "INSTANCE" "WITH"
                       "THEOREM" "LEMMA" "PROOF" "BY"
                       "LET" "IN"
                       "EXCEPT"
                       "BOOLEAN" "TRUE" "FALSE"
                       "IF" "THEN" "ELSE"
                       "CHOOSE"
                       "CASE" "OTHER"
                       "ENABLED" "UNCHANGED"
                       "DOMAIN" "SUBSET" "UNION"
                       "STRING"))

(define (alt-parsers lst)
  (map (compose try/p string/p) (sort lst by-decreasing-length)))

(define alnum/p
   (do [hd <- letter/p]
       [tl <- (many/p (or/p letter/p digit/p (char/p #\')))]
     (pure (list->string (cons hd tl)))))

(define symbol-parsers (alt-parsers (hash-keys tla-symbols)))
(define symbol/p
  (do [sym <- (apply or/p symbol-parsers)]
    (pure (cons 'sym sym))))

(define keyword-parsers (alt-parsers tla-keywords))
(define keyword/p
  (do [kwd <- (apply or/p keyword-parsers)]
    (pure (cons 'kwd kwd))))

(define number/p
  (do [n <- integer/p]
    (pure (cons 'num (number->string n)))))

(define quoted-string/p
  (do [char/p #\"]
      [s <- (many/p (char-not/p #\"))]
      [char/p #\"]
    (pure (cons 'str (list->string s)))))

(define identifier/p
  (do [id <- alnum/p]
    (pure (cons 'id id))))

(define punct/p
  (do [p <- (char-in/p "(){}[],.:!+-='_")]
    (pure (cons 'pun (string p)))))

(define tuple/p
  (do [lb <- (string/p "<<")]
      [tu <- (many/p (char-not/p #\>))]
      [rb <- (string/p ">>")]
    (pure (string-join (list lb (list->string tu) rb) ""))))

(define stok/p
  (do [many/p space/p]
      [or/p (try/p (syntax-box/p symbol/p))
            (try/p (syntax-box/p punct/p))
            (try/p (syntax-box/p keyword/p))
            (try/p (syntax-box/p number/p))
            (try/p (syntax-box/p quoted-string/p))
            (try/p (syntax-box/p identifier/p))]))

(define subscript/p
  (do [char/p #\_]
      [s <- (or/p (try/p tuple/p) (try/p alnum/p))]
    (pure (cons 'sub (parse-result! (parse-string (many/p stok/p) s))))))

(define spaces/p
  (do [s <- (many+/p space/p)]
      (pure (cons 'spc (list->string s)))))


(define ttok/p
  (do [many/p space/p]
      [or/p (try/p (syntax-box/p symbol/p))
            (try/p (syntax-box/p subscript/p))
            (try/p (syntax-box/p punct/p))
            (try/p (syntax-box/p keyword/p))
            (try/p (syntax-box/p number/p))
            (try/p (syntax-box/p quoted-string/p))
            (try/p (syntax-box/p identifier/p))
            ]))

(define (line->tokens line)
  ;; parse a line into a series of tokens, ignoring whitespace
  (parse-result! (parse-string (many/p ttok/p) line)))

(define (lines->tokens lines)
  (map line->tokens lines))

(define (token-start token)
  (srcloc-column (syntax-box-srcloc token)))

(define (token-length token)
  (srcloc-span (syntax-box-srcloc token)))

(define (token-end token)
  (sub1 (+ (token-start token) (token-length token))))

(define (token-text token)
  (syntax-box-datum token))

(define (first-column line)
  (token-start (car line)))

(define nbsp  (string->symbol "nbsp"))
(define ldquo (string->symbol "ldquo"))
(define rdquo (string->symbol "rdquo"))

(define (token->html token)
  (match (syntax-box-datum token)
    [ `(id  . ,identifier) `(i ,identifier)]
    [ `(sym . ,symbol)      (hash-ref tla-symbols symbol)]
    [ `(num . ,n)           n]
    [ `(kwd . ,kwd)        `(span ((class "tla-kwd")) ,kwd)]
    [ `(sub . ,sub-tokens) `(sub ,@(tokens->html sub-tokens)) ]
    [ `(str . ,q-string)   `(span ,ldquo ,q-string ,rdquo)]
    [ `(,_  . ,text)        text]))

(define (tokens->html tokens)
  ;; render a sequence of tokens as html, inserting whitespace
  ;; where necessary
  (let loop ([tokens tokens] [elements empty])
    (match tokens
      [`() (reverse elements)]
      [`(,curr . ,tail)
       (let ([element (token->html curr)])
         (if (empty? tail)
             (loop tail (cons element elements))
             (let ([end-pos (+ (token-start curr) (token-length curr))]
                   [next-pos (token-start (car tail))])
               (if (= next-pos end-pos)
                   (loop tail (cons element elements))
                   (loop tail (cons `(span ,element ,nbsp) elements))))))])))

(define (split-token token at)
  ;; Split a single token into two at the specified column
  (match token
    [(syntax-box (cons type str) (srcloc src line start _ span))
     (let* ([end  (- (string-length str) (- (+ start span) at))]
            [pre  (substring str 0 end)]
            [post (substring str end)])
       (values (syntax-box (cons type pre) (srcloc src line start (add1 start) (string-length pre)))
               (syntax-box (cons type post) (srcloc src line end (add1 end) (string-length post)))))]))

(define (split-tokens tokens col)
  ;; Split a sequence of tokens into two at the specified column
  (let-values ([(pre post) (splitf-at tokens (λ (t) (> col (token-start t))))])
    (if (empty? pre)
        (values pre post)
        (let* ([prec (last pre)]
               [type (car (syntax-box-datum prec))])
          (if (and (not (or (eq? 'sym type) (eq? 'sub type))) ; ignore tla-symbols and subscripts
                   (>= (token-end prec) col))
              (let-values ([(s-before s-after) (split-token prec col)])
                (values (append (drop-right pre 1) (list s-before)) (cons s-after post)))
              (values pre post))))))

(define (maybe-insert-space pre post)
  ;; insert a hard space between two tokens that have whitespace between them
  (if (or (empty? pre)
          (empty? post)
          (= (- (token-start (first post)) (token-end (last pre))) 1))
      empty
      (list nbsp)))

(define (find-line-alignments line)
  ;; Any token preceded by more than one space is considered
  ;; an alignment (although it might be ignored later - see
  ;; is-aligned-token?)
  (for/fold ([alignments '()] [last-end-pos 0]  #:result alignments)
            ([token line])
    (let* ([start-pos  (token-start token)]
           [end-pos    (+ start-pos (token-length token))]
           [num-spaces (- start-pos last-end-pos)])
      (if (> num-spaces 1)
          (values (cons start-pos alignments) end-pos)
          (values alignments end-pos)))))

(define (find-block-alignments block)
  ;; Find all potential alignments in a block of lines
  (for/fold ([alignments '()] #:result (sort alignments <))
            ([line block])
    (set-union alignments (find-line-alignments line))))

(define current-block (make-parameter undefined))
(define current-alignments (make-parameter undefined))

(define (is-aligned-token? column token line)
  ;; Only certain alignments are valid
  (or
   ;; is leading token in line
   (and (eq? token (first line))
        (eq? column (token-start token)))

   ;; is leading token in following line
   (let ([tail (memq line (current-block))])
     (and (> (length tail) 1)
          (eq? column (first-column (second tail)))))

   ;; is a symbol, with a matching symbol on another line
   (match (token-text token)
     [`(sym . ,sym) (and (not (and (eq? token (last line))
                                   (equal? sym "==")))
                         (for*/or ([other-line (current-block)]
                                   [other-token other-line])
                           (and (not (eq? token other-token))
                                (equal? token other-token))))]
     [_ #f])))

(define (align-line-tokens line)
  ;; Take a sequence of tokens and group them into divs that form
  ;; the content of the grid.
  (define/match (increase-span div) [(`(div (,start . ,end) ,content)) `(div (,start . ,(add1 end)) ,content)])
  (define/match (new-div-after div) [(`(div (,_ . ,end) ,_)) `(div (,(add1 end) . ,(add1 end)) ())])
  (define/match (append-token div token) [(`(div ,dims ,content) _) `(div ,dims ,(append content (list token)))])
  (define (append-div divs div) (if (empty? (third div)) divs (append divs (list div))))

  (let loop ([tokens line] [cols (current-alignments)] [div '(div (1 . 1) ())] [divs empty])
    (if (empty? tokens)
        (append-div divs div)

        (if (empty? cols)
            (loop (rest tokens) cols (increase-span (append-token div (first tokens))) divs)
            (let ([col (first cols)]
                  [token (first tokens)])
              (cond
                ;; token is a non-symbol that spans the current column
                [(and (not (eq? 'sym (car (token-text token))))
                      (<  (token-start token) col)
                      (>= (token-end   token) col))
                 ;(println "span and split")
                 (if (is-aligned-token? col token line)
                     (let-values ([(pre post) (split-token token col)])
                       (loop `(,pre ,post ,@(rest tokens)) cols div divs))
                     (loop (rest tokens) (rest cols) (increase-span (append-token div token)) divs))]

                ;; token is aligned to the column
                [(= (token-start token) col)
                 ;; (println (format "aligned: ~a" token))
                 (if (is-aligned-token? col token line)
                     (loop (rest tokens) (rest cols) (append-token (new-div-after div) token) (append-div divs div))
                     (loop (rest tokens) (rest cols) (increase-span (append-token div token)) divs))]

                ;; token before current column
                [(< (token-end token) col)
                 ;;(println (format "before: ~a" token))
                 (loop (rest tokens) cols (append-token div token) divs)]

                ;; token after current column
                [(> (token-start token) col)
                 ;; (println (format "after: ~a ~a" col token))
                 (loop tokens (rest cols) (new-div-after div) (append-div divs div))]

                ;; shouldn't get here
                [else (raise-user-error "Unexpected token position")]))))))

(define (line->html line line-no)
  (for/fold ([divs empty] [next-pos #f] #:result (reverse divs))
            ([track line])
    (match track
      [`(div (,start . ,end) ,tokens)
       (let* ([content (tokens->html tokens)]
              [div    `(div ((style ,(format "grid-row: ~a / ~a; grid-column: ~a / ~a;" line-no line-no start end)))
                            ,@content)])
         (let ([divs (if (and next-pos (> (token-start (first tokens)) next-pos))
                         (match (first divs)
                           [`(div ,style ,@content)
                            (cons `(div ,style ,@(append content (list nbsp))) (rest divs))])
                         divs)])
           (values (cons div divs) (add1 (token-end (last tokens))))))])))

(define (block->html block alignments)
  (case (length block)
    [(0)  empty]
    [(1)  `(div ,@(tokens->html (first block)))]
    [else
     (parameterize* ([current-block block]
                     [current-alignments alignments])
       (if (equal? '(sym . "==") (token-text (last (first block))))
           (let-values ([(pre post) (split-tokens (first block) (first (current-alignments)))])
             `(div ((class "tla-grid") (style "grid-template-columns: auto auto;"))
                   (div ,@(tokens->html pre) ,@(maybe-insert-space pre post))
                   (div ,@(tokens->html post))
                   (div)
                   (div ,(block->html (rest (current-block)) (rest (current-alignments))))))
           (let ([line-count (length (current-block))]
                 [column-count (add1 (length (current-alignments)))])
             (for/fold ([divs empty]
                        #:result `(div ((class "tla-grid")
                                        (style ,(string-append
                                                 (format "grid-template-rows: repeat(~a, auto); " line-count)
                                                 (format "grid-template-columns: repeat(~a, auto);" column-count))))
                                       ,@divs))
                       ([line-text block]
                        [line-number (in-range 1 (add1 line-count))])
               (append divs (line->html (align-line-tokens line-text) line-number))))))]))

(define (render-group group)
  (let ([block (lines->tokens group)])
    (block->html block (find-block-alignments block))))

(define (tla . elems)
  (let* ([single-line (string-join elems "")]
         [blocks (string-split single-line "\n\n")]
         [groups (map (lambda (b) (string-split b "\n")) blocks)])
    `(div ((class "tla-pretty")) ,@(map render-group (filter-not empty? groups)))))

