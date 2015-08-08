(ns clot.core)

(defn- push
  "Push op onto the result stack, combining things where possible"
  [stack op]
  (let [top (peek stack)]
    (cond
     (nil? op) stack
     (and (number? op) (zero? op)) stack
     (and (string? op) (empty? op)) stack
     (and (map? op) (= (:d op) 0)) stack
     (and (string? top) (string? op)) (conj (pop stack) (str top op))
     (and (number? top) (number? op)) (conj (pop stack) (+ top op))
     :default (conj stack op))))

(defn- swap
  "Swap the top of the stack with val"
  [stack val]
  (push (pop stack) val))

(defn- append
  "Helper function to append an op"
  [[left right] op]
  [(if (string? op) ;in case of an insert, add skips on other side
     (push left (count op))
     left)
   (push right op)])

(defn transform
  "Transform 2 operations"
  [client server]
  ; build stacks for client and server ops
  (loop [client (vec (reverse client))
         server (vec (reverse server))
         left []
         right []]
    (let [cop (peek client) ;client op
          sop (peek server)] ;server op
      (if cop
        (cond
         ;; no more server ops, so just append the client op
         (nil? sop) (let [[left right] (append [left right] cop)]
                      (recur
                       (pop client)
                       server
                       left
                       right))
         ;; server inserts always win
         (string? sop) (recur
                        client
                        (pop server)
                        (push left sop)
                        (push right (count sop)))
         ;; client skip
         (number? cop) (cond
                        ;; if server op is also a skip, skip however many chars
                        ;; overlap
                        (number? sop) (let [skp (min cop sop)]
                                        (recur
                                         (push (pop client) (- cop skp))
                                         (push (pop server) (- sop skp))
                                         (push left skp)
                                         (push right skp)))
                        ;; if server is a delete, there's no need to skip over
                        ;; chars that are already deleted. So lower the skip
                        ;; with the number of deleted chars. If the delete is
                        ;; longer than the skip there's no need to skip at all
                        (map? sop) (let [cop (max (- cop (:d sop)) 0)]
                                     (recur
                                      (push (pop client) cop)
                                      (pop server)
                                      (push left sop)
                                      right)))
         ;; Just insert the string and skip characters on the other side
         (string? cop) (recur
                        (pop client)
                        server
                        (push left (count cop))
                        (push right cop))
         ;; Handle client side deletes. We've already covered server side
         ;; inserts, so here we only need to deal with server side skips and
         ;; deletes. No need to skip or delete chars that have already been
         ;; deleted, so lower the server side by the number of deleted chars
         ;; on the client side.
         (map? cop) (let [sop (if (number? sop)
                                (max (- sop (:d cop)) 0)
                                {:d (max (- (:d sop)
                                            (:d cop)) 0)})]
                      (recur
                       (pop client)
                       (push (pop server) sop)
                       left
                       (push right cop))))
        ;; Append any trailing server side ops
        (reverse (reduce append [right left] (reverse server)))))))

(defn compose
  "Compose 2 ops"
  [op1 op2]
  (loop [op1 (vec (reverse op1)) ;convert ops to stacks
         op2 (vec (reverse op2))
         res []]
    (let [c1 (peek op1)
          c2 (peek op2)]
      (println op1 op2 res)
      (if c1
        (cond
         ;; No more ops in op2
         (nil? c2) (recur
                    (pop op1)
                    op2
                    (push res c1))
         ;; Delete
         (map? c1) (recur
                    (pop op1)
                    op2
                    (push res c1))
         ;; Insert
         (string? c1) (cond
                       (number? c2) (recur
                                     (pop op1)
                                     (swap op2 (max (- c2 (count c1)) 0))
                                     (push res c1))
                       (string? c2) (recur
                                     (pop op1)
                                     op2
                                     (push res c1))
                       (map? c2) (let [len (min (:d c2) (count c1))
                                       c1 (subs c1 len)
                                       c2 {:d (- (:d c2) len)}]
                                   (recur
                                    (swap op1 c1)
                                    (swap op2 c2)
                                    res)))
         (number? c1) (cond
                       (number? c2) (let [skp (min c1 c2)]
                                      (recur
                                       (swap op1 (- c1 skp))
                                       (swap op2 (- c2 skp))
                                       (push res skp)))
                       (string? c2) (recur
                                     op1
                                     (pop op2)
                                     (push res c2))
                       (map? c2) (let [skp (min (:d c2) c1)]
                                   (recur
                                    (swap op1 (- c1 skp))
                                    (swap op2 {:d (- (:d c2) skp)})
                                    (push res {:d skp})))))
        (concat res (reverse op2))))))
