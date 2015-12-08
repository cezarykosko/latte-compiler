(ns latte-compiler.util
  )

(defn println-err
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn println-ip-meta
  [obj]
  (let [m (meta obj)]
    (binding [*out* *err*]
      (println (str "line " (:instaparse.gll/start-line m) ", column " (:instaparse.gll/start-column m))))))

(defprotocol CompilationPhase
  "temporary result of compilation"
  (successful [this] "true if no violations occured during this phase")
  (output [this] "output to be passed to another phase")
  )

(defn apply-phase
  [fun prev-phase]
  (if (successful prev-phase)
    (apply fun (output prev-phase))
    ))
